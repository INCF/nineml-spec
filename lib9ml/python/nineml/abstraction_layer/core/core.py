"""
Python module for reading 9ML abstraction layer files in XML format.

Copyright Andrew P. Davison, Eilif B. Muller, 2010, Mike Hull 2011 # if you edit this file, add your name here
"""


from operator import and_
from expressions import *
from conditions import *
from ports import *
from ..xmlns import *

import nineml.utility


from itertools import chain



class UnimplementedError(RuntimeError):
    pass



class Transition(object):

    def __init__(self,state_assignments=None, event_outputs=None, target_regime_name=None):
        if target_regime_name:
            assert isinstance(target_regime_name, basestring)

        self._state_assignments = state_assignments or []
        self._event_outputs = event_outputs or [] 

        self._target_regime_name = target_regime_name
        self._source_regime_name = None

        # Set later, once attached to a regime:
        self._target_regime = None
        self._source_regime = None
    

    def set_source_regime(self, source_regime):
        assert isinstance( source_regime, Regime)
        assert not self._source_regime
        if self._source_regime_name: 
            assert self._source_regime_name == source_regime.name
        else:
            self._source_regime_name = source_regime.name
        self._source_regime = source_regime
        
    def set_target_regime_name(self, target_regime_name):
        assert isinstance( target_regime_name, basestring)
        assert not self._target_regime
        assert not self._target_regime_name 
        self._target_regime_name = target_regime_name


    def set_target_regime(self, target_regime):
        assert isinstance( target_regime, Regime)
        if self._target_regime:
            assert self.target_regime == target_regime
            return 

        # Did we already set the target_regime_name
        if self._target_regime_name: 
            assert self._target_regime_name == target_regime.name
        else:
            self._target_regime_name = target_regime.name
        self._target_regime = target_regime
    
    @property
    def target_regime_name(self):
        if self._target_regime_name:
            assert isinstance( self._target_regime_name, basestring) 
        return self._target_regime_name
    @property
    def source_regime_name(self):
        if self._source_regime:
            assert False, 'This function should not be called by users. Use source_regime.name instead'
        assert self._source_regime_name
        return self._source_regime_name

    @property
    def target_regime(self):
        assert self._target_regime
        return self._target_regime
    @property
    def source_regime(self):
        assert self._source_regime
        return self._source_regime
    

    @property
    def state_assignments(self):
        return self._state_assignments
    
    @property
    def event_outputs(self):
        return self._event_outputs







class OnEvent(Transition):

    def AcceptVisitor(self, visitor,**kwargs):
        return visitor.VisitOnEvent(self,**kwargs)

    def __init__(self, src_port, state_assignments=None, event_outputs=None, target_regime_name=None):
        Transition.__init__(self,state_assignments=state_assignments, event_outputs=event_outputs, target_regime_name=target_regime_name)
        self._src_port = src_port

    @property
    def src_port(self):
        return self._src_port



class OnCondition(Transition):
    element_name = "OnCondition"

    def AcceptVisitor(self, visitor,**kwargs):
        return visitor.VisitOnCondition(self,**kwargs)

    def __init__(self, trigger, state_assignments=None, event_outputs=None, target_regime_name=None):
        if isinstance( trigger, Condition):    self._trigger = trigger.clone()
        elif isinstance( trigger, basestring): self._trigger = Condition( rhs = trigger )
        else:  assert False

        Transition.__init__(self,state_assignments=state_assignments, event_outputs=event_outputs, target_regime_name=target_regime_name)


    def __str__(self):
        return 'OnCondition( %s )'%self.trigger
    
    @property
    def trigger(self):
        return self._trigger

















class Regime(object):
    """
    A regime is something that contains ODEs, has temporal extent, defines a set of Transitions
    which occur based on conditions, and can be join the Regimes to other Regimes.
    """

    element_name = "Regime"
    n = 0
    
    # Visitation:
    # -------------
    def AcceptVisitor(self, visitor,**kwargs):
        return visitor.VisitRegime(self,**kwargs)


    def __init__(self, name, time_derivatives, on_events=None, on_conditions=None, transitions=None):

        self._name = name if name else "Regime%d"%Regime.n
        Regime.n = Regime.n+1

        # This is not nice, but we support passing in 'transitions', which is a list of 
        # both OnEvents and OnConditions. So lets filter this by type and add them 
        # appropriately:
        transitions = transitions or []
        fDict = nineml.utility.FilterDiscreteTypes( transitions, (OnEvent,OnCondition) ) 


        # Time Derivatives may be specified as strings:
        def strToTimeDeriv(s):
            r = re.compile(r"""\s* d(?P<var>[a-zA-Z][a-zA-Z0-9_]*)/dt \s* = \s* (?P<rhs> .*) """, re.VERBOSE) 
            m = r.match(s)
            return ODE( dependent_variable = m.groupdict()['var'], indep_variable='t', rhs=m.groupdict()['rhs'] )
        tdTypeDict = nineml.utility.FilterDiscreteTypes( time_derivatives, (basestring, ODE ) )
        tds = tdTypeDict[ODE] + [ strToTimeDeriv(o) for o in tdTypeDict[basestring] ] 


        self._time_derivatives = tds
        self._on_events = [] 
        self._on_conditions = [] 

        # Add all the OnEvents and OnConditions:
        for s in (on_events or [] ) + fDict[OnEvent] :
            self.add_on_event(s)
        for s in (on_conditions or [] ) + fDict[OnCondition]:
            self.add_on_condition(s)



    def _resolve_references_on_transition(self,transition):
        if not transition.target_regime_name:
            transition.set_target_regime(self)
        
        assert not transition._source_regime_name
        transition.set_source_regime( self )


    def add_on_event(self, on_event):
        assert isinstance(on_event, OnEvent)
        self._resolve_references_on_transition(on_event)
        self._on_events.append( on_event )

    def add_on_condition(self, on_condition):
        assert isinstance(on_condition, OnCondition)
        self._resolve_references_on_transition(on_condition)
        self._on_conditions.append( on_condition )


    def __repr__(self):
        return "%s(%s)" % (self.__class__.__name__, self.name)


    # Regime Properties:
    # ------------------
    @property
    def time_derivatives(self):
        return iter(self._time_derivatives)

    @property
    def transitions(self):
        return chain(  self._on_events, self._on_conditions )

    @property 
    def on_events(self):
        return iter(self._on_events)

    @property 
    def on_conditions(self):
        return iter(self._on_conditions)

    @property
    def name(self):
        return self._name
























# Forwarding Function:
def On( trigger, do=None, to=None ):
    if isinstance(do, basestring ): do = [do]
    elif isinstance(do, InputEvent): do = [do]
    elif do == None: do = []
    else: pass

    if isinstance( trigger, InputEvent):
        return DoOnEvent(input_event=trigger, do=do,to=to)
    elif isinstance( trigger, (OnCondition, basestring)):
        return DoOnCondition(condition=trigger, do=do,to=to)
    else:
        assert False





def doToAsssignmentsAndEvents(doList):
    if not doList: return [],[]
    # 'doList' is a list of strings, OutputEvents, and StateAssignments.
    doTypes = nineml.utility.FilterDiscreteTypes(doList, (OutputEvent,basestring, Assignment) )
    
    #Convert strings to StateAssignments:
    for s in doTypes[basestring]:
        lhs,rhs = s.split('=')
        sa = Assignment( to=lhs, expr=rhs )
        doTypes[Assignment].append(sa)
    del doTypes[basestring]

    return doTypes[Assignment], doTypes[OutputEvent]


def DoOnEvent(input_event, do=None, to=None):
    assert isinstance( input_event, InputEvent) 
    
    assignments,output_events = doToAsssignmentsAndEvents( do ) 
    return OnEvent( src_port=input_event.port,
                    state_assignments = assignments,
                    event_outputs=output_events,
                    target_regime_name = to )



def DoOnCondition( condition, do=None, to=None ):
    assignments,output_events = doToAsssignmentsAndEvents( do ) 
    return OnCondition( trigger=condition,
                        state_assignments = assignments,
                        event_outputs=output_events,
                        target_regime_name = to )








    








class Parameter(object):
    element_name = 'Parameter'

    def __init__(self, name, ):
        self.name = name

    def __str__(self):
        return "<Parameter: %s>"%(self.name)

    def AcceptVisitor(self, visitor, **kwargs):
        return visitor.VisitParameter(self, **kwargs)

class StateVariable(object):
    element_name = 'StateVariable'
    def AcceptVisitor(self, visitor, **kwargs):
        return visitor.VisitStateVariable(self, **kwargs)
    def __init__(self, name, ):
        self.name = name

    def __str__(self):
        return "<StateVariable: %s>"%(self.name)

class Dynamics(object):
    element_name = 'Dynamics'
    def __init__(self, regimes = None, aliases = None, state_variables = None):
        aliases = aliases or  []
        regimes = regimes or []
        state_variables = state_variables or []


        # Time Derivatives may be specified as strings:
        def strToAlias(s):
            lhs,rhs = s.split(':=')
            return Alias( lhs = lhs.strip(), rhs = rhs.strip() )
        aliasTD = nineml.utility.FilterDiscreteTypes( aliases, (basestring, Alias ) )
        aliases = aliasTD[Alias] + [ strToAlias(o) for o in aliasTD[basestring] ] 

        self._regimes = regimes
        self._aliases = aliases
        self._state_variables = state_variables

    def AcceptVisitor(self,visitor,**kwargs):
        return visitor.VisitDynamics(self, **kwargs)

    @property
    def regimes(self):
        return iter( self._regimes )

    @property
    def transitions(self):
        return chain( *[r.transitions for r in self._regimes] )

    @property
    def aliases(self):
        return iter( self._aliases )

    @property
    def aliases_map(self):
        return dict( [ (a.lhs,a) for a in self._aliases ] )

    @property
    def state_variables(self):
        return iter( self._state_variables )
    



# Wrapper for writing XML:
def parse(filename):
    from nineml.abstraction_layer.readers import XMLReader
    return XMLReader.read_component(filename)

