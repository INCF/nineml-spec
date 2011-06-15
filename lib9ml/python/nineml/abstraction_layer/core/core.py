"""
Python module for reading 9ML abstraction layer files in XML format.

Copyright Andrew P. Davison, Eilif B. Muller, 2010 # if you edit this file, add your name here
"""


from operator import and_
from nineml import __version__
import re
import copy
import itertools

from nineml.cache_decorator import im_cache_decorator as cache
#from nineml.abstraction_layer import math_namespace
import math_namespace
#from nineml import helpers
from expressions import *
from conditions import *
from ports import *
from cond_parse import cond_parse
from expr_parse import expr_parse
from ..xmlns import *

from nineml.utility import *


#from nineml.abstraction_layer.xml_writer import XMLWriter
#from nineml.abstraction_layer.xml_reader import XMLReader


from itertools import chain


class UnimplementedError(RuntimeError):
    pass

def dot_escape(s):

    dot_escape_table = {
    "&": "&amp;",
    '"': "&quot;",
    "'": "&apos;",
    ">": "&gt;",
    "<": "&lt;",
    "(": "&#40;",
    ")": "&#41;",
    }

    return "".join(dot_escape_table.get(c,c) for c in s)


class Reference(object):
    
    def __init__(self, cls, name):
        self.cls = cls
        self.name = name
        assert name!=None, "Got reference to name=None."
    def get_ref(self):
        """ return self """
        return self

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.cls == other.cls and self.name == other.name

    def __repr__(self):
        return "Reference(%s, name='%s')" % (self.cls.__name__, self.name)





class OnEvent(object):

    def AcceptVisitor(self, visitor,**kwargs):
        return visitor.VisitOnEvent(self,**kwargs)

    def __init__(self, src_port,state_assignments=[], event_outputs=[], target_regime=None):
        if target_regime:
            print target_regime
            assert isinstance(target_regime, basestring)

        self._src_port = src_port
        self._state_assignments = state_assignments
        self._event_outputs = event_outputs
        self._target_regime = target_regime

        self._from=None

    @property
    def to(self):
        return self._target_regime

    @property
    def src_port(self):
        return self._src_port

    @property
    def state_assignments(self):
        return self._state_assignments
    
    @property
    def event_outputs(self):
        return self._event_outputs

    @property
    def nodes(self):
        #assert False
        return chain( self._state_assignments, self._event_outputs)

    @property
    def target_regime(self):
        return self._target_regime
    @property
    def to(self):
        assert isinstance( self._target_regime, basestring)
        return self._target_regime

class OnCondition(object):
    element_name = "OnCondition"

    def AcceptVisitor(self, visitor,**kwargs):
        return visitor.VisitOnCondition(self,**kwargs)

    def __init__(self, trigger, state_assignments=[], event_outputs=[], target_regime=None):
        if isinstance( trigger, Condition):
            self._trigger = trigger.clone()
        elif isinstance( trigger, basestring):
            self._trigger = Condition( rhs = trigger )
        else:
            assert False

        if target_regime:
            assert isinstance(target_regime, basestring)

        self._state_assignments = state_assignments
        self._event_outputs = event_outputs
        self._target_regime = target_regime

        #self._to=None
        self._from=None

    def __str__(self):
        return 'OnCondition( %s )'%self.trigger

    def clone(self, *args,**kwargs):
        assert False
        pass
    
    @property
    def trigger(self):
        return self._trigger

    @property
    def target_regime(self):
        assert isinstance( self._target_regime, basestring)
        return self._target_regime


    @property
    def state_assignments(self):
        return self._state_assignments
    
    @property
    def event_outputs(self):
        return self._event_outputs

    #To go:
    @property
    def condition(self):
        return self._trigger

    @property
    def nodes(self):
        #assert False
        return chain( self._state_assignments, self._event_outputs)

    @property
    def to(self):
        assert isinstance( self._target_regime, basestring)
        return self._target_regime


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


    def __init__(self, name, time_derivatives, on_events=None, on_conditions=None, transitions=None):
        on_events = on_events or []
        on_conditions = on_conditions or []
        transitions = transitions or []

        self._name = name if name else "Regime%d"%Regime.n
        Regime.n = Regime.n+1

        # This is not nice, but we support passing in 'transitions', which is a list of 
        # both OnEvents and OnConditions. So lets filter this by type and add them 
        # appropriately:
        fDict = FilterDiscreteTypes(transitions, (OnEvent,OnCondition) ) 
        on_events.extend( fDict[OnEvent] ) 
        on_conditions.extend( fDict[OnCondition] )


        # Time Derivatives may be specified as strings:
        def strToTimeDeriv(s):
            r = re.compile(r"""\s* d(?P<var>[a-zA-Z][a-zA-Z0-9_]*)/dt \s* = \s* (?P<rhs> .*) """, re.VERBOSE) 
            m = r.match(s)
            return ODE( dependent_variable = m.groupdict()['var'], indep_variable='t', rhs=m.groupdict()['rhs'] )
        tdTypeDict = FilterDiscreteTypes( time_derivatives, (basestring, ODE ) )
        tds = tdTypeDict[ODE] + [ strToTimeDeriv(o) for o in tdTypeDict[basestring] ] 


        self._time_derivatives = tds
        self._on_events = [] #on_events
        self._on_conditions = [] #on_conditions

        for s in on_events:
            self.add_on_event(s)
        for s in on_conditions:
            self.add_on_condition(s)

#        for s in chain(self._on_conditions, self._on_events):
#            s._from = self.name
#            if not s._target_regime:
#                s._target_regime = self.name


    def add_on_event(self, on_event):
        assert isinstance(on_event, OnEvent)
        if not on_event.target_regime:
            assert not ( on_event._target_regime or  on_event._from)
            on_event._target_regime = self.name
        #on_event._target_regime = self.name
        on_event._from = self.name
        self._on_events.append( on_event )

    def add_on_condition(self, on_condition):
        """ Add a transition to the regime"""
        print on_condition
        assert isinstance(on_condition, OnCondition)

        if not on_condition.target_regime:
            assert not ( on_condition._target_regime or  on_condition._from)
            on_condition.target_regime = self.name
        #on_condition._target_regime = self.name
        on_condition._from = self.name
        self._on_conditions.append( on_condition )

                
    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        
        sort_key = lambda node: node.name
        return reduce(and_, (self.name == other.name, 
                             sorted(self.nodes, key=sort_key) == sorted(other.nodes, key=sort_key),
                             sorted(self.transitions, key=sort_key) == sorted(other.transitions, key=sort_key)))

    def __repr__(self):
        return "%s(%s)" % (self.__class__.__name__, self.name)

    def get_ref(self):
        """ Returns a reference to this regime """
        return Reference(Regime, self.name)





    #def _add_node_to_collection(self, node):
    #    assert False
    #    raise UnimplementedError, "Regime is doesn't know how to collect nodes, use subclass Union or Sequence."

    #@property
    #def odes(self):
    #    return iter(self._time_derivatives)

    #@property
    #def nodes(self):
    #    return chain( self._time_derivatives, self._on_events, self._on_conditions)
    #def add_node(self, node):
    #    assert False

    #    if isinstance(node, (RegimeElement)):
    #        if isinstance(node, Assignment):
    #            raise ValueError, "Assignments are now only allowed in Transitions.  Use a function alias instead"
    #        else:
    #            if node.to in self.symbol_map:
    #                    raise ValueError, "Adding node to Regime '%s', expression '%s' symbol='%s' collides with existing node '%s'" %\
    #                          (self.name, node.as_expr(), node.to, self.symbol_map[node.to].as_expr())
    #            self.symbol_map[node.to]=node
    #                
    #    else:
    #        raise ValueError, "Invalid node '%s' in Regime.add_node. " % repr(node)

    #    self._add_node_to_collection(node)

    #def add_transition(self, t):
    #    assert False
    #    """ Add a transition to the regime"""
    #    if isinstance(t, Transition):
    #        if t.from_ is None:
    #            t.from_=self
    #        if not t.from_==self:
    #            print "WARNING: Transition whose from_ was reassigned to the Regime."
    #        #I think we decided that circular transitions were ok# assert t.to!=self, "Transition '%s' assigns Regime '%s' to itself!." % (t.name, self.name)
    #    else:
    #        assert isinstance(t,Reference) and t.cls==Transition, "Regime.add_transition(t): t must be Transition or Reference(Transition, name)"
    #    
    #    
    #    self.transitions.add(t)

    #

    #@property
    #def neighbors(self):
    #    """ Get all regimes we transition to via an Transition """
    #    return [t.to for t in self.transitions_with_target]

    #@property
    #def transitions_with_target(self):
    #    """ Get all Transitions which define a target"""
    #    for t in self.transitions:
    #        if t.to:
    #            yield t

    #def get_transition_to(self,to):
    #    """ Returns Transition if Regime transitions to Regime 'to', otherwise None """

    #    for t in self.transitions:
    #        if t.to == to:
    #            return t
    #    else:
    #        return None

    #def neighbor_map(self):
    #    """Returns a map of {neighbor:transition,...} with transition.to=neighbor, from_=self

    #    """
    #    
    #    d = {}
    #    for t in self.transitions:
    #        # transition might have no target
    #        if t.to:
    #            d[t.to] = t

    #    return d

    #@property
    #def equations(self):
    #    """
    #    Yields all the equations contained within this Regime or any of its
    #    children.

    #    As nodes_filter is a generator, so too is this function.
    #    """
    #    return self.nodes_filter(lambda x: isinstance(x,Equation))


    #@property
    #def assignments(self):
    #    """ Yields all equations in the Transition"""
    #    return self.nodes_filter(lambda x: isinstance(x,Assignment))
    #

    #@property
    #def aliases(self):
    #    """
    #    Yields all the aliases contained within this Regime or any of its
    #    children.

    #    As nodes_filter is a generator, so too is this function.
    #    """
    #    return self.nodes_filter(lambda x: isinstance(x,Alias))

    #@property
    #def odes(self):
    #    """
    #    Yields all odes in the regime or any of its children.
    #    
    #    As nodes_filter is a generator, so too is this function.
    #    """
    #    return self.nodes_filter(lambda x: isinstance(x,ODE))


    #def nodes_filter(self, filter_func):
    #    """
    #    Yields all the nodes contained within this Regime

    #    Example of valid filter_func:
    #    filter_func = lambda x: isinstance(x, Equation)
    #    
    #    """
    #    for node in self.nodes:
    #        if filter_func(node):
    #            yield node

    #def regimes_in_graph(self, regimes_set=None):
    #    """ Set of all regimes by walking through transition graph
    #    starting with this regime

    #    regimes_set is None when called by user,
    #    but used in recursion to identify routes we've already been
    #    
    #    """

    #    # If we are starting out, create a regimes_set
    #    if regimes_set is None:
    #        regimes_set = set()

    #    # Add self to regimes set
    #    regimes_set.add(self)

    #    # Go through Transitions of this regime
    #    for t in self.transitions:
    #        # if found a new regime, have it add itself and
    #        # all its new regimes recursively
    #            
    #        # Transitions may have to=None
    #        if t.to:
    #            assert not isinstance(t.to, Reference), "Unresolved references.  Is this Regime part of a Component?"

    #            assert isinstance(t.to,Regime), "Regime graph contains a non-Regime: %s" % str(t.to)

    #            if t.to not in regimes_set:
    #                t.to.regimes_in_graph(regimes_set)
    #            
    #    return regimes_set


    #@classmethod
    #def from_xml(cls, element):
    #    assert element.tag == NINEML+cls.element_name
    #    nodes = []
    #    kwargs = {}
    #    tag_class_map = {}
    #    name = element.get("name")
    #    for node_cls in (ODE, Alias):
    #        tag_class_map[NINEML+node_cls.element_name] = node_cls
    #    for elem in element.iterchildren():
    #        node_cls = tag_class_map[elem.tag]
    #        tmp = node_cls.from_xml(elem)
    #        nodes.append(tmp)

    #    if name is not None:
    #        kwargs["name"] = name
    #        
    #    return cls(*nodes, **kwargs)

    def dot_content(self):

        # template & namespace
        ns = {}
        t = '<tr><td align="left" port="n_%(node_id)s">%(node_content)s</td></tr>\\\n\t\t'

        # header
        contents = []

        node_id = 0
        for n in self.nodes:
            # render node contents
            ns['node_id'] = str(node_id)
            node_id+=1
            ns['node_content'] = dot_escape(n.as_expr())
            contents += [t % ns]

        return ''.join(contents)

    #def _add_node_to_collection(self, node):
    #    self.nodes.add(node)


def On(condition, do=None, to=None, name=None):
    """ returns new Transition which goes to 'to' if condition is True.

    Equivalent to :
    Transition(from_=None,to=Reference(Regime,to),condition=condition)

    'On' is syntactic sugar for defining light regimes.

    The resulting Transition has from_=None, so it must be added to a Regime
    to be activated.
    
    """
    if do:
        # handle one do op more gracefully
        if isinstance(do, (str, EventPort)):
            do = (do,)
        return Transition(*do, **dict(to=to, condition=condition, name=name))
    else:
        return Transition(to=to, condition=condition, name=name)
        










def doToAsssignmentsAndEvents(doList):
    # 'do' is a list of strings, OutputEvents, and StateAssignments.
    doTypes = FilterDiscreteTypes(doList, (OutputEvent,basestring, Assignment) )
    
    #Convert strings to StateAssignments:
    for s in doTypes[basestring]:
        print s
        lhs,rhs = s.split('=')
        print lhs,rhs
        sa = Assignment( to=lhs, expr=rhs )
        doTypes[Assignment].append(sa)
    del doTypes[basestring]

    return doTypes[Assignment], doTypes[OutputEvent]




# Forwarding Function:
def DoOn( trigger, do=[], to=None ):
    if isinstance( trigger, InputEvent):
        return DoOnEvent(input_event=trigger, do=do,to=to)
    elif isinstance( trigger, (OnCondition, basestring)):
        return DoOnCondition(condition=trigger, do=do,to=to)
    else:
        assert False







def DoOnEvent( input_event, do=[], to=None):
    assert isinstance( input_event, InputEvent) 
    
    assignments,output_events = doToAsssignmentsAndEvents( do ) 
    return OnEvent( src_port=input_event.port,
                    state_assignments = assignments,
                    event_outputs=output_events,
                    target_regime = to )



def DoOnCondition( condition, do=[], to=None ):
    assignments,output_events = doToAsssignmentsAndEvents( do ) 
    return OnCondition( trigger=condition,
                        state_assignments = assignments,
                        event_outputs=output_events,
                        target_regime = to )








    








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
        aliasTD = FilterDiscreteTypes( aliases, (basestring, Alias ) )
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
    def state_variables(self):
        return iter( self._state_variables )
    
    @property
    def aliases_map(self):
        return dict( [ (a.lhs,a) for a in self._aliases ] )

    @property
    def state_variables(self):
        return self._state_variables





from core_component import ComponentClass



    
    
        
        
#def resolve_reference(ref, *where):
#        val = None
#        for D in where:
#            if ref in D:
#                val = D[ref]
#                break
#        if val:
#            return val
#        else:
#            raise Exception("Can't resolve reference %s" % ref)

def parse(filename):
    from xml_reader import XMLReader
    return XMLReader.read_component(filename)

#    doc = etree.parse(filename)
#    root = doc.getroot()
#    assert root.nsmap[None] == nineml_namespace
#    component = root.find(NINEML+"component")
#    return ComponentClass.from_xml(component)

