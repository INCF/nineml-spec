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
from nineml.abstraction_layer import math_namespace
from nineml import helpers
from nineml.abstraction_layer.expressions import *
from nineml.abstraction_layer.conditions import *
from nineml.abstraction_layer.ports import *
from nineml.abstraction_layer.cond_parse import cond_parse
from nineml.abstraction_layer.expr_parse import expr_parse
from nineml.abstraction_layer.xmlns import *


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







#class Trigger(str):
#   pass 


class OnEvent(object):

    def AcceptVisitor(self, visitor):
        return visitor.VisitOnEvent(self)

    def __init__(self, src_port,state_assignments=[], event_outputs=[], target_regime=None):

        self._src_port = src_port
        self._state_assignments = state_assignments
        self._event_outputs = event_outputs
        self._target_regime = target_regime

        #self._to=None
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

class OnCondition(object):
    element_name = "OnCondition"

    def AcceptVisitor(self, visitor):
        return visitor.VisitOnCondition(self)

    def __init__(self, trigger, state_assignments=[], event_outputs=[], target_regime=None):
        if isinstance( trigger, Condition):
            self._trigger = trigger.clone()
        elif isinstance( trigger, basestring):
            self._trigger = Condition( rhs = trigger )
        else:
            assert False

        self._state_assignments = state_assignments
        self._event_outputs = event_outputs
        self._target_regime = target_regime

        #self._to=None
        self._from=None

    

    def clone(self, *args,**kwargs):
        assert False
        pass
    
    @property
    def trigger(self):
        return self._trigger


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



class Regime(object):
    """
    A regime is something that contains ODEs, has temporal extent, defines a set of Transitions
    which occur based on conditions, and can be join the Regimes to other Regimes.
    """

    element_name = "Regime"
    n = 0
    
    # Visitation:
    # -------------
    def AcceptVisitor(self, visitor):
        return visitor.VisitRegime(self)

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


    def __init__(self, name, time_derivatives, on_events=[], on_conditions=[]):
        self._name = name if name else "Regime%d"%Regime.n
        Regime.n = Regime.n+1

        self._time_derivatives = time_derivatives
        self._on_events = on_events
        self._on_conditions = on_conditions

        for s in self._on_conditions:
            s.to = self

    def add_on_event(self, on_event):
        assert isinstance(on_event, OnEvent)
        assert not ( on_event._target_regime or  on_event._from)
        on_event._target_regime = self.name
        on_event._from = self.name
        self._on_events.append( on_event )

    def add_on_condition(self, on_condition):
        """ Add a transition to the regime"""
        print on_condition
        assert isinstance(on_condition, OnCondition)
        assert not ( on_condition._target_regime or  on_condition._from)
        on_condition._target_regime = self.name
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
    def __init__(self, regimes = [], aliases = [], state_variables = []):
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



class ComponentClass(object):
    element_name = "ComponentClass"

    def AcceptVisitor(self,visitor):
        return visitor.VisitComponent(self)
    
    def __init__(self, name, parameters = [], analog_ports = [], event_ports = [], dynamics=None):
        self._name = name
        self._parameters = parameters
        self._analog_ports = analog_ports
        self._event_ports = event_ports
        self._dynamics = dynamics

    @property
    def dynamics(self):
        return self._dynamics

    @property
    def name(self):
        return self._name

    @property
    def parameters(self):
        return iter(self._parameters)

    # <- Forwarding functions -->
    @property
    def ports_map(self):
        return dict( [ (p.name,p) for p in chain(self._analog_ports, self._event_ports) ] )
    
    @property
    def aliases_map(self):
        return self._dynamics.aliases_map

    @property
    def aliases(self):
        return self._dynamics.aliases

    @property
    def alias_symbols(self):
        return [ a.lhs for a in self.aliases ]

    @property
    def regimes(self):
        return self._dynamics.regimes


    @property
    def analog_ports(self):
        return self._analog_ports

    @property
    def event_ports(self):
        return self._event_ports

    @property
    def transitions(self):
        return self._dynamics.transitions

    @property
    def state_variables(self):
        return self._dynamics.state_variables




    def get_regimes_to(self,regime):
        """ Gets as a list all regimes that transition to regime"""
        
        return [t.from_ for t in self.transitions if t.to==regime]

    @property
    def ports(self):    
        """ yield all ports for component"""
        for p in self.analog_ports:
            yield p 
        for p in self.event_ports:
            yield p

    @property
    def reduce_ports(self):    
        """ yield all reduce ports for component"""
        for p in self.analog_ports:
            if p.mode=='reduce':
                yield p 
    

    def check_ports(self):
        for p in self.analog_ports:
            if not isinstance(p,AnalogPort):
                raise ValueError, "Component ports attribute can contain only AnalogPort objects."+\
                      "EventPorts go in Transition conditions(recv) and Transition nodes (send)"
            # may only write to user_parameters
            if p.mode=="recv" and p.symbol in self.non_parameter_symbols:
                raise ValueError, "'recv' AnalogPorts may not target existing alias symbols,"+\
                      "ODE lhs vars, or lhs of Assignments ops."

            alias_names = [b.lhs for b in self.aliases]
            #print alias_names
            if p.mode=="send" and p.expr==None and (p.symbol not in self.variables and not p.symbol in alias_names) :
                raise ValueError, "'send' AnalogPort with symbol='%s' source undefined in component." % (p.symbol,)
        

    def filter_ports(self,cls=None,mode=None,symb=None):
        """ yields all ports filtered by class, mode, symb

        mode,symb can be strings, or collections of strings
        on which "in" is defined.
        
        """
        if isinstance(mode,str):
            mode = (mode,)
        if isinstance(symb,str):
            symb = (symb,)

        for p in self.ports:
            if cls and not isinstance(p,cls): continue
            if mode and p.mode not in mode: continue
            if symb and p.symbol not in symb: continue
            yield p

    def backsub_aliases(self):
        """ This function finds aliases with undefined functions, and uses
        the alias_map to attempt to resolve them. """

        # build alias dependency tree
        # and perform substitution, recursively
        def build_and_resolve_bdtree(b):
            _bd_tree = {}
            for f in b.missing_functions:
                if f in self.aliases_map:
                    _bd_tree[f] = build_and_resolve_bdtree(self.aliases_map[f])
                    # resolve (lower level is already resolved now) 
                    b.substitute_alias(self.aliases_map[f])
                    # re-calc functions
                    b.parse()
                else:
                    raise ValueError, "alias '%s' calls unresolvable functions." % b.as_expr()
            return _bd_tree  
        
        bd_tree = {}
        for b in self.aliases_map.itervalues():
            bd_tree[b.name] = build_and_resolve_bdtree(b)

    def backsub_equations(self):
        """ this function finds all undefined functions in equations, and uses
        the alias_map to resolve them """
        from nineml.abstraction_layer.visitors import InPlaceTransform

        for alias in self.aliases:
            trans = InPlaceTransform( originalname = alias.lhs, targetname = "(%s)"%alias.rhs )
            # Since we do not want to backsub in lhs of alias, we can't call self.AcceptVisitor() directly
            for r in self.regimes:
                r.AcceptVisitor(trans)




    
    def resolve_references(self):
        """ Uses self.regimes_map and self.transitions_map to resolve references in self.regimes and self.transitions"""

        # resolve transition from_=None to=None to parent regime
        for r in self.regimes:
            for t in r.transitions:
                if t.from_==None:
                    t.from_=r
                if t.to==None:
                    t.to=r

        # resolve regime references in transitions:
        for t in self.transitions:
            for attr in ('to','from_'):
                ref = t.__getattribute__(attr)
                # transition defines no to,from
                if ref==None: continue
                if not isinstance(ref,Regime):
                    assert isinstance(ref,Reference) and issubclass(ref.cls,Regime), "Expected Regime reference or Regime"
                    t.__setattr__(attr,self.regime_map[ref.name])

        # resolve transition references in regimes

        for r in self.regimes:
            for t in r.transitions:
                if isinstance(t, Transition ): continue
                assert isinstance(t,Reference) and r.cls==Transition, "Expected Transition reference or Transition" 
                r.transitions.remove(t)
                r.transitions.add(self.transition_map[t.name])

        # add transitions to regimes:
        for t in self.transitions:
            t.from_.add_transition(t)
            


    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False

        sort_key = lambda node: node.name

        return reduce(and_, (self.name == other.name,
                             self.parameters == other.parameters,
                             sorted(self.transitions, key=sort_key) == sorted(other.transitions, key=sort_key),
                             sorted(self.regimes, key=sort_key) == sorted(other.regimes, key=sort_key),
                             sorted(self.aliases, key=sort_key) == sorted(other.aliases, key=sort_key)))

    @property
    def odes(self):
        for r in self.regimes:
            for ode in r.odes:
                yield ode

    @property
    def equations(self):

        for r in self.regimes:
            for t in r.transitions:
                for eq in t.equations:
                    yield eq
            for eq in r.equations:
                yield eq
        # 'send' AnalogPorts can have an expression
        for p in self.filter_ports(cls=AnalogPort,mode='send'):
            if p.expr: yield p.expr

    @property
    def conditions(self):
        """ Returns all conditions """
        # TODO transitions
        for t in self.transitions:
            yield t.condition

    #@property
    #def aliases(self):
    #    return self.aliases_map.itervalues()


    def check_alias_expressions(self):
        """ Bound symbols (which are static when running the model)
        can depend only on 'user parameters' (which are static when running the model)

        This parses the alias rhs expressions to verify this is so.
        """

        params = self.user_parameters
        
        for alias in self.aliases:
            # It is up to the user to call backsub at the appropriate time,
            # or implement other facilities for resolving user defined functions
            # aliases ...
            # There for the following check is removed:
            #for f in alias.missing_functions:
            #    raise ValueError, "Alias '%s' calls undefined function '%s' " % str(alias.as_expr(),f)
            
            non_param_names = self.non_parameter_symbols.intersection(alias.names)
            # may reference other aliases
            non_param_names = non_param_names.difference(self.aliases_map.iterkeys())
            if non_param_names:
                raise ValueError, "Alias symbols referencing variables is illegal: %s" % str(non_param_names)

    def check_non_parameter_symbols(self):
        """ Check that non-parameters symbols are not conflicting
        with math_namespace symbols """
        if self.non_parameter_symbols.intersection(math_namespace.symbols)!=set():
            raise ValueError, "Non-parameters symbols (variables and bound symbols) may "+\
                  "not redefine math symbols (such as 'e','pi')"
            
                

    @property
    @cache
    def user_parameters(self):
        """ User parameters for the component. """
        # TODO: cache once determined
        # compare to the parameters lists declared by the user 


        symbols = set([])
        for e in self.equations:
            symbols.update( e.names )
            
            
        # now same for conditions
        for c in self.conditions:
            symbols.update(c.names)

        # now same for aliases
        for b in self.aliases:
            symbols.update(b.names)

        symbols = symbols.difference(self.non_parameter_symbols)
        symbols = symbols.difference(math_namespace.symbols)
        # remove symbols of AnalogPorts with mode='recv'
        symbols = symbols.difference([p.symbol for p in self.filter_ports(cls=AnalogPort,mode=('recv'))])
        return symbols.difference(math_namespace.reserved_symbols)

                 
    @property
    @cache
    def non_parameter_symbols(self):
        """ All aliases, assignment and inplace left-hand-sides, plus X for ODE dX/dt = ...""" 
        # TODO: cache once determined
        symbols = set([])
        symbols.update(self.variables)
        symbols.update(self.bound_symbols)
        return symbols

    @property
    @cache
    def variables(self):
        symbols = set([])
        symbols.update(self.integrated_variables)
        symbols.update(self.assigned_variables)
        symbols.update(self.independent_variables)
        return symbols

#    @property
#    @cache
#    def state_variables(self):
#        symbols = set([])
#        symbols.update(self.integrated_variables)
#        symbols.update(self.assigned_variables)
#        return symbols
#

    @property
    @cache
    def bound_symbols(self):
        # TODO: cache once determined
        """ Return symbols which are subject to aliases (static assignments)"""
        # construct set of keys (bound symbols)
        statics = set(self.aliases_map)

        # check user is not writing to bound variables
        if statics.intersection(self.integrated_variables)!=set():
            raise ValueError, "Error: user bound symbols which appear on lhs of ODEs"
        if statics.intersection(self.assigned_variables)!=set():
            raise ValueError, "Error: user bound symbols which appear on lhs of Assignments OPs"
        
        return statics

    @property
    @cache
    def assigned_variables(self):
        """ All assignment and inplace lhs' (which may also be ODE integrated variables),
        but not aliases (which are not variables, but static) """

        # TODO: cache once determined
        variables = set([])
        for equation in self.equations:
            if isinstance(equation, (Assignment)):
                variables.add(equation.to)
        return variables

    @property
    @cache
    def independent_variables(self):
        """ All X for ODE dY/dX """
        # TODO: cache once determined
        variables = set([])
        for r in self.regimes:
            for eq in r.odes:
                variables.add(eq.indep_variable)
        return variables

    
    @property
    @cache
    def integrated_variables(self):
        """ All X for ODE dX/dt """
        # TODO: cache once determined
        variables = set([])
        for equation in self.equations:
            if isinstance(equation, ODE):
                variables.add(equation.dependent_variable)
        return variables

    def add_prefix(self, prefix, exclude=[]):
        """Add a prefix to all symbols except those explicitly excluded"""
        #Temporary solution for namespacing, until we get namespaces figured out.
        for eq in self.equations:
            if eq.to not in exclude:
                eq.to = prefix + eq.to
            eq.rhs = eq.prefix(prefix, exclude=exclude)
            eq.parse() # update names, funcs
        for port in self.ports:
            if port.symbol not in exclude and port.symbol != 't':
                self.ports_map.pop(port.symbol)
                port.symbol = prefix + port.symbol
                self.ports_map[port.symbol] = port

        # invalidate the instance method cache, triggering all the @cache decorated methods to be re-calculated
        self.__cache__={}

        # TODO: still this is not working. See test_component_symbols.py -> test_add_prefix
        # Presumably, for this to work inplace, we need to "re-run" certain parts of the
        # constructor ... maybe this is improved with Namespaces ...

        # See also: test_expressions.py -> test_prefixing
    
    def to_xml(self):
        from nineml.abstraction_layer.xml_writer import XMLWriter
        return XMLWriter().VisitComponent(self)

  
    @classmethod
    def from_xml(cls, element):
        """
        Parse an XML ElementTree structure and return a Component instance.
        
        `element` - should be an ElementTree Element instance.
        
        See:
            http://docs.python.org/library/xml.etree.elementtree.html
            http://codespeak.net/lxml/
        """
        assert False
        from nineml.abstraction_layer.xml_writer import XMLReader
        return XMLReader.read_component(elem)

        assert element.tag == NINEML+cls.element_name
        parameters = [p.get("name") for p in element.findall(NINEML+"parameter")]
        aliases = [Alias.from_xml(b) for b in element.findall(NINEML+Alias.element_name)] 
        regimes = [Regime.from_xml(e) for e in element.findall(NINEML+Regime.element_name)]

        analog_ports = []
        for port_cls in (AnalogPort,):
            for e in element.findall(NINEML+port_cls.element_name):
                analog_ports.append(port_cls.from_xml(e))

        transitions = [Transition.from_xml(t) for t in element.findall(NINEML+Transition.element_name)]


        new_comp = cls(element.get("name"), parameters, regimes=regimes, transitions=transitions, aliases=aliases, ports=analog_ports)

        return new_comp


    def write(self, file):
        """
        Export this model to a file in 9ML XML format.

        file is filename or file object.
        """
        doc = E.nineml(self.to_xml(), xmlns=nineml_namespace)
        etree.ElementTree(doc).write(file, encoding="UTF-8",
                                     pretty_print=True, xml_declaration=True)

    def to_dot(self,out, show_contents=True, relabel_nodes=False):
        """ Write a DOT graph representation of component

        http://en.wikipedia.org/wiki/DOT_language

        Convert a dot file to an image using, i.e.:
          dot -Tsvg spike_generator.dot -o spike_generator.svg
          dot -Tpng spike_generator.png -o spike_generator.png

        """

        # if out is a str, make a file
        if isinstance(out,str):
            out = file(out,'w')

        out.write("""digraph "NineML Component '%s'" {\n""" % self.name)

        out.write('\toverlap = "scale";\n')

        regime_id = dict([(kv[0],i) for i,kv in enumerate(self.regime_map.iteritems())])

        #TransitionLabelDict:
        node_labels = {}
        for r in itertools.chain(self.regimes):
            node_labels[r] = "Regime:%d"%len(node_labels)

        for t in itertools.chain(self.transitions):
            node_labels[t] = "Trans:%d"%len(node_labels)


        if show_contents:

            out.write('\tgraph [fontsize=30 labelloc="t" label="" splines=true overlap=false rankdir = "LR"];\n\tratio = auto\n');
            props = 'style = "filled, bold" penwidth = 1 fillcolor = "white" fontname = "Courier New" shape = "Mrecord" '
            # regime template
            t_regime = '\t"%(node)s" [ style = "filled, bold" penwidth = 1 fillcolor = "white" fontname = "Courier New" '+\
                       'shape = "Mrecord" \\\n\t\tlabel =<<table border="0" cellborder="0" cellpadding="3" bgcolor="white">'+\
                       '<tr><td bgcolor="black" \\\n\t\talign="center" colspan="2"><font color="white">'+\
                       '%(regime_name)s</font></td></tr>\\\n\t\t%(contents)s</table>> ];\n ' 
            # to fill: node, regime_name, contents
            ns = {}

            for r in self.regimes:
                
                ns['node'] = "regime_%d" % regime_id[r.name]
                ns['regime_name'] = r.name if not relabel_nodes else node_labels[r]
                ns['contents'] = r.dot_content()
                out.write(t_regime % ns)



        # transition template
        t_transition = '\t"%(from)s" -> "%(to)s" [ style = "filled, bold" penwidth = 1 fillcolor = "white" fontname = "Courier New" \\\n'+\
                       '\t\tlabel =<<table border="0" cellborder="0" cellpadding="3" bgcolor="#C0C0C0"> \\\n'+\
                       '\t\t<tr><td bgcolor="blue" align="center" colspan="2"><font color="white"> \\\n'+\
                       '\t\t%(trans_name)s</font> \\\n'+\
                       '\t\t</td></tr> \\\n'+\
                       '\t\t<tr><td bgcolor="green" align="center" colspan="2"><font color="black"> \\\n'+\
                       '\t\t @ %(condition)s</font></td></tr> \\\n'+\
                       '\t\t%(contents)s</table>> ];\n ' 
        
        for t in self.transitions:
            if show_contents:

                #out.write('label="%s @ %s"' % (t.name.encode('utf-8'), t.condition.encode('utf-8'),))
                #out.write('\tregime_%d -> regime_%d [label="%s @ %s"];\n' % (regime_id[t.from_.name], regime_id[t.to.name],
                #                                                   t.name.encode('utf-8'), t.condition.encode('utf-8')))

                # to fill: node, regime_name, contents
                ns = {}

                ns['from'] = "regime_%d" % regime_id[t.from_.name]
                ns['to'] = "regime_%d" % regime_id[t.to.name]
                ns['trans_name'] = t.name if not relabel_nodes else node_labels[t]
                ns['condition'] = dot_escape(t.condition.as_expr())
                #ns['contents'] = t.dot_content()
                ns['contents'] = t.dot_content()
                out.write(t_transition % ns)
                
            else:
                out.write('\tregime_%d -> regime_%d [label="%s"];\n' % (regime_id[t.from_.name], regime_id[t.to.name],
                                                                   t.name.encode('utf-8')))

        out.write('}')


    def join(self, component, prefix=None, **port_connections):
        """ Creates a component which is the self component joined with component.

        For example, this is great for constructing HH components with additional channels in
        a modular way.

        The Regime spaces are 'crossed' such that if c1 as regimes=(a,b), and c2 has regimes=(X,Y)
        then the joined component, CJ, has regimes Union(a,X), Union(a,Y), Union(b,X), Union(b,Y),
        where Union here refers to the Regime "Union".  Transition 'to=' are set up to transition so as
        to leave the other component unaffected, i.e. if c2.X has an transition with to=Y, then
        Union(a,X) has an Transition with to=Union(a,Y) and Union(b,X) has an Transition with to=Union(b,Y).

        Symbol/Name collisions:

        Unioning two components brings with it the possibility for name collisions.

        An example would be the HH model and an additional channel model both using the symbol 'm'
        to denote a gating variable.

        As such, the component to join has all its variables prefixed
        by its component name by default.  If the component name is
        such that this is not possible, or the user simple chooses to
        do so, the user may override the preix with the prefix kwarg.

        hh_im = hh.join(im,prefix="Im",I=im.I)

        This will prefix the im component state vars, aliases, parameters with Im_ in the resulting Component hh_im.

        Notes:

         - RecvPorts which are connected are no longer visible in the ComponentUnion.

         - ReducePorts, SendPorts persist and are visible in the ComponentUnion.

         - If SendPorts are connected to a ReducePort, the local symbol (LS_reduce) of the reduce port is replaced
           by: (LS_reduce <reduce_op> SP0 <reduce_op> ... SPN)  where SP0..SPN are the local symbols of the SendPorts.
           In this way, the ReducePort remains available in the joined Component for additional connections.

         - EventPorts persist and are visible in the ComponentUnion.

         - Connected EventPorts:
           1) For the InputEvent, the "do" must be moved to where the OutputEvent
              the "to" should modify the "to" of the Transition which generates the OutputEvent
              to induce the appropriate transition in the Regime space of the InputEvent component.

           2) OutputEvents persist.  Connected InputEvents do not persist in the joined model.


        """

        # cross regime spaces
        # setup new transitions

        # component name prefixing:
        # aliases: lhs, rhs names & funcs if not in math_namespace
        # ode dep var, rhs names & funcs if not in math_namespace
        # assignment lhs, rhs names & funcs if not in math_namespace
        # inplace lhs, rhs names & funcs if not in math_namespace
        # conditions, cond names & funcs if not in math_namespace
        

        # contianer for cross of regime spaces of 2 components
        new_regimes = []
        for r1 in self.regimes:
            for r2 in component.regimes:
                name = "%s*%s" % (r1.name, r2.name)
                # we will lose all sub-Union-like nodes this way
                nodes = list(r2.nodes_filter(lambda x: True))
                # AnalogPorts: What needs to be done:
                # - RecvPorts -> substitute local name of sent variable to prefixed port symbol
                # - SendPorts -> prefix
                # - ReducePorts -> substitute: prefixed_port_symbol op sent_var

                nodes += list(r1.nodes_filter(lambda x: True))
                # AnalogPorts: What needs to be done:
                # - RecvPorts -> substituted local (prefixed name) of sent variable to port symbol
                # - SendPorts -> nothing.
                # - ReducePorts -> substitute: port_symbol op prefixed_sent_var

                # transitions:
                transitions = []
                for e in r1.transitions:
                    # prevent name collision
                    e_name = "%s.%s"  % (r1.name,e.name)
                    # to name does not change r2.regime
                    to_name = "%s*%s" % (e.to.name,r2.name)
                    transitions += [Transition(*e.nodes, to=to_name, name=e_name, condition=e.condition)]

                new_regimes += [Regime(nodes, name=name,transitions=transitions)]

        # ports


        # build list of outward facing AnalogPorts

        name = "*".join([c.name for c in comps])
        return Component(name, regimes = regimes, ports=ports)






    
    
        
        
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

