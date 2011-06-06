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

        
class Regime(object):
    """
    A regime is something that contains ODEs, has temporal extent, defines a set of Transitions
    which occur based on conditions, and can be join the Regimes to other Regimes.

    """
    element_name = "regime"
    n = 0
    
    def __init__(self, *nodes, **kwargs):
        """A node may either be an Equation or a Regime."""
        self.nodes = set()
        self.name = kwargs.get("name")
        if self.name is None:
            self.name = "Regime%d" % Regime.n
        Regime.n += 1
      
        
        nodes = map(expr_to_obj,nodes)
        

        # check user isn't using 'events' kwarg anymore
        events = kwargs.get("events")
        if events!=None:
            raise ValueError, "'events' kwarg is deprecated.  Use 'transitions'."


        # deal with user supplied transitions
        transitions = kwargs.get('transitions')
        if transitions:
            # handle only one transition more gracefully
            if isinstance(transitions,Transition):
                transitions = (transitions,)
            transitions=set(transitions)
        else:
            transitions=set()

        self.transitions = transitions

        self.symbol_map = {}
        for node in nodes:
            self.add_node(node)
                
    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        
        sort_key = lambda node: node.name
        return reduce(and_, (self.name == other.name, 
                             sorted(self.nodes, key=sort_key) == sorted(other.nodes, key=sort_key),
                             sorted(self.transitions, key=sort_key) == sorted(other.transitions, key=sort_key)))

    def __repr__(self):
        return "%s(%s)" % (self.__class__.__name__, self.name)

    def _add_node_to_collection(self, node):
        raise UnimplementedError, "Regime is doesn't know how to collect nodes, use subclass Union or Sequence."

    def get_ref(self):
        """ Returns a reference to this regime """
        return Reference(Regime, self.name)

    def add_node(self, node):

        if isinstance(node, (RegimeElement)):
            if isinstance(node, Assignment):
                raise ValueError, "Assignments are now only allowed in Transitions.  Use a function binding instead"
            else:
                if node.to in self.symbol_map:
                        raise ValueError, "Adding node to Regime '%s', expression '%s' symbol='%s' collides with existing node '%s'" %\
                              (self.name, node.as_expr(), node.to, self.symbol_map[node.to].as_expr())
                self.symbol_map[node.to]=node
                    
        else:
            raise ValueError, "Invalid node '%s' in Regime.add_node. " % repr(node)

        self._add_node_to_collection(node)

    def add_transition(self, t):
        """ Add a transition to the regime"""
        if isinstance(t, Transition):
            if t.from_ is None:
                t.from_=self
            if not t.from_==self:
                print "WARNING: Transition whose from_ was reassigned to the Regime."
            #I think we decided that circular transitions were ok# assert t.to!=self, "Transition '%s' assigns Regime '%s' to itself!." % (t.name, self.name)
        else:
            assert isinstance(t,Reference) and t.cls==Transition, "Regime.add_transition(t): t must be Transition or Reference(Transition, name)"
        
        self.transitions.add(t)

    @property
    def neighbors(self):
        """ Get all regimes we transition to via an Transition """
        return [t.to for t in self.transitions_with_target]

    @property
    def transitions_with_target(self):
        """ Get all Transitions which define a target"""
        for t in self.transitions:
            if t.to:
                yield t

    def get_transition_to(self,to):
        """ Returns Transition if Regime transitions to Regime 'to', otherwise None """

        for t in self.transitions:
            if t.to == to:
                return t
        else:
            return None

    def neighbor_map(self):
        """Returns a map of {neighbor:transition,...} with transition.to=neighbor, from_=self

        """
        
        d = {}
        for t in self.transitions:
            # transition might have no target
            if t.to:
                d[t.to] = t

        return d

    @property
    def equations(self):
        """
        Yields all the equations contained within this Regime or any of its
        children.

        As nodes_filter is a generator, so too is this function.
        """
        return self.nodes_filter(lambda x: isinstance(x,Equation))


    @property
    def assignments(self):
        """ Yields all equations in the Transition"""
        return self.nodes_filter(lambda x: isinstance(x,Assignment))
    

    @property
    def bindings(self):
        """
        Yields all the bindings contained within this Regime or any of its
        children.

        As nodes_filter is a generator, so too is this function.
        """
        return self.nodes_filter(lambda x: isinstance(x,Binding))

    @property
    def odes(self):
        """
        Yields all odes in the regime or any of its children.
        
        As nodes_filter is a generator, so too is this function.
        """
        return self.nodes_filter(lambda x: isinstance(x,ODE))


    def nodes_filter(self, filter_func):
        """
        Yields all the nodes contained within this Regime

        Example of valid filter_func:
        filter_func = lambda x: isinstance(x, Equation)
        
        """
        for node in self.nodes:
            if filter_func(node):
                yield node

    def regimes_in_graph(self, regimes_set=None):
        """ Set of all regimes by walking through transition graph
        starting with this regime

        regimes_set is None when called by user,
        but used in recursion to identify routes we've already been
        
        """

        # If we are starting out, create a regimes_set
        if regimes_set is None:
            regimes_set = set()

        # Add self to regimes set
        regimes_set.add(self)

        # Go through Transitions of this regime
        for t in self.transitions:
            # if found a new regime, have it add itself and
            # all its new regimes recursively
                
            # Transitions may have to=None
            if t.to:
                assert not isinstance(t.to, Reference), "Unresolved references.  Is this Regime part of a Component?"

                assert isinstance(t.to,Regime), "Regime graph contains a non-Regime: %s" % str(t.to)

                if t.to not in regimes_set:
                    t.to.regimes_in_graph(regimes_set)
                
        return regimes_set

    def to_xml(self):
        kwargs = {}
        return E(self.element_name,
                 name=self.name,
                 *[node.to_xml() for node in self.nodes],
                 **kwargs)

    @classmethod
    def from_xml(cls, element):
        assert element.tag == NINEML+cls.element_name
        nodes = []
        kwargs = {}
        tag_class_map = {}
        name = element.get("name")
        for node_cls in (ODE, Binding):
            tag_class_map[NINEML+node_cls.element_name] = node_cls
        for elem in element.iterchildren():
            node_cls = tag_class_map[elem.tag]
            tmp = node_cls.from_xml(elem)
            nodes.append(tmp)

        if name is not None:
            kwargs["name"] = name
            
        return cls(*nodes, **kwargs)

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

    def _add_node_to_collection(self, node):
        self.nodes.add(node)


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
        

class Transition(object):
    element_name = "transition"
    n = 0
    
    def __init__(self, *nodes, **kwargs):
        """
        Transition

        nodes = is a collection of assignments, inplace operations, or EventPorts of mode="send"
                which happen when the condition triggers.
                
        'condition' may be be a string like "V>10" or an EventPort of mode="recv"

        from_ = Regime to transition from
        to = Regime to transition to (maybe None)
        name = name for Transition, maybe None, in which case one is assigned.

        """

        #from_=None, to=None, condition=None, name=None
        from_ = kwargs.get("from_")
        to = kwargs.get("to")
        condition = kwargs.get("condition")
        name = kwargs.get("name")

      

        # handle to from_ as string
        if isinstance(to,str):
            to=Reference(Regime,to)
        if isinstance(from_,str):
            from_=Reference(Regime,from_)

        # check types
        if isinstance(from_, (Regime, type(None))):
            pass
        elif isinstance(from_, Reference) and issubclass(from_.cls,Regime):
            pass
        else:
            raise ValueError, "expected Regime, Reference(Regime,name), name as str, or None for kwarg 'from_', got '%s'" % repr(from_)
            
        if isinstance(to, (Regime, type(None))):
            pass
        elif isinstance(to, Reference) and issubclass(to.cls,Regime):
            pass
        else:
            raise ValueError, "expected Regime, Reference(Regime,name), name as str, or None for kwarg 'to', got '%s'" % repr(to)
            
        self.from_ = from_
        self.to = to

        if isinstance(condition,EventPort):
            self.condition=condition
            if condition.mode!="recv":
                raise ValueError, "Transition condition as an EventPort: EventPort mode must be 'recv'"
        else:
            # cond_to_obj does type checking
            self.condition = cond_to_obj(condition)

        if not self.condition:
            raise ValueError, "Transition condition may not be none"

        if self.condition.is_bool():
            raise ValueError, "condition is always true or false."


        self.name = name or ("Transition%d" % Transition.n)
        Transition.n += 1

        self.nodes = []
        for node in nodes:
            self.add_node(node)

    def add_node(self, node):
        if isinstance(node,str):
            node = expr_to_obj(node)
        
        if isinstance(node, (Assignment)):
            self.nodes.append(node)
        elif isinstance(node, EventPort):
            if node.mode=="recv":
                raise ValueError, "EventPort node '%s' must have mode='send'." % repr(node)
            self.nodes.append(node)
        else:
            raise ValueError, "Transition node '%s' not of valid type." % repr(node)

    @property
    def event_ports(self):
        """ Yields all EventPorts in the Transition"""
        if isinstance(self.condition,EventPort):
            yield self.condition
        for p in self.nodes_filter(lambda x: isinstance(x,EventPort)):
            yield p

    @property
    def event_port_nodes(self):
        """ Yields all EventPorts in the Transition nodes"""
        for p in self.nodes_filter(lambda x: isinstance(x,EventPort)):
            yield p

    @property
    def equations(self):
        """ Yields all equations in the Transition"""
        return self.nodes_filter(lambda x: isinstance(x,Equation))

    @property
    def assignments(self):
        """ Yields all equations in the Transition"""
        return self.nodes_filter(lambda x: isinstance(x,Assignment))



    def nodes_filter(self, filter_func):
        """
        Yields all the nodes contained within this Regime or any of its
        children for which filter_func yields true.

        Example of valid filter_func:
        filter_func = lambda x: isinstance(x, Equation)
        
        """
        for node in self.nodes:
            if filter_func(node):
                yield node
        

    def __repr__(self):
        return "Transition(from %s to %s if %s)" % (self.from_, self.to, self.condition)

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False

        # to prevent infinite loop, one should only check if
        # the references from_,to are equal, not the objects

        try:
            from_eq = self.from_.get_ref() == other.from_.get_ref()
        except AttributeError:
            # one is None, so it is OK to check equality of the objects 
            from_eq = self.from_==other.from_

        try:
            to_eq = self.to.get_ref() == other.to.get_ref()
        except AttributeError:
            # one is None, so it is OK to check equality of the objects
            to_eq = self.to==other.to

            
        sort_key = lambda node: node.name

        return reduce(and_, (self.name == other.name,
                             from_eq,
                             to_eq,
                             self.condition == other.condition,
                             sorted(self.nodes, key=sort_key) == sorted(other.nodes, key=sort_key)))

    def to_xml(self):
        attrs = {"name": self.name}
        args = []

        # TODO this duality of EventPorts and Conditions
        # should be cleaned up
        if isinstance(self.condition,EventPort):
            args+=[E("condition-on-event-port", self.condition.to_xml())]
        else:
            attrs["condition"] = self.condition.rhs
            #attrs["condition"] = self.condition.cond

            
        if self.to:
            attrs['to'] = self.to.name
        if self.from_:
            attrs['from'] = self.from_.name
        
        return E(self.element_name, *args+[node.to_xml() for node in self.nodes], **attrs)

##     def resolve(self, regimes):
##         for attr_name in ("from_", "to"):
##             ref = getattr(self, attr_name)
##             if isinstance(ref, Reference):
##                 resolved_obj = regimes[ref.name]
##                 assert isinstance(resolved_obj, ref.cls)
##                 setattr(self, attr_name, resolved_obj)

##     def resolve_condition(self):
##         if self.condition in ('true', 'false'):
##             return eval(self.condition.title())
##         else:
##             return self.condition


    @classmethod
    def from_xml(cls, element):
        assert element.tag == NINEML+cls.element_name
        from_ = element.get("from")
        if from_:
            from_ = Reference(Regime,from_ )
        to = element.get("to")
        if to:
            to = Reference(Regime,to )

        # Handling condition="V>Vth", but also EventPort as condition
        condition = element.get("condition")
        on_event_port = element.findall(NINEML+"condition-on-event-port")
        if not condition and not on_event_port:
            raise ValueError, "Transition did not define condition attribute, or condition-on-event-port element"
        if condition and on_event_port:
            raise ValueError, "Transition defined both condition attribute, and condition-on-event-port element"
        if len(on_event_port)>1:
            raise ValueError, "multiple condition-on-event-port elements"
        if on_event_port:
            oep = on_event_port[0]
            ep_elems = oep.findall(NINEML+EventPort.element_name)
            if len(ep_elems)>1:
                raise ValueError, "condition-on-event-port defined multiple event-port elements"
            condition = EventPort.from_xml(ep_elems[0])

            
        name = element.get("name")
        nodes = []
        tag_class_map = {}
        for node_cls in (EventPort, Assignment):
            tag_class_map[NINEML+node_cls.element_name] = node_cls
        for elem in element.iterchildren():
            if elem.tag==NINEML+"condition-on-event-port": continue
            node_cls = tag_class_map[elem.tag]
            tmp = node_cls.from_xml(elem)
            nodes.append(tmp)
        
        return cls(*nodes, **dict(from_=from_, to=to, condition=condition, name=name))

    def dot_content(self):

        # template & namespace
        ns = {}
        t = '<tr><td align="left" port="n_%(node_id)s">%(node_content)s</td></tr>\\\n\t\t'
        t_eventport = '<tr><td align="left" port="n_%(node_id)s"><font color="blue">%(node_content)s</font></td></tr>\\\n\t\t'

        contents = []

        node_id = 0
        for n in self.equations:
            # render node contents
            ns['node_id'] = str(node_id)
            node_id+=1
            ns['node_content'] = dot_escape(n.as_expr())
            contents += [t % ns]

        for p in self.nodes_filter(lambda x: isinstance(x,EventPort)):
            # render node contents
            ns['node_id'] = str(node_id)
            node_id+=1
            ns['node_content'] = dot_escape(repr(p))
            contents += [t_eventport % ns]

        return ''.join(contents)


class Component(object):
    element_name = "component"
    
    def __init__(self, name, parameters = [], regimes = [], transitions=[],
                 ports = [], bindings = []):
        """
        Regime graph should not be edited after contructing a component

        *TODO*: if the user maintains a ref to a regime in regimes,
        etc. they can violate this.  Code generators will need to
        query regimes, transitions, so we can't privatize self
        attributes {_regimes, _transitions, _regime_map,
        _transition_map} by prefixing with "_".

        We should do some privatizing for regimes, transitions, or do a deepcopy here.
        We could make regimes and transitions tuples, and expose only read-only apis in
        Regime and Transition class.  Then the _map could be made a sort of ImmutableDict.
        *END TODO*


        Specifying Regimes & Transitions
        --------------------------------

        The user passed 'regimes' and 'transitions' should contain true objects, i.e.
        they may not contain References. 
        
        Options to the user:
        
        1) provide both 'regimes' and 'transitions' (references will be resolved)
        2) provide 'regimes' only (in which case there must be no unresolved references),
        3) provide 'transitions' only (in which case there must be at least
           one transition in the model, and no unresolved references),

        """

        # this must be early, as we override attribute lookup to grap from the
        # ports map
        self.ports_map = {}
        self.name = name

        # check for empty component, we do not support inplace building of a component.
        if not regimes and not transitions:
            raise ValueError, "Component constructor needs at least 'regimes'"+\
                  "or 'transitions' to build component graph."

        # add to transitions from regimes
        # get only true transition objects (not references) from regimes
        # these will be added to transition map in next step
        transition_objects = [t for r in regimes for t in r.transitions if isinstance(t,Transition)]
        # model with no transitions is indeed allowed.

        transition_refs = [t for t in transitions if isinstance(t,Reference)]
        assert not transition_refs, "Component constructor: kwarg 'transitions' may not"+\
               "contain references."

        transitions = set(transitions)
        transitions.update(transition_objects)

        # add to regimes from transitions
        # get only true regime objects (not references) from transitions
        # these will be added to regime map in next step
        regime_objects = [r for t in transitions for r in (t.to,t.from_) if isinstance(r,Regime)]

        if not regimes:
            assert regime_objects, "Cannot build regime set: User supplied only Transitions "+\
                   "to Component constructor, but all 'to','from_' attributes are references!"
        regime_refs = [r for r in regimes if isinstance(r,Reference)]
        assert not regime_refs, "Component constructor: kwarg 'regimes' may not contain references."

        regimes = set(regimes)
        regimes.update(regime_objects)

        # build regime map
        self.regime_map = {}
        for r in regimes:
            if self.regime_map.has_key(r.name) and self.regime_map[r.name] != r:
                raise ValueError("Regime collection has Regimes with colliding names.")
            self.regime_map[r.name] = r

        # build transition map
        self.transition_map = {}
        for t in transitions:
            if self.transition_map.has_key(t.name) and self.transition_map[t.name] != t:
                raise ValueError, "Transition collection has Transitions with colliding names."+ str(t.name)
            self.transition_map[t.name] = t
               
        # store final regime and transition sets for this component
        self.regimes = set(regimes)
        self.transitions = set(transitions)

        # We have extracted all implicit knowledge of graph members, proceed to
        # resolve references.
        self.resolve_references()

        # check that there is an island regime only if there is only 1 regime
        island_regimes = set([r for r in self.regimes if not list(r.transitions_with_target) and\
                              not self.get_regimes_to(r)])
        if island_regimes:
            assert len(self.regimes)==1, "User Error: Component contains island regimes"+\
                   "and more than one regime."

        # Allow strings for bindings, map using expr_to_obj
        # Eliminate duplicates

        # This should not be a set, but a list!
        # We resolve later colliding bindings
        bindings = map(expr_to_obj,set(bindings))
        for r in self.regimes:
            bindings+=list(r.bindings)
        #self.bindings = bindings

        # build bindings map
        bindings_map = {}
        for b in bindings:
            assert isinstance(b, Binding), "Received invalid binding."
            if b.lhs in bindings_map and b.as_expr()!=bindings_map[b.lhs].as_expr():
                raise ValueError, "Multiple non-equal bindings on '%s' " % b.lhs
            bindings_map[b.lhs] = b
        self.bindings_map = bindings_map


        # Get the user defined ports and check them
        # this must happen before computing self.user_parameters
        # as 'recv' ports should not appear as user_parameters

        # implicit_send_ports: all variables can be read from by default
        self.analog_ports = []
        send_ports = [p.symbol for p in ports if p.mode=='send']
        implicit_send_ports = [AnalogPort(var,'send') for var in self.variables if var not in send_ports]
        # setup analog_ports
        self.analog_ports = set(list(ports)+implicit_send_ports)
        self.check_ports()
        for p in self.ports:
            self.ports_map[p.symbol] = p
            


        #print "user_parameters"
        #print self.user_parameters
        #print "--------"
        
        
        # Up till now, we've inferred parameters
        # Now let's check what the user provided
        # is consistant and finally set self.parameters
        if parameters:
            parameters_as_set = set(parameters)
            if self.user_parameters!=parameters_as_set:
                additional = parameters_as_set.difference(self.user_parameters)
                missing = self.user_parameters.difference(parameters_as_set)
                raise ValueError, "Declared parameter list %s does not match inferred parameter list %s.\nMissing: %s\nAdditional: %s" \
                      % (str(sorted(parameters)),str(sorted(self.user_parameters)), str(sorted(missing)),str(sorted(additional)))

        self.parameters = self.user_parameters

        # check bindings only have static parameters and functions on rhs
        #self.check_binding_expressions()
        
        # check we aren't redefining math symbols (like e,pi)
        self.check_non_parameter_symbols()

        # We should not do this for the user
        #self.backsub _bindings()

        # now would be a good time to backsub expressions
        # but we should not do this for the user.
        #self.backsub _equations()


    def __getattr__(self, name):
        if name in self.__getattribute__('ports_map'):
            return self.ports_map[name]
        else:
            return self.__getattribute__(name)

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
    

    @property
    def event_ports(self):
        """ return all event ports in regime transitions"""
        for t in self.transitions:
            #print 'event_ports:transition', t
            for ep in set(t.event_ports):
                    #print 'event_ports:eventport', ep
                    yield ep

    def check_ports(self):
        for p in self.analog_ports:
            if not isinstance(p,AnalogPort):
                raise ValueError, "Component ports attribute can contain only AnalogPort objects."+\
                      "EventPorts go in Transition conditions(recv) and Transition nodes (send)"
            # may only write to user_parameters
            if p.mode=="recv" and p.symbol in self.non_parameter_symbols:
                raise ValueError, "'recv' AnalogPorts may not target existing binding symbols,"+\
                      "ODE lhs vars, or lhs of Assignments ops."

            binding_names = [b.lhs for b in self.bindings]
            #print binding_names
            if p.mode=="send" and p.expr==None and (p.symbol not in self.variables and not p.symbol in binding_names) :
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

    def backsub_bindings(self):
        """ This function finds bindings with undefined functions, and uses
        the binding_map to attempt to resolve them. """

        # build binding dependency tree
        # and perform substitution, recursively
        def build_and_resolve_bdtree(b):
            _bd_tree = {}
            for f in b.missing_functions:
                if f in self.bindings_map:
                    _bd_tree[f] = build_and_resolve_bdtree(self.bindings_map[f])
                    # resolve (lower level is already resolved now) 
                    b.substitute_binding(self.bindings_map[f])
                    # re-calc functions
                    b.parse()
                else:
                    raise ValueError, "binding '%s' calls unresolvable functions." % b.as_expr()
            return _bd_tree  
        
        bd_tree = {}
        for b in self.bindings_map.itervalues():
            bd_tree[b.name] = build_and_resolve_bdtree(b)

    def backsub_equations(self):
        """ this function finds all undefined functions in equations, and uses
        the binding_map to resolve them """

        for e in self.equations:
            for f in e.missing_functions:
                if f in self.bindings_map:
                    e.substitute_binding(self.bindings_map[f])
                else:
                    raise ValueError, "Equation '%s' calls unresolvable functions." % e.as_expr()
            #e.parse()
            for n in e.names:
                if n in self.bindings_map:
                    e.substitute_binding(self.bindings_map[n])
            #e.parse()

        # There should be no missing functions now.
        assert [f for e in self.equations for f in e.missing_functions] == []

#    def backsub_ports(self):
#        """ this function finds all send ports that are not connected to state_variables, and substitutes
#        in the relevant bindings"""
#        
#        for port in self.analog_ports:
#            print port
#            if port.name in self.bindings_map:
#                b = self.bindings_map[port.name]
#                port.expr = b
#                print '  (Binding -> %s'% b
#                print port
#            else:
#                pass
#
#        #assert False



    
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
                             sorted(self.bindings, key=sort_key) == sorted(other.bindings, key=sort_key)))

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

    @property
    def bindings(self):
        return self.bindings_map.itervalues()


    def check_binding_expressions(self):
        """ Bound symbols (which are static when running the model)
        can depend only on 'user parameters' (which are static when running the model)

        This parses the binding rhs expressions to verify this is so.
        """

        params = self.user_parameters
        
        for binding in self.bindings:
            # It is up to the user to call backsub at the appropriate time,
            # or implement other facilities for resolving user defined functions
            # bindings ...
            # There for the following check is removed:
            #for f in binding.missing_functions:
            #    raise ValueError, "Binding '%s' calls undefined function '%s' " % str(binding.as_expr(),f)
            
            non_param_names = self.non_parameter_symbols.intersection(binding.names)
            # may reference other bindings
            non_param_names = non_param_names.difference(self.bindings_map.iterkeys())
            if non_param_names:
                raise ValueError, "Binding symbols referencing variables is illegal: %s" % str(non_param_names)

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

        # now same for bindings
        for b in self.bindings:
            symbols.update(b.names)

        symbols = symbols.difference(self.non_parameter_symbols)
        symbols = symbols.difference(math_namespace.symbols)
        # remove symbols of AnalogPorts with mode='recv'
        symbols = symbols.difference([p.symbol for p in self.filter_ports(cls=AnalogPort,mode=('recv'))])
        return symbols.difference(math_namespace.reserved_symbols)

                 
    @property
    @cache
    def non_parameter_symbols(self):
        """ All bindings, assignment and inplace left-hand-sides, plus X for ODE dX/dt = ...""" 
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

    @property
    @cache
    def state_variables(self):
        symbols = set([])
        symbols.update(self.integrated_variables)
        symbols.update(self.assigned_variables)
        return symbols


    @property
    @cache
    def bound_symbols(self):
        # TODO: cache once determined
        """ Return symbols which are subject to bindings (static assignments)"""
        # construct set of keys (bound symbols)
        statics = set(self.bindings_map)

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
        but not bindings (which are not variables, but static) """

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
        elements = [E.parameter(name=p) for p in self.parameters] + \
                   [p.to_xml() for p in self.analog_ports] +\
                   [r.to_xml() for r in self.regimes] + \
                   [b.to_xml() for b in self.bindings] +\
                   [t.to_xml() for t in self.transitions]
        attrs = {"name": self.name}
        return E(self.element_name, *elements, **attrs)
  
    @classmethod
    def from_xml(cls, element):
        """
        Parse an XML ElementTree structure and return a Component instance.
        
        `element` - should be an ElementTree Element instance.
        
        See:
            http://docs.python.org/library/xml.etree.elementtree.html
            http://codespeak.net/lxml/
        """
        assert element.tag == NINEML+cls.element_name
        parameters = [p.get("name") for p in element.findall(NINEML+"parameter")]
        bindings = [Binding.from_xml(b) for b in element.findall(NINEML+Binding.element_name)] 
        regimes = [Regime.from_xml(e) for e in element.findall(NINEML+Regime.element_name)]

        analog_ports = []
        for port_cls in (AnalogPort,):
            for e in element.findall(NINEML+port_cls.element_name):
                analog_ports.append(port_cls.from_xml(e))

        transitions = [Transition.from_xml(t) for t in element.findall(NINEML+Transition.element_name)]


        #print "Parameters from XML:"
        #for p in parameters:
        #    print p
        #print "-----------------"
        # allocate new component
        new_comp = cls(element.get("name"), parameters, regimes=regimes, transitions=transitions, bindings=bindings, ports=analog_ports)

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

        This will prefix the im component state vars, bindings, parameters with Im_ in the resulting Component hh_im.

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
        # bindings: lhs, rhs names & funcs if not in math_namespace
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
    doc = etree.parse(filename)
    root = doc.getroot()
    assert root.nsmap[None] == nineml_namespace
    component = root.find(NINEML+"component")
    return Component.from_xml(component)

