"""
Python module for reading 9ML abstraction layer files in XML format.

Copyright Andrew P. Davison, Eilif B. Muller, 2010 # if you edit this file, add your name here
"""


from lxml import etree
from lxml.builder import E
#from mathml.lmathdom import MathDOM
#from mathml.utils import pyterm
from operator import and_
from nineml import __version__
import re
import copy

from nineml.cache_decorator import cache_decorator as cache
from nineml import math_namespace


MATHML = "{http://www.w3.org/1998/Math/MathML}"
nineml_namespace = 'http://nineml.org/9ML/0.1'
NINEML = "{%s}" % nineml_namespace

class UnimplementedError(RuntimeError):
    pass


try:
    from itertools import product # in version 2.6 or later
except ImportError:
    def product(*args, **kwds):
    # product('ABCD', 'xy') --> Ax Ay Bx By Cx Cy Dx Dy
    # product(range(2), repeat=3) --> 000 001 010 011 100 101 110 111
        pools = map(tuple, args) * kwds.get('repeat', 1)
        result = [[]]
        for pool in pools:
            result = [x+[y] for x in result for y in pool]
        for prod in result:
            yield tuple(prod)




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


    
class RegimeElement(object):
    """ Base class for all things that can be elements of a regime """
    pass

class Binding(RegimeElement):
    # this is very similar to Assignment. Maybe we don't need it.
    # EM: In the context of NEST and GPU code generation, bindings make sense:
    #  They are constants, i.e. a binding which takes state vars or ports in rhs
    #  should throw an exception.
    #  Users can specify them manually for eash of short hands,
    #  but automatic symbolic symplification of the expressions may well produce
    #  new bindings which can be pre-calculated outside of the integration loop.
    #
    #  Let's keep this in mind, and keep Bindings as we move forward!
    
    element_name = "binding"
    
    def __init__(self, name, value):
        self.name = name
        self.value = value

    @property
    def rhs(self):
        return self.value

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.name == other.name and self.value == other.value

    def to_xml(self):
        return E(self.element_name,
                 E("math-inline", self.value),
                 name=self.name)

    def as_expr(self):
        return "%s := %s" % (self.name, self.value)

    @classmethod
    def from_xml(cls, element):
        return cls(element.get("name"), element.find(NINEML+"math-inline").text)





class Equation(RegimeElement):
    pass

class Port(object):
    """ Base class for EventPort and AnalogPort, etc."""
    def __init__(self,id, mode='r', ):
        self.id = id


class EventPort(Port):
    element_name = "event"
    
    def __init__(self,id, mode='r'):
        Port.__init__(self, id, mode=mode)


    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.id == other.id

    def __repr__(self):
        return "Event(id='%s')" % (self.id)


    def to_xml(self):
        return E(self.element_name, id=self.id)

    @property
    def name(self):
        return self.element_name+"_"+self.id

    @classmethod
    def from_xml(cls,element):
        assert element.tag == NINEML+cls.element_name
        id = element.get("id")
        return cls(id)
        

SpikeOutputEvent = EventPort('spike_output')
SpikeInputEvent = EventPort('spike_input','w')


class ODE(Equation):
    """
    Represents a first-order, ordinary differential equation.
    """
    element_name = "ode"
    n = 0
    
    def __init__(self, dependent_variable, indep_variable, rhs, name=None):
        self.dependent_variable = dependent_variable
        self.indep_variable = indep_variable
        self.rhs = rhs
        self.name = name or ("ODE%d" % ODE.n)
        ODE.n += 1
        
    def __repr__(self):
        return "ODE(d%s/d%s = %s)" % (self.dependent_variable,
                                      self.indep_variable,
                                      self.rhs)

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False

        return reduce(and_, (self.name == other.name,
                             self.dependent_variable == other.dependent_variable,
                             self.indep_variable == other.indep_variable,
                             self.rhs == other.rhs))

    def as_expr(self):
        return "d%s/d%s = %s" % (self.dependent_variable,
                                 self.indep_variable,
                                 self.rhs)

    def to_xml(self):
        return E(self.element_name,
                 E("math-inline", self.rhs),
                 name=self.name,
                 dependent_variable=self.dependent_variable,
                 independent_variable = self.indep_variable)

    @classmethod
    def from_xml(cls, element):
        assert element.tag == NINEML+cls.element_name
        rhs = element.find(NINEML+"math-inline").text
        return cls(element.get("dependent_variable"),
                   element.get("independent_variable"),
                   rhs,
                   name=element.get("name"))
    

class Assignment(Equation):
    element_name = "assignment"
    n = 0
    
    @property
    def rhs(self):
        return self.expr

    def __init__(self, to, expr, name=None):
        self.to = to
        self.expr = expr
        self.name = name or ("Assignment%d" % Assignment.n)
        Assignment.n += 1

    def __repr__(self):
        return "Assignment('%s', '%s')" % (self.to, self.expr)

    def as_expr(self):
        return "%s = %s" % (self.to,
                            self.expr)

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False

        return reduce(and_, (self.name == other.name,
                             self.to == other.to,
                             self.expr == other.expr))

    def to_xml(self):
        return E(self.element_name,
                 E("math-inline", self.expr),
                 name=self.name,
                 to=self.to)
                 
    @classmethod
    def from_xml(cls, element):
        assert element.tag == NINEML+cls.element_name
        math = element.find(NINEML+"math-inline").text
        return cls(to=element.get("to"), name=element.get("name"),
                   expr=math)



class Inplace(Equation):
    element_name = "inplace"
    n = 0
    op_name_map = {'+=':'Add','-=':'Sub','*=':'Mul','/=':'Div'}

    op = "+="
    
    @property
    def rhs(self):
        return self.expr

    def __init__(self, to, op, expr, name=None):
        
        self.to = to
        self.op = op

        # catch invalid ops and give the user feedback
        try:
            self.op_name = self.op_name_map[op]
        except KeyError:
            raise ValueError, "Unsupported inplace operation '%s', supported ops: %s" %(self.op_name, str(self.op_name_map))
        
        self.expr = expr
        self.name = name or ("Inplace%s%d" % (self.op_name,Inplace.n))
        Inplace.n += 1

    def __repr__(self):
        return "Inplace('%s', '%s', '%s')" % (self.to,self.op,self.expr)

    def as_expr(self):
        return "%s %s %s" % (self.to,self.op, self.expr)


    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False

        return reduce(and_, (self.name == other.name,
                             self.to == other.to,
                             self.op == other.op,
                             self.expr == other.expr))

    def to_xml(self):
        return E(self.element_name,
                 E("math-inline", self.expr),
                 name=self.name,
                 to=self.to, op=self.op)
                 
    @classmethod
    def from_xml(cls, element):
        assert element.tag == NINEML+cls.element_name
        math = element.find(NINEML+"math-inline").text
        return cls(to=element.get("to"), op=element.get("op"), expr=math,
                   name=element.get("name"))

# factories for Inplace ops
def InplaceAdd(to,expr):
    return Inplace(to,'+=',expr)

def InplaceSub(to,expr):
    return Inplace(to,'-=',expr)

def InplaceMul(to,expr):
    return Inplace(to,'*=',expr)

def InplaceDiv(to,expr):
    return Inplace(to,'/=',expr)


class Reference(object):
    
    def __init__(self, cls, name):
        self.cls = cls
        self.name = name
    def get_ref(self):
        """ return self """
        return self

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.cls == other.cls and self.name == other.name

    def __repr__(self):
        return "Reference(%s, name='%s')" % (self.cls.__name__, self.name)

# This is a reference to the only regime that transitions to this regime
# The concept is used for the On function, so that a standalone do
# Can return to the previous regime on completion.
# It is for syntactic sugar, and requires no additional concepts in the XML ...

        


def expr_to_obj(s, name = None):
    """ Construct nineml objects from expressions """ 

    # Is our job already done?
    if isinstance(s,RegimeElement):
        return s

    # strip surrounding whitespace
    s = s.strip()

    # re for an expression -> groups into lhs, op, rhs
    p_eqn = re.compile(r"(?P<lhs>[a-zA-Z_]+[a-zA-Z_0-9]*(/?[a-zA-Z_]+[a-zA-Z_0-9]*)?)\s*(?P<op>[+\-*/:]?=)\s*(?P<rhs>.*)")
    # re for lhs for ODE
    p_ode_lhs = re.compile(r"(?:d)([a-zA-Z_]+[a-zA-Z_0-9]*)/(?:d)([a-zA-Z_]+[a-zA-Z_0-9]*)")


    m = p_eqn.match(s)
    if not m:
        raise ValueError, "Not a valid nineml expression: %s" % s

    # get lhs, op, rhs
    lhs, op, rhs = [m.group(x) for x in ['lhs','op','rhs']]

    # do we have an ODE?
    m = p_ode_lhs.match(lhs)
    if m:
        if op!="=":
            raise ValueError, "ODE lhs, but op not '=' in %s" % s

        dep_var = m.group(1)
        indep_var = m.group(2)
        return ODE(dep_var,indep_var,rhs, name = name)

    # Do we have an Inplace op?
    if op in Inplace.op_name_map.keys():
        return Inplace(lhs,op,rhs, name = name)

    # Do we have an assignment?
    if op=="=":
        return Assignment(lhs,rhs, name = name)

    # Do we have a binding?
    if op==":=":
        return Binding(lhs,rhs)

        
    # If we get here, what do we have?
    raise ValueError, "Cannot map expr '%s' to a nineml class" % s




class Regime(RegimeElement):
    """A regime is something that can be joined by a transition.

    This is an Abstract base class.  Union and Sequence are subclasses for the user.
    This class does not define and element_name so it cannot be written to XML.

    """
    n = 0
    
    def __init__(self, *nodes, **kwargs):
        """A node may either be an Equation or a Regime.""" 
        self.name = kwargs.get("name")
        if self.name is None:
            self.name = "Regime%d" % Regime.n
        Regime.n += 1

        # user can define transitions emanating from this
        # regime
        t = kwargs.get('transitions')
        if t:
            self.transitions=set(t)
        else:
            self.transitions=set()

        for node in nodes:
            assert isinstance(node, RegimeElement)
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

        self._add_node_to_collection(node)


    def add_transition(self, t):
        """ Add a transition to the regime"""
        if isinstance(t, Transition):
            if t.from_ is None:
                t.from_=self
            if not t.from_==self:
                print "WARNING: transition whose from_ was reassigned to the Regime."
            assert t.to!=self, "transition '%s' assigns Regime '%s' to itself!." % (t.name, self.name)
        else:
            assert isinstance(t,Reference) and t.cls==Transition, "Regime.add_transition(t): t must be Transition or Reference(Transition, name)"
        
        self.transitions.add(t)


    def neighbors(self):
        """ Get all regimes we transition to """
        for t in self.transitions:
            yield t.to

    def get_transition_to(self,to):
        """ Returns transition if Regime transitions to Regime 'to', otherwise None """

        for t in self.transitions:
            if t.to == to:
                return t
        else:
            return None

    def neighbor_map(self):
        """Returns a map of neighbors to transitions, from self

        Such that neighbor_map[neighbor] is the transition from self to neighbor """

        d = {}
        for t in self.transitions:
            d[t.to] = t

        return d

    def equations(self):
        """
        Yields all the equations contained within this Regime or any of its
        children.
        """
        return self.nodes_filter(lambda x: isinstance(x,Equation))

    def bindings(self):
        """
        Yields all the equations contained within this Regime or any of its
        children.
        """
        return self.nodes_filter(lambda x: isinstance(x,Binding))


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
            elif isinstance(node, Regime):
                for cnode in node.nodes_filter(filter_func):
                    yield cnode
            else:
                pass


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

        # Go through transitions of this regime
        for t in self.transitions:
            # if found a new regime, have it add itself and
            # all its new regimes recursively
                
            assert not isinstance(t.to, Reference), "Unresolved references.  Is this Regime part of a Component?"

            assert isinstance(t.to,Regime), "Regime graph contains a non-Regime: %s" % str(t.to)

            if t.to not in regimes_set:
                t.to.regimes_in_graph(regimes_set)
                
        return regimes_set

                    
    def odes(self):
        return [eqn for eqn in self.equations() if isinstance(eqn, ODE)]

    def to_xml(self):
        kwargs = {}
        return E(self.element_name,
                 name=self.name,
                 *[node.to_xml() for node in self.nodes], **kwargs)

    @classmethod
    def from_xml(cls, element):
        assert element.tag == NINEML+cls.element_name
        nodes = []
        tag_class_map = {}
        name = element.get("name")
        for node_cls in (ODE, Assignment, Sequence, Union, Inplace):
            tag_class_map[NINEML+node_cls.element_name] = node_cls
        for elem in element.iterchildren():
            node_cls = tag_class_map[elem.tag]
            tmp = node_cls.from_xml(elem)
            nodes.append(tmp)
        if name is not None:
            kwargs = {"name": name}
        return cls(*nodes, **kwargs)


    def dot_content(self,level=0):

        # template & namespace
        ns = {'level':level}
        t = '<tr><td align="left" port="n_%(level)d_%(node_id)s">%(node_content)s</td></tr>\\\n\t\t'

        # header
        level_pad = ''.join(['  ']*level)
        ns['node_id'] = 'root'
        ns['node_content'] = level_pad+self.__class__.__name__+dot_escape('(name="%s"'%self.name)
        contents = [t % ns]

        node_id = 0
        for n in self.nodes:
            if isinstance(n,Regime):
                contents +=[n.dot_content(level+1)]
            else:
                # render node contents
                ns['node_id'] = str(node_id)
                node_id+=1
                ns['node_content'] = level_pad+'  '+dot_escape(n.as_expr())
                contents += [t % ns]

        # footer
        ns['node_id'] = 'tail'
        ns['node_content'] = level_pad+dot_escape(')')
        contents += [t % ns]

        return ''.join(contents)


class Sequence(Regime):
    element_name = "sequence"
    
    def __init__(self, *nodes, **kwargs):
        nodes = map(expr_to_obj,nodes)
        self.nodes = []
        Regime.__init__(self, *nodes, **kwargs)
        self.nodes = list(nodes)

    def _add_node_to_collection(self, node):
        self.nodes.append(node)
    

        
class Union(Regime):
    element_name = "union"
    
    def __init__(self, *nodes, **kwargs):
        nodes = map(expr_to_obj,nodes)
        self.nodes = set()
        Regime.__init__(self, *nodes, **kwargs)

    def _add_node_to_collection(self, node):
        self.nodes.add(node)


def On(condition, to=None):
    """ returns new Transition which goes to 'to' if condition is True.

    Equivalent to :
    Transition(from_=None,to=Reference(Regime,to),condition=condition)

    'On' is syntactic sugar for defining light regimes.

    The resulting Transition has from_=None, so it must be added to a Regime
    to be activated.
    
    """

    return Transition(from_=None,to=Reference(Regime,to),condition=condition)
        


        

class Transition(object):
    element_name = "transition"
    n = 0
    
    def __init__(self, from_=None, to=None, condition=None, assignment=None, name=None):
        assert isinstance(from_, (Regime, Reference, type(None)))
        assert isinstance(to, (Regime, Reference))
        assert isinstance(condition, basestring) or condition is None
        if assignment:
            assert isinstance(assignment, Assignment)
        self.from_ = from_
        self.to = to
        self.condition = condition or "true"
        self.assignment = assignment
        self.name = name or ("Transition%d" % Transition.n)
        Transition.n += 1

    def __repr__(self):
        return "Transition(from %s to %s if %s)" % (self.from_, self.to, self.condition)

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False

        return reduce(and_, (self.name == other.name,
                             self.from_.get_ref() == other.from_.get_ref(),
                             self.to.get_ref() == other.to.get_ref(),
                             self.condition == other.condition,
                             self.assignment == other.assignment))

    def to_xml(self):
        attrs = {"name": self.name,
                 "from": self.from_.name,
                 "to": self.to.name,
                 "condition": self.condition}
        if self.assignment:
            return E(self.element_name, self.assignment.to_xml(), **attrs)
        else:
            return E(self.element_name, **attrs)

##     def resolve(self, regimes):
##         for attr_name in ("from_", "to"):
##             ref = getattr(self, attr_name)
##             if isinstance(ref, Reference):
##                 resolved_obj = regimes[ref.name]
##                 assert isinstance(resolved_obj, ref.cls)
##                 setattr(self, attr_name, resolved_obj)

##     def resolve_condition(self):
##         """
##         condition is a boolean variable, whose value must be defined in the
##         "from" regime. This method returns the assignment that sets the value of
##         condition.
##         """
##         if self.condition in ('true', 'false'):
##             return eval(self.condition.title())
##         condition_equation = None
##         for eqn in self.from_.equations():
##             if isinstance(eqn, Assignment) and eqn.to == self.condition:
##                  condition_equation = eqn
##                  break
##         if condition_equation:
##             return condition_equation
##         else:
##             raise Exception("Could not resolve transition condition '%s'" % self.condition)

    @classmethod
    def from_xml(cls, element):
        assert element.tag == NINEML+cls.element_name
        from_ = Reference(Regime, element.get("from"))
        to = Reference(Regime, element.get("to"))
        condition = element.get("condition")
        name = element.get("name")
        assignment_element = element.find(NINEML+Assignment.element_name)
        if assignment_element is not None:
            assignment = Assignment.from_xml(assignment_element)
        else:
            assignment = None
        return cls(from_=from_, to=to, condition=condition, assignment=assignment, name=name)

class Component(object):
    element_name = "component"
    
    def __init__(self, name, parameters = [], regimes = [], transitions = [],
                 ports = [], bindings = []):
        """
        Regime graph should not be edited after contructing a component

        *TODO*: if the user maintains a ref to a regime in regimes,
        etc. they can violate this.  Code generators will need to
        query regimes, transtions, so we can't privatize self
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

        self.name = name

        # check for empty component, we do not support inplace building of a component.
        if not regimes and not transitions:
            raise ValueError, "Component constructor needs at least 'regimes'"+\
                  "or 'transitions' to build component graph."

        # add to transitions from regimes
        # get only true transition objects (not references) from regimes
        # these will be added to transition map in next step
        trans_objects = [t for r in regimes for t in r.transitions if isinstance(t,Transition)]
        # model with no transitions is indeed allowed.

        trans_refs = [t for t in transitions if isinstance(t,Reference)]
        assert not trans_refs, "Component constructor: kwarg 'transitions' may not"+\
               "contain references."

        transitions = set(transitions)
        transitions.update(trans_objects)

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
            if self.regime_map.has_key(r.name):
                raise ValueError, "Regime collection has Regimes with colliding names."
            self.regime_map[r.name] = r

        # build transitions map
        self.transition_map = {}
        for t in transitions:
            if self.transition_map.has_key(t.name):
                raise ValueError, "Transition collection has Transitions with colliding names."
            self.transition_map[t.name] = t
               
        # store final regime and transition sets for this component
        self.regimes = set(regimes)
        self.transitions = set(transitions)

        # We have extracted all implicit knowledge of graph members, proceed to
        # resolve references.
        self.resolve_references()


        # check that there is an island regime only if there is only 1 regime
        island_regimes = set([r for r in self.regimes if not r.transitions and \
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
            bindings+=list(r.bindings())
        #self.bindings = bindings

        # build bindings map
        bindings_map = {}
        for b in bindings:
            assert isinstance(b, Binding), "Received invalid binding."
            if b.name in bindings_map and b.as_expr()!=bindings_map[b.name].as_expr():
                raise ValueError, "Multiple non-equal bindings on '%s' " % b.name
            bindings_map[b.name] = b
        self.bindings_map = bindings_map

        # check bindings only have static parameters and functions on rhs
        self.check_binding_expressions()

        # check we aren't redefining math symbols (like e,pi)
        self.check_non_parameter_symbols()

        # Up till now, we've inferred parameters
        # Now let's check what the user provided
        # is consistant and finally set self.parameters
        if parameters:
            if self.user_parameters!=set(parameters):
                raise ValueError, "Declared parameter list %s does not match inferred parameter list %s." % (str(self.parameters),str(self.user_parameters))

        self.parameters = self.user_parameters
        self.ports = ports
        
        # we should check that parameters is correct
        # even better, we could auto-generate parameters

    def get_regimes_to(self,regime):
        """ Gets as a list all regimes that transition to regime"""
        
        return [t.from_ for t in self.transitions if t.to==regime]
            
    def resolve_references(self):
        """ Uses self.regimes_map and self.transitions_map to resolve references in self.regimes and self.transitions"""

        # resolve transition from_=None to parent regime
        for r in self.regimes:
            for t in r.transitions:
                if t.from_==None:
                    t.from_=r

        # resolve regime references in transitions:
        for t in self.transitions:
            for attr in ('to','from_'):
                r = t.__getattribute__(attr)
                if not isinstance(r,Regime):
                    assert isinstance(r,Reference) and r.cls==Regime, "Expected Regime reference or Regime"
                    t.__setattr__(attr,self.regime_map[r.name])

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
    def equations(self):
        #for transition in self.transitions:
        #    for regime in transition.from_, transition.to:
        for r in self.regimes:
            for t in r.transitions:
                if t.assignment:
                    yield t.assignment
            for equation in r.equations():
                yield equation

    @property
    def conditions(self):
        """ Returns all conditions """
        # TODO events
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

        from nineml.expr_parse import expr_parse

        params = self.user_parameters
        
        for binding in self.bindings:
            names, funcs = expr_parse(binding.rhs)
            undef_funcs = funcs.difference(math_namespace.functions)
            if undef_funcs:
                funcs.difference(math_namespace.functions)
                raise ValueError, "In binding '%s', undefined functions: %s" % \
                      (binding.as_expr(),repr(list(undef_funcs)))


            if binding.name in names:
                raise ValueError, "Binding expression '%s': may not self reference." % binding.name
            non_param_names = self.non_parameter_symbols.intersection(names)
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

        # parse the math blocks

        from nineml.expr_parse import expr_parse
        from nineml.cond_parse import cond_parse

        symbols = set([])
        for e in self.equations:
            names, funcs = expr_parse(e.rhs)
            undef_funcs = funcs.difference(math_namespace.functions)
            if undef_funcs:
                funcs.difference(math_namespace.functions)
                raise ValueError, "In expression '%s', undefined functions: %s" % \
                      (e.as_expr(),repr(list(undef_funcs)))
            
            symbols.update(names)

        # now same for conditions
        for c in self.conditions:
            names, funcs = cond_parse(c)
            undef_funcs = funcs.difference(math_namespace.functions)
            if undef_funcs:
                funcs.difference(math_namespace.functions)
                raise ValueError, "In conditional '%s', undefined functions: %s" % \
                      (c,repr(list(undef_funcs)))
            
            symbols.update(names)


        symbols = symbols.difference(self.non_parameter_symbols)
        symbols = symbols.difference(math_namespace.symbols)
        return symbols.difference(math_namespace.reserved_symbols)

                 
    @property
    @cache
    def non_parameter_symbols(self):
        """ All bindings, assignment and inplace left-hand-sides, plus X for ODE dX/dt = ... """ 
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
    def bound_symbols(self):
        # TODO: cache once determined
        """ Return symbols which are subject to bindings (static assignments)"""
        # construct set of keys (bound symbols)
        statics = set(self.bindings_map)

        # check user is not writing to bound variables
        if statics.intersection(self.integrated_variables)!=set():
            raise ValueError, "Error: user bound symbols which appear on lhs of ODEs"
        if statics.intersection(self.assigned_variables)!=set():
            raise ValueError, "Error: user bound symbols which appear on lhs of Assignments"+\
                  "and Inplace OPs"
        
        return statics



    @property
    @cache
    def assigned_variables(self):
        """ All assignment and inplace lhs' (which may also be ODE integrated variables),
        but not bindings (which are not variables, but static) """

        # TODO: cache once determined
        variables = set([])
        for equation in self.equations:
            if isinstance(equation, (Assignment,Inplace)):
                variables.add(equation.to)
        return variables

    @property
    @cache
    def independent_variables(self):
        """ All X for ODE dY/dX """
        # TODO: cache once determined
        variables = set([])
        for r in self.regimes:
            for eq in r.odes():
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
    
    def to_xml(self):
        elements = [E.parameter(name=p) for p in self.parameters] + \
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

        regimes = []
        for regime_cls in (Sequence, Union):
            for e in element.findall(NINEML+regime_cls.element_name):
                regimes.append(regime_cls.from_xml(e))

        transitions = [Transition.from_xml(t) for t in element.findall(NINEML+Transition.element_name)]

        # allocate new component
        new_comp = cls(element.get("name"), parameters, regimes=regimes, transitions=transitions, bindings=bindings)

        return new_comp


    def write(self, file):
        """
        Export this model to a file in 9ML XML format.

        file is filename or file object.
        """
        doc = E.nineml(self.to_xml(), xmlns=nineml_namespace)
        etree.ElementTree(doc).write(file, encoding="UTF-8",
                                     pretty_print=True, xml_declaration=True)

    def to_dot(self,out, show_contents=True):
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
                ns['regime_name'] = r.name
                ns['contents'] = r.dot_content()
                out.write(t_regime % ns)
        
        for t in self.transitions:
            if show_contents:
    
                out.write('\tregime_%d -> regime_%d [label="%s @ %s"];\n' % (regime_id[t.from_.name], regime_id[t.to.name],
                                                                   t.name.encode('utf-8'), t.condition.encode('utf-8')))
            else:
                out.write('\tregime_%d -> regime_%d [label="%s"];\n' % (regime_id[t.from_.name], regime_id[t.to.name],
                                                                   t.name.encode('utf-8'), t.condition.encode('utf-8')))

        out.write('}')


        
        
        
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

