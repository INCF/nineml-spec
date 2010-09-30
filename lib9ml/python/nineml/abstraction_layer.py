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
    
    def to_xml(self):
        return E(self.element_name,
                 E("math-inline", self.value),
                 name=self.name)

    @classmethod
    def from_xml(cls, element):
        return cls(element.get("name"), element.find(NINEML+"math-inline").text)





class Equation(RegimeElement):
    pass

class Port(object):
    pass

class Event(RegimeElement,Port):
    element_name = "event"
    
    def __init__(self,id):
        self.id = id


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
        

SpikeOutputEvent = Event('spike_output')
SpikeInputEvent = Event('spike_input')
ClockTic = Event('clock_tic')







class ODE(Equation):
    """
    Represents a first-order, ordinary differential equation.
    """
    element_name = "ode"
    n = 0
    
    def __init__(self, dependent_variable, bound_variable, rhs, name=None):
        self.dependent_variable = dependent_variable
        self.bound_variable = bound_variable
        self.rhs = rhs
        self.name = name or ("ODE%d" % ODE.n)
        ODE.n += 1
        
    def __repr__(self):
        return "ODE(d%s/d%s = %s)" % (self.dependent_variable,
                                      self.bound_variable,
                                      self.rhs)

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False

        return reduce(and_, (self.name == other.name,
                             self.dependent_variable == other.dependent_variable,
                             self.bound_variable == other.bound_variable,
                             self.rhs == other.rhs))

    def to_xml(self):
        return E(self.element_name,
                 E("math-inline", self.rhs),
                 name=self.name,
                 dependent_variable=self.dependent_variable,
                 bound_variable = self.bound_variable)

    @classmethod
    def from_xml(cls, element):
        assert element.tag == NINEML+cls.element_name
        rhs = element.find(NINEML+"math-inline").text
        return cls(element.get("dependent_variable"),
                   element.get("bound_variable"),
                   rhs,
                   name=element.get("name"))
    

class Assignment(Equation):
    element_name = "assignment"
    n = 0
    
    def __init__(self, to, expr, name=None):
        self.to = to
        self.expr = expr
        self.name = name or ("Assignment%d" % Assignment.n)
        Assignment.n += 1

    def __repr__(self):
        return "Assignment('%s', '%s')" % (self.to, self.expr)

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

        self.condition = kwargs.get("condition")

        for node in nodes:
            assert isinstance(node, RegimeElement)
            self.add_node(node)


    
    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        
        sort_key = lambda node: node.name
        return reduce(and_, (self.name == other.name, self.condition == other.condition,
                             sorted(self.nodes, key=sort_key) == sorted(other.nodes, key=sort_key)))

    def __repr__(self):
        return "%s(%s)" % (self.__class__.__name__, self.name)

    def _add_node_to_collection(self, node):
        raise UnimplementedError, "Regime is doesn't know how to collect nodes, use subclass Union or Sequence."

    def get_ref(self):
        """ Returns a reference to this regime """
        return Reference(Regime, self.name)

    def add_node(self, node):
        if isinstance(node, Transition):
            if node.from_ is None:
                node.from_=self
            assert node.from_==self, "Regime contains a transition whose from_ is not referencing the Regime."
            assert node.to!=self, "Regime contains a transition to itself!."

        self._add_node_to_collection(node)


    def transitions(self):
        """
        Yields all the transitions contained within this Regime
        """
        for node in self.nodes:
            if isinstance(node, Transition):
                assert node.from_==self, "Regime contains Transition for which from_!=Regime"
                yield node
            #else:
            #    for t in node.Transitions():
            #        yield t


    def neighbors(self):
        """ Get all regimes we transition to """
        for t in self.transitions():
            yield t.to

    def get_transition_to(self,to):
        """ Returns transition if Regime transitions to Regime 'to', otherwise None """

        for t in self.transitions():
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
        for node in self.nodes:
            if isinstance(node, Equation):
                yield node
            elif isinstance(node, Regime):
                for eqn in node.equations():
                    yield eqn
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
        transitions = self.transitions()
        for t in transitions:
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
        if self.condition:
            kwargs['condition'] = self.condition
        return E(self.element_name,
                 name=self.name,
                 *[node.to_xml() for node in self.nodes], **kwargs)

    @classmethod
    def from_xml(cls, element):
        assert element.tag == NINEML+cls.element_name
        nodes = []
        tag_class_map = {}
        name = element.get("name")
        condition = element.get("condition")
        for node_cls in (ODE, Assignment, Sequence, Union, Event, Transition, Inplace):
            tag_class_map[NINEML+node_cls.element_name] = node_cls
        for elem in element.iterchildren():
            node_cls = tag_class_map[elem.tag]
            tmp = node_cls.from_xml(elem)
            if isinstance(tmp, Transition):
                assert tmp.from_.name == name, "Regime contains a Transition node for which from_!=Regime."
                tmp.from_ = None # resolved by Regime contructor to be Regime parent.
            nodes.append(tmp)
        if name is not None:
            kwargs = {"name": name}
        if condition is not None:
            kwargs["condition"] = condition
        return cls(*nodes, **kwargs)
        

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


def On(condition, do=None):
    """ returns new RegimeElement which executes 'do' if condition is True

    do is a Regime, RegimeElement, iterable of expressions (mapped with expr_to_obj),
    
    """

    if isinstance(do, (Regime, RegimeElement)):
        # make interal deepcopy of do, so that user
        # cannot modify it after we add it.
        # such as i.e. add transitions

        # TODO: test suite here to evaluate
        # that user has lost all references to 'do' internals
        
        do = copy.deepcopy(do)

        # If user keeps a reference to do and
        # adds it elsewhere in the code, we have
        # the chance to have two regimes with same name
        # as our copy has the same name as original do.
        #
        # TODO: test suite should raise exception if
        #  user tries to define two regimes with same name
        

        do = Sequence(do, condition=condition)

    # Support do as an interable of expressions
    elif hasattr(do,'__iter__'):
        try:
            do = map(expr_to_obj,do)
        except ValueError:
            raise ValueError, "if On 'do' kwargs is iterable, it must be map-able with expr_to_obj."

        # make a sequence of it
        # NB: user has no reference to this sequence
        do = Sequence(*do, condition=condition)

    else:
        raise ValueError, "Invalid object for 'do' kwargs"

    return do
        


        

class Transition(RegimeElement):
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
    
    def __init__(self, name, parameters, regimes = [], initial_regime = None, ports = [], bindings=[]):
        """ Regime graph should not be edited after contructing a component"""

        self.name = name
        self.parameters = parameters

        # if user only provides initial_regime, there must be no references in graph
        # TODO: maybe make deepcopies of everything
        # so the user can't edit the graph after Component construction
        if regimes == []:
            if initial_regime:
                self.regimes = initial_regime.regimes_in_graph()
                self.initial_regime = initial_regime 
            else:
                raise ValueError, "Need either regimes, or initial_regime without references in the graph."

        else:
            if initial_regime:
                self.initial_regime = initial_regime
            else:
                self.initial_regime = regimes[0]

            self.regimes = set(regimes)

        # build regime map
        self.regime_map = {}
        for r in self.regimes:
            if self.regime_map.has_key(r.name):
                raise ValueError, "Regime collection had Regimes with colliding names."
            self.regime_map[r.name] = r

        # check that user didn't define superfluous regimes
        if self.initial_regime.regimes_in_graph() != self.regimes:
            raise ValueError, "User defined island regimes."

        # Resolve any remaining Regime references in transitions

        for t in self.transitions:
            if isinstance(t, Reference):
                t.to = regime_map[t.to.name]
                t.from_ = regime_map[t.from_.name]
        
        # Allow strings for bindings, map using expr_to_obj
        # Eliminate duplicates
        bindings = set(map(expr_to_obj,set(bindings)))
        for b in bindings:
            assert isinstance(b, Binding), "Received invalid binding."
        self.bindings = bindings
        
        self.ports = ports
        # we should check that parameters is correct
        # even better, we could auto-generate parameters

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False

        sort_key = lambda node: node.name

        return reduce(and_, (self.name == other.name,
                             self.parameters == other.parameters,
                             sorted(self.transitions, key=sort_key) == sorted(other.transitions, key=sort_key),
                             sorted(self.regimes, key=sort_key) == sorted(other.regimes, key=sort_key),
                             self.bindings == other.bindings))
    
    @property
    def equations(self):
        #for transition in self.transitions:
        #    for regime in transition.from_, transition.to:
        for r in self.regimes:
            for t in r.transitions():
                if t.assignment:
                    yield transition.assignment
            for equation in regime.equations():
                yield equation

    @property
    def transitions(self):
        trans_set = set()
        for r in self.regimes:
            trans_set.update(r.transitions())
        return trans_set
                        
    #@property
    #def regimes(self):
    #    regime_set = set([])
    #    for transition in self.transitions:
    #        regime_set.add(transition.from_)
    #        regime_set.add(transition.to)
    #    return regime_set

    @property
    def fixed_parameters(self):
        # for now we trust the parameters list, but really we
        # should parse the math blocks, and cross-check
        return set(self.parameters).difference(self.dependent_variables).difference(self.independent_variables)
       
    @property
    def independent_variables(self):
        variables = set([])
        #for equation in self.equations:
        #    if isinstance(equation, ODE):
        #        variables.add(equation.bound_variable)
        for r in self.regimes:
            for eq in r.odes():
                variables.add(eq.bound_variable)
        return variables
                 
    @property
    def dependent_variables(self):
        variables = set([])
        for equation in self.equations:
            if isinstance(equation, ODE):
                variables.add(equation.dependent_variable)
            elif isinstance(equation, Assignment):
                variables.add(equation.to)
        for binding in self.bindings:
            variables.add(binding.name)
        return variables
        
    @property
    def assigned_variables(self):
        variables = set([])
        for equation in self.equations:
            if isinstance(equation, Assignment):
                variables.add(equation.to)
        for binding in self.bindings:
            variables.add(binding.name)
        return variables.difference(self.integrated_variables)
    
    @property
    def integrated_variables(self): # "state" variables?
        variables = set([])
        for equation in self.equations:
            if isinstance(equation, ODE):
                variables.add(equation.dependent_variable)
        return variables
    
    def to_xml(self):
        elements = [E.parameter(name=p) for p in self.parameters] + \
                   [r.to_xml() for r in self.regimes] + \
                   [b.to_xml() for b in self.bindings] # +\
                   # Transitions are now regime nodes
                   #[t.to_xml() for t in self.transitions]
        attrs = {"name": self.name, "initial_regime": self.initial_regime.name}
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

        initial_regime_name = element.get("initial_regime")
        if not initial_regime_name:
            raise ValueError, "Component attribute 'initial_regime' not declared."

        regime_map = {}
        for regime_cls in (Sequence, Union):
            for e in element.findall(NINEML+regime_cls.element_name):
                r = regime_cls.from_xml(e)
                if regime_map.has_key(r.name):
                    raise ValueError, "Regime collection has Regimes with colliding names."
                regime_map[r.name] = r

        # Set initial regime
        try:
            initial_regime = regime_map[initial_regime_name]
        except KeyError:
            raise ValueError, "Declared initial regime '%s' not found in component element tree." % s
        
        # Transtions are part of regimes, as they cannot exist without a from_
        #transitions = set([Transition.from_xml(t) for t in element.findall(NINEML+Transition.element_name)])
        regimes = set(regime_map.values())

        # resolve transition references
        for r in regimes:
            for t in r.transitions():
                t.to = regime_map[t.to.name]
                r = regime_map[t.from_.name]
                t.from_ = r

        # allocate new component
        new_comp = cls(element.get("name"), parameters, regimes, initial_regime, bindings)

        return new_comp


    def write(self, file):
        """
        Export this model to a file in 9ML XML format.

        file is filename or file object.
        """
        doc = E.nineml(self.to_xml(), xmlns=nineml_namespace)
        etree.ElementTree(doc).write(file, encoding="UTF-8",
                                     pretty_print=True, xml_declaration=True)
        
        
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

