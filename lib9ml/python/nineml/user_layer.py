"""
Python module for reading/writing 9ML user layer files in XML format.

Functions
---------

    parse - read a 9ML file in XML format and parse it into a Model instance.

Classes
-------
    Model
    Definition
    BaseComponent
        SpikingNodeType
        SynapseType
        CurrentSourceType
        Structure
        ConnectionRule
        ConnectionType
        RandomDistribution
    Parameter
    ParameterSet
    Value
    Group
    Population
    PositionList
    Projection

Copyright Andrew P. Davison, 2010 # if you edit this file, add your name here
"""

from itertools import chain
import urllib
from lxml import etree
from lxml.builder import E
from operator import and_, or_

nineml_namespace = 'http://nineml.org/9ML/0.1'
NINEML = "{%s}" % nineml_namespace
PARAMETER_NAME_AS_TAG_NAME = True

def parse(url):
    """
    Read a NineML user-layer file and return a Model object.
    
    If the URL does not have a scheme identifier, it is taken to refer to a
    local file.
    """
    if not isinstance(url,file):
        f = urllib.urlopen(url)
        doc = etree.parse(f)
        f.close()
    else:
        doc = etree.parse(url)
        
    root = doc.getroot()
    for import_element in root.findall(NINEML+"import"):
        url = import_element.find(NINEML+"url").text
        imported_doc = etree.parse(url)
        root.extend(imported_doc.getroot().iterchildren())
    return Model.from_xml(root)


class Model(object):
    """
    Representation of an entire 9ML model.
    """

    def __init__(self, name):
        """
        Create an empty model with a given name.
        """
        self.name = name
        self.components = {}
        self.groups = {}
        self._unresolved = {}
    
    def __eq__(self, other):
        return reduce(and_, (self.name == other.name,
                             self.components == other.components,
                             self.groups == other.groups))
        
    def __ne__(self, other):
        return not self == other
    
    def add_component(self, component):
        """
        Add a component, defined in a 9ML abstraction layer file, to the model.
        
        Components include spiking nodes, synapse models, random number
        distributions, network structure representations, connection methods.
        
        `component` - should be a sub-class of BaseComponent.
        """
        assert isinstance(component, BaseComponent), type(component)
        if component.unresolved:
            # try to resolve it
            if component.reference in self.components:
                component.resolve(self.components[component.reference])
            # otherwise add to the list of unresolved components
            else:
                self._unresolved[component.reference] = component
        else:
            # see if this component can be used to resolve an unresolved one
            if component.name in self._unresolved:
                other_component = self._unresolved.pop(component.name)
                other_component.resolve(component)
        if component.name in self.components and self.components[component.name] != component:
            raise Exception("A different component with the name '%s' already exists" % component.name)
        self.components[component.name] = component
    
    def _resolve_components(self):
        for component in self.components.values():
            if component.unresolved:
                component.resolve(self.components[component.reference])
    
    def add_group(self, group):
        """
        Add a group to the model. Groups contain populations of nodes, where
        the nodes may be either individual neurons or other groups.
        
        `group` - should be a Group instance.
        """
        assert isinstance(group, Group)
        for component in group.get_components():
            self.add_component(component)
        for subgroup in group.get_subgroups():
            self.add_group(subgroup)
        self.groups[group.name] = group   
    
    @classmethod
    def from_xml(cls, element):
        """
        Parse an XML ElementTree structure and return a Model instance.
        
        `element` - should be an ElementTree Element instance.
        
        See:
            http://docs.python.org/library/xml.etree.elementtree.html
            http://codespeak.net/lxml/
        """
        assert element.tag == NINEML+'nineml'
        model = cls(element.attrib["name"])        
        # Note that the components dict initially contains elementtree elements,
        # but is modified within Group.from_xml(), and at the end contains
        # Component instances.
        components = {}
        groups = {}
        for child in element.findall(NINEML+BaseComponent.element_name):
            components[child.attrib["name"]] = child
        for child in element.findall(NINEML+Group.element_name):
            group = Group.from_xml(child, components, groups)
            model.groups[group.name] = group
        for name, c in components.items():
            assert isinstance(c, BaseComponent), "%s is %s" % (name, c)
        model.components = components
        model._resolve_components()
        return model

    def to_xml(self):
        """
        Return an ElementTree representation of this model.
        """
        # this should determine where references can be used to avoid duplication
        assert len(self._unresolved) == 0, str(self._unresolved)
        root = E("nineml", xmlns=nineml_namespace, name=self.name)
        for component in self.components.values():
            root.append(component.to_xml())
        for group in self.groups.values():
            root.append(group.to_xml())
        return root

    def write(self, filename):
        """
        Export this model to a file in 9ML XML format.
        """
        assert isinstance(filename, basestring) or (hasattr(filename, "seek") and hasattr(filename, "read"))
        etree.ElementTree(self.to_xml()).write(filename, encoding="UTF-8",
                                               pretty_print=True, xml_declaration=True)

    def check(self):
        """
        Export the model to XML, read it back in, and check that the model is
        unchanged.
        """
        import StringIO
        f = StringIO.StringIO()
        self.write(f)
        f.seek(0)
        new_model = self.__class__.from_xml(etree.parse(f).getroot())
        f.close()
        assert self == new_model


class Definition(object):
    """
    Encapsulate a component definition.
    
    For now, this holds only the URI of an abstraction layer file, but this
    could be expanded later to include definitions external to 9ML.
    """
    element_name = "definition"

    def __init__(self, url):
        self.url = url

    def __eq__(self, other):
        return self.url == other.url

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        return hash(self.url)

    def retrieve(self):
        f = urllib.urlopen(self.url)
        content = f.read()
        f.close()
        return content

    def to_xml(self):
        return E(self.element_name, (E.url(self.url)))

    @classmethod
    def from_xml(cls, element):
        return cls(element.find(NINEML+"url").text)


class BaseComponent(object):
    """
    Base class for model components that are defined in the abstraction layer.
    
    This is called a node in Anatoli's document AG-YLF-20091218.01, but I don't
    think "node" is a good name as it is easily confused with "node in a
    neuronal network" (something that emits spikes).
    """
    element_name = "node"

    def __init__(self, name, definition=None, parameters={}, reference=None):
        """
        Create a new component with the given name, definition and parameters,
        or create a reference to another component that will be resolved later.
        
        `name` - a name for the component that can be used to reference it.
        `definition` - a Definition instance, the URL of a component definition,
                       or None if creating a reference.
        `parameters` - a ParameterSet instance or a dictionary containing
                       (value,unit) pairs.
        `reference` - the name of another component in the model, or None.
        """
        self.name = name
        if isinstance(definition, Definition):
            self.definition = definition
            assert reference is None, "Cannot give both definition and reference."
        elif isinstance(definition, basestring): # should also check is a valid uri
            self.definition = Definition(url=definition)
            assert reference is None, "Cannot give both definition and reference."
        elif definition is None:
            assert reference is not None, "Either definition or reference must be given."
            assert isinstance(reference, basestring), "reference should be the name of a component"
            self.definition = None
        else:
            raise Exception()
        if isinstance(parameters, ParameterSet):
            self.parameters = parameters
        elif isinstance(parameters, dict):
            self.parameters = ParameterSet(**parameters)
        else:
            raise Exception()
        self.reference = reference

    def __eq__(self, other):
        assert isinstance(other, self.__class__)
        assert not (self.unresolved or other.unresolved)
        return reduce(and_, (self.name==other.name,
                             self.definition==other.definition,
                             self.parameters==other.parameters))

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        assert not self.unresolved
        return hash(self.__class__) ^ hash(self.name) ^ hash(self.definition) ^ hash(self.parameters)

    def __repr__(self):
        if self.definition:
            return '%s(name="%s", definition="%s")' % (self.__class__.__name__,
                                                       self.name, self.definition.url)
        else:
            return '%s(name="%s", UNRESOLVED)' % (self.__class__.__name__, self.name)

    @property
    def unresolved(self):
        return self.definition is None
    
    def resolve(self, other_component):
        """
        If the component is unresolved (contains a reference to another
        component), copy the definition and parameters from the other component,
        and update those parameters with the parameters from this component.
        """
        assert other_component.__class__ == self.__class__
        assert self.reference == other_component.name
        self.definition = other_component.definition
        self.parameters.complete(other_component.parameters) # note that this behaves oppositely to dict.update
    
    def get_definition(self):
        import nineml.abstraction_layer
        return nineml.abstraction_layer.parse(self.definition.url)
    
    def to_xml(self):
        element = E(self.element_name,
                    self.definition.to_xml(),
                    self.parameters.to_xml(),
                    name=self.name)
        return element
    
    @classmethod
    def from_xml(cls, element, components):
        if element.tag != NINEML+cls.element_name:
            raise Exception("Expecting tag name %s%s, actual tag name %s" % (
                NINEML, cls.element_name, element.tag))
        name = element.attrib.get("name", None)
        parameters = ParameterSet.from_xml(element.find(NINEML+ParameterSet.element_name), components)
        definition_element = element.find(NINEML+Definition.element_name)
        if definition_element is not None:
            definition = Definition.from_xml(definition_element)
            return cls(name, definition, parameters)
        else:
            reference_element = element.find(NINEML+"reference")
            if reference_element is not None:
                return cls(name, None, parameters, reference=reference_element.text)
            else:
                raise Exception("A component must contain either a defintion or a reference")


class SpikingNodeType(BaseComponent):
    """
    Component representing a model of a spiking node, i.e. something that can
    emit (and optionally receive) spikes.
    
    Should perhaps be called SpikingNodePrototype, since this is type + parameters
    """
    pass


class SynapseType(BaseComponent):
    """
    Component representing a model of a post-synaptic response, i.e. the current
    produced in response to a spike.
    
    This class is probably mis-named. Should be PostSynapticResponseType.
    """
    pass


class CurrentSourceType(BaseComponent):
    """
    Component representing a model of a current source that may be injected into
    a spiking node.
    """
    pass


class Structure(BaseComponent):
    """
    Component representing the structure of a network, e.g. 2D grid, random
    distribution within a sphere, etc.
    """
    pass

    def generate_positions(self, number):
        """
        Generate a number of node positions according to the network structure. 
        """
        raise NotImplementedError


class ConnectionRule(BaseComponent):
    """
    Component representing an algorithm for connecting two populations of nodes.
    """
    pass


class ConnectionType(BaseComponent):
    """
    Component representing a model of a synaptic connection, including weight,
    delay, optionally a synaptic plasticity rule.
    """
    pass

    
class RandomDistribution(BaseComponent):
    """
    Component representing a random number distribution, e.g. normal, gamma,
    binomial.
    """
    pass


class Parameter(object):
    """
    Representation of a numerical- or string-valued parameter.
    
    A numerical parameter is a (name, value, unit) triplet, a string parameter
    is a (name, value) pair.
    
    Numerical values may either be numbers, or a component that generates
    numbers, e.g. a RandomDistribution instance.
    """
    element_name = "value" # only used if PARAMETER_NAME_AS_TAG_NAME is False
    
    def __init__(self, name, value, unit=None):
        self.name = name
        self.value = value
        self.unit = unit
    
    def __repr__(self):
        return "Parameter(name=%s, value=%s, unit=%s)" % (self.name, self.value, self.unit)

    def __eq__(self, other):
        return reduce(and_, (self.name==other.name,
                             self.value==other.value,
                             self.unit==other.unit)) # obviously we should resolve the units, so 0.001 V == 1 mV

    def __ne__(self, other):
        return not self == other
    
    def __hash__(self):
        return hash(self.name) ^ hash(self.value) ^ hash(self.unit)

    def is_random(self):
        return isinstance(self.value, RandomDistribution)

    def _to_xml_name_as_tag(self):
        if isinstance(self.value, RandomDistribution):
            value_element = E.reference(self.value.name)
        else:
            value_element = str(self.value)
        if self.unit:
            return E(self.name,
                     E(Value.element_name, value_element),
                     E.unit(self.unit))
        else:
            return E(self.name,
                     E(Value.element_name, value_element))
            
    def _to_xml_generic_tag(self):
        if isinstance(self.value, RandomDistribution):
            value_element = E.reference(self.value.name)
        else:
            value_element = str(self.value)
        attrs = {"parameter": self.name}
        if self.unit:
            attrs["unit"] = self.unit
        return E(Parameter.element_name,
                 value_element,
                 **attrs)
    
    def to_xml(self):
        if PARAMETER_NAME_AS_TAG_NAME:
            return self._to_xml_name_as_tag()
        else:
            return self._to_xml_generic_tag()
    

    @classmethod
    def _from_xml_name_as_tag(cls, element, components):
        value = Value.from_xml(element.find(NINEML+Value.element_name), components)
        unit_element = element.find(NINEML+"unit")
        if unit_element is None:
            unit = None
        else:
            unit = unit_element.text
        return Parameter(name=element.tag.replace(NINEML,""),
                         value=value,
                         unit=unit)
    
    @classmethod
    def _from_xml_generic_tag(cls, element, components):
        assert element.tag == NINEML+cls.element_name, "Found <%s>, expected <%s>" % (element.tag, cls.element_name)
        return Parameter(name=element.attrib["parameter"],
                         value = Value.from_xml(element, components),
                         unit = element.get("unit"))
    
    @classmethod
    def from_xml(cls, element, components):
        if PARAMETER_NAME_AS_TAG_NAME:
            return cls._from_xml_name_as_tag(element, components)
        else:
            return cls._from_xml_generic_tag(element, components)


class ParameterSet(dict):
    """
    Container for the set of parameters for a component.
    """
    element_name = "properties"
    
    def __init__(self, *parameters, **kwparameters):
        """
        `*parameters` - should be Parameter instances
        `**kwparameters` - should be name=(value,unit)
        """
        for parameter in parameters:
            self[parameter.name] = parameter # should perhaps do a copy
        for name, (value, unit) in kwparameters.items():
            self[name] = Parameter(name, value, unit)
    
    def __hash__(self):
        return hash(tuple(self.items()))
    
    def __repr__(self):
        return "ParameterSet(%s)" % dict(self)
    
    def complete(self, other_parameter_set):
        """
        Pull parameters from another parameter set into this one, if they do
        not already exist in this one.
        """
        for name,parameter in other_parameter_set.items():
            if name not in self:
                self[name] = parameter # again, should perhaps copy
    
    def get_random_distributions(self):
        return [p.value for p in self.values() if p.is_random()]
    
    def to_xml(self):
        return E(self.element_name,
                 *[p.to_xml() for p in self.values()])
    
    @classmethod
    def from_xml(cls, element, components):
        assert element.tag == NINEML+cls.element_name
        parameters = []
        for parameter_element in element.getchildren():
            parameters.append(Parameter.from_xml(parameter_element, components))
        return cls(*parameters)


class Value(object):
    """
    Not intended to be instantiated: just provides the from_xml() classmethod.
    """
    element_name = "value"
    
    @classmethod
    def from_xml(cls, element, components):
        """
        Parse an XML ElementTree structure and return either a numerical or
        string value or something that generates a numerical value, e.g. a
        RandomDistribution instance.
        
        `element` - should be an ElementTree Element instance.
        """
        #rd_element = element.find(NINEML+RandomDistribution.element_name)
        ref_element = element.find(NINEML+"reference")
        #if rd_element is not None:
        #    value = RandomDistribution.from_xml(rd_element)
        #elif ref_element is not None:
        if ref_element is not None:
            value = get_or_create_component(ref_element.text, RandomDistribution, components)
        else:
            try:
                value = float(element.text)
            except ValueError:
                value = element.text
        return value


class Group(object):
    """
    Container for populations and projections between those populations. May be
    used as the node prototype within a population, allowing hierarchical
    structures.
    """
    element_name = "group"
    
    def __init__(self, name):
        self.name = name
        self.populations = {}
        self.projections = {}
        self.selections = {}
    
    def __eq__(self, other):
        return reduce(and_, (self.name==other.name,
                             self.populations==other.populations,
                             self.projections==other.projections,
                             self.selections==other.selections))
    
    def __ne__(self, other):
        return not self == other
    
    def add(self, *objs):
        """
        Add one or more Population, Projection or Selection instances to the group.
        """
        for obj in objs:
            if isinstance(obj, Population):
                self.populations[obj.name] = obj
            elif isinstance(obj, Projection):
                self.projections[obj.name] = obj
            elif isinstance(obj, Selection):
                self.selections[obj.name] = obj
            else:
                raise Exception("Groups may only contain Populations, Projections, Selections or Groups")

    def _resolve_population_references(self):
        for prj in self.projections.values():
            for name in ('source', 'target'):
                if prj.references[name] in self.populations:
                    obj = self.populations[prj.references[name]]
                elif prj.references[name] in self.selections:
                    obj = self.selections[prj.references[name]]
                elif prj.references[name] == self.name:
                    obj = self
                else:
                    raise Exception("Unable to resolve population/selection reference ('%s') for %s of %s" % (prj.references[name], name, prj))
                setattr(prj, name, obj)
    
    def get_components(self):
        components = []
        for p in chain(self.populations.values(), self.projections.values()):
            components.extend(p.get_components())
        return components
    
    def get_subgroups(self):
        return [p.prototype for p in self.populations.values() if isinstance(p.prototype, Group)]
    
    def to_xml(self):
        return E(self.element_name,
                 name=self.name,
                 *[p.to_xml() for p in chain(self.populations.values(),
                                             self.selections.values(),
                                             self.projections.values())])
    
    @classmethod
    def from_xml(cls, element, components, groups):
        assert element.tag == NINEML+cls.element_name
        group = cls(name=element.attrib["name"])
        groups[group.name] = group
        for child in element.getchildren():
            if child.tag == NINEML+Population.element_name:
                obj = Population.from_xml(child, components, groups)
            elif child.tag == NINEML+Projection.element_name:
                obj = Projection.from_xml(child, components)
            elif child.tag == NINEML+Selection.element_name:
                obj = Selection.from_xml(child, components)
            else:
                raise Exception("<%s> elements may not contain <%s> elements" % (cls.element_name, child.tag))
            group.add(obj)
        group._resolve_population_references()
        return group



def get_or_create_component(ref, cls, components):
    """
    Each entry in `components` is either an instance of a BaseComponent subclass,
    or the XML (elementtree Element) defining such an instance.
    
    If given component does not exist, we create it and replace the XML in
    `components` with the actual component. We then return the component.
    """
    assert ref in components, "%s not in %s" % (ref, components.keys())
    if not isinstance(components[ref], BaseComponent):
        components[ref] = cls.from_xml(components[ref], components)
    return components[ref]

def get_or_create_prototype(prototype_ref, components, groups):
    if prototype_ref in groups:
        return groups[prototype_ref]
    else:
        return get_or_create_component(prototype_ref, SpikingNodeType, components)


class Population(object):
    """
    A collection of network nodes all of the same type. Nodes may either be
    individual spiking nodes (neurons) or groups (motifs, microcircuits,
    columns, etc.)
    """
    element_name = "population"
    
    def __init__(self, name, number, prototype, positions):
        self.name = name
        self.number = number
        assert isinstance(prototype, (SpikingNodeType, Group))
        self.prototype = prototype
        assert isinstance(positions, PositionList)
        self.positions = positions
    
    def __eq__(self, other):
        return reduce(and_, (self.name==other.name,
                             self.number==other.number,
                             self.prototype==other.prototype,
                             self.positions==other.positions))
    
    def __ne__(self, other):
        return not self == other
    
    def __str__(self):
        return 'Population "%s": %dx"%s" %s' % (self.name, self.number, self.prototype.name, self.positions)
    
    def get_components(self):
        components = []
        if self.prototype:
            if isinstance(self.prototype, SpikingNodeType):
                components.append(self.prototype)
                components.extend(self.prototype.parameters.get_random_distributions())
            elif isinstance(self.prototype, Group):
                components.extend(self.prototype.get_components())
        return components + self.positions.get_components()
    
    def to_xml(self):
        return E(self.element_name,
                 E.number(str(self.number)),
                 E.prototype(self.prototype.name),
                 self.positions.to_xml(),
                 name=self.name)

    @classmethod
    def from_xml(cls, element, components, groups):
        assert element.tag == NINEML+cls.element_name
        prototype_ref = element.find(NINEML+'prototype').text
        return cls(name=element.attrib['name'],
                   number=int(element.find(NINEML+'number').text),
                   prototype=get_or_create_prototype(prototype_ref, components, groups),
                   positions=PositionList.from_xml(element.find(NINEML+PositionList.element_name), components))


class PositionList(object):
    """
    Represents a list of network node positions. May contain either an
    explicit list of positions or a Structure instance that can be used to
    generate positions.
    """
    element_name = "positions"
    
    def __init__(self, positions=[], structure=None):
        """
        Create a new PositionList.
        
        Either `positions` or `structure` should be provided. Providing both
        will raise an Exception.
        
        `positions` should be a list of (x,y,z) tuples or a 3xN (Nx3?) numpy array.
        `structure` should be a Structure component.
        """
        if positions and structure:
            raise Exception("Please provide either positions or structure, not both.")
        assert not isinstance(positions, Structure)
        self._positions = positions
        if isinstance(structure, Structure):
            self.structure = structure
        elif structure is None:
            self.structure = None
        else:
            raise Exception("structure is", structure)

    def __eq__(self, other):
        if self._positions:
            return self._positions == other._positions
        else:
            return self.structure == other.structure
    
    def __ne__(self, other):
        return not self == other
    
    def __str__(self):
        if self.structure:
            return "positioned according to '%s'" % self.structure.name
        else:
            return "with explicit position list"
    
    def get_positions(self, population):
        """
        Return a list or 1D numpy array of (x,y,z) positions.
        """
        if self._positions:
            assert len(self._positions) == population.number
            return self._positions
        elif self.structure:
            return self.structure.generate_positions(population.number)
        else:
            raise Exception("Neither positions nor structure is set.")
    
    def get_components(self):
        if self.structure:
            return [self.structure]
        else:
            return []
    
    def to_xml(self):
        element = E(self.element_name)
        if self._positions:
            for pos in self._positions:
                x,y,z = pos
                element.append(E.position(x=str(x),y=str(y),z=str(z),unit="um"))
        elif self.structure:
            element.append(E.structure(self.structure.name))
        else:
            raise Exception("Neither positions nor structure is set.")
        return element
    
    @classmethod
    def from_xml(cls, element, components):
        assert element.tag == NINEML+cls.element_name
        structure_element = element.find(NINEML+'structure')
        if structure_element is not None:
            return cls(structure=get_or_create_component(structure_element.text, Structure, components))
        else:
            positions = [(float(p.attrib['x']), float(p.attrib['y']), float(p.attrib['z']))
                         for p in element.findall(NINEML+'position')]
            return cls(positions=positions)

# this approach is crying out for a class factory
class Operator(object):
    
    def __init__(self, *operands):
        self.operands = operands
    
    def __eq__(self, other):
        return self.operands == other.operands
    
    def __ne__(self, other):
        return not self == other
    
    def to_xml(self):
        operand_elements = []
        for c in self.operands:
            if isinstance(c, (basestring, float, int)):
                operand_elements.append(E(Value.element_name, str(c)))
            else:
                operand_elements.append(c.to_xml())
        return E(self.element_name,
                 *operand_elements)
    
    @classmethod
    def from_xml(cls, element, components):
        if hasattr(cls, "element_name") and element.tag == NINEML+cls.element_name:
            dispatch = {
                NINEML+Value.element_name: Value.from_xml,
                NINEML+Eq.element_name: Eq.from_xml,
                NINEML+Any.element_name: Any.from_xml,
                NINEML+All.element_name: All.from_xml,
                NINEML+Not.element_name: Not.from_xml,
                NINEML+In.element_name: In.from_xml,
            }
            operands = []
            for child in element.iterchildren():
                operands.append(dispatch[element.tag](child, components))
            return cls(*operands)
        else:
            return {
                NINEML+Eq.element_name: Eq,
                NINEML+Any.element_name: Any,
                NINEML+All.element_name: All,
                NINEML+Not.element_name: Not,
                NINEML+Value.element_name: Value,
                NINEML+In.element_name: In,
            }[element.tag].from_xml(element, components)

def qstr(obj):
    if isinstance(obj, basestring):
        return '"%s"' % obj
    else:
        return obj.__str__()

class SelectionOperator(Operator):
    pass

class Any(SelectionOperator):
    element_name = "any"
    
    def __str__(self):
        return "(" + ") or (".join(qstr(op) for op in self.operands) + ")"
    
class All(SelectionOperator):
    element_name = "all"
    
    def __str__(self):
        return "(" + ") and (".join(qstr(op) for op in self.operands) + ")"
    
class Not(SelectionOperator):
    element_name = "not"

    def __init__(self, *operands):
        assert len(operands) == 1
        SelectionOperator.__init__(self, *operands)

class Comparison(Operator):
    
    def __init__(self, value1, value2):
        Operator.__init__(self, value1, value2)

class Eq(Comparison):
    element_name = "equal"
    
    def __str__(self):        
        return "(%s) == (%s)" % tuple(qstr(op) for op in self.operands)

class In(Comparison):
    element_name = "in"
    
    def __init__(self, item, sequence):
        Operator.__init__(self, item, sequence)

    def __str__(self):
        item = self.operands[0]
        sequence = self.operands[1:]
        return "%s in %s" % tuple(qstr(op) for op in self.operands)
    

class Selection(object):
    """
    A set of network nodes selected from existing populations within the Group.
    """
    element_name = "set"
    
    def __init__(self, name, condition):
        """
        condition - instance of an Operator subclass
        """
        assert isinstance(condition, Operator)
        self.name = name
        self.condition = condition

    def __eq__(self, other):
        return reduce(and_, (self.name==other.name,
                             self.condition==other.condition))

    def __ne__(self, other):
        return not self == other
    
    def to_xml(self):
        return E(self.element_name,
                 E.select(self.condition.to_xml()),
                 name=self.name)

    @classmethod
    def from_xml(cls, element, components):
        assert element.tag == NINEML+cls.element_name
        select_element = element.find(NINEML+'select')
        assert len(select_element) == 1
        return cls(element.attrib["name"],
                   Operator.from_xml(select_element.getchildren()[0], components))

      
class Projection(object):
    """
    A collection of connections between two Populations.
    
    If the populations contain spiking nodes, this is straightforward. If the
    populations contain groups, it is not so obvious. I guess the interpretation
    is that connections are made to all the populations within all the groups,
    recursively.
    """
    element_name = "projection"
    
    def __init__(self, name, source, target, rule, synaptic_response, connection_type):
        """
        Create a new projection.
        
        name - a name for this Projection
        source - the presynaptic Population
        target - the postsynaptic Population
        rule - a ConnectionRule instance, encapsulating an algorithm for wiring
               up the connections.
        synaptic_response - a PostSynapticResponse instance that will be used by
                            all connections.
        connection_type - a ConnectionType instance that will be used by all
                          connections.
        """
        self.name = name
        self.references = {}
        self.source = source
        self.target = target
        self.rule = rule
        self.synaptic_response = synaptic_response
        self.connection_type = connection_type
        for name, cls_list in (('source', (Population, Selection, Group)),
                               ('target', (Population, Selection, Group)),
                               ('rule', (ConnectionRule,)),
                               ('synaptic_response', (SynapseType,)),
                               ('connection_type', (ConnectionType,))):
            attr = getattr(self, name)
            if isinstance(attr, cls_list):
                self.references[name] = attr.name
            elif isinstance(attr, basestring):
                setattr(self, name, None)
                self.references[name] = attr
            else:
                raise Exception()

    def __eq__(self, other):
        test_attributes = ["name", "source", "target", "rule", "synaptic_response", "connection_type"]
        # to avoid infinite recursion, we do not include source or target in the tests if they are Groups
        if isinstance(self.source, Group):
            test_attributes.remove("source")
        if isinstance(self.target, Group):
            test_attributes.remove("target")
        return reduce(and_,
                      (getattr(self, attr)==getattr(other, attr) for attr in test_attributes)
                     )

    def __ne__(self, other):
        return not self == other
    
    def get_components(self):
        components = []
        for name in ('rule', 'synaptic_response', 'connection_type'):
            component = getattr(self, name)
            if component is not None:
                components.append(component)
        return components

    def to_xml(self):
        return E(self.element_name,
                 E.source(self.source.name),
                 E.target(self.target.name),
                 E.rule(self.rule.name),
                 E.response(self.synaptic_response.name),
                 E.synapse(self.connection_type.name),
                 name=self.name)

    @classmethod
    def from_xml(cls, element, components):
        assert element.tag == NINEML+cls.element_name
        return cls(name=element.attrib["name"],
                   source=element.find(NINEML+"source").text,
                   target=element.find(NINEML+"target").text,
                   rule=get_or_create_component(element.find(NINEML+"rule").text, ConnectionRule, components),
                   synaptic_response=get_or_create_component(element.find(NINEML+"response").text, SynapseType, components),
                   connection_type=get_or_create_component(element.find(NINEML+"synapse").text, ConnectionType, components))
