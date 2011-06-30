

# Automatically Generated Testing Skeleton Template:
import warnings
import unittest
import nineml






# Testing Skeleton for class: ComponentClass

class ComponentClass_test(unittest.TestCase):
    
    def test_Constructor(self):
        pass


    def test_accept_visitor(self):
        # Signature: name(self, visitor, **kwargs)
		# |VISITATION| 
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_aliases(self):
        # Signature: name
		# Forwarding function to self.dynamics.aliases
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_aliases_map(self):
        # Signature: name
		# Forwarding function to self.dynamics.alias_map
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_analog_ports(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_backsub_aliases(self):
        # Signature: name(self)
		# Expands all alias definitions within the local aliases.
		# 
		# This function finds aliases with which are defined in terms of other
		# aliases, and expands them, such that each aliases only has Parameters,
		# StateVariables and recv/reduce AnalogPorts on the RHS.
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_backsub_all(self):
        # Signature: name(self)
		# Expand all alias definitions in local equations.
		# 
		# This function finds ``Aliases``, ``TimeDerivatives``, ``SendPorts``, ``Assignments``
		# and ``Conditions``  with which are defined in terms of other aliases,
		# and expands them, such that each only has Parameters,
		# StateVariables and recv/reduce AnalogPorts on the RHS.
		# 
		# It is syntactic sugar for::
		#     
		#     self.backsub_aliases()
		#     self.backsub_equations()
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_backsub_equations(self):
        # Signature: name(self)
		# Expands all equations definitions within the local aliases.
		# 
		# This function finds ``TimeDerivatives``, ``SendPorts``, ``Assignments``
		# and ``Conditions``  with which are defined in terms of other aliases,
		# and expands them, such that each only has Parameters,
		# StateVariables and recv/reduce AnalogPorts on the RHS.
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_connect_ports(self):
        # Signature: name(self, src, sink)
		# Connects the ports of 2 subcomponents.
		# 
		# The ports can be specified as ``string`` s or ``NamespaceAddresses`` es.
		# 
		# 
		# :param src: The source port of one sub-component; this should either an
		#     event port or analog port, but it *must* be a send port.
		# 
		# :param sink: The sink port of one sub-component; this should either an
		#     event port or analog port, but it *must* be either a 'recv' or a
		#     'reduce' port.
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_dynamics(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_event_ports(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_flattener(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_get_node_addr(self):
        # Signature: name(self)
		# Get the namespace address of this component
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_insert_subnode(self):
        # Signature: name(self, subnode, namespace)
		# Insert a subnode into this component
		# 
		# 
		# :param subnode: An object of type ``ComponentClass``.
		# :param namespace: A `string` specifying the name of the component in
		#     this components namespace.
		# 
		# :raises: ``NineMLRuntimeException`` if there is already a subcomponent at
		#     the same namespace location
		# 
		# .. note::
		#     
		#     This method will clone the subnode.
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_is_flat(self):
        # Signature: name(self)
		# Is this component flat or does it have subcomponents?
		# 
		# Returns a ``Boolean`` specifying whether this component is flat; i.e.
		# has no subcomponent
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_name(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_parameters(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_query(self):
        # Signature: name
		# Returns the ``ComponentQuery`` object associated with this class
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_regimes(self):
        # Signature: name
		# Forwarding function to self.dynamics.regimes
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_set_flattener(self):
        # Signature: name(self, flattener)
		# No Docstring
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_state_variables(self):
        # Signature: name
		# Forwarding function to self.dynamics.state_variables
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_transitions(self):
        # Signature: name
		# Forwarding function to self.dynamics.transitions
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_was_flattened(self):
        # Signature: name(self)
		# No Docstring
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_write(self):
        # Signature: name(self, file, flatten=True)
		# Export this model to an XML file.
		# 
		# :params file: A filename or fileobject
		# :params flatten: Boolean specifying whether the component should be
		#     flattened before saving
        #from nineml.abstraction_layer.component.component import ComponentClass
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: ComponentClassMixinFlatStructure

class ComponentClassMixinFlatStructure_test(unittest.TestCase):
    
    def test_Constructor(self):
        pass


    def test_aliases(self):
        # Signature: name
		# Forwarding function to self.dynamics.aliases
        #from nineml.abstraction_layer.component.component import ComponentClassMixinFlatStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_aliases_map(self):
        # Signature: name
		# Forwarding function to self.dynamics.alias_map
        #from nineml.abstraction_layer.component.component import ComponentClassMixinFlatStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_analog_ports(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.component import ComponentClassMixinFlatStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_backsub_aliases(self):
        # Signature: name(self)
		# Expands all alias definitions within the local aliases.
		# 
		# This function finds aliases with which are defined in terms of other
		# aliases, and expands them, such that each aliases only has Parameters,
		# StateVariables and recv/reduce AnalogPorts on the RHS.
        #from nineml.abstraction_layer.component.component import ComponentClassMixinFlatStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_backsub_all(self):
        # Signature: name(self)
		# Expand all alias definitions in local equations.
		# 
		# This function finds ``Aliases``, ``TimeDerivatives``, ``SendPorts``, ``Assignments``
		# and ``Conditions``  with which are defined in terms of other aliases,
		# and expands them, such that each only has Parameters,
		# StateVariables and recv/reduce AnalogPorts on the RHS.
		# 
		# It is syntactic sugar for::
		#     
		#     self.backsub_aliases()
		#     self.backsub_equations()
        #from nineml.abstraction_layer.component.component import ComponentClassMixinFlatStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_backsub_equations(self):
        # Signature: name(self)
		# Expands all equations definitions within the local aliases.
		# 
		# This function finds ``TimeDerivatives``, ``SendPorts``, ``Assignments``
		# and ``Conditions``  with which are defined in terms of other aliases,
		# and expands them, such that each only has Parameters,
		# StateVariables and recv/reduce AnalogPorts on the RHS.
        #from nineml.abstraction_layer.component.component import ComponentClassMixinFlatStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_dynamics(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.component import ComponentClassMixinFlatStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_event_ports(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.component import ComponentClassMixinFlatStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_name(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.component import ComponentClassMixinFlatStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_parameters(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.component import ComponentClassMixinFlatStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_regimes(self):
        # Signature: name
		# Forwarding function to self.dynamics.regimes
        #from nineml.abstraction_layer.component.component import ComponentClassMixinFlatStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_state_variables(self):
        # Signature: name
		# Forwarding function to self.dynamics.state_variables
        #from nineml.abstraction_layer.component.component import ComponentClassMixinFlatStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_transitions(self):
        # Signature: name
		# Forwarding function to self.dynamics.transitions
        #from nineml.abstraction_layer.component.component import ComponentClassMixinFlatStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_write(self):
        # Signature: name(self, file, flatten=True)
		# Export this model to an XML file.
		# 
		# :params file: A filename or fileobject
		# :params flatten: Boolean specifying whether the component should be
		#     flattened before saving
        #from nineml.abstraction_layer.component.component import ComponentClassMixinFlatStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: ComponentClassMixinNamespaceStructure

class ComponentClassMixinNamespaceStructure_test(unittest.TestCase):
    
    def test_Constructor(self):
        pass


    def test_connect_ports(self):
        # Signature: name(self, src, sink)
		# Connects the ports of 2 subcomponents.
		# 
		# The ports can be specified as ``string`` s or ``NamespaceAddresses`` es.
		# 
		# 
		# :param src: The source port of one sub-component; this should either an
		#     event port or analog port, but it *must* be a send port.
		# 
		# :param sink: The sink port of one sub-component; this should either an
		#     event port or analog port, but it *must* be either a 'recv' or a
		#     'reduce' port.
        #from nineml.abstraction_layer.component.component import ComponentClassMixinNamespaceStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_get_node_addr(self):
        # Signature: name(self)
		# Get the namespace address of this component
        #from nineml.abstraction_layer.component.component import ComponentClassMixinNamespaceStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_insert_subnode(self):
        # Signature: name(self, subnode, namespace)
		# Insert a subnode into this component
		# 
		# 
		# :param subnode: An object of type ``ComponentClass``.
		# :param namespace: A `string` specifying the name of the component in
		#     this components namespace.
		# 
		# :raises: ``NineMLRuntimeException`` if there is already a subcomponent at
		#     the same namespace location
		# 
		# .. note::
		#     
		#     This method will clone the subnode.
        #from nineml.abstraction_layer.component.component import ComponentClassMixinNamespaceStructure
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: InterfaceInferer

class InterfaceInferer_test(unittest.TestCase):
    
    def test_Constructor(self):
        pass


    def test_action_alias(self):
        # Signature: name(self, alias, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_action_analogport(self):
        # Signature: name(self, port, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_action_assignment(self):
        # Signature: name(self, assignment, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_action_componentclass(self):
        # Signature: name(self, component, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_action_condition(self):
        # Signature: name(self, condition, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_action_dynamics(self):
        # Signature: name(self, dynamics, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_action_eventport(self):
        # Signature: name(self, port, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_action_oncondition(self):
        # Signature: name(self, on_condition, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_action_onevent(self):
        # Signature: name(self, on_event, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_action_outputevent(self):
        # Signature: name(self, output_event, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_action_parameter(self):
        # Signature: name(self, parameter, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_action_regime(self):
        # Signature: name(self, regime, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_action_statevariable(self):
        # Signature: name(self, state_variable, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_action_timederivative(self):
        # Signature: name(self, time_derivative, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_check_pass(self):
        # Signature: name(self)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_notify_atom(self):
        # Signature: name(self, atom)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_visit(self):
        # Signature: name(self, obj, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_visit_alias(self):
        # Signature: name(self, alias, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_visit_analogport(self):
        # Signature: name(self, port, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_visit_assignment(self):
        # Signature: name(self, assignment, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_visit_componentclass(self):
        # Signature: name(self, component, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_visit_condition(self):
        # Signature: name(self, condition, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_visit_dynamics(self):
        # Signature: name(self, dynamics, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_visit_eventport(self):
        # Signature: name(self, port, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_visit_inputevent(self):
        # Signature: name(self, input_event, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_visit_oncondition(self):
        # Signature: name(self, on_condition, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_visit_onevent(self):
        # Signature: name(self, on_event, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_visit_outputevent(self):
        # Signature: name(self, output_event, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_visit_parameter(self):
        # Signature: name(self, parameter, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_visit_regime(self):
        # Signature: name(self, regime, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_visit_statevariable(self):
        # Signature: name(self, state_variable, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_visit_timederivative(self):
        # Signature: name(self, time_derivative, **kwargs)
		# No Docstring
        #from nineml.abstraction_layer.component.component import InterfaceInferer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








