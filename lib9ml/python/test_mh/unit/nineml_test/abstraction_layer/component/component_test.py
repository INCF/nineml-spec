

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
        from nineml.abstraction_layer.component.component import ComponentClass
        # Check the Component is forwarding arguments: 
        class TestVisitor(object):
            def visit(self, obj, **kwargs):
                return obj.accept_visitor(self, **kwargs)
            def visit_componentclass(self, component, **kwargs):
                return kwargs
        
        c = ComponentClass(name='MyComponent')
        v = TestVisitor()
        
        self.assertEqual(
            v.visit(c, kwarg1='Hello', kwarg2='Hello2'),
            {'kwarg1':'Hello', 'kwarg2':'Hello2'}
            )

        



    def test_aliases(self):
        # Signature: name
		# Forwarding function to self.dynamics.aliases
        from nineml.abstraction_layer.component import ComponentClass
        from nineml.abstraction_layer.component import Alias, Dynamics
        from nineml.exceptions import NineMLRuntimeError
            
        # No Aliases:
        self.assertEqual(
                list( ComponentClass(name='C1').aliases ),
                []
                )

        # 2 Aliases
        C = ComponentClass(name='C1', aliases=['G:= 0', 'H:=1'] )
        self.assertEqual( len(list( ( C.aliases ) ) ), 2)
        self.assertEqual(
                set( C.aliases_map.keys() ), set(['G','H'] )
                )

        C = ComponentClass(name='C1', aliases=['G:= 0', 'H:=1', Alias('I','3')] )
        self.assertEqual( len(list( ( C.aliases ) ) ), 3)
        self.assertEqual(
                set( C.aliases_map.keys() ), set(['G','H','I'] )
                )


        # Using Dynamics Parameter:
        C = ComponentClass(name='C1', dynamics = Dynamics( aliases=['G:= 0', 'H:=1'] ))
        self.assertEqual( len(list( ( C.aliases ) ) ), 2)
        self.assertEqual(
                set( C.aliases_map.keys() ), set(['G','H'] )
                )

        C = ComponentClass(name='C1', dynamics=Dynamics(aliases=['G:= 0','H:=1',Alias('I','3')] ) )
        self.assertEqual( len(list( ( C.aliases ) ) ), 3)
        self.assertEqual(
                set( C.aliases_map.keys() ), set(['G','H','I'] )
                )
       
        
        
        # Invalid Construction:
        # Invalid Valid String:
        self.assertRaises( 
                NineMLRuntimeError,
                ComponentClass, name='C1', aliases = ['H=0']
                )

        # Duplicate Alias Names:
        ComponentClass( name='C1', aliases = ['H:=0','G:=1'] )
        self.assertRaises( 
                NineMLRuntimeError,
                ComponentClass, name='C1', aliases = ['H:=0','H:=1']
                )
        
        self.assertRaises( 
                NineMLRuntimeError,
                ComponentClass, name='C1', aliases = ['H:=0', Alias('H','1') ]
                )

        # Defining through dynamics and Component:
        self.assertRaises( 
                NineMLRuntimeError,
                ComponentClass, 
                    name='C1', 
                    aliases = ['H:=0' ],
                    dynamics = Dynamics( aliases = ['G:=1'] ),
                )

        self.assertRaises( 
                NineMLRuntimeError,
                ComponentClass, 
                    name='C1', 
                    aliases = [Alias('H','0') ],
                    dynamics = Dynamics( aliases = [ Alias('G','1')] ),
                )

        # Self referential aliases:
        self.assertRaises( 
                NineMLRuntimeError,
                ComponentClass, 
                    name='C1', aliases = ['H := H +1'],
                )
        self.assertRaises( 
                NineMLRuntimeError,
                ComponentClass, 
                    name='C1', aliases = ['H := G + 1', 'G := H + 1'],
                )

        # Referencing none existant symbols:
        self.assertRaises( 
                NineMLRuntimeError,
                ComponentClass, 
                    name='C1', 
                    aliases = ['H := G + I'],
                    parameters=[],
                    analog_ports=[],
                )

        # Invalid Names:
        self.assertRaises( 
                NineMLRuntimeError,
                ComponentClass, 
                    name='C1', aliases = ['H.2 := 0'],
                )

        self.assertRaises( 
                NineMLRuntimeError,
                ComponentClass, 
                    name='C1', aliases = ['2H := 0'],
                )

        self.assertRaises( 
                NineMLRuntimeError,
                ComponentClass, 
                    name='C1', aliases = ['E(H) := 0'],
                )

        self.assertRaises( 
                NineMLRuntimeError,
                ComponentClass, 
                    name='C1', aliases = ['tanh := 0'],
                )
        self.assertRaises( 
                NineMLRuntimeError,
                ComponentClass, 
                    name='C1', aliases = ['pi := 0'],
                )

    def test_aliases_map(self):
        # Signature: name
		# Forwarding function to self.dynamics.alias_map
        from nineml.abstraction_layer.component.component import ComponentClass
        
        self.assertEqual(
            ComponentClass( name='C1' ).aliases_map, {} 
            )

        self.assertEqual(
                sorted( ComponentClass( name='C1', aliases=['A:=3']).aliases_map.keys() ), 
                ['A']
            )

        self.assertEqual(
                sorted( ComponentClass( name='C1', 
                                        aliases=['A:=3', 'B:=3']).aliases_map.keys() ), 
                ['A','B']
            )


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















