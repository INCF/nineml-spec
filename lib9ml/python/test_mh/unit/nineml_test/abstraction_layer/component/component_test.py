

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
        from nineml.abstraction_layer.component import ComponentClass, Dynamics
        
        self.assertEqual(
            ComponentClass( name='C1' ).aliases_map, {} 
            )

        c1 = ComponentClass( name='C1', aliases=['A:=3'])
        self.assertEqual( c1.aliases_map['A'].rhs_as_python_func()(), 3)
        self.assertEqual( len(c1.aliases_map), 1)


        c2 = ComponentClass( name='C1', aliases=['A:=3','B:=5'])
        self.assertEqual( c2.aliases_map['A'].rhs_as_python_func()(), 3)
        self.assertEqual( c2.aliases_map['B'].rhs_as_python_func()(), 5)
        self.assertEqual( len(c2.aliases_map), 2)


        c3 = ComponentClass( name='C1', dynamics=Dynamics(aliases=['C:=13','Z:=15']))
        self.assertEqual( c3.aliases_map['C'].rhs_as_python_func()(), 13)
        self.assertEqual( c3.aliases_map['Z'].rhs_as_python_func()(), 15)

        self.assertEqual( len(c3.aliases_map), 2)
        

    def test_analog_ports(self):
        # Signature: name
		# No Docstring
        from nineml.abstraction_layer import ComponentClass 
        from nineml.abstraction_layer import SendPort, RecvPort, ReducePort
        from nineml.exceptions import NineMLRuntimeError
        
        c = ComponentClass( name='C1')
        self.assertEqual( len( c.analog_ports), 0)

        c = ComponentClass( name='C1')
        self.assertEqual( len( c.analog_ports), 0)

        
        c = ComponentClass( name='C1', aliases=['A:=2'], analog_ports=[SendPort('A')])
        self.assertEqual( len( c.analog_ports), 1)
        self.assertEqual( c.analog_ports[0].mode, 'send' )
        self.assertEqual( len(c.query.analog_send_ports), 1 )
        self.assertEqual( len(c.query.analog_recv_ports), 0 )
        self.assertEqual( len(c.query.analog_reduce_ports), 0 )

        c = ComponentClass( name='C1', analog_ports=[RecvPort('B')])
        self.assertEqual( len( c.analog_ports), 1)
        self.assertEqual( c.analog_ports[0].mode, 'recv' )
        self.assertEqual( len(c.query.analog_send_ports), 0 )
        self.assertEqual( len(c.query.analog_recv_ports), 1 )
        self.assertEqual( len(c.query.analog_reduce_ports), 0 )

        c = ComponentClass( name='C1', analog_ports=[ReducePort('B', reduce_op='+')])
        self.assertEqual( len( c.analog_ports), 1)
        self.assertEqual( c.analog_ports[0].mode, 'reduce' )
        self.assertEqual( c.analog_ports[0].reduce_op, '+' )
        self.assertEqual( len(c.query.analog_send_ports), 0 )
        self.assertEqual( len(c.query.analog_recv_ports), 0 )
        self.assertEqual( len(c.query.analog_reduce_ports), 1 )


        # Duplicate Port Names:
        self.assertRaises(
                NineMLRuntimeError,
                ComponentClass, 
                    name='C1', 
                    aliases = ['A:=1'],
                    analog_ports=[ReducePort('B',reduce_op='+'), SendPort('B')]
                )

        self.assertRaises(
                NineMLRuntimeError,
                ComponentClass, 
                    name='C1', 
                    aliases = ['A:=1'],
                    analog_ports=[SendPort('A'), SendPort('A')]
                )


        self.assertRaises(
                NineMLRuntimeError,
                ComponentClass, 
                    name='C1', 
                    aliases = ['A:=1'],
                    analog_ports=[RecvPort('A'), RecvPort('A')]
                )

        self.assertRaises(
                NineMLRuntimeError,
                lambda: ComponentClass(name='C1', analog_ports=[RecvPort('1')])
                )

        self.assertRaises(
                NineMLRuntimeError,
                lambda: ComponentClass(name='C1', analog_ports=[RecvPort('?')])
                )


    def duplicate_port_name_event_analog(self):
        from nineml.abstraction_layer import ComponentClass 
        from nineml.abstraction_layer import SendPort, RecvPort, ReducePort
        from nineml.exceptions import NineMLRuntimeError

        #Check different names are OK:
        ComponentClass(
            name='C1', aliases = ['A:=1'],
            event_ports = [ RecvEventPort('A') ],
            analog_ports=[ SendPort('A')] )


        self.assertRaises(
                NineMLRuntimeError,
                ComponentClass, 
                    name='C1', 
                    aliases = ['A:=1'],
                    event_ports = [ RecvEventPort('A') ],
                    analog_ports=[ SendPort('A')]
                )


    # Testing done in test_backsub_all()
    def test_backsub_aliases(self):
        pass
    def test_backsub_equations(self):
        pass



    def test_backsub_all(self):

        from nineml.abstraction_layer.component import ComponentClass, Dynamics
        from nineml.exceptions import NineMLRuntimeError


        # Check the aliases:
        # ====================== #
        c2 = ComponentClass( name='C1', aliases=['A:=1+2','B:=5*A','C:=B+2'])
        self.assertEqual( c2.aliases_map['A'].rhs_as_python_func()(), 3)

        # This should assert, because its not yet back-subbed
        c2.backsub_all()
        self.assertEqual( c2.aliases_map['B'].rhs_as_python_func()(), 15)
        # Check the ordering:
        self.assertEqual( c2.aliases_map['C'].rhs_as_python_func()(), ((5*(3))+2) )
        # ====================== #



        # Check the equations:
        # ====================== #
        warnings.warn('Tests not implemented')
        # ====================== #






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

        from nineml.abstraction_layer import ComponentClass
        from nineml.abstraction_layer.testing_utils import TestableComponent
        from nineml.exceptions import NineMLRuntimeError

        tIaf = TestableComponent('iaf')
        tCoba = TestableComponent('coba_synapse')
        
        # Should be fine:
        c = ComponentClass( name = 'C1',
                subnodes = { 'iaf': tIaf(), 'coba':tCoba() } )
        c.connect_ports( 'iaf.V', 'coba.V' )

        c = ComponentClass( name = 'C1',
                subnodes = { 'iaf': tIaf(), 'coba':tCoba() },
                portconnections = [ ('iaf.V', 'coba.V') ]
                )


        # Non existant Ports:

        c = ComponentClass( name = 'C1',
                subnodes = { 'iaf': tIaf(), 'coba':tCoba() } )
        self.assertRaises(
                NineMLRuntimeError,
                c.connect_ports, 'iaf.V1', 'coba.V' )
        self.assertRaises(
                NineMLRuntimeError,
                c.connect_ports, 'iaf.V', 'coba.V1' )

        self.assertRaises(
                NineMLRuntimeError,
                    ComponentClass,
                        name = 'C1',
                        subnodes = { 'iaf': tIaf(), 'coba':tCoba() },
                        portconnections = [ ('iaf.V1', 'coba.V') ]
                )

        self.assertRaises(
                NineMLRuntimeError,
                    ComponentClass,
                        name = 'C1',
                        subnodes = { 'iaf': tIaf(), 'coba':tCoba() },
                        portconnections = [ ('iaf.V', 'coba.V1') ]
                )

        # Connect ports the wronf way around:
        # [Check the wright way around works:]
        c = ComponentClass( name = 'C1',
                subnodes = { 'iaf': tIaf(), 'coba':tCoba() },
                portconnections = [ ('coba.I', 'iaf.ISyn') ]
                )
        # And the wrong way around:
        c = ComponentClass( name = 'C1',
                subnodes = { 'iaf': tIaf(), 'coba':tCoba() } )
        self.assertRaises(
                NineMLRuntimeError,
                c.connect_ports, 'iaf.ISyn.', 'coba.I' )
        self.assertRaises(
                NineMLRuntimeError,
                c.connect_ports, 'coba.V', 'iaf.V' )


        # Error raised on duplicate port-connection:
        c = ComponentClass( name = 'C1',
                subnodes = { 'iaf': tIaf(), 'coba':tCoba() },
                )

        c.connect_ports( 'coba.I', 'iaf.ISyn' )
        self.assertRaises(
                NineMLRuntimeError,
                c.connect_ports, 'coba.I', 'iaf.ISyn'  )



    def test_dynamics(self):
        pass


    def test_event_ports(self):
        # Signature: name
		# No Docstring
        from nineml.abstraction_layer import ComponentClass, Regime, On
        import nineml.abstraction_layer as al

        # Check inference of event ports:
        c = ComponentClass( 
                name = 'Comp1',
                regimes = Regime(
                    transitions = [
                        On('V > a', do=al.OutputEvent('ev_port1') ),
                        On('V < b', do=al.OutputEvent('ev_port2') ),
                        ]
                    ),
                )
        self.assertEquals( len(c.event_ports), 2 ) 
        


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















