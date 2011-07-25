

# Automatically Generated Testing Skeleton Template:
import warnings
import unittest
import nineml



from nineml.abstraction_layer import AnalogPort
from nineml.abstraction_layer import RecvPort, SendPort, ReducePort
from nineml.abstraction_layer import RecvEventPort, SendEventPort
from nineml.exceptions import NineMLRuntimeError



# Testing Skeleton for class: AnalogPort

class AnalogPort_test(unittest.TestCase):
    
    def test_Constructor(self):
        pass


    def test_accept_visitor(self):
        from nineml.abstraction_layer import RecvPort, SendPort, ReducePort
        # Check the Component is forwarding arguments: 
        class TestVisitor(object):
            def visit(self, obj, **kwargs):
                return obj.accept_visitor(self, **kwargs)
            def visit_analogport(self, component, **kwargs):
                return kwargs
        
        v = TestVisitor()
        
        self.assertEqual( 
            v.visit(SendPort('V'), kwarg1='Hello', kwarg2='Hello2'),
            {'kwarg1':'Hello', 'kwarg2':'Hello2'}
            )
        self.assertEqual( 
            v.visit(RecvPort('V'), kwarg1='Hello', kwarg2='Hello2'),
            {'kwarg1':'Hello', 'kwarg2':'Hello2'}
            )

        self.assertEqual( 
            v.visit(ReducePort('V',reduce_op='+'), kwarg1='Hello', kwarg2='Hello2'),
            {'kwarg1':'Hello', 'kwarg2':'Hello2'}
            )


    def test_is_incoming(self):
        # Signature: name(self)
		# Returns True if the port's mode is 'recv' or 'reduce' 
        #from nineml.abstraction_layer.component.ports import AnalogPort
        self.assertEqual( RecvPort('V').is_incoming(), True)
        self.assertEqual( ReducePort('V', reduce_op='+').is_incoming(), True)
        self.assertEqual( SendPort('V').is_incoming(), False)



    def test_is_outgoing(self):
        # Signature: name(self)
		# Returns True if the port's mode is 'send' 
        #from nineml.abstraction_layer.component.ports import AnalogPort
        self.assertEqual( RecvPort('V').is_outgoing(), False)
        self.assertEqual( ReducePort('V', reduce_op='+').is_outgoing(), False)
        self.assertEqual( SendPort('V').is_outgoing(), True)


    def test_mode(self):
        # Signature: name
		# The mode of the port. ['send','recv' or 'reduce'] 
        #from nineml.abstraction_layer.component.ports import AnalogPort
        self.assertEqual( AnalogPort('V', mode='send').is_outgoing(), True)
        self.assertEqual( AnalogPort('V', mode='recv').is_outgoing(), False)
        self.assertEqual( AnalogPort('V', mode='reduce', reduce_op='+').is_outgoing(), False)

        self.assertRaises( 
                NineMLRuntimeError,
                AnalogPort, 'V', mode='reducing', reduce_op='+')
        self.assertRaises( 
                NineMLRuntimeError,
                AnalogPort, 'V', mode='SEND')


    def test_name(self):
        # Signature: name
		# The name of the port, local to the current component
        self.assertEqual( RecvPort('A').name, 'A')
        self.assertEqual( ReducePort('B', reduce_op='+').name, 'B')
        self.assertEqual( SendPort('C').name, 'C')


    def test_reduce_op(self):
        # Signature: name
		# The reduction operation of the port, if it is a 'reduce' port
        #from nineml.abstraction_layer.component.ports import AnalogPort
        self.assertRaises( 
                NineMLRuntimeError,
                AnalogPort, 'V', mode='reduce', reduce_op='-')









# Testing Skeleton for class: EventPort

class EventPort_test(unittest.TestCase):
    
    def test_Constructor(self):
        pass


    def test_accept_visitor(self):

        from nineml.abstraction_layer import RecvEventPort, SendEventPort
        class TestVisitor(object):
            def visit(self, obj, **kwargs):
                return obj.accept_visitor(self, **kwargs)
            def visit_eventport(self, component, **kwargs):
                return kwargs
        
        v = TestVisitor()
        
        self.assertEqual( 
            v.visit(SendEventPort('EV'), kwarg1='Hello', kwarg2='Hello2'),
            {'kwarg1':'Hello', 'kwarg2':'Hello2'}
            )
        self.assertEqual( 
            v.visit(RecvEventPort('EV'), kwarg1='Hello', kwarg2='Hello2'),
            {'kwarg1':'Hello', 'kwarg2':'Hello2'}
            )



    def test_is_incoming(self):
        self.assertEqual( RecvEventPort('V').is_incoming(), True)
        self.assertEqual( SendEventPort('V').is_incoming(), False)


    def test_is_outgoing(self):

        self.assertEqual( RecvEventPort('V').is_outgoing(), False)
        self.assertEqual( SendEventPort('V').is_outgoing(), True)


    def test_name(self):
        self.assertEqual( RecvEventPort('A').name, 'A')
        self.assertEqual( SendEventPort('C').name, 'C')





    def test_mode(self):
        # Signature: name
		# The mode of the port. ['send','recv' or 'reduce'] 
        #from nineml.abstraction_layer.component.ports import EventPort
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()

    def test_reduce_op(self):
        warnings.warn('Tests not implemented')












