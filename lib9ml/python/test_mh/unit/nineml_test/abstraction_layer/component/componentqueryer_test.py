

# Automatically Generated Testing Skeleton Template:
import warnings
import unittest
import nineml






# Testing Skeleton for class: ComponentQueryer

class ComponentQueryer_test(unittest.TestCase):
    


    def test_event_send_recv_ports(self):
        # Signature: name(self)
		# Get the ``recv`` EventPorts
        #from nineml.abstraction_layer.component.componentqueryer import ComponentQueryer
        from nineml.abstraction_layer import ComponentClass, Regime, On
        import nineml.abstraction_layer as al

        # Check inference of output event ports:
        c = ComponentClass( 
                name = 'Comp1',
                regimes = Regime(
                    transitions = [
                        On('in_ev1', do=al.OutputEvent('ev_port1') ),
                        On('V < b', do=al.OutputEvent('ev_port1') ),
                        On('V < c', do=al.OutputEvent('ev_port2') ),
                        ]
                    ),
                )
        self.assertEquals( len(c.query.event_recv_ports()), 1 ) 
        self.assertEquals( (list(c.query.event_recv_ports)[0]).name, 'in_ev1' ) 

        self.assertEquals( len(c.query.event_send_ports), 2 ) 
        self.assertEquals( (list(c.query.event_send_ports)[0]).name, 'ev_port1' ) 
        self.assertEquals( (list(c.query.event_send_ports)[1]).name, 'ev_port2' ) 
        
        # Check inference of output event ports:
        c = ComponentClass( 
                name = 'Comp1',
                regimes = [
                    Regime(name='r1',
                    transitions = [
                        On('V > a', do=al.OutputEvent('ev_port1'), to='r2' ),
                        On('in_ev1', do=al.OutputEvent('ev_port2') ),
                        ]
                    ),

                    Regime(name='r2',
                    transitions = [
                        On('V > a', do=al.OutputEvent('ev_port2'), to='r1' ),
                        On('in_ev2', do=al.OutputEvent('ev_port3') ),
                        ]
                    )
                    ]
                )
        self.assertEquals( len(c.query.event_recv_ports), 2 ) 
        self.assertEquals( (list(c.query.event_recv_ports)[0]).name, 'in_ev1' ) 
        self.assertEquals( (list(c.query.event_recv_ports)[1]).name, 'in_ev2' ) 

        self.assertEquals( len(c.query.event_send_ports), 3 ) 
        self.assertEquals( (list(c.query.event_send_ports)[0]).name, 'ev_port1' ) 
        self.assertEquals( (list(c.query.event_send_ports)[1]).name, 'ev_port2' ) 
        self.assertEquals( (list(c.query.event_send_ports)[2]).name, 'ev_port3' ) 


        # Check inference of output event ports:
        c = ComponentClass( 
                name = 'Comp1',
                regimes = [
                    Regime(name='r1',
                    transitions = [
                        On('spikeinput1', do=[] ),
                        On('spikeinput2', do=[ al.OutputEvent('ev_port1'),
                            al.OutputEvent('ev_port2')],to='r2' ),
                        ]
                    ),

                    Regime(name='r2',
                    transitions = [
                        On('V > a', do=al.OutputEvent('ev_port2') ),
                        On('spikeinput3', do=al.OutputEvent('ev_port3'), to='r1' ),
                        ]
                    )
                    ]
                )
        self.assertEquals( len(c.query.event_recv_ports), 3 ) 
        self.assertEquals( (list(c.query.event_recv_ports)[0]).name, 'spikeinput1' ) 
        self.assertEquals( (list(c.query.event_recv_ports)[1]).name, 'spikeinput2' ) 
        self.assertEquals( (list(c.query.event_recv_ports)[2]).name, 'spikeinput3' ) 

        self.assertEquals( len(c.query.event_send_ports), 3 ) 
        self.assertEquals( (list(c.query.event_send_ports)[0]).name, 'ev_port1' ) 
        self.assertEquals( (list(c.query.event_send_ports)[1]).name, 'ev_port2' ) 
        self.assertEquals( (list(c.query.event_send_ports)[2]).name, 'ev_port3' ) 




    def test_get_fully_qualified_port_connections(self):
        # Signature: name(self)
		# Used by the flattening code.
		# 
		# This method returns a d list of tuples of the 
		# the fully-qualified port connections
        #from nineml.abstraction_layer.component.componentqueryer import ComponentQueryer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_ports(self):
        # Signature: name
		# Return an iterator over all the port (Event & Analog) in the
		# component
        #from nineml.abstraction_layer.component.componentqueryer import ComponentQueryer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_recurse_all_components(self):
        # Signature: name
		# Returns an iterator over this component and all subcomponents
        #from nineml.abstraction_layer.component.componentqueryer import ComponentQueryer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_regime(self):
        # Signature: name(self, name=None)
		# Find a regime in the component by name
        #from nineml.abstraction_layer.component.componentqueryer import ComponentQueryer
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








