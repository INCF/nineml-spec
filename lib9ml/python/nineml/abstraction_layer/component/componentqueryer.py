"""Definitions for the ComponentQuery Class"""

from itertools import chain

#from namespaceaddress import NamespaceAddress
#import nineml.utility
from nineml.utility import filter_expect_single


__all__ = ['ComponentQueryer']

class ComponentQueryer(object):
    """
    ComponentQueryer provides a way of adding methods to query a 
    ComponentClass object, without polluting the class
    """

    def __init__(self, component):
        """Constructor for the ComponentQueryer"""
        self.component = component



    @property
    def ports(self):
        """Return an iterator over all the port (Event & Analog) in the
        component"""
        return chain(self.component.analog_ports, self.component.event_ports)  


    # Find basic properties by name
    def regime(self, name=None,):
        """Find a regime in the component by name"""
        assert isinstance(name, basestring)

        return filter_expect_single( self.component.regimes, 
                                     lambda r:r.name==name ) 
        

    # Query Ports:
    def event_send_ports(self):
        """Get the ``send`` EventPorts"""
        return [ p for p in self.component.event_ports if p.mode == 'send']
    
    def event_recv_ports(self):
        """Get the ``recv`` EventPorts"""
        return [ p for p in self.component.event_ports if p.mode == 'recv']
    
    @property
    def analog_reduce_ports(self):
        """Get the ``reduce`` EventPorts"""
        return [ p for p in self.component.analog_ports if p.mode == 'reduce']


    @property
    def analog_send_ports(self):
        """Get the ``send`` AnalogPorts"""
        return [ p for p in self.component.analog_ports if p.mode == 'send']

    @property
    def analog_recv_ports(self):
        """Get the ``recv`` AnalogPorts"""
        return [ p for p in self.component.analog_ports if p.mode == 'recv']


    #def get_fully_addressed_analogports_new(self):
    #    """Used by the flattening code.
    #    
    #    This method returns a ditionary mapping
    #    the fully-qulified-addresses to the port
    #    """
    #    assert False
    #    comp_addr = self.component.get_node_addr()

    #    kv = lambda port : (comp_addr.get_subns_addr(port.name), port) 
    #    return dict( [ kv(port) for port in self.component.analog_ports] )

    def get_fully_qualified_port_connections(self):
        """Used by the flattening code.
        
        This method returns a d list of tuples of the 
        the fully-qualified port connections
        """
        namespace = self.component.get_node_addr()
        conns = []
        for src, sink in self.component.portconnections:
            src_new = namespace.get_subns_addr(src)
            sink_new = namespace.get_subns_addr(sink)
            conns.append( (src_new, sink_new) )
        return conns



    @property
    def recurse_all_components(self):
        """Returns an iterator over this component and all subcomponents"""
        yield self.component
        for subcomponent in self.component.subnodes.values():
            for subcomp in subcomponent.query.recurse_all_components:
                yield subcomp






