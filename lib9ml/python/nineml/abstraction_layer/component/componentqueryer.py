
from itertools import chain

from namespaceaddress import NamespaceAddress
#import nineml.utility
from nineml.utility import filter_expect_single


__all__ = ['ComponentQueryer']

class ComponentQueryer(object):
    def __init__(self, component):
        self.component = component



    @property
    def ports(self):
        return chain(self.component.analog_ports, self.component.event_ports)  


    # Find basic properties by name
    def regime(self, name=None,):
        assert isinstance(name, basestring)

        return filter_expect_single( self.component.regimes, 
                                     lambda r:r.name==name ) 
        

    # Query Ports:
    def event_send_ports(self):
        return [ p for p in self.component.event_ports if p.mode == 'send']
    
    def event_recv_ports(self):
        return [ p for p in self.component.event_ports if p.mode == 'recv']
    
    @property
    def analog_reduce_ports(self):
        return [ p for p in self.component.analog_ports if p.mode == 'reduce']




    def get_fully_addressed_analogports_new(self):
        comp_addr = self.component.get_node_addr()

        kv = lambda port : (comp_addr.get_subns_addr(port.name), port) 
        return dict( [ kv(port) for port in self.component.analog_ports] )

    def get_fully_qualified_port_connections(self):
        namespace = self.component.get_node_addr()
        def make_fqname(target):
            return NamespaceAddress.concat( namespace, target)
        conns = [ (make_fqname(src), make_fqname(sink)) for (src, sink) in
                self.component.portconnections ]
        return conns



    @property
    def recurse_all_components(self):
        yield self.component
        for subcomponent in self.component.subnodes.values():
            for subcomp in subcomponent.query.recurse_all_components:
                yield subcomp






    ##Not currently used, but maybe useful in future:
    #@property
    #def ports_map(self):
    #    assert False
    #    return dict( [ (p.name, p) for p in self.ports ] )

    #@property
    #def alias_symbols(self):
    #    assert False
    #    return [ a.lhs for a in self.component.aliases ]


    #@property
    #def on_conditions(self):
    #    assert False
    #    for regime in self.component.regimes:
    #        for condition in regime.on_conditions:
    #            yield condition
