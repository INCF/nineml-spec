

#from nineml.utility import expect_single, filter_expect_single, filter, filter_by_type
import nineml.utility

class ComponentQueryer(object):
    def __init__(self, component):
        self.component = component

    # Find basic properties by name
    def regime(self, name=None,):
        assert isinstance(name,basestring)
        return nineml.utility.filter_expect_single( self.component.regimes, lambda r:r.name==name ) 
        

    # Query Ports:
    def event_send_ports(self):
        return [ p for p in self.component.event_ports if p.mode=='send']
    
    def event_recv_ports(self):
        return [ p for p in self.component.event_ports if p.mode=='recv']
    
    @property
    def analog_reduce_ports(self):
        reduce_ports = [ p for p in self.component.analog_ports if p.mode=='reduce' ]
        return reduce_ports

    def get_fully_addressed_analogports_new(self):
        comp_addr = self.component.get_node_addr()
        return dict( [ (comp_addr.get_subns_addr(port.name), port) for port in self.component.analog_ports] )




    @property
    def recurse_all_components(self):
        yield self.component
        for subcomponent in self.component.subnodes.values():
            for sc in subcomponent.query.recurse_all_components:
                yield sc
    #More advanced searches on just this node:

