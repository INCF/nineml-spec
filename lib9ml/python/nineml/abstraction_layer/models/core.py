
from nineml.utility import ExpectSingle, FilterExpectSingle, Filter, FilterType

# System Imports:
import copy
import itertools



# Relative Imports:
import util



#import nineml.abstraction_layer as al
from nineml.abstraction_layer import ComponentClass, Dynamics



class TreeNode(object):
    def __init__(self):  
        self._parentmodel = None

    

    # Naming Functions:
    # != TO GO -!
    def getContainedNamespaceName(self):
        from nineml.utility import invertDictionary
        if not self.getParentModel(): return ""
        return invertDictionary(self.getParentModel().subnodes)[self]

    def getTreePosition(self, jointoken=None):
        """ Returns a list of the parents"""
        if self.getParentModel():
            tokens = self.getParentModel().getTreePosition(jointoken=None) + [ self.getContainedNamespaceName() ]
        else:
            tokens = [ ]

        if jointoken:
            return jointoken.join(tokens)
        else:
            return tokens

    def getPathString(self):
        return self.getTreePosition( jointoken="/" )
    # != TO GO TIL HERE -!


    def get_node_addr(self):
        if not self.getParentModel():
            return NamespaceAddress.create_root()
        else:
            return self.getParentModel().get_node_addr().get_subns_addr( self.getContainedNamespaceName() ) 


    # Parenting:
    def setParentModel(self,parentmodel):
        assert not self._parentmodel
        self._parentmodel = parentmodel
    def getParentModel(self): 
        return self._parentmodel
        
    # Visitation
    def AcceptVisitor(self,visitor,**kwargs):
        raise NotImplementedException



#name=None, 
    
class Model(TreeNode):
    def __init__(self, subnodes={}, model=None):
        super(Model,self).__init__()
        
        #self.name = name
        #print self.name
        #assert not self.name

        self.subnodes = {}
        self.portconnections = []

        for namespace,subnode in subnodes.iteritems():
                self.insert_subnode(subnode=subnode, namespace=namespace)

        
    def insert_subnode(self, subnode, namespace):
        assert not namespace in self.subnodes
        self.subnodes[namespace] = copy.deepcopy( subnode ) 
        self.subnodes[namespace].setParentModel(self)

    def connect_ports( self, src, sink ):
        #TODO: Check that the ports are connected to items in this model.
       self.portconnections.append( (NamespaceAddress(src),NamespaceAddress(sink) ) ) 
    

    def get_fully_qualified_port_connections(self):
        self.namespace = self.get_node_addr()
        def make_fqname(target):
            return NamespaceAddress.concat( self.namespace, target)
        conns = [ (make_fqname(src),make_fqname(sink)) for (src,sink) in self.portconnections ]
        return conns
        
    # Visitation 
    def AcceptVisitor(self,visitor,**kwargs):
        return visitor.VisitModelNode(self,**kwargs)





class ComponentNode(ComponentClass,TreeNode, ):
    def __init__(self, name, parameters = None,  analog_ports = None, event_ports=None, dynamics=None, model=None):
        parameters = parameters or []
        analog_ports = analog_ports or []
        event_ports = event_ports or []

        TreeNode.__init__(self, )
        ComponentClass.__init__(self, name=name, parameters = parameters, analog_ports=analog_ports, event_ports = event_ports, dynamics = dynamics)
        self.query = ComponentQueryer(self)

        

#Interface
    def getAllComponents(self,):
        return [self]

    def AcceptVisitor(self,visitor,**kwargs):
        return visitor.VisitComponentNode(self,**kwargs)






class ComponentNodeCombined( ComponentNode, Model ):

    def __init__(self, name, parameters=[],  analog_ports=[], event_ports=[], dynamics=None, subnodes={}, model=None):
        if dynamics == None:
            dynamics = Dynamics()
        ComponentNode.__init__(self, name=name, parameters = parameters, analog_ports = analog_ports, event_ports=event_ports, dynamics=dynamics, model=model)
        Model.__init__(self,  subnodes=subnodes, model=model)

    def getAllComponents(self,):
        assert False
        return [self]  

    def AcceptVisitor(self,visitor,**kwargs):
        return visitor.VisitComponentNodeCombined(self)









class ComponentQueryer(object):
    def __init__(self, component):
        self.component = component

    # Find basic properties by name
    def regime(self, name=None,):
        assert isinstance(name,basestring)
        rFunc = lambda r:r.name==name 
        return FilterExpectSingle( self.component.regimes,rFunc ) 
        
    def event_send_ports(self):
        return [ p for p in self.component.event_ports if p.mode=='send']

    def event_recv_ports(self):
        return [ p for p in self.component.event_ports if p.mode=='recv']
    
    def get_fully_addressed_analogports_new(self):
        comp_addr = self.component.get_node_addr()
        return dict( [ (comp_addr.get_subns_addr(port.name), port) for port in self.component.analog_ports] )

    #More advanced searches on just this node:
    @property
    def analog_reduce_ports(self):
        reduce_ports = [ p for p in self.component.analog_ports if p.mode=='reduce' ]
        return reduce_ports





class NamespaceAddress(object):
    def __init__(self, loc):
        if isinstance(loc, basestring):
            self.loctuple = loc.split('.')
        elif isinstance(loc, tuple):
            self.loctuple = loc
        elif isinstance(loc, NamespaceAddress):
            self.loctuple = loc.loctuple
        else:
            print loc, type(loc)
            assert False

    
    def __hash__(self):
        return hash(self.loctuple)

    def __eq__(self,rhs):
        if not isinstance(rhs, self.__class__): 
            return False
        return self.loctuple == rhs.loctuple


    def get_subns_addr(self, component_name):
        return NamespaceAddress( loc = tuple( list(self.loctuple) + [component_name] ) )
    def get_parent_addr(self):
        assert len(self.loctuple) > 0
        return NamespaceAddress( loc = self.loctuple[:-1] )

    def __repr__(self):
        return "NSAddr: '" + "//" + "/".join( self.loctuple) + "/'"

    @classmethod
    def create_root(cls):
        return NamespaceAddress( loc=() )

    def getstr(self):
        return "_".join( self.loctuple )

    def get_str_prefix(self):
        return self.getstr() + "_" 

    @classmethod
    def concat(cls,*args):
        print 'Concatenating:', args
        from nineml.utility import flattenFirstLevel
        loctuple = tuple( flattenFirstLevel( [ list(a.loctuple) for a in args  ] ) )
        res = NamespaceAddress(loc=loctuple)
        print 'yields:', res
        return res
