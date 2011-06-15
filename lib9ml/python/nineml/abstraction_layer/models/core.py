

# System Imports:
import copy
import itertools



# Relative Imports:
import util



import nineml.abstraction_layer as nineml




class TreeNode(object):
    def __init__(self):  
        self._parentmodel = None

    

    # Naming Functions:
    # != TO GO -!
    def getContainedNamespaceName(self):
        if not self.getParentModel(): return ""
        return util.invertDictionary(self.getParentModel().subnodes)[self]

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
    def AcceptVisitor(self,visitor):
        raise NotImplementedException




    
class Model(TreeNode):
    def __init__(self, name, subnodes={}, model=None):
        super(Model,self).__init__()
        
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
        

        model_addr = self.getTreePosition()
        def make_fqname(target):
            return tuple ( model_addr + target.split(".") )

        conns = [ (make_fqname(src),make_fqname(sink)) for (src,sink) in self.portconnections ]
        return conns


    # Visitation 
    def AcceptVisitor(self,visitor):
        return visitor.AcceptModelNode(self)




from nineml.abstraction_layer import ComponentClass

class ComponentNode(ComponentClass,TreeNode, ):
    #def __init__(self, name, parameters = [], regimes = [],  analog_ports = [], aliases = [], model=None):
    def __init__(self, name, parameters = [],  analog_ports = [], event_ports=[], dynamics=None, model=None):
        TreeNode.__init__(self, )
        ComponentClass.__init__(self, name=name, parameters = parameters, analog_ports=analog_ports, event_ports = event_ports, dynamics = dynamics)

        self.query = ComponentQueryer(self)

#Interface
    def getAllComponents(self,):
        return [self]

    def AcceptVisitor(self,visitor):
        return visitor.AcceptComponentNode(self)




from nineml.utility import ExpectSingle, FilterExpectSingle, Filter, FilterType



class ComponentQueryer(object):
    def __init__(self, component):
        self.component = component

    # Find basic properties by name
    def regime(self, name=None,):
        assert isinstance(name,basestring)
        rFunc = lambda r:r.name==name 
        return FilterExpectSingle( self.component.regimes,rFunc ) 
        
    def transition(self, name=None):
        return FilterExpectSingle( self.component.transitions, lambda c:c.name==name ) 
        
    def port(self, cls=None, mode=None, symb=None):
        raise UnimplementedError

    def subcomponent(self, name):
        raise UnimplementedError


    
    def get_fully_addressed_analogports_new(self):
        comp_addr = self.component.get_node_addr()
        return dict( [ (comp_addr.get_subns_addr(port.name), port) for port in self.component.ports if isinstance(port, nineml.AnalogPort) ] )

    # TO GO:
    def get_fully_addressed_ports_new(self):
        comp_addr = self.component.get_node_addr()
        return dict( [ (comp_addr.get_subns_addr(port.name), port) for port in self.component.ports ] )
    def get_fully_addressed_ports(self):
        comp_addr = self.component.getTreePosition()
        return dict( [ (tuple(comp_addr + [port.name]),port) for port in self.component.ports ] )
    ###########



    #More advanced searches on just this node:
    @property
    def conditions(self):
        raise UnimplementedError

    @property
    def odes(self):
        raise UnimplementedError

    @property
    def equations(self):
        raise UnimplementedError
    
    @property
    def non_parameter_symbols(self):
        raise UnimplementedError

    @property
    def variables(self):
        raise UnimplementedError

    @property
    def state_variables(self):
        raise UnimplementedError

    @property
    def bound_symbols(self):
        raise UnimplementedError

    @property
    def assigned_variables(self):
        raise UnimplementedError

    @property
    def independent_variables(self):
        raise UnimplementedError

    @property
    def integrated_variables(self):
        raise UnimplementedError






class NamespaceAddress(object):
    def __init__(self, loc):
        if isinstance(loc, basestring):
            self.loctuple = loc.split('.')
        elif isinstance(loc, tuple):
            self.loctuple = loc
        else:
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

    def get_str_prefix(self):
        return "_".join( self.loctuple ) + "_"

    @classmethod
    def concat(cls,*args):
        loctuple = tuple( util.flattenFirstLevel( [ list(a.loctuple) for a in args ] ) )
        return NamespaceAddress(loc=loctuple)
