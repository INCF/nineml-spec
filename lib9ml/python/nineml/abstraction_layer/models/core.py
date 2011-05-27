

# System Imports:
import copy
import itertools



# Relative Imports:
import util








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



class Component(TreeNode):
    def __init__(self, name, parameters = [], regimes = [],  analog_ports = [], bindings = [], model=None):
        super(Component,self).__init__()
        self._regimes = regimes
        #self._transitions = transitions
        self._analog_ports = analog_ports
        self._bindings = bindings

        self.query = ComponentQueryer(self)
        self._parent = None
        self._name = name

        # Do some checking:
        self._parameters = parameters



    @property
    def name(self):
        return self._name

    #Interface
    def getAllComponents(self,):
        return [self]


    # Setting the parent model. [Used for working out our name]


    ## Querying:
    ############
    # Basic properties as lists:
    @property
    def regimes(self):
        return self._regimes 

    @property 
    def transitions(self):
        for r in self.regimes:
            for t in r.transitions:
                yield t
        #return self._transitions

    @property
    def ports(self):
        for p in self._analog_ports:
            yield p
        for t in self.transitions:
            for e in t.event_ports:
                yield e
                 

   # @property
   # def subcomponents(self):
   #     return self.

    @property
    def bindings(self):
        raise UnimplementedError
        



    def AcceptVisitor(self,visitor):
        return visitor.AcceptComponentNode(self)


def ExpectSingle(lst):
    if len(lst) != 1:
        print "Filter Expect Single: ",lst
        assert False

    assert len(lst) == 1
    return lst[0]


def FilterExpectSingle(lst, func):
    #lst = [ l for l in lst if l and func(l) ]
    return ExpectSingle( Filter(lst, func) )

def Filter(lst,func):
    return  [ l for l in lst if l and func(l) ]

def FilterType(lst, acceptedtype):
    return Filter( lst, lambda x: isinstance(x,acceptedtype))



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


