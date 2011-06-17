
from operator import and_


from namespaceaddress import NamespaceAddress
import componentqueryer
import nineml.utility
import dynamics as dyn


import copy
import itertools


class ComponentClassMixin_FlatStructure(object):

    def __init__(self, name, parameters = None, analog_ports = None, event_ports = None, dynamics=None):
        """Create a ComponentClass object
        
        Keyword arguments:

        :param name: the name of the container
        :type parameters: integer or None
        :rtype: list of strings 
        
        name -- the name of the container
        parameters -- [..]
        
        """

        self._name = name
        self._parameters = parameters or []
        self._analog_ports = analog_ports or []
        self._event_ports = event_ports or []
        self._dynamics = dynamics


    # Basic properties:
    @property
    def name(self):
        return self._name

    @property
    def parameters(self):
        return iter(self._parameters)

    @property
    def analog_ports(self):
        return self._analog_ports

    @property
    def event_ports(self):
        return self._event_ports

    @property
    def dynamics(self):
        return self._dynamics
    # -------------------------- #

    #  Forwarding functions to the dynamics #
    @property
    def aliases_map(self):
        return self._dynamics.aliases_map

    @property
    def aliases(self):
        return self._dynamics.aliases

    @property
    def regimes(self):
        return self._dynamics.regimes

    @property
    def transitions(self):
        return self._dynamics.transitions

    @property
    def state_variables(self):
        return self._dynamics.state_variables
    # -------------------------- #

    @property
    def ports_map(self):
        return dict( [ (p.name,p) for p in itertools.chain(self._analog_ports, self._event_ports) ] )
    @property
    def alias_symbols(self):
        return [ a.lhs for a in self.aliases ]



    @property
    def conditions(self):
        """ Returns all conditions """
        # TODO transitions
        for t in self.transitions:
            yield t.condition

    @property
    def on_conditions(self):
        for r in self.regimes:
            for c in r.on_conditions:
                yield c



    


    def backsub_aliases(self):
        """ This function finds aliases with undefined functions, and uses
        the alias_map to attempt to resolve them. """

        # build alias dependency tree
        # and perform substitution, recursively
        def build_and_resolve_bdtree(b):
            _bd_tree = {}
            for f in b.missing_functions:
                if f in self.aliases_map:
                    _bd_tree[f] = build_and_resolve_bdtree(self.aliases_map[f])
                    # resolve (lower level is already resolved now) 
                    b.substitute_alias(self.aliases_map[f])
                    # re-calc functions
                    b.parse()
                else:
                    raise ValueError, "alias '%s' calls unresolvable functions." % b.as_expr()
            return _bd_tree  
        
        bd_tree = {}
        for b in self.aliases_map.itervalues():
            bd_tree[b.name] = build_and_resolve_bdtree(b)

    def backsub_equations(self):
        """ this function finds all undefined functions in equations, and uses
        the alias_map to resolve them """
        from nineml.abstraction_layer.visitors import InPlaceTransform

        for alias in self.aliases:
            trans = InPlaceTransform( originalname = alias.lhs, targetname = "(%s)"%alias.rhs )
            # Since we do not want to backsub in lhs of this alias, we can't call self.AcceptVisitor() directly
            for r in self.regimes:
                r.AcceptVisitor(trans)


    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False

        sort_key = lambda node: node.name

        return reduce(and_, (self.name == other.name,
                             self.parameters == other.parameters,
                             sorted(self.transitions, key=sort_key) == sorted(other.transitions, key=sort_key),
                             sorted(self.regimes, key=sort_key) == sorted(other.regimes, key=sort_key),
                             sorted(self.aliases, key=sort_key) == sorted(other.aliases, key=sort_key)))


    def write(self, file, flatten=True):
        """
        Export this model to a file in 9ML XML format.
        file is filename or file object.
        """
        from nineml.abstraction_layer.writers import XMLWriter
        return XMLWriter.write(component=self, file=file, flatten=flatten)






class ComponentClassMixin_NamespaceStructure(object):
    def __init__(self, subnodes = None, portconnections=None):  
        subnodes = subnodes or {}
        portconnections = portconnections or []


        self._parentmodel = None
        self.subnodes = {}
        self.portconnections = []

        for namespace,subnode in subnodes.iteritems():
            self.insert_subnode(subnode=subnode, namespace=namespace)
        
        for src,sink in portconnections:
            self.connect_ports(src,sink)

    # Naming Functions:
    # != TO GO -!
    def getContainedNamespaceName(self):
        if not self.getParentModel(): return ""
        return nineml.utility.invert_dictionary(self.getParentModel().subnodes)[self]

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
        

    def insert_subnode(self, subnode, namespace):
        assert not namespace in self.subnodes
        self.subnodes[namespace] = copy.deepcopy( subnode ) 
        self.subnodes[namespace].setParentModel(self)

    def connect_ports( self, src, sink ):
        #TODO: Check that the ports are connected to items in this model.
        self.portconnections.append( (NamespaceAddress(src),NamespaceAddress(sink) ) ) 

    def isLeaf(self):
        return len(self.subnodes) == 0


    



class ComponentClass( ComponentClassMixin_FlatStructure, ComponentClassMixin_NamespaceStructure ):
    """ComponentClass represents a..."""
    
    element_name = "ComponentClass"
    def isflat(self):
        return self.isLeaf()

    def __init__(self, name, parameters=None, analog_ports=None, event_ports=None, dynamics=None, subnodes=None):

        self.query = componentqueryer.ComponentQueryer(self)

        # We should always create a dynamics object, even is it is empty:
        if dynamics == None:
            dynamics = dyn.Dynamics()

        ComponentClassMixin_FlatStructure.__init__(self, name=name, parameters = parameters, analog_ports=analog_ports, event_ports = event_ports, dynamics = dynamics)
        ComponentClassMixin_NamespaceStructure.__init__(self,subnodes=subnodes)

        #Finalise Initiation:
        self.ResolveTransitionRegimeNames()
 

        
    def ResolveTransitionRegimeNames(self):
        # Check that the names of the regimes are unique:
        names = [ r.name for r in self.regimes ]
        nineml.utility.assert_no_duplicates(names)

        #Create a map of regime names to regimes:
        regimeMap = dict( [ (r.name,r) for r in self.regimes] )

        # We only worry about 'target' regimes, since source regimes are taken 
        # care of for us by the Regime objects they are attached to.
        for t in self.transitions:
            assert t.target_regime_name in regimeMap, "Can't resolve transition's target regime: %s"%t.target_regime_name
            t.set_target_regime( regimeMap[t.target_regime_name] )


        
    # Connections and Subnodes:
    def get_fully_qualified_port_connections(self):
        namespace = self.get_node_addr()
        def make_fqname(target):
            return NamespaceAddress.concat( namespace, target)
        conns = [ (make_fqname(src),make_fqname(sink)) for (src,sink) in self.portconnections ]
        return conns

    def AcceptVisitor(self, visitor,**kwargs):
        return visitor.VisitComponentClass(self)



