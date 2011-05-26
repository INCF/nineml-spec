

# System Imports:
import copy
import itertools



# Relative Imports:
import util




def flattenFirstLevel( nestedList ):
    return list( itertools.chain(*nestedList) )

def safe_dictionary_merge( dictionaries ):
    newDict = {}
    for d in dictionaries:
        for k,v in d.iteritems():
            assert not k in newDict
            newDict[k] = v
    return newDict




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






# Base class for depth-first visitation:
class ModelVisitorDF(object):

    def Visit(self,model):
        model.AcceptVisitor(self)

    def AcceptComponentNode(self,node):
        self.VisitComponentNode(node)

    def AcceptModelNode(self,node):
        self.VisitModelNode(node)
        for sn in sorted(node.subnodes.values(), key=lambda sn:sn.getContainedNamespaceName() ):
            sn.AcceptVisitor(self)


    # To be overridden
    def VisitComponentNode(self, node):
        pass

    # To be overridden
    def VisitModelNode(self, node):
        pass





# Base class for visitation:
class ModelVisitorDF_NodeCollector(ModelVisitorDF):
    def __init__(self, model=None):
        self.nodes = []
        if model: self.Visit(model)

    def VisitComponentNode(self, node):
        self.nodes.append(node)
    def VisitModelNode(self, node):
        self.nodes.append(node)

    def __iter__(self):
        return iter(self.nodes)

class ModelVisitorDF_ComponentCollector(ModelVisitorDF):
    def __init__(self, model=None):
        self.components = []
        if model: self.Visit(model)

    def VisitComponentNode(self, node):
        self.components.append(node)

    def __iter__(self):
        return iter(self.components)


class ModelVisitorDF_ModelCollector(ModelVisitorDF):
    def __init__(self, model=None,include_root=True):
        assert include_root
        self.models = []
        if model: self.Visit(model)

    def VisitModelNode(self, node):
        self.models.append(node)

    def __iter__(self):
        return iter(self.components)





import nineml.abstraction_layer as nineml


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
        loctuple = tuple( flattenFirstLevel( [ list(a.loctuple) for a in args ] ) )
        return NamespaceAddress(loc=loctuple)


class ModelToSingleComponentReducer(object):
    def __init__(self,model, componentname):
        self.model = model 
        self.componentname=componentname

        self.modelcomponents = ModelVisitorDF_ComponentCollector(self.model).components
        self.modelsubmodels = ModelVisitorDF_ModelCollector(self.model, include_root=True).models

        self.build_new_regime_space()
        self.remap_ports()




    # 1/ Create a new regime space:
    ###############################
    def copy_and_prefix_odes_from_regime(self,component,regime):
        # TODO: Proper prefixing:
        prefix = component.getTreePosition(jointoken="_") + "_"
        excludes = ['t']
        return [ expr.clone(prefix=prefix,prefix_excludes=excludes) for expr in regime.nodes ]



    def create_compound_regime( self, regimetuple, index ):
        component_regimes_zip = zip(self.modelcomponents, regimetuple)
        
        # Make a name for the state:
        namebuilder = lambda (comp,regime) : "%s:%s" % (comp.getPathString(), regime.name) 
        regime_name = "State%d"%index + ",".join([ namebuilder(rc) for rc in component_regimes_zip]) 
        regime_name = None
        
        # Copy accross all the nodes from each regime. We need to 
        # prefix all nodes with the names to prevent collision.
        regime_equations = flattenFirstLevel( [ self.copy_and_prefix_odes_from_regime(*rc) for rc in component_regimes_zip ] )
        
        
        return nineml.Regime(*regime_equations, name = regime_name )

    def create_transition(self, oldtransition, oldcomponent, fromRegime, toRegime):
        transitionName = "%s#%s#%s"%( fromRegime.name, toRegime.name, oldtransition.name )    
        transitionName = None
        oldtransitionnamespace = oldcomponent.getTreePosition("_") + "_"


            
        # Remap all the nodes:
        node_remapper = { 
                     nineml.Assignment: lambda n: n.clone( prefix = oldtransitionnamespace, prefix_excludes=['t'] ),
                     nineml.EventPort:  lambda e: e.clone( prefix = oldtransitionnamespace, prefix_excludes=['t'] ),
                    }
        
        mappednodes = [ node_remapper[ type(n) ] (n) for n in oldtransition.nodes ] 
        


        # Remap the condition:
        condition_remapper = { 
                nineml.Condition:  lambda c: c.clone( prefix=oldtransitionnamespace, prefix_excludes=['t'] ) , 
                nineml.EventPort : lambda p: p.clone( prefix=oldtransitionnamespace, prefix_excludes=['t'] ) 
        }
        newcondition = condition_remapper[ type(oldtransition.condition) ](oldtransition.condition)



        t = nineml.Transition( *mappednodes, 
                        from_=fromRegime,
                        to=toRegime.name,
                        condition= newcondition,
                        name= transitionName )
        return t

    
    def build_new_regime_space(self):

        # Create our new 'Regime-Space'. This is the cross product off all
        # the 'Regime-Spaces' from each component.
        
        #print "Building new Regime Space"
        #print "  Taking cross-product of existing regime-spaces"
        newRegimeLookupMap = {}
        regimes = [ comp.regimes for comp in self.modelcomponents]
        for i,regimetuple in enumerate( itertools.product(*regimes) ):
            newRegime = self.create_compound_regime( regimetuple, i ) 
            newRegimeLookupMap[regimetuple] = newRegime

        
        # Now we are in a position to setup the transitions:
        # Lets reiterate over the regime-space:
        
        for regimetuple,regimeNew in newRegimeLookupMap.iteritems():
            #For each regime in the regimetuple, lets see what we can reach from there:
            
            #print "RegimeTuple:",regimetuple
            for regimeIndex, regime in enumerate( regimetuple ):
                #print 'RegimeIndex:',regimeIndex, "NTransitions:", len(regime.transitions)
                for oldtransition in regime.transitions:
                    # We calculate the tuple of the Regime this transitions should jump to:
                    #print oldtransition
                    srcRegime = list( regimetuple )
                    dstRegime = srcRegime[:]

                    # Points to another node:
                    dstRegimeName = oldtransition.to.get_ref() if oldtransition.to else regime
                    dstRegimeOld = self.modelcomponents[regimeIndex].query.regime(name=dstRegimeName.name) 
                    dstRegime[regimeIndex] = dstRegimeOld
                    newRegimeTo = newRegimeLookupMap[ tuple(dstRegime) ] 
                    transName = newRegimeTo.name

                    oldcomponent = self.modelcomponents[regimeIndex]
                    t = self.create_transition(oldtransition=oldtransition,oldcomponent=oldcomponent, fromRegime=regimeNew, toRegime=newRegimeTo)
                    regimeNew.add_transition( t = t )
        
        self.newRegimeLookupMap = newRegimeLookupMap

    

    # This is a mess, to be cleaned:
    @classmethod
    def global_remap_port_ext(cls, originalname, targetname, new_ports, newRegimeLookupMap):
        print 'Global-Remap [%s- >%s]'%(originalname,targetname)
                    
        for p in new_ports.values():
            if not p.expr: continue
            print '  ', p, p.expr
            p.expr.rhs = p.expr.rhs_name_transform( {originalname:targetname} )
            p.expr.parse()
            print '->', p, p.expr
        
        for regime in newRegimeLookupMap.values():
            for eqn in regime.equations:
                print '  ', eqn, 
                eqn.rhs = eqn.rhs_name_transform( {originalname:targetname} )
                eqn.parse()
                print '->', eqn
            
            
            for av in regime.assignments:
                print '  ', av,
                av.rhs = av.rhs_name_transform( {originalname:targetname} )
                av.parse()
                print '->', av
            
            for transition in regime.transitions:
                
                # Transform Transition
                for node in transition.nodes:
                    if  isinstance(node, nineml.EventPort):
                        if node.symbol == originalname: node.symbol = targetname
                    elif isinstance(node, nineml.Assignment):
                        print '  ', node, 
                        node.rhs = node.rhs_name_transform( {originalname:targetname} )
                        node.parse()
                        print '->', node
                    else:
                        assert False

                #print "Condition type:", type(transition.condition)
                if isinstance(transition.condition, nineml.EventPort):
                    if  transition.condition.symbol == originalname: 
                        transition.condition.symbol = targetname
                elif isinstance(transition.condition, nineml.Condition):
                        transition.condition.cond = transition.condition.rhs_name_transform( {originalname:targetname} )
                        transition.condition.parse()
                else: 
                    assert False



    def remap_ports(self):
        newRegimeLookupMap = self.newRegimeLookupMap

        # Create a maps { Namespaces -> Ports}
        old_port_dicts = [ comp.query.get_fully_addressed_analogports_new() for comp in self.modelcomponents ]
        old_ports = safe_dictionary_merge( old_port_dicts )

        # Create the new analog ports with prefixed names:
        new_ports = {}
        for ns,port in old_ports.iteritems():
            new_ports[ns] = port.clone( prefix=ns.get_parent_addr().get_str_prefix() )
        

        # [Forwarding function]
        def globalRemapPort(originalname,targetname):
            return ModelToSingleComponentReducer.global_remap_port_ext(originalname=originalname, targetname=targetname, new_ports=new_ports, newRegimeLookupMap=newRegimeLookupMap)
        
        def get_send_port_subsitution(srcPort):
            if srcPort.expr:
                return "(%s)"%srcPort.expr.rhs
            return srcPort.name 



        # Handle port mappings:
        # portconnections = [ (NS -> NS),(NS -> NS ), (NS -> NS) ]
        portconnections = [model.get_fully_qualified_port_connections() for model in self.modelsubmodels] 
        portconnections = list( itertools.chain(* portconnections ) )

        # A. Handle Recieve Ports:
        for srcAddr,dstAddr in portconnections[:]:
            srcPort = new_ports[srcAddr]
            dstPort = new_ports[dstAddr]
            if dstPort.mode == 'recv':
                globalRemapPort( dstPort.name, get_send_port_subsitution(srcPort) )
                del new_ports[ dstAddr ]
                portconnections.remove( (srcAddr,dstAddr) )


        # B. Handle Reduce Ports:
        # 1/ Make a map { reduce_port -> [send_port1, send_port2, send_port3], ...}
        from collections import defaultdict
        reduce_connections = defaultdict( list )
        for src,dst in portconnections:
            dstport = new_ports[dst]
            srcport = new_ports[src]
            if dstport.mode == 'reduce':
                reduce_connections[dstport].append(srcport)

        # 2/ Subsitute each reduce port in turn:
        for dstport, srcportList in reduce_connections.iteritems():
            src_subs = [ get_send_port_subsitution(s) for s in srcportList ]
            terms = [dstport.name] + src_subs
            reduce_expr= dstport.reduce_op.join(terms) 
            globalRemapPort( dstport.name, reduce_expr )

        self.reducedcomponent = nineml.Component( self.componentname, regimes = newRegimeLookupMap.values(), ports=new_ports.values() )
        
        









            









def reduce_to_single_component( model, componentname ):
    reducer = ModelToSingleComponentReducer(model,componentname)
    #print reducer.portmappings
    return reducer.reducedcomponent







#c = Component(...)
#c.query["./%TRANSITION{TO=...}"]
#c.query["./%REGIME{NAME=...}"]
#c.query["./subcomponent1/%REGIME{NAME=...}"]
#print c.query.ports


from Cheetah.Template import Template

def dump_reduced(component, filename):
    
    tmpl = """
    MODEL: 

    PORTS:
    ==========

    #for p in $component.analog_ports:
      AnalogPort: $p.name, $p.mode, $p.reduce_op
    #end for
    
    #for p in $component.event_ports:
      EventPort: $p.name, $p.mode, $p.reduce_op
    #end for


    PARAMETERS
    ===============
    #for up in $component.user_parameters:
      Parameter: $up
    #end for

    REGIMES:
    ========
    #for regime in $component.regimes:
    
    Regime: $regime
    ----------------
    
    #for eqn in $regime.equations:
       Eqn: $eqn
    #end for

        Transitions
        ~~~~~~~~~~~~~~
        #for $transi in $regime.transitions:
           Transition: $transi.name

             Condition: $transi.condition

           #for node in $transi.nodes:
             Node: $node
           #end for

        #end for


    #end for

    """


    data = { 'component':component }
    f = open(filename,"w")
    s = Template(tmpl, data).respond()
    f.write(s)
    f.close()















def writeCSolverSimple( component, filename):
    assert isinstance(component, nineml.Component)


    templ1 = """
    
    int current_regime;

    // State Variables:
    #for statevar in $component.odes:
    float $statevar.dependent_variable = 0;
    #end for





    """


    for i in range(5): 
        print 

    cppTxt = Template( templ1, {'component':component } ).respond()

    print cppTxt
