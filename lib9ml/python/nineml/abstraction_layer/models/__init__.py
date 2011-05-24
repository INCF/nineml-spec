

# System Imports:
import copy
import itertools



# Relative Imports:
import util





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
       self.portconnections.append( (src,sink) ) 
    

    def get_fully_qualified_port_connections(self):
        model_addr = self.getTreePosition()
        def make_fqname(target):
            return tuple ( model_addr + target.split(".") )

        conns = [ (make_fqname(src),make_fqname(sink)) for (src,sink) in self.portconnections ]
        return conns


    # Visitation 
    def AcceptVisitor(self,visitor):
        return visitor.AcceptModelNode(self)



class Component(TreeNode):
    def __init__(self, name, parameters = [], regimes = [], transitions=[], ports = [], bindings = [], model=None):
        super(Component,self).__init__()
        self._regimes = regimes
        self._transitions = transitions
        self._ports = ports
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
        return self._transitions

    @property
    def ports(self):
        return self._ports

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


    
    def get_fully_addressed_ports(self):
        comp_addr = self.component.getTreePosition()
        return dict( [ (tuple(comp_addr + [port.name]),port) for port in self.component.ports ] )

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





class ModelToSingleComponentReducer(object):
    def __init__(self,model):
        self.model = model 
        self.modelcomponents = ModelVisitorDF_ComponentCollector(self.model).components
        self.modelsubmodels = ModelVisitorDF_ModelCollector(self.model, include_root=True).models

        self.build_new_regime_space()

        #self.reducedcomponent = None




    # 1/ Create a new regime space:
    ###############################
    def copy_and_prefix_odes_from_regime(self,component,regime):
        # TODO: Proper prefixing:
        prefix = component.getTreePosition(jointoken="_") + "_"

        for n in regime.nodes: assert isinstance(n,nineml.Equation)
        excludes = ['t']
        #return [ expr.prefix(prefix=prefix,exclude=excludes) for expr in regime.nodes ]
        return [ expr.clone(prefix=prefix,prefix_excludes=excludes) for expr in regime.nodes ]



    def create_compound_regime( self, regimetuple, index ):
        component_regimes_zip = zip(self.modelcomponents, regimetuple)
        
        # Make a name for the state:
        namebuilder = lambda (comp,regime) : "%s:%s" % (comp.getPathString(), regime.name) 
        regime_name = "State%d"%index + ",".join([ namebuilder(rc) for rc in component_regimes_zip]) 
        
        # Copy accross all the nodes from each regime. We need to 
        # prefix all nodes with the names to prevent collision.
        regime_equations = [ self.copy_and_prefix_odes_from_regime(*rc) for rc in component_regimes_zip ]
        regime_equations = list( itertools.chain(*regime_equations ) ) 

        return nineml.Regime(*regime_equations, name = regime_name )

    def create_transition(self,oldtransition, oldcomponent, fromRegime, toRegime):
        transitionName = "%s#%s#%s"%( fromRegime.name, toRegime.name, oldtransition.name )    
        transitionName = None
        oldtransitionnamespace = oldcomponent.getTreePosition("_") + "_"


        # Functions to remap nodes:
        def remapNode_Assignment(n):
            return n.clone( prefix = oldtransitionnamespace, prefix_excludes=['t'] )
            
        def remapNode_EventPort(n):
            return nineml.EventPort( 
                    internal_symbol = oldtransitionnamespace + n.symbol,
                    mode=n.mode,
                    op=n.reduce_op )

        node_remapper = { 
                     nineml.Assignment: remapNode_Assignment,
                     nineml.EventPort: remapNode_EventPort,
                    }

        # Remap the nodes in the transition:
        mappednodes = [ node_remapper[ type(n) ] (n) for n in oldtransition.nodes ] 
        


        # Remap the condition:

        def remap_Condition_Condition(c):
            return c.clone( prefix=oldtransitionnamespace, prefix_excludes=['t'] )
            #return c.prefix(prefix = oldtransitionnamespace, exclude=['t'], expr=c.cond)
        
            #return newcondition
        def remap_Condition_EventPort(p):
            return nineml.EventPort( oldtransitionnamespace + p.name, mode=p.mode, op=p.reduce_op)

        condition_remapper = { 
                nineml.Condition: remap_Condition_Condition, 
                nineml.EventPort : remap_Condition_EventPort 
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
        print "Building new Regime Space"
        print "  Taking cross-product of existing regime-spaces"
        newRegimeLookupMap = {}
        regimes = [ comp.regimes for comp in self.modelcomponents]
        for i,regimetuple in enumerate( itertools.product(*regimes) ):
            newRegime = self.create_compound_regime( regimetuple, i ) 
            newRegimeLookupMap[regimetuple] = newRegime

        
        # Now we are in a position to setup the transitions:
        # Lets reiterate over the regime-space:
        print "  Setting up transition map"
        for comp in self.modelcomponents:
            print comp.getTreePosition()

        for regimetuple,regimeNew in newRegimeLookupMap.iteritems():
            #For each regime in the regimetuple, lets see what we can reach from there:
            print
            print "RegimeTuple:",regimetuple
            for regimeIndex, regime in enumerate( regimetuple ):
                print 'RegimeIndex:',regimeIndex, "NTransitions:", len(regime.transitions)
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
                    

    




        # Remapping of ports:
        #########################

        # Build the reduced component.
        # Each Component can have:
        #    - Analogue SendPorts (To be Exposed)
        #    - Analogue RecvPort  (To be exposed if not connected internally)
        #    - Analogue ReducePorts (To be exposed)
        #    - Input Event Ports
        #    - Output Event Ports
        
        
        # Compile a large dictionary, that maps addresses in the tree to ports:
        port_dicts = [ comp.query.get_fully_addressed_ports() for comp in self.modelcomponents ]
        ports = safe_dictionary_merge( port_dicts )
        portLocations = util.invertDictionary(ports)
        # ports = { ('comp1','comp1'):AnalogPorts(), ('comp1','comp2'):AnalogPorts(), ('comp1','comp3'):AnalogPorts(),}


        # Compile a large list of tuples (src,dst) containing all the fully qualified port mappings:
        # portconnections = [ (('comp1','comp1'),('comp1','comp2')), (('comp1','comp1'),('comp1','comp3')), ]
        portconnections = [model.get_fully_qualified_port_connections() for model in self.modelsubmodels] 
        portconnections = list( itertools.chain(* portconnections ) )


        internally_connectedports = []
        external_ports = []

        analogports = FilterType( ports.values(), nineml.AnalogPort )
        eventports = FilterType( ports.values(), nineml.EventPort  )



        # Resolve the internally connected ports:
        print "  Resolving internal connections"
        

        def remapNameGlobally(originalname, targetname):
            for regime in newRegimeLookupMap.values():
                for eqn in regime.equations:
                    eqn.rhs = eqn.rhs_name_transform( {originalname:targetname} )

                for transition in regime.transitions:
                    
                    # Transform Transition
                    for node in transition.nodes:
                        if isinstance(node, nineml.EventPort):
                            if node.symbol == originalname: node.symbol = targetname
                        elif isinstance(node, nineml.Assignment):
                            node.rhs = node.rhs_name_transform( {originalname:targetname} )
                        else:
                            assert False

                    print "Condition type:", type(transition.condition)
                    if isinstance(transition.condition, nineml.EventPort):
                        if transition.condition.symbol == originalname: 
                            transition.condition.symbol = targetname
                    elif isinstance(transition.condition, nineml.Condition):
                            transition.condition.cond = transition.condition.rhs_name_transform( {originalname:targetname} )
                    else: 
                        assert False
                        




        # Create a dictionary of all the remappings: send->recv:
        for src,dst in portconnections:
            print "   * Resolving  Internal Port (Recv)"
            dstport = ports[dst]
            srcport = ports[src]
            assert dstport.mode in ['recv','reduce']
            # Recieve Port:
            if dstport.mode == 'recv':
                # This is a recieve port. This means that we can hardwire the send-port data into it.
                # To do this, we remap all expressions in the dst-component model that use this port.
                
                # We need to check whether the send port is a binding. If it is, then lets 
                # substitute in the 'binding expression' if it isn't, then we can use the full name of the 
                # port:

                subExpr = srcport.expr if srcport.expr else srcport.symbol
                dstName = "_".join(dst)
                srcName = "_".join(src)
                print 'Substituting:', srcName, ' => ', dstName
                remapNameGlobally( originalname = dstName, targetname=srcName)

                # Finally remove the port:
                analogports.remove(dstport)



        # Resolve the Reduce ports that have an internal connection to a send port:
        #1/ Create a dictionary to pull all the things connected to each reduce port:
        reduceConnections = {}
        for src,dst in portconnections:
            dstport = ports[dst]
            srcport = ports[src]
            if dstport.mode == 'reduce':
                if not dst in reduceConnections:
                    reduceConnections[dst] = [ src ]
                else:
                    reduceConnections[dst].append(src)

        print "Active Reduce Ports:"
        for port,connections in reduceConnections.iteritems():
            reduce_op = ports[port].reduce_op
            reduce_port_name = '.'.join(port) 
            reduceexpr = reduce_op.join( [ "_".join(c) for c in connections ] )
            replacementexpr = "( ( %s ) "%(reduce_port_name) + reduce_op + " ( " + reduceexpr + " ) )"
            print port, "->", connections
            print replacementexpr
            remapNameGlobally( originalname=reduce_port_name, targetname=replacementexpr)




        

        # Find all  of the dstport-name:


        # Resolve the externally connected ports:
        #TODO: CLEAN UP THIS CODE:
        newports = []
        for p in analogports:
            newportname = '_'.join( portLocations[p] )
            if p.mode == 'send':
                newports.append( nineml.AnalogPort( internal_symbol = newportname, mode = p.mode, op = p.reduce_op ) ) 
            elif p.mode == 'recv':
                newports.append( nineml.AnalogPort( internal_symbol = newportname, mode = p.mode, op = p.reduce_op ) ) 
            elif p.mode == 'reduce':
                newports.append( nineml.AnalogPort( internal_symbol = newportname, mode = p.mode, op = p.reduce_op ) ) 
            else:
                assert False


        


        print "Building constructing reduced component"
        #self.reducedcomponent = nineml.Component( "reduced", regimes = newRegimeLookupMap.values(), ports=[] )
        self.reducedcomponent = nineml.Component( "reduced", regimes = newRegimeLookupMap.values(), ports=newports )



            









def reduce_to_single_component( model ):
    return ModelToSingleComponentReducer(model).reducedcomponent







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
