

# System Imports:
import copy
import itertools


import nineml.abstraction_layer as nineml
# Relative Imports:
import util

from visitors import ModelVisitorDF_ComponentCollector, ModelVisitorDF_ModelCollector


class ModelToSingleComponentReducer(object):
    
    @classmethod
    def flatten_namespace(cls, ns):
        return ns.replace('.','_')
    
    @classmethod
    def flatten_namespace_dict(cls, ns):
        newParams = {}
        for k,v in ns.iteritems():
            newParams[ cls.flatten_namespace(k) ] = v
        return newParams
     
    
    
    
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
        regime_equations = util.flattenFirstLevel( [ self.copy_and_prefix_odes_from_regime(*rc) for rc in component_regimes_zip ] )
        
        
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
        old_ports = util.safe_dictionary_merge( old_port_dicts )

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