

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
        newnodes= [ expr.clone(prefix=prefix,prefix_excludes=excludes,prefix_name=True) for expr in regime.time_derivatives ]
        return newnodes



    def create_compound_regime( self, regimetuple, index ):
        component_regimes_zip = zip(self.modelcomponents, regimetuple)
        
        regime_name = None
        
        # Copy accross all the nodes from each regime. We need to 
        # prefix all nodes with the names to prevent collision.
        time_derivatives = util.flattenFirstLevel( [ self.copy_and_prefix_odes_from_regime(*rc) for rc in component_regimes_zip ] )
        
        # Don't worry about transitions yet, 
        return nineml.Regime(name=None, time_derivatives=time_derivatives, on_events=[], on_conditions=[] )

#    def create_transition(self, oldtransition, oldcomponent, fromRegime, toRegime):
#        transitionName = None
#        oldtransitionnamespace = oldcomponent.getTreePosition("_") + "_"
#
#
#            
#        # Remap all the nodes:
#        node_remapper = { 
#                     nineml.Assignment: lambda n: n.clone( prefix = oldtransitionnamespace, prefix_excludes=['t'] ),
#                     #nineml.EventPort:  lambda e: e.clone( prefix = oldtransitionnamespace, prefix_excludes=['t'] ),
#                     nineml.OutputEvent:  lambda e: e.clone( prefix = oldtransitionnamespace, prefix_excludes=['t'] ),
#                     nineml.InputEvent:  lambda e: e.clone( prefix = oldtransitionnamespace, prefix_excludes=['t'] ),
#                    }
#        
#        mappednodes = [ node_remapper[ type(n) ] (n) for n in oldtransition.nodes ] 
#        
#
#
#        # Remap the condition:
#        condition_remapper = { 
#                nineml.Condition:  lambda c: c.clone( prefix=oldtransitionnamespace, prefix_excludes=['t'] ) , 
#                nineml.EventPort : lambda p: p.clone( prefix=oldtransitionnamespace, prefix_excludes=['t'] ) 
#        }
#        newcondition = condition_remapper[ type(oldtransition.condition) ](oldtransition.condition)
#
#
#
#        t = nineml.Transition(  *mappednodes, 
#                                from_=fromRegime,
#                                to=toRegime.name,
#                                condition= newcondition,
#                                name= transitionName )
#        return t
#


    def create_on_condition(self, oldtransition, oldcomponent, fromRegime, toRegime):
        transitionName = None
        oldtransitionnamespace = oldcomponent.getTreePosition("_") + "_"


        newAssignments = [ a.clone(prefix=oldtransitionnamespace, prefix_excludes=['t']) for a in oldtransition.state_assignments]  
        newEventOutputs = [ e.clone(prefix=oldtransitionnamespace, prefix_excludes=['t']) for e in oldtransition.event_outputs]  

       # # Remap all the nodes:
       # node_remapper = { 
       #              nineml.Assignment: lambda n: n.clone( prefix = oldtransitionnamespace, prefix_excludes=['t'] ),
       #              #nineml.EventPort:  lambda e: e.clone( prefix = oldtransitionnamespace, prefix_excludes=['t'] ),
       #              nineml.OutputEvent:  lambda e: e.clone( prefix = oldtransitionnamespace, prefix_excludes=['t'] ),
       #              nineml.InputEvent:  lambda e: e.clone( prefix = oldtransitionnamespace, prefix_excludes=['t'] ),
       #             }
       # 
       # mappednodes = [ node_remapper[ type(n) ] (n) for n in oldtransition.nodes ] 
       # 


       # # Remap the condition:
       # condition_remapper = { 
       #         nineml.Condition:  lambda c: c.clone( prefix=oldtransitionnamespace, prefix_excludes=['t'] ) , 
       #         nineml.EventPort : lambda p: p.clone( prefix=oldtransitionnamespace, prefix_excludes=['t'] ) 
       # }
       # newcondition = condition_remapper[ type(oldtransition.condition) ](oldtransition.condition)



        t = nineml.OnCondition(  trigger = oldtransition.trigger.clone(prefix=oldtransitionnamespace, prefix_excludes=['t'] ),
                                  state_assignments = newAssignments,
                                  event_outputs = newEventOutputs )
#                                  *mappednodes, 
#                                  from_=fromRegime,
#                                  to=toRegime.name,
#                                  condition= newcondition,
#                                  name= transitionName
#                             )
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

                for oldtransition in regime.on_events:
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
                    print oldtransition
                    t = self.create_transition(oldtransition=oldtransition,oldcomponent=oldcomponent, fromRegime=regimeNew, toRegime=newRegimeTo)
                    regimeNew.add_on_event( on_event=t )
        
                for oldtransition in regime.on_conditions:
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
                    t = self.create_on_condition(oldtransition=oldtransition,oldcomponent=oldcomponent, fromRegime=regimeNew, toRegime=newRegimeTo)
                    regimeNew.add_on_condition( on_condition = t )
        self.newRegimeLookupMap = newRegimeLookupMap

    

    # This is a mess, to be cleaned:
    @classmethod
    def global_remap_port_ext(cls, originalname, targetname, new_ports, newRegimeLookupMap):
        #print 'Global-Remap [%s- >%s]'%(originalname,targetname)
                    
        for p in new_ports.values():
            if not p.expr: continue
            p.expr.rhs_name_transform_inplace( {originalname:targetname} )
        
        for regime in newRegimeLookupMap.values():
            
            for alias in regime.aliases:
                alias.name_transform_inplace( {originalname:targetname} )

            for eqn in regime.equations:
                eqn.rhs_name_transform_inplace( {originalname:targetname} )
            
            
            for av in regime.assignments:
                av.rhs = av.rhs_name_transform( {originalname:targetname} )
            
            for transition in regime.transitions:
                
                # Transform Transition
                for node in transition.nodes:
                    if  isinstance(node, nineml.EventPort):
                        if node.symbol == originalname: node.symbol = targetname
                    elif isinstance(node, nineml.Assignment):
                        node.rhs_name_transform_inplace( {originalname:targetname} )
                    else:
                        assert False

                #print "Condition type:", type(transition.condition)
                if isinstance(transition.condition, nineml.EventPort):
                    if  transition.condition.symbol == originalname: 
                        transition.condition.symbol = targetname
                elif isinstance(transition.condition, nineml.Condition):
                        #transition.condition.cond = transition.condition.rhs_name_transform( {originalname:targetname} )
                        transition.condition.rhs_name_transform_inplace( {originalname:targetname} )
                        #transition.condition.parse()
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
            new_ports[ns] = port.clone( prefix=ns.get_parent_addr().get_str_prefix(), prefix_excludes=['t'] )
        

        # [Forwarding function]
        def globalRemapPort(originalname,targetname):
            return ModelToSingleComponentReducer.global_remap_port_ext(originalname=originalname, targetname=targetname, new_ports=new_ports, newRegimeLookupMap=newRegimeLookupMap)
        
        def get_send_port_subsitution(srcPort):
            if srcPort.expr:
                return "(%s)"%srcPort.expr.rhs
            return srcPort.name 


        #for ns,port in new_ports.iteritems():
        #    print ns, port

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



        #for ns,p in new_ports.iteritems():
        #    print p, p.symbol
        
        self.reducedcomponent = nineml.Component( self.componentname, regimes = newRegimeLookupMap.values(), ports=new_ports.values() )
        
        









            









def reduce_to_single_component( model, componentname ):
    reducer = ModelToSingleComponentReducer(model,componentname)
    #print reducer.portmappings
    return reducer.reducedcomponent
