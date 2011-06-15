
from nineml.utility import invertDictionary,flattenFirstLevel

# System Imports:
import copy
import itertools


#import nineml.abstraction_layer as nineml
import nineml.abstraction_layer as al
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
        # Are we trying to flatten something that it already flattened
        from nineml.abstraction_layer import ComponentClass
        from nineml.abstraction_layer.models import ComponentNode, Model
        if isinstance( model, (ComponentClass,ComponentNode) ):
            self.reducedcomponent = model
            return

        # Check we have a model:
        assert isinstance( model, Model )

        self.componentname=componentname

        # Flatten all the namespaces:
        from nineml.abstraction_layer.visitors import ClonerVisitor, ModelPrefixerVisitor
        self.model = ModelPrefixerVisitor().VisitModelClass(model)

        self.modelcomponents = ModelVisitorDF_ComponentCollector(self.model).components
        self.modelsubmodels = ModelVisitorDF_ModelCollector(self.model, include_root=True).models
        
        self.build_new_regime_space()
        self.remap_ports()






    def create_compound_regime( self, regimetuple, index ):
        
        # Copy accross all the odes from each regime. 
        # [Don't worry about transitions yet, we deal with them later]
        return al.Regime( name=None, 
                        time_derivatives = flattenFirstLevel( [ r.time_derivatives for r in regimetuple ] ),
                        on_events=[], 
                        on_conditions=[] )



    def create_on_condition(self, oldtransition, oldcomponent, fromRegime, toRegime):
        from nineml.abstraction_layer.visitors import ClonerVisitor
        return oldtransition.AcceptVisitor( ClonerVisitor(prefix='',prefix_excludes=[]) )

    def create_on_event(self, oldtransition, oldcomponent, fromRegime, toRegime):
        from nineml.abstraction_layer.visitors import ClonerVisitor
        return oldtransition.AcceptVisitor( ClonerVisitor(prefix='',prefix_excludes=[]) )

    
    def build_new_regime_space(self):

        # Create our new 'Regime-Space'. This is the cross product off all
        # the 'Regime-Spaces' from each component.
        
        #print "Building new Regime Space"
        #print "  Taking cross-product of existing regime-spaces"
        newRegimeLookupMap = {}
        regimes = [ comp.regimes for comp in self.modelcomponents]
        print "Regimes:", regimes
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
                    #t = self.create_transition(oldtransition=oldtransition,oldcomponent=oldcomponent, fromRegime=regimeNew, toRegime=newRegimeTo)
                    t = self.create_on_event(oldtransition=oldtransition,oldcomponent=oldcomponent, fromRegime=regimeNew, toRegime=newRegimeTo)
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

    






    def remap_ports(self):
        from nineml.utility import safe_dictionary_merge
        newRegimeLookupMap = self.newRegimeLookupMap

        from nineml.utility import flattenFirstLevel
        from nineml.abstraction_layer.models import NamespaceAddress

        new_ports = flattenFirstLevel( [comp.analog_ports for comp in self.modelcomponents]) 
        new_ports = dict( [ (p.name, p) for p in new_ports ] ) 


        # Remap Ports:
        def globalRemapPort(originalname,targetname):
            print 'Global-Remap [%s -> %s]'%(originalname,targetname)
            from nineml.abstraction_layer.visitors import InPlaceTransform
            transform = InPlaceTransform( originalname=originalname, targetname=targetname)
            for regime in newRegimeLookupMap.values():
                regime.AcceptVisitor(transform)

        
        # Handle port mappings:
        # portconnections = [ (NS -> NS),(NS -> NS ), (NS -> NS) ]
        portconnections = [model.get_fully_qualified_port_connections() for model in self.modelsubmodels] 
        portconnections = list( itertools.chain(* portconnections ) )

        # A. Handle Recieve Ports:
        for srcAddr,dstAddr in portconnections[:]:
            srcPort = new_ports[srcAddr.getstr()]
            dstPort = new_ports[dstAddr.getstr()]
            if dstPort.mode == 'recv':
                globalRemapPort( dstPort.name, srcPort.name )
                del new_ports[ dstAddr.getstr() ]
                portconnections.remove( (srcAddr,dstAddr) )


        # B. Handle Reduce Ports:
        # 1/ Make a map { reduce_port -> [send_port1, send_port2, send_port3], ...}
        from collections import defaultdict
        reduce_connections = defaultdict( list )
        for src,dst in portconnections:
            dstport = new_ports[dst.getstr() ]
            srcport = new_ports[src.getstr()]
            if dstport.mode == 'reduce':
                reduce_connections[dstport].append(srcport)

        # 2/ Subsitute each reduce port in turn:
        for dstport, srcportList in reduce_connections.iteritems():
            src_subs = [ s.name for s in srcportList ]
            terms = [dstport.name] + src_subs
            reduce_expr= dstport.reduce_op.join(terms) 
            globalRemapPort( dstport.name, reduce_expr )



        from nineml.utility import flattenFirstLevel
        from nineml.abstraction_layer import ComponentClass

        dynamics = al.Dynamics( regimes = newRegimeLookupMap.values(),
                                aliases = flattenFirstLevel( [ m.aliases for m in self.modelcomponents ] ),
                                state_variables = flattenFirstLevel( [ m.state_variables for m in self.modelcomponents ]  ),
                                )  

        self.reducedcomponent = al.models.ComponentNode( self.componentname, 
                                                         dynamics=dynamics, 
                                                         analog_ports=new_ports.values() , 
                                                         event_ports= flattenFirstLevel( [comp.event_ports for comp in self.modelcomponents] ), 
                                                         parameters=flattenFirstLevel( [ m.parameters for m in self.modelcomponents ] ) )

        
        









            









def reduce_to_single_component( model, componentname ):
    reducer = ModelToSingleComponentReducer(model,componentname)
    #print reducer.portmappings
    return reducer.reducedcomponent
