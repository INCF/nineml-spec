
#from nineml.utility import invert_dictionary, flatten_first_level, expect_single
from nineml.utility import  flatten_first_level, expect_single

# System Imports:
#import copy
import itertools


from nineml.abstraction_layer.visitors import ClonerVisitor
from nineml.abstraction_layer.visitors import ClonerVisitorPrefixNamespace
import nineml





class TransitionResolver(object):
    def __init__(self, oldtransition, regime_tuple, transition_regime_tuple_index, flattener ):
        print 'TRANSITION RESOLVER:', oldtransition

        self.src_regime_tuple = tuple( regime_tuple )
        self.dst_regime_tuple = tuple( regime_tuple )

        # Clone the old Node
        oldtransition = oldtransition.accept_visitor( ClonerVisitor(), prefix='', prefix_excludes=[] )

        portconnections = [model.portconnections for model in flattener.all_components]
        self.portconnections = list( itertools.chain(* portconnections ) )
        self.flattener = flattener

        self.events_emitted = []
        self.unresolved_transitions = [ (oldtransition,transition_regime_tuple_index) ]
        self.changed_regime_indices = set()

        self.state_assignments = [] 
        self.event_outputs = [] 


        self.send_rev_map = {}
        for src,dst in self.portconnections:
            if not src.get_local_name() in self.send_rev_map:
                self.send_rev_map[src.get_local_name()] = []
            self.send_rev_map[src.get_local_name()].append(dst.get_local_name())


        self.resolve_transitions() 

        self.targetRegime = flattener.old_regime_tuple_to_new_regime_map[ tuple(self.dst_regime_tuple)  ]




    def resolve_transitions(self):
        while self.unresolved_transitions:
            transition, regime_index = self.unresolved_transitions.pop()
            
            if regime_index in self.changed_regime_indices:
                raise NineMLRuntimeError('Something has gone wrong with event resolution. Changing Regime Twice!')
            self.changed_regime_indices.add(regime_index)

            self.resolve_transition(transition, regime_index)

    
    def resolve_transition(self, transition, transition_regime_tuple_index):
        print ' - Resolving Transition', transition

        # Update the target regime:
        self.dst_regime_tuple = self.flattener.getNewRegimeTupleFromTransition( 
                                    currentRegimeTuple = self.dst_regime_tuple, 
                                    regimeIndex=transition_regime_tuple_index, 
                                    oldtransition=transition)


        #self.state_assignments.extend( transition.state_assignments )
        #self.event_outputs.extend( transition.event_outputs )
        
        for eo in transition.event_outputs:
            self.event_outputs.append( ClonerVisitor().visit(eo) )
        for sa in transition.state_assignments:
            self.state_assignments.append( ClonerVisitor().visit(sa) )




        #Are any of the output events connected to other event_ports?
        #print self.send_rev_map.keys()
        for ev_out in transition.event_outputs:
            print ' -- Checking for port connections from:', ev_out
            if not ev_out.port_name in self.send_rev_map:
                continue

            for recv_port in self.send_rev_map[ev_out.port_name]:
                print '    -> Connection to ', recv_port

                # So this port is connected, but are we in a regime that triggers from
                # this port??
                
                for cascade_regime_index, regime in enumerate(self.dst_regime_tuple):
                    for ev in regime.on_events:
                        if ev.src_port_name == recv_port:
                            print '     (**Cascade found**)'
                            self.unresolved_transitions.append( (ev,cascade_regime_index ))








class ComponentFlattener(object):
    

    # Utility Functions: 
    # ------------------ #
    @classmethod
    def flatten_namespace(cls, ns):
        return ns.replace('.', '_')
    
    @classmethod
    def flatten_namespace_dict(cls, ns):
        newParams = {}
        for k, v in ns.iteritems():
            newParams[ cls.flatten_namespace(k) ] = v
        return newParams
     


    # Useful function possibly called later #
    # ------------------------------------- #
    def get_new_regime(self, old_regime_string ):
        """ 
        for example:
        old_regime_string = iaf:subthresholdregime cobaInhib:cobadefaultregime cobaExcit:cobadefaultregime'
        """

        # Lets create a dictionary that maps 'NamespaceAddress' to regime name
        # from the input string:
        #  old_regime_string = 'iaf:subthresholdregime cobaInhib:cobadefaultregime cobaExcit:cobadefaultregime'
        nsstr_regimename = [ l.split(':') for l in old_regime_string.split()  ]
        ns_regimename = dict([ (nineml.al.NamespaceAddress(ns), regime_name) for (ns, regime_name) in nsstr_regimename] )

        # OK, now lets go through our old componentswithregimes,
        # and find the regime that was specified. 
        target_regime_tuple = []
        for c in self.componentswithregimes:
            comp_ns = c.get_node_addr()
            if not comp_ns in ns_regimename:
                err =  'Looking for a regime in namespace: %s, but not found.' % str(comp_ns)
                err += '\nNamespaces: %s' % ','.join([str(ns) for ns in  ns_regimename.keys()])
                err += '\nSpecified String: %s'% old_regime_string
                raise nineml.exceptions.NineMLRuntimeError(err)
            target_regime_name = ns_regimename[comp_ns]
            
            regime_map = dict( [ (r.name, r) for r in c.regimes] )
            if not target_regime_name in regime_map:
                err =  'Namespace has no regime named: %s'
                err += '\nRegimes: %s' % (str(regime_map.keys()))
                raise nineml.exceptions.NineMLRuntimeError(err)

            target_regime_tuple.append( regime_map[target_regime_name] )

        
        target_regime_tuple = tuple(target_regime_tuple)
        
        new_regime =  self.old_regime_tuple_to_new_regime_map[target_regime_tuple]
        return new_regime
    
    
    
    # Flattening Functions:
    # --------------------- #


    def __init__(self, component, componentname=None):
        assert isinstance( component, nineml.al.ComponentClass)

        # Is our component already flat??
        if component.is_flat():
            self.reducedcomponent = ClonerVisitor().visit( component )
            if component.was_flattened():
                self.reducedcomponent.set_flattener( component.flattener )
            return

        # New components name
        self.componentname = componentname if componentname else component.name


        # Make a clone of the component; in which all hierachical components
        # have their internal symbols prefixed:
        cloned_comp = ClonerVisitorPrefixNamespace().visit(component)


        # Make a list of all components, and those components with regimes: 
        self.all_components = list( cloned_comp.query.recurse_all_components )
        self.componentswithregimes = [ m for m in self.all_components if list(m.regimes) ] 
        

        # This will get filled in build_new_regime_space():
        # (It maps { (Regime,Regime,...,Regime) : Regime, (Regime,Regime,...,Regime) : Regime,}
        # Where the key tuple represents the regimes in the hierachical component,
        # corresponding to self.componentswithregimes.
        # And the values are the regimes in the new component.
        self.old_regime_tuple_to_new_regime_map = None
        


        # OK, Heavy-lifting Code:
        # ===================== #

        self.build_new_regime_space()
        
        #Build Our New Component
        self.reducedcomponent = nineml.al.ComponentClass(
                 name=self.componentname, 
                 aliases = flatten_first_level( [ m.aliases for m in self.all_components ] ),
                 state_variables = flatten_first_level( [ m.state_variables for m in self.all_components ]  ),
                 regimes = self.old_regime_tuple_to_new_regime_map.values(),
                 analog_ports=flatten_first_level( [comp.analog_ports for comp in self.all_components] ), 
                 event_ports= flatten_first_level( [comp.event_ports for comp in self.all_components] ), 
                 parameters=  flatten_first_level( [ m.parameters for m in self.all_components ] ) )

        self.remap_analog_ports()
        

        # Attach this flattening information to the component:
        self.reducedcomponent.set_flattener( self )










    @classmethod
    def create_compound_regime( cls, regimetuple ):
        
        # Copy accross all the odes from each regime. 
        # [Don't worry about transitions yet, we deal with them later]

        # We need to clone the time_derivatives:
        time_derivatives = flatten_first_level( [ r.time_derivatives for r in regimetuple ] )
        time_derivatives = [ ClonerVisitor().visit(td) for td in time_derivatives ]

        return nineml.al.Regime( 
                name=None, 
                time_derivatives = time_derivatives, 
                transitions=[],  )


        



#    def create_new_transition( self, oldtransition, regimetuple, regimeIndex, event_port_map ):
#
#        tr = TransitionResolver( 
#                oldtransition = oldtransition, 
#                regime_tuple=regimetuple, 
#                transition_regime_tuple_index=regimeIndex, 
#                flattener=self,
#                )
#
#        return tr.state_assignments, tr.event_outputs, tr.targetRegime.name




        ## This new transition will have the same state_assignments, but we need
        ## to check the output_events, and see if they are locally connected.
        ## If they are; then we need to work out what will happen from here.

        #newRegimeTuple = self.getNewRegimeTupleFromTransition( currentRegimeTuple = regimetuple, regimeIndex=regimeIndex, oldtransition=oldtransition)
        #state_assignments = list( oldtransition.state_assignments )
        #event_outputs = []

        #portconnections = [model.portconnections for model in self.all_components]
        #portconnections = list( itertools.chain(* portconnections ) )
        #



        #send_rev_map = {}
        #for src,dst in portconnections:
        #    if not src in send_rev_map:
        #        send_rev_map[src.get_local_name()] = []
        #    send_rev_map[src.get_local_name()].append(dst.get_local_name())

        #
        #regime_indices_changed = set()

        #for ev in oldtransition.event_outputs:
        #    portname = ev.port_name
        #    #print type(portname), portname
        #    if portname in send_rev_map:
        #        #print 'Port is a send port:', portname, '->', send_rev_map[portname]
        #        
        #        # OK, since this event port is connected, perhaps we have a
        #        # cascade of events. Find all the knock-on transitions:

        #        # Ensure we don't have event loops:
        #        events_resolved = set()
        #        events_to_resolve =  list( send_rev_map[portname]  )

        #        while events_to_resolve:
        #            target_port = events_to_resolve.pop(0)
        #            assert isinstance(target_port, basestring)
        #            #print 'target_port:', target_port
        #            if target_port in events_resolved:
        #                raise NineMLRuntimeError('Event Emission Cycle!')
        #            events_resolved.add(target_port)
        #            event_outputs.append( nineml.abstraction_layer.OutputEvent(target_port) )

        #            #print ' - Resolving', target_port
        #            # Find the transitions triggered by this event. The
        #            # transitions have to start from transitions in the
        #            # regime tuple:
        #            for old_regime_index, old_regime in enumerate(regimetuple):
        #                transitions_on_ev = [ on_ev for on_ev in old_regime.on_events if on_ev.src_port_name == target_port ] 
        #                #print '   - Checking old regime for transitions triggered by this event', old_regime, transitions_on_ev
        #                if transitions_on_ev == []:
        #                    continue
        #                trans_on_ev = nineml.utility.filter_expect_single(transitions_on_ev)

        #                # We have found a cascaded event.
        #                # 1. Copy new state assignments across
        #                state_assignments.extend( trans_on_ev.state_assignments )

        #                # 2. Update the target regime_tuple.
        #                newRegimeTuple = self.getNewRegimeTupleFromTransition( currentRegimeTuple = newRegimeTuple, regimeIndex=old_regime_index, oldtransition=trans_on_ev )
        #                if old_regime_index in regime_indices_changed:
        #                    raise NineMLRuntimeError('Updating the same regime index twice. Something has gone wrong')
        #                regime_indices_changed.add(old_regime_index)

        #                # 3. Add any knock-on events.
        #                events_to_resolve.extend( [ eo.port_name for eo in trans_on_ev.event_outputs ])


        #    else:
        #        print 'Unconnected Port -%s-'% portname

        #    


        ##handled_events = []
        ##unhandled_events = []

        ##state_assignments = oldtransition.state_assignments
        ##output_events = oldtransition.event_outputs
        ##unhandled_events.extend( flatten_first_level(
        ##    [self.distribute_event( ev, event_port_map) for ev in output_events ]) ) 

        ##newRegimeTuple = self.getNewRegimeTupleFromTransition( currentRegimeTuple = regimetuple, regimeIndex=regimeIndex, oldtransition=oldtransition)
        ##
        ##while unhandled_events:
        ##    ev = unhandled_events.pop()
        ##    new_state_assignments, new_event_outputs, newRegimeTuple = self.getRegimeTupleResponseToEvent(newRegimeTuple, ev ) 
        ##    
        ##    # Check for event recursion:
        ##    for new_ev in new_event_outputs: assert not new_ev in handled_events and new_ev != ev
        ##    
        ##    state_assignments.extend( new_state_assignments )
        ##    output_events.extend( new_event_outputs )
        ##    unhandled_events.extend( flatten_first_level(
        ##        self.distribute_event( new_event_outputs, event_port_map ) ) )
        ##    handled_events.append(ev)

        ##
        #
        #targetRegime = self.old_regime_tuple_to_new_regime_map[ tuple(newRegimeTuple)  ]

        #return state_assignments, event_outputs, targetRegime.name


    #def distribute_event(self, event_output, event_port_map):
    #    #print 'Distributing Event', event_output, event_output.port_name
    #    events = set()
    #    for p1, p2 in event_port_map:
    #        if p1 == event_output.port_name:
    #            events.append( p2 )
    #            events = events + self.distribute_event(p2)
    #    return events



    #def getRegimeTupleResponseToEvent( self, regimeTuple, eventName ):
    #    " Do not recurse, but iterate once over each regime in the tuple"
    #    import nineml.al.visitors as visitors

    #    state_assignments = []
    #    event_outputs = []
    #    newRegimeTuple = list( regimeTuple )

    #    for index, regime in enumerate(regimeTuple):
    #        on_events = [ oe for oe in regime.on_events if oe.src_port_name == eventName ] 
    #        assert len(on_events) in [0, 1]
    #        if not on_events: continue

    #        on_event = on_events[0]
    #        state_assignments.extend( [sa.accept_visitor( visitors.ClonerVisitor() ) for sa in on_event.state_assignments ]) 
    #        event_outputs.extend( [eo.accept_visitor( visitors.ClonerVisitor() ) for eo in one_event.event_outputs ]) 
    #        
    #        #Update dstRegime
    #        dstRegimeName = oldtransition.to.get_ref() if oldtransition.to else regime
    #        dstRegime = self.componentswithregimes[regimeIndex].query.regime(name=dstRegimeName.name) 
    #        newRegimeTuple[index] = dstRegime

    #    print 'EventOutputs', event_outputs
    #    return state_assignments, event_outputs, tuple( newRegimeTuple )
    

    def getNewRegimeTupleFromTransition( self, currentRegimeTuple, regimeIndex, oldtransition ):
        srcRegime = list( currentRegimeTuple )
        dstRegimeTuple = srcRegime[:]

        # Points to another node:
        name = oldtransition.target_regime_name
        dstRegimeOld = self.componentswithregimes[regimeIndex].query.regime(name=name) 
        dstRegimeTuple[regimeIndex] = dstRegimeOld

        #print 'New Regime Transition:', currentRegimeTuple, '->', tuple( dstRegimeTuple )
        return tuple(dstRegimeTuple)



    def build_new_regime_space(self):


        # Build the new Regime Space, by taking the cross-product of
        # the old regimes. We create a map linking the old regimes tuples
        # to the new Regime. 
        # At this point, there are no transitions:
        self.old_regime_tuple_to_new_regime_map = {}
        regimes = [ comp.regimes for comp in self.componentswithregimes]
        for i, regimetuple in enumerate( itertools.product(*regimes) ):
            newRegime = ComponentFlattener.create_compound_regime( regimetuple ) 
            self.old_regime_tuple_to_new_regime_map[regimetuple] = newRegime

        

        # Check for event-emission cycles:
        # TODO
        #event_port_map = flatten_first_level( [comp.query.get_fully_qualified_port_connections() for comp in self.all_components] )
        #event_port_map = [ (p1.getstr(), p2.getstr() ) for (p1, p2) in event_port_map ] 

        event_port_map = flatten_first_level( 
                [comp.event_ports for comp in self.all_components]) 
        event_port_map = dict( [ (p.name, p) for p in event_port_map ] ) 

        # Create New Events for the Regime-Map
        for regimetuple, regime_new in self.old_regime_tuple_to_new_regime_map.iteritems():
            for regimeIndex, regime in enumerate( regimetuple ):
                
                for oldtransition in regime.on_conditions:

                    tr = TransitionResolver( 
                            oldtransition = oldtransition, 
                            regime_tuple=regimetuple, 
                            transition_regime_tuple_index=regimeIndex, 
                            flattener=self,
                            )
                    newOnCondition = nineml.al.OnCondition(oldtransition.trigger, 
                            state_assignments=tr.state_assignments, 
                            event_outputs = tr.event_outputs, 
                            target_regime_name = tr.targetRegime.name)

                    regime_new.add_on_condition( newOnCondition)
                    

                for oldtransition in regime.on_events:
                    tr = TransitionResolver( 
                            oldtransition = oldtransition, 
                            regime_tuple=regimetuple, 
                            transition_regime_tuple_index=regimeIndex, 
                            flattener=self,
                            )
                    newOnEvent = nineml.al.OnEvent(oldtransition.src_port_name, 
                                                       state_assignments=tr.state_assignments, 
                                                       event_outputs = tr.event_outputs, 
                                                       target_regime_name = tr.targetRegime.name)
                    regime_new.add_on_event( newOnEvent)
                    
                





    def remap_analog_ports(self):
        from nineml.abstraction_layer.visitors import ExpandPortDefinition


        new_analog_ports = flatten_first_level( 
                [comp.analog_ports for comp in self.all_components]) 
        new_analog_ports = dict( [ (p.name, p) for p in new_analog_ports ] ) 


        
        # Handle port mappings:
        # portconnections = [ (NS -> NS), (NS -> NS ), (NS -> NS) ]
        portconnections = [model.portconnections for model in self.all_components]
        portconnections = list( itertools.chain(* portconnections ) )
        

        # ONLY ANALOG PORTS
        print 'TESTING CODE NEEDS TO REMOVED!'
        portconnections = [pc for pc in portconnections if pc[0].get_local_name() in new_analog_ports] 




        # A. Handle Receive Ports:
        for srcAddr, dstAddr in portconnections[:]:
            

            srcPort = new_analog_ports[srcAddr.get_local_name() ]
            dstPort = new_analog_ports[dstAddr.get_local_name() ]
            if dstPort.mode == 'recv':

                ExpandPortDefinition( originalname=dstPort.name, targetname=srcPort.name).visit(self.reducedcomponent)
                
                del new_analog_ports[ dstAddr.get_local_name() ]
                self.reducedcomponent._analog_ports.remove( expect_single([p for
                    p in self.reducedcomponent.analog_ports if p.name ==
                    dstAddr.get_local_name() ]) )

                portconnections.remove( (srcAddr, dstAddr) )

        # B. Handle Reduce Ports:
        # 1/ Make a map { reduce_port -> [send_port1, send_port2, send_port3], ...}
        from collections import defaultdict
        reduce_connections = defaultdict( list )
        for src, dst in portconnections:


            dstport = new_analog_ports[dst.get_local_name() ]
            srcport = new_analog_ports[src.get_local_name() ]
            if dstport.mode == 'reduce':
                reduce_connections[dstport].append(srcport)

        # 2/ Subsitute each reduce port in turn:
        for dstport, srcportList in reduce_connections.iteritems():
            src_subs = [ s.name for s in srcportList ]
            terms = [dstport.name] + src_subs
            reduce_expr = dstport.reduce_op.join(terms) 

            #globalRemapPort( dstport.name, reduce_expr )
            ExpandPortDefinition( originalname=dstport.name, targetname=reduce_expr).visit(self.reducedcomponent)


def flatten( model, componentname=None ):
    reducer = ComponentFlattener(model, componentname)
    return reducer.reducedcomponent




