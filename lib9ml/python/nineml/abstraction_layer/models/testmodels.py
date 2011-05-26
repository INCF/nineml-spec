






def get_iaf_1coba():
    #def buildTestComponent():
    import random, os
    import nineml.abstraction_layer as nineml
    
    #from nineml.abstraction_layer import NewComponent, NewModel
    import nineml.abstraction_layer.models as models
    iaf = models.Component( "iaf",
            regimes = [
                nineml.Regime(
                    "dV/dt = ( (gl+gSynapticInput)*(v_rest - V) + i + i_offset)/(cm)",
                    transitions = [nineml.On("V > v_thresh", do=["t_spike = t", "V = v_reset", nineml.SpikeOutputEvent], to="refractoryregime"), ],
                    name = "subthresholdregime"
                    ),
    
                nineml.Regime(
                    "dV/dt = 0",
                    transitions = [ nineml.On("t >= t_spike + tau_refrac", to="subthresholdregime") ],
                    name = "refractoryregime"
                    )
                    ],
            ports = [   nineml.SendPort("V"), 
                        nineml.ReducePort("gSynapticInput", op="+"), 
                        nineml.RecvPort("q") ]
                )
        
    
    coba = models.Component( "CobaSyn",
            regimes = [
                nineml.Regime(
                    "dg/dt = -g/tau",
                    transitions = [
                           nineml.On(nineml.EventPort('spikeinput', mode="recv"), do="g=g+q"),
                           ],
                    name = "default-regime"
                    )
                    ],
    
                ports = [ nineml.RecvPort("V"), nineml.SendPort("g"), nineml.RecvPort("q") ]
                )
    
    
    # Create a model, composed of an iaf neuron, and 
    iaf_1coba_model = models.Model( name="iaf_1coba", subnodes = {"iaf" : iaf, "cobaExcit" : coba} )
    iaf_1coba_model.connect_ports( "iaf.V", "cobaExcit.V" )
    
    
    return iaf_1coba_model





def get_iaf_2coba():
    #def buildTestComponent():
    import random, os
    import nineml.abstraction_layer as nineml
    
    #from nineml.abstraction_layer import NewComponent, NewModel
    import nineml.abstraction_layer.models as models
    iaf = models.Component( "iaf",
            regimes = [
                nineml.Regime(
                    "dV/dt = ( gl*( vrest - V ) + ISyn)/(cm)",
                    transitions = [nineml.On("V > vthresh", do=["tspike = t", "V = vreset", nineml.SpikeOutputEvent], to="refractoryregime"), ],
                    name = "subthresholdregime"
                    ),
    
                nineml.Regime(
                    "dV/dt = 0",
                    transitions = [ nineml.On("t >= tspike + taurefrac", to="subthresholdregime") ],
                    name = "refractoryregime"
                    )
                    ],
            analog_ports = [   nineml.SendPort("V"), 
                        nineml.ReducePort("ISyn", op="+"),
                     ]
                )
        
    
    coba = models.Component( "CobaSyn",
            regimes = [
                nineml.Regime(
                    "dg/dt = -g/tau",
                    transitions = [
                           nineml.On(nineml.EventPort('spikeinput', mode="recv"), do="g=g+q"),
                           ],
                    name = "defaultregime"
                    )
                    ],
    
                analog_ports = [ nineml.RecvPort("V"), 
                          nineml.SendPort("I=g*(vrev-V)"), 
                          nineml.RecvPort("q") ]
                )
    
    
    
    
    # Create a model, composed of an iaf neuron, and 
    iaf_2coba_model = models.Model( name="iaf_2coba", subnodes = {"iaf" : iaf, "cobaExcit" : coba, "cobaInhib" : coba} )
    
    # Connections have to be setup as strings, because we are deep-copying objects.
    iaf_2coba_model.connect_ports( "iaf.V", "cobaExcit.V" )
    iaf_2coba_model.connect_ports( "iaf.V", "cobaInhib.V" )
    iaf_2coba_model.connect_ports( "cobaExcit.I", "iaf.ISyn" )
    iaf_2coba_model.connect_ports( "cobaInhib.I", "iaf.ISyn" )
    
    
    #print type(coba)
    #print "PORTS:"
    #for port in coba.ports:
    #    print port
    #print "----------\n\n"
    
    #for regime in coba.regimes:
    #    print regime 
    #    for transition in regime.transitions:
    #        print "Trans:", transition
    #        print "  ", list( transition.event_ports )
            
    #print "OK"
    
    #
    #import sys
    #sys.exit(0)
    
    return iaf_2coba_model

    
    
    
    
    
def get_iaf_2coba_network(nNeurons = 2):
    """
    A composite leaky integrate-and-fire with conductance-based, exponential
    synapses, like the IF_cond_exp standard cell model in PyNN
    
    """
    import random, os
    import nineml.abstraction_layer as nineml
    
    #from nineml.abstraction_layer import NewComponent, NewModel
    import nineml.abstraction_layer.models as models
    iaf = models.Component( "iaf",
            regimes = [
                nineml.Regime(
                    "dV/dt = ( (gl+gSynapticInput)*(v_rest - V) + i + i_offset)/(cm)",
                    transitions = [nineml.On("V > v_thresh", do=["t_spike = t", "V = v_reset", nineml.SpikeOutputEvent], to="refractoryregime"), ],
                    name = "subthresholdregime"
                    ),
    
                nineml.Regime(
                    "dV/dt = 0",
                    transitions = [ nineml.On("t >= t_spike + tau_refrac", to="subthresholdregime") ],
                    name = "refractoryregime"
                    )
                    ],
            ports = [   nineml.SendPort("V"), 
                        nineml.ReducePort("gSynapticInput", op="+"), 
                        nineml.RecvPort("q") ]
                )
        
    
    coba = models.Component( "CobaSyn",
            regimes = [
                nineml.Regime(
                    "dg/dt = -g/tau",
                    transitions = [
                           nineml.On(nineml.EventPort('spikeinput', mode="recv"), do="g=g+q"),
                           ],
                    name = "default-regime"
                    )
                    ],
    
                ports = [ nineml.RecvPort("V"), nineml.SendPort("g"), nineml.RecvPort("q") ]
                )
    
    
    
    
    # Create a model, composed of an iaf neuron, and 
    iaf_2coba_model = models.Model( name="iaf_2coba", subnodes = {"iaf" : iaf, "coba1" : coba, "coba2" : coba} )
    
    # Connections have to be setup as strings, because we are deep-copying objects.
    iaf_2coba_model.connect_ports( "iaf.V", "coba1.V" )
    iaf_2coba_model.connect_ports( "iaf.V", "coba2.V" )
    
    
    
    networkModel = models.Model(name="NetworkModel")
    
    namespaces = dict( [ (i, "GID%d"%i) for i in range(nNeurons) ] ) 
    
    
    # Create the submodels:
    for i in range(nNeurons):
        networkModel.insert_subnode( subnode = iaf_2coba_model, namespace = namespaces[i] )
    
    #Randomly connect them together:
    for i in range(nNeurons):
        sink1 = random.randint(0,nNeurons-1)
        sink2 = random.randint(0,nNeurons-1)
        networkModel.connect_ports( src = namespaces[i]+".coba1.g", sink = namespaces[sink1] + ".iaf.gSynapticInput")
        networkModel.connect_ports( src = namespaces[i]+".coba1.g", sink = namespaces[sink2] + ".iaf.gSynapticInput")

    return networkModel
