

import random, os
import nineml.abstraction_layer as al


def get_iaf():
    iaf = al.ComponentClass( 
                            name = "iaf",
                            dynamics = al.Dynamics( 
                                regimes = [
                                    al.Regime(
                                        time_derivatives = ["dV/dt = ( gl*( vrest - V ) + ISyn)/(cm)"],
                                        transitions = [al.On("V > vthresh",
                                                                 do=["tspike = t",
                                                                     "V = vreset",
                                                                     al.OutputEvent('spikeoutput')],
                                                                 to="refractoryregime"),
                                                       ],
                                        name = "subthresholdregime"
                                        ),

                                    al.Regime(
                                        time_derivatives = ["dV/dt = 0"],
                                        transitions = [ al.On("t >= tspike + taurefrac",
                                                                  to="subthresholdregime") ],
                                        name = "refractoryregime"
                                        )
                                    ],
                                aliases = [],
                                state_variables = [
                                    al.StateVariable( 'V'),
                                    al.StateVariable( 'tspike'),
                                                
                                    ]
                            ),
                            analog_ports = [   al.SendPort("V"), 
                                               al.ReducePort("ISyn", reduce_op="+"),],
                                               
                            event_ports = [ al.SendEventPort('spikeoutput'),],
                             parameters = [ al.Parameter(p) for p in ['cm','taurefrac','gl','vreset','vrest','vthresh']  ]
                            )
    return iaf

def get_coba():
    coba = al.ComponentClass( 
                             name = "CobaSyn",
                             dynamics = 
                                al.Dynamics(
                                    aliases = ["I:=g*(vrev-V)", ],
                                    regimes = [
                                      al.Regime(
                                         name = "cobadefaultregime",
                                         time_derivatives = ["dg/dt = -g/tau",],
                                         transitions = [
                                             al.On('spikeinput', do=["g=g+q"]),
                                             ],
                                         )
                                        ],
                                    state_variables = [ al.StateVariable('g') ]
                                    ),
                             
                             analog_ports = [ al.RecvPort("V"), al.SendPort("I"), ],
                             event_ports = [al.RecvEventPort('spikeinput') ],
                             parameters = [ al.Parameter(p) for p in ['tau','q','vrev']  ]
                             )
    return coba



def nmda():
    inter_event_regime = al.Regime(
        name="intereventregime",
        time_derivatives = ["dA/dt = -A/taur", "dB/dt = -B/taud" ],
        transitions=[al.On('spikeinput',
                              do=["A = A + weight*factor",
                                  "B = B + weight*factor"])] 
        )

    dynamics = al.Dynamics(
                    aliases = [
                                "taupeak := taur*taud/(taud - taur)*log(taud/taur)",
                                "factor := 1/(exp(-taupeak/taud) - exp(-taupeak/taur))",
                                "gB := 1/(1 + mgconc*exp(-1*gamma*V)/beta)",
                                "g := gB*gmax*(B-A)",
                                "I := g * df",
                                "df := (E-V)",
                              ],
                    state_variables = [al.StateVariable(o) for o in ('A','B') ] , 
                    regimes = [inter_event_regime],
                          ) 

    nmda = al.ComponentClass(name="NMDAPSR",
                     dynamics = dynamics,
                     analog_ports = [al.RecvPort("V"), al.SendPort("I"), ],
                     event_ports =  [al.RecvEventPort('spikeinput') ],
                     parameters =   [ al.Parameter(p) for p in ['taur','taud','gmax','mgconc','gamma','beta','E','weight']  ]
                     )

    return nmda








def get_hierachical_iaf_2coba():

    
    # Create a model, composed of an iaf neuron, and 
    iaf_2coba_model = al.ComponentClass( name="iaf_2coba", subnodes = {"iaf" : get_iaf(), "cobaExcit" : get_coba(), "cobaInhib" : get_coba()} )
    
    # Connections have to be setup as strings, because we are deep-copying objects.
    iaf_2coba_model.connect_ports( "iaf.V", "cobaExcit.V" )
    iaf_2coba_model.connect_ports( "iaf.V", "cobaInhib.V" )
    iaf_2coba_model.connect_ports( "cobaExcit.I", "iaf.ISyn" )
    iaf_2coba_model.connect_ports( "cobaInhib.I", "iaf.ISyn" )
    
    return iaf_2coba_model

def get_hierachical_iaf_3coba():
    
    # Create a model, composed of an iaf neuron, and 
    iaf_3coba_model = al.ComponentClass( name="iaf_2coba", subnodes = {"iaf" : get_iaf(), "AMPA" : get_coba(), "GABAa" : get_coba(), "GABAb": get_coba()} )
    
    # Connections have to be setup as strings, because we are deep-copying objects.
    iaf_3coba_model.connect_ports( "iaf.V", "AMPA.V" )
    iaf_3coba_model.connect_ports( "iaf.V", "GABAa.V" )
    iaf_3coba_model.connect_ports( "iaf.V", "GABAb.V" )
    iaf_3coba_model.connect_ports( "AMPA.I", "iaf.ISyn" )
    iaf_3coba_model.connect_ports( "GABAa.I", "iaf.ISyn" )
    iaf_3coba_model.connect_ports( "GABAb.I", "iaf.ISyn" )
    
    return iaf_3coba_model




def get_hierachical_iaf_nmda():

    
    
    # Create a model, composed of an iaf neuron, and 
    iaf_nmda_model = al.ComponentClass( name="iaf_2coba", subnodes = {"iaf" : get_iaf(), "nmda" : nmda(), 'cobaExcit':get_coba()} )
    
    iaf_nmda_model.connect_ports( "iaf.V", "cobaExcit.V" )
    iaf_nmda_model.connect_ports( "iaf.V", "nmda.V" )
    iaf_nmda_model.connect_ports( "cobaExcit.I", "iaf.ISyn" )
    iaf_nmda_model.connect_ports( "nmda.I", "iaf.ISyn" )
    
    return iaf_nmda_model

    
    
    



















def get_hierachical_iaf_2coba_network(nNeurons = 2):
    assert False, "This is out of date. See Mike Hull before use"
    
    iaf = models.ComponentNode( "iaf",
            regimes = [
                al.Regime(
                    "dV/dt = ( (gl+gSynapticInput)*(v_rest - V) + i + i_offset)/(cm)",
                    transitions = [al.On("V > v_thresh", do=["t_spike = t", "V = v_reset", al.SpikeOutputEvent], to="refractoryregime"), ],
                    name = "subthresholdregime"
                    ),
    
                al.Regime(
                    "dV/dt = 0",
                    transitions = [ al.On("t >= t_spike + tau_refrac", to="subthresholdregime") ],
                    name = "refractoryregime"
                    )
                    ],
            ports = [   al.SendPort("V"), 
                        al.ReducePort("gSynapticInput", reduce_op="+"), 
                        al.RecvPort("q") ]
                )
        
    
    coba = models.ComponentNode( "CobaSyn",
            regimes = [
                al.Regime(
                    "dg/dt = -g/tau",
                    transitions = [
                           al.On(al.EventPort('spikeinput', mode="recv"), do="g=g+q"),
                           ],
                    name = "default-regime"
                    )
                    ],
    
                ports = [ al.RecvPort("V"), al.SendPort("g"), al.RecvPort("q") ]
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
