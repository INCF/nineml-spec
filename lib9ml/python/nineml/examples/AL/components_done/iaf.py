import nineml.abstraction_layer as al

def get_component():
    iaf = al.ComponentClass( 
                name = "iaf",
                regimes = [
                        al.Regime(
                            name = "subthresholdregime",
                            time_derivatives = ["dV/dt = ( gl*( vrest - V ) + ISyn)/(cm)"],
                            transitions = al.On("V > vthresh",
                                                     do=["tspike = t",
                                                         "V = vreset",
                                                         al.OutputEvent('spikeoutput')],
                                                     to="refractoryregime"),
                            ),

                        al.Regime(
                            name = "refractoryregime",
                            time_derivatives = ["dV/dt = 0"],
                            transitions = [ al.On("t >= tspike + taurefrac",
                                                      to="subthresholdregime") ],
                            )
                        ],
                    state_variables = [
                        al.StateVariable( 'V'),
                        al.StateVariable( 'tspike'),
                        ],
                analog_ports = [   al.SendPort("V"), 
                                   al.ReducePort("ISyn", reduce_op="+"),],
                                   
                event_ports = [ al.SendEventPort('spikeoutput'),],
                 parameters = [ 'cm','taurefrac','gl','vreset','vrest','vthresh' ]
                )
    return iaf
