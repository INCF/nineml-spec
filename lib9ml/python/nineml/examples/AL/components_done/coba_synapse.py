import nineml.abstraction_layer as al

def get_component():
    coba = al.ComponentClass( 
            name = "CobaSyn",
            aliases = ["I:=g*(vrev-V)", ],
            regimes = [ 
                al.Regime(
                 name = "cobadefaultregime",
                 time_derivatives = ["dg/dt = -g/tau",],
                 transitions = al.On('spikeinput', do=["g=g+q"]),
                    )
                ],
            state_variables = [ al.StateVariable('g') ],
            analog_ports = [ al.RecvPort("V"), al.SendPort("I"), ],
            parameters = [ 'tau','q','vrev']  
                             )
    return coba
