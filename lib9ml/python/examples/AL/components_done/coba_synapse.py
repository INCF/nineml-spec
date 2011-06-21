from nineml.abstraction_layer import *

# Simple quantal-increase exponential-decay conductance-based synapse
# This synapse model is linear.


class MetaData(object):
    is_neuron_model=False

def get_component():

    parameters = ['tau','E', 'q']

    regimes = [
        Regime(name="synaptic_decay",
            time_derivatives= ["dg/dt = -g/tau"],
            transition = On('spikeinput', do="g=g+q"),
            
            )]

    analog_ports = [RecvPort("V"), SendPort("Isyn")]

    coba_syn = ComponentClass("CoBaSynapse", 
                        regimes=regimes, 
                        aliases = [ "Isyn := g*(E-V)" ],
                        analog_ports=analog_ports,
                        parameters=parameters)

    return coba_syn

