# A leaky_iaf which defines ports to play with an ampa synapse
import nineml.abstraction_layer as nineml

# Leaky iaf
regimes = [
    nineml.Sequence(
    "dV/dt = (-gL*(V-vL) + Isyn)/C",
    transitions = [nineml.On("V>Vth","set_tspike")],
    events = [nineml.SpikeOutputPort("V>Vth")],
    name = "sub-threshold-regime"
    ),

    nineml.Union(
    "tspike = t",
    "V = Vreset",
    transitions = [nineml.On("true","refractory-regime")],
    name = "set_tspike"
    ),

    nineml.Union(
    transitions = [nineml.On("t >= tspike + trefractory","sub-threshold-regime")],
    name = "refractory-regime"
    )]

c1 = nineml.Component("LeakyIAF", regimes = regimes)

