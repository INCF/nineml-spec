# A leaky_iaf which defines ports to play with an ampa synapse


# Leaky iaf
regimes = [
    nineml.Sequence(
    "dV/dt = (-gL*(V-vL) + Isyn)/C",
    transitions = [On("V>Vth","set_tspike")],
    events = [SpikeOutputPort("V>Vth")],
    name = "sub-threshold-regime"
    ),

    nineml.Union(
    "tspike = t",
    "V = Vreset",
    transitions = [On("true","refractory-regime")],
    name = "set_tspike"
    ),

    nineml.Union(
    transitions = [On("t >= tspike + trefractory","sub-threshold-regime")]
    name = "refractory-regime"
    )]

c1 = nineml.Component("LeakyIAF", regimes = regimes)

