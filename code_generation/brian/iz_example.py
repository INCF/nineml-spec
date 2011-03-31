import nineml.abstraction_layer as nineml
import nineml_stateupdater as brian9ml
from brian.stateupdater import NonlinearStateUpdater
import brian
import brian.stdunits as units

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


ports = [nineml.Port("V"),
         nineml.ReducePort("Isyn",op="+")]


c1 = nineml.Component("LeakyIAF", regimes = regimes, ports = ports)




# this is a hack until 9ml defines units,
# which defines the unit of time in Brian ODE
__time_factor__ = 1.*units.ms

model = brian9ml.NineMLStateUpdater(c1,solver=NonlinearStateUpdater,
                                    base_regime=regimes[0])

# Initial conditions
init = {'V': 0.0, 'U':0.0, 'Isyn':1.0}
t_init = [init.get(x,0.0) for x in model.state_vars]


# Threshold is 0.5 to detect a spike event on the spikeout var,
# which is 0.0 if no spike, 1.0 if spike on that clock tick.
# reset resets spikeout var to 0.0

P = NeuronGroup(10, model, init=t_init, threshold=0.5, reset=0.0)
