#Author: Eilif Muller, Andrew Davison


import nineml.abstraction_layer as nineml
import nineml.abstraction_layer.names as names



#parameters = ["V", "t", "spike", "Isyn", "subthreshold", "refractory_end", "gL",
#              "vL", "theta", "Vreset", "C", "trefractory", "tspike"]

#EM: "subthreshold" superfluous and removed
#EM: added parameter Iext for static used defined current

# These are true "parameters"
parameters = ["Iext", "gL", "vL", "theta", "Vreset", "C", "trefractory"]

#EM: Others are not, and can be infered as left-hand-sides of ODEs and assignments:
#EM: V, spike, refractory_end, tspike, Isyn

#EM: i.e. after creation we can do: component.get_state_vars()
#EM: ['V', 'spike','refractory_end', 'tspike', 'Isyn']

#EM: t should be special and always refer to time, so we should have nineml.names.t
#EM: perhaps even for V, in_spike, out_spike, Isyn

#EM: Next we assign meaning to state vars:

#EM: Concept of ports:
# Event and Analog are subclasses of Port

#EM: define
#nineml.SpikeOutEvent = EventPort(names.out_spike,'output spike event',mode = 'w')
#nineml.SpikeInEvent = EventPort(names.in_spike,'input spike event',mode = 'r')

#EM: Voltage can be read for conduntance based synapses
#EM: or written for V jump synapses, etc.

#EM: Synapses accumulate in Isyn

# Namespace: Mapping of local names to ports
ports = {'Isyn':nineml.AnalogPort(names.Isyn, 'synaptic current', mode='rw'),
         'V':nineml.AnalogPort(names.V, 'membrane voltage', mode = 'rw')}


subthreshold_regime = nineml.Sequence(
    "dV/dt = (-gL*(V-vL) + Isyn)/C",
    "Isyn = 0.0",
    nineml.On("V > theta",do=nineml.Sequence("tspike = t", nineml.SpikeOutputEvent),
              to=nineml.Sequence(
                  "V = Vreset",
                  nineml.On("t >= tspike + trefractory", to="subthreshold_regime"),
                  name = "refractory_regime"
                  )),
    name="subthreshold_regime"
    )


component = nineml.Component("LeakyIAF", parameters, subthreshold_regime,
                             ports = ports)



component.write("leaky_iaf2.xml")
