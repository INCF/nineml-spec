#Author: Eilif Muller, Andrew Davison

import nineml.abstraction_layer as nineml
import nineml.abstraction_layer.names as names


# User parameters
parameters = ["tau_r", "tau_d", "mg_conc", "eta", "gamma", "gmax","E_rev"]

#Bindings
tau_peak = nineml.Binding("tau_peak", "tau_r*tau_d/(tau_d - tau_r)*log(tau_d/tau_r)")
factor = nineml.Binding("factor", "1/(exp(-tau_peak/tau_d) - exp(-tau_peak/tau_r))")
bindings = [tau_peak, factor] 


# State Vars inferred as left-hand-sides which are not ports.
# It is illegal to assign to a parameter, they are read-only.
# NB: Test suite should check that implementation throws exception when building component
# that tries to assign to parameters or read-only ports

#internal_variables = ["gB"]
#state_variables = ["g", "A", "B"] + ["gB"]

#variables_from_elsewhere = ["V", "weight", "inputEvent"]
ports = {'weight':nineml.AnalogPort(names.SynapticWeight, 'synaptic weight', mode='r'),
         'V':nineml.AnalogPort(names.V, 'membrane voltage', mode = 'r'),
         'Isyn':nineml.AnalogPort(names.Isyn, 'synaptic current', mode='w')}

#implicit port: 
#nineml.SpikeInputPort

inter_event_regime = nineml.Sequence(
    nineml.On(nineml.SpikeInputEvent,
              do=nineml.Sequence(
                  "A+= weight*factor", #AddInplace
                  "B+= weight*factor"), #AddInplace
x              ),
    nineml.Union(
        "dA/dt = -A/tau_r", #ODE
        "dB/dt = -B/tau_d", #ODE
        "gB = 1/(1 + mg_conc*eta*exp(-1*gamma*V))" #Assignment
        ),
    "g = gB*gmax*(B-A)", #Assignment
    "Isyn+=g*(E_rev-V)", #AddInplace
    name="inter_event_regime"
    )


component = Component("NMDA_PSR", parameters,
                      regime = inter_event_regime,
                      ports = ports,
                      bindings = bindings)

component.write("nmda.xml")
