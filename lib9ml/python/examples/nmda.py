from nineml.abstraction_layer import *
 
# Bindings
tau_peak = Binding("tau_peak", "tau_r*tau_d/(tau_d - tau_r)*log(tau_d/tau_r)")
factor = Binding("factor", "1/(exp(-tau_peak/tau_d) - exp(-tau_peak/tau_r))")

bindings = [tau_peak,factor]

# regime 1

a_eqn = ODE("A", "t", "-A/tau_r")
b_eqn = ODE("B", "t", "-B/tau_d")
voltage_block = Assignment("gB", "1/(1 + mg_conc*eta*exp(-1*gamma*V))")

stage1 = Union(a_eqn, b_eqn, voltage_block)

g_eqn = Assignment("g", "gB*gmax*(B-A)")

inter_event_regime = Sequence(stage1, g_eqn, name="inter_event_regime")

# regime 2

a_update = Assignment("A", "A + weight*factor")
b_update = Assignment("B", "B + weight*factor")

on_event_regime = Union(a_update, b_update, name="on_event_regime")

# transitions

transition1 = Transition(inter_event_regime, on_event_regime, condition="SpikeInEvent")

transition2 = Transition(on_event_regime, inter_event_regime, condition=None)

 
# Parameters

external_parameters =  ["tau_r", "tau_d", "mg_conc", "eta", "gamma", "gmax"]
variables_from_elsewhere = ["V", "weight", "inputEvent"]
state_variables = ["g", "A", "B"]
internal_variables = ["tau_peak", "factor", "gB"]

parameters = external_parameters + variables_from_elsewhere + state_variables + internal_variables


c1 = Component("NMDA_PSR", parameters, transitions=(transition1, transition2), bindings = (tau_peak, factor))


# write to file object f if defined
try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:
    import os

    base = "nmda"
    c1.write(base+".xml")
    c2 = parse(base+".xml")
    assert c1==c2

    c1.to_dot(base+".dot")
    os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))
