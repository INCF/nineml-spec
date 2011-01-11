from nineml.abstraction_layer import *

# An nmda synapse

# providing parameters allows a sanity check on component construction
parameters = ['E', 'eta', 'gamma', 'gmax', 'mg_conc', 'tau_d', 'tau_r', 'weight']

inter_event_regime = Regime(
    "tau_peak := tau_r*tau_d/(tau_d - tau_r)*log(tau_d/tau_r)",
    "factor := 1/(exp(-tau_peak/tau_d) - exp(-tau_peak/tau_r))",
    "gB(V) := 1/(1 + mg_conc*eta*exp(-1*gamma*V))",
    "g(V,A,B) := gB(V)*gmax*(B-A)",
    "dA/dt = -A/tau_r",
    "dB/dt = -B/tau_d",
    name="inter_event_regime",
    transitions=On(SpikeInputEvent,
              do=["A = A + weight*factor",
                  "B = B + weight*factor"])
    )

ports = [RecvPort("weight"),
         RecvPort("V"),
         SendPort("Isyn = g(V,A,B)*(E - V)"), # this notation takes the assignment of Isyn out of the Regime
         SendPort("gsyn = g(V,A,B)")]

c1 = Component("NMDA_PSR", regimes=[inter_event_regime], ports = ports, parameters = parameters)

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
