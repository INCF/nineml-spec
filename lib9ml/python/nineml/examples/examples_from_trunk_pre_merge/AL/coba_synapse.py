from nineml.abstraction_layer import *

# Simple quantal-increase exponential-decay conductance-based synapse
# This synapse model is linear.

parameters = ['tau','E', 'q']

regimes = [
    Regime(
        "dg/dt = -g/tau",
        transitions = On(SpikeInputEvent, do="g+=q"),
        name="synaptic_decay"
        )]

ports = [RecvPort("V"),
         SendPort("Isyn = g*(E-V)")]

coba_syn = Component("CoBaSynapse", regimes=regimes, ports=ports,
                     parameters=parameters)

# User layer connects
# leaky_iaf.ports['V'] -> coba_syn.ports['V']
# coba_syn.ports['Isyn'] -> leaky_iaf.ports['Isyn'] for multiple synapses (reduce port)
# Simulator attaches to SpikeInput and SpikeOutput ports for input and output.


# write to file object f if defined

c1 = coba_syn

try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:
    import os

    base = "coba_synapse"
    c1.write(base+".xml")
    c2 = parse(base+".xml")
    assert c1==c2

    c1.to_dot(base+".dot")
    os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))

