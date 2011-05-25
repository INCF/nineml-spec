"""
A composite leaky integrate-and-fire with conductance-based, exponential
synapses, like the IF_cond_exp standard cell model in PyNN

"""

import nineml.abstraction_layer as nineml

regimes = [
    nineml.Regime(
        "dV/dt = (v_rest - V)/tau_m + (gE*(e_rev_E - V) + gI*(e_rev_I - V) + i_offset)/cm",
        "dgE/dt = -gE/tau_syn_E",
        "dgI/dt = -gI/tau_syn_I",        
        transitions = (nineml.On("V > v_thresh",
                                 do=["t_spike = t",
                                     "V = v_reset",
                                     nineml.SpikeOutputEvent],
                                 to="refractory-regime"),
                       nineml.On(nineml.EventPort('excitatory', mode="recv"), do="gE+=q"),
                       nineml.On(nineml.EventPort('inhibitory', mode="recv"), do="gI+=q"),
                      ),
        name = "sub-threshold-regime"
    ),
    nineml.Regime(
        "dgE/dt = -gE/tau_syn_E",
        "dgI/dt = -gI/tau_syn_I",
        transitions = (nineml.On("t >= t_spike + tau_refrac", to="sub-threshold-regime"),
                       nineml.On(nineml.EventPort('excitatory', mode="recv"), do="gE+=q"),
                       nineml.On(nineml.EventPort('inhibitory', mode="recv"), do="gI+=q"),
                       ),
        name = "refractory-regime"
    )]


ports = [nineml.SendPort("V"), nineml.SendPort("gE"), nineml.SendPort("gI"),
         nineml.RecvPort("q")]

c1 = nineml.Component("IF_cond_exp", regimes=regimes, ports=ports)


try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:
    import os

    base = "if_cond_exp"
    c1.write(base+".xml")
    c2 = nineml.parse(base+".xml")
    assert c1==c2

    c1.to_dot(base+".dot")
    os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))


