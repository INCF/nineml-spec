"""

Implements linear poisson neuron as described in eq. 5 of:

R. Guetig, R. Aharonov, S. Rotter, and Haim Sompolinsky (2003), 
Learning Input Correlations through Nonlinear Temporally Asymmetric 
Hebbian Plasticity, J. Neuroscience, 23(9) 3697--3714


Author: Abigail Morrison, 1/2011.

"""


import nineml.abstraction_layer as nineml

inter_event_regime = nineml.Regime(
	transitions = nineml.On(nineml.SpikeInputEvent,
                            do=["pfire = W/N", "p = rand()"],
                            to="firing_regime"),
    name="inter_event_regime"
    )

firing_regime = nineml.Regime(
    transitions = [nineml.On("pfire >= p",
                            do=nineml.SpikeOutputEvent,
                            to=inter_event_regime),
                   nineml.On("pfire < p",
                            to=inter_event_regime)],
    name="firing_regime"
    )

ports = [nineml.ReducePort("W",op="+")]

c1 = nineml.Component("LPNid8", regimes = [inter_event_regime, firing_regime])
#problems: still don't know how to access the weight of an incoming connection, 
#assumes the presence of a decent rand function


# write to file object f if defined
try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:
    import os

    base = "LPNid8"
    c1.write(base+".xml")
    c2 = nineml.parse(base+".xml")
    assert c1==c2

    c1.to_dot(base+".dot")
    os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))
