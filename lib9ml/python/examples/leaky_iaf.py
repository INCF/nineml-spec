import nineml.abstraction_layer as nineml

parameters = ["Isyn", "gL", "vL", "theta", "Vreset", "C", "trefractory"]

subthreshold_regime = nineml.Sequence(
    nineml.ODE("V", "t", "(-gL*(V-vL) + Isyn)/C"),
    )


refractory_regime = nineml.Union(
    nineml.Assignment("V", "Vreset"),
    )

tspike_assignment = nineml.Assignment("tspike", "t")
spike_transition = nineml.Transition(subthreshold_regime,
                                     refractory_regime,
                                     "V > theta",
                                     tspike_assignment)

subthreshold_transition = nineml.Transition(refractory_regime,
                                            subthreshold_regime,
                                            "t >= tspike + trefractory")

c1 = nineml.Component("LeakyIAF", parameters,
                             transitions=(spike_transition, subthreshold_transition))


# write to file object f if defined
try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:
    import os

    base = "leaky_iaf"
    c1.write(base+".xml")
    c2 = nineml.parse(base+".xml")
    assert c1==c2

    c1.to_dot(base+".dot")
    os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))
