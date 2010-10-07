import nineml.abstraction_layer as nineml


parameters = ["Isyn", "gL", "vL", "theta", "Vreset", "C", "trefractory"]


subthreshold_regime = nineml.Sequence(
                        nineml.ODE("V", "t", "(-gL*(V-vL) + Isyn)/C",
                                   name="membrane_equation"),
                        name="subthreshold_regime"
                      )

refractory_regime = nineml.Union(
                        nineml.Assignment("V", "Vreset", name="membrane_reset"),
                        name="refractory_regime"
                    )

spike_transition = nineml.Transition(
                        from_=subthreshold_regime,
                        to=refractory_regime,
                        condition="V > theta",
                        assignment=nineml.Assignment("tspike", "t"),
                        name="spike_transition"
                    )

subthreshold_transition = nineml.Transition(
                            from_=refractory_regime,
                            to=subthreshold_regime,
                            condition="t >= tspike + trefractory",
                            name="subthreshold_transition"
                          )

c1 = nineml.Component("LeakyIAF", parameters,
                             transitions=(spike_transition, subthreshold_transition))



# write to file object f if defined
try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:
    import os

    base = "leaky_iaf2"
    c1.write(base+".xml")
    c2 = nineml.parse(base+".xml")
    assert c1==c2

    c1.to_dot(base+".dot")
    os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))
