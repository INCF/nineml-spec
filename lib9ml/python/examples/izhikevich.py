import nineml.abstraction_layer as nineml


parameters = ["Isyn", "a", "b", "c", "d", "theta"]

subthreshold_regime = nineml.Sequence(
    nineml.Union(
        "dV/dt = 0.04*V*V + 5*V + 140.0 - U + Isyn",
        "dU/dt = a*(b*V - U)",
        ),
    name="subthreshold_regime"
    )

post_spike_regime = nineml.Union(
    "V = c",
    "U += d",
    name="post_spike_regime"
    )

spike_transition = nineml.Transition(
                        from_=subthreshold_regime,
                        to=post_spike_regime,
                        condition="V > theta",
                        assignment=nineml.Assignment("tspike", "t"),
                        name="spike_transition"
                    )

return_transition = nineml.Transition(
                            from_=post_spike_regime,
                            to=subthreshold_regime,
                            condition=None,
                            name="return_transition"
                          )

c1 = nineml.Component("Izhikevich", parameters,
                             transitions=(spike_transition, return_transition))


# write to file object f if defined
try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:
    import os

    base = "izhikevich"
    c1.write(base+".xml")
    c2 = nineml.parse(base+".xml")
    assert c1==c2

    c1.to_dot(base+".dot")
    os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))

