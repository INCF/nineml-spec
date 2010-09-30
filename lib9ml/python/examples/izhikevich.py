import nineml.abstraction_layer as nineml


parameters = ["V", "U", "t", "spike", "Isyn",
              "a", "b", "c", "d", "theta"]

subthreshold_regime = nineml.Sequence(
    nineml.Union(
        "dV/dt = 0.04*V*V + 5*V + 140.0 - U + Isyn",
        "dU/dt = a*(b*V - U)",
        ),
    "spike = V > theta",
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
                        condition="spike",
                        assignment=nineml.Assignment("tspike", "t"),
                        name="spike_transition"
                    )

return_transition = nineml.Transition(
                            from_=post_spike_regime,
                            to=subthreshold_regime,
                            condition=None,
                            name="return_transition"
                          )

component = nineml.Component("Izhikevich", parameters,
                             (spike_transition, return_transition))


component.write("izhikevich.xml")
