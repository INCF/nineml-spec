import nineml.abstraction_layer as nineml


parameters = ["V", "t", "spike", "Isyn", "subthreshold", "refractory_end", "gL",
              "vL", "theta", "Vreset", "C", "trefractory", "tspike"]


subthreshold_regime = nineml.Sequence(
                        nineml.ODE("V", "t", "(-gL*(V-vL) + Isyn)/C", name="membrane_equation"),
                        nineml.Assignment("spike", "V > theta", name="threshold_crossing"),
                        name="subthreshold_regime"
                      )

refractory_regime = nineml.Union(
                        nineml.Assignment("V", "Vreset", name="membrane_reset"),
                        nineml.Assignment("refractory_end", "t >= tspike + trefractory", name="check_refractory_end"),
                        name="refractory_regime"
                    )

spike_transition = nineml.Transition(
                        from_=subthreshold_regime,
                        to=refractory_regime,
                        condition="spike",
                        assignment=nineml.Assignment("tspike", "t", name="assign_last_spike_time"),
                        name="spike_transition"
                    )

subthreshold_transition = nineml.Transition(
                            from_=refractory_regime,
                            to=subthreshold_regime,
                            condition="refractory_end",
                            name="subthreshold_transition"
                          )

c1 = nineml.Component("LeakyIAF", parameters,
                             transitions=(spike_transition, subthreshold_transition))



# write to file object f if defined
try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:
    c1.write("leaky_iaf2.xml")
    c2 = parse("leaky_iaf2.xml")
    assert c1==c2
