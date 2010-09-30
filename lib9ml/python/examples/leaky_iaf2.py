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

component = nineml.Component("LeakyIAF", parameters,
                             (spike_transition, subthreshold_transition))


component.write("leaky_iaf2.xml")