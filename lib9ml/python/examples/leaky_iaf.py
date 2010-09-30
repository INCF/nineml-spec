import nineml.abstraction_layer as nineml


parameters = ["V", "t", "spike", "Isyn", "subthreshold", "refractory_end", "gL",
              "vL", "theta", "Vreset", "C", "trefractory", "tspike"]

subthreshold_equation = nineml.ODE("V", "t", "(-gL*(V-vL) + Isyn)/C")
subthreshold_node = nineml.Reference(subthreshold_equation)

threshold_detect = nineml.Assignment("spike", "V > theta")
threshold_node = nineml.Reference(threshold_detect)

subthreshold_regime = nineml.Sequence(subthreshold_node, threshold_node)


refractory_equation = nineml.Assignment("V", "Vreset")
refractory_node = nineml.Reference(refractory_equation)

check_refractory_time = nineml.Assignment("refractory_end", "t >= tspike + trefractory")
check_refractory_node = nineml.Reference(check_refractory_time)

refractory_regime = nineml.Union(refractory_node, check_refractory_node)

tspike_assignment = nineml.Assign("tspike", "t")
spike_transition = nineml.Transition(subthreshold_regime,
                                     refractory_regime,
                                     "spike",
                                     tspike_assignment)

subthreshold_transition = nineml.Transition(refractory_regime,
                                            subthreshold_regime,
                                            "refractory_end")

component = nineml.Component("LeakyIAF", parameters,
                             (spike_transition, subthreshold_transition))
