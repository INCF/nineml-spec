"""
Script for generating a single-compartment Hodgkin-Huxley cell in NineML XML format.

Andrew Davison, 2010
"""

from nineml.abstraction_layer import *

q10_binding = Binding("q10",  "3**((celsius - 6.3)/10)")

sodium_state_update = Union(
    Sequence(
        Union(
            Assignment("alpha_m", "-0.1*(V+40)/(exp(-(V+40)/10) - 1)"),
            Assignment("beta_m", "4*exp(-(V+65)/18)"),
        ),
        Union(
            Assignment("mtau", "1/(q10*(alpha_m + beta_m))"),
            Assignment("minf", "alpha_m/(alpha_m + beta_m)"),
        ),
        ODE("m", "t", "(minf-m)/mtau"),
        name="sodium_activation_system"
    ),
    Sequence(
        Union(
            Assignment("alpha_h", "0.07*exp(-(V+65)/20)"),
            Assignment("beta_h", "1/(exp(-(V+35)/10) + 1)"),
        ),
        Union(
            Assignment("htau", "1/(q10*(alpha_h + beta_h))"),
            Assignment("hinf", "alpha_h/(alpha_h + beta_h)"),
        ),
        ODE("h", "t", "(hinf-h)/htau"),
        name="sodium_inactivation_system"
    ),
    name="sodium_state_update"
)

potassium_state_update = Sequence(
    Union(
        Assignment("alpha_n", "-0.01*(V+55)/(exp(-(V+55)/10) - 1)"),
        Assignment("beta_n", "0.125*exp(-(V+65)/80)"),
    ),
    Union(
        Assignment("ntau", "1/(q10*(alpha_n + beta_n))"),
        Assignment("ninf", "alpha_n/(alpha_n + beta_n)")
    ),
    ODE("n", "t", "(ninf-n)/ntau"),
    name="potassium_state_update"
)

state_updates = Union(sodium_state_update,
                      potassium_state_update,
                      name="state_updates")

current_calculation = Union(
    Sequence(
        Assignment("gna", "gnabar*m*m*m*h"),
        Assignment("ina", "gna*(V - ena)"),
    ),
    Sequence(
        Assignment("gk", "gkbar*n*n*n*n"),
        Assignment("ik", "gk*(V - ek)"),
    ),
    Assignment("il", "gl*(V - el)"),
    name="current_calculation"
)

hh_regime = Sequence( # or Union? do we solve for m,h,n first then V, or all together?
    state_updates,
    current_calculation,
    ODE("V", "t", "(ina + ik + il + Isyn)/C", name="membrane_equation"),
    Assignment("spike", "V > theta", name="test_threshold_crossing"),
    name="hh_regime"
)

spike_transition = Transition(
    from_=hh_regime,
    to=hh_regime,
    condition="spike",
    assignment=Assignment("tspike", "t"),
    name="spike_emitted"
)

input_variables = ["Isyn", "t"]
state_variables = ["m", "h", "n", "V"]
fixed_parameters = ["C", "gnabar", "gkbar", "gl", "ena", "ek", "el", "celsius", "theta"]
assigned_variables = ["q10", "alpha_m", "beta_m", "alpha_h", "beta_h",
                      "alpha_n", "beta_n", "gna", "gk", "tspike"]
parameters = input_variables + state_variables + fixed_parameters + assigned_variables

c1 = Component("Hodgkin-Huxley", parameters,
                      transitions=(spike_transition,),
                      bindings=[q10_binding])

# write to file object f if defined
try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:
    c1.write("hh.xml")
    c2 = parse("hh.xml")
    assert c1==c2
