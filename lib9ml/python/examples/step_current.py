import nineml.abstraction_layer as nineml

# define a step current

parameters = ["t_step","I0","Delta_I"]

down_regime = nineml.Union("I = I0", transitions=[nineml.On("t>t_step",to="up_regime")])

up_regime = nineml.Union("I = I0 + Delta_I", name = "up_regime")


c1 = nineml.Component("Step Current", parameters,
                      regimes=(down_regime, up_regime))

# write to file object f if defined
try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:
    import os

    base = "step_current"
    c1.write(base+".xml")
    c2 = nineml.parse(base+".xml")
    assert c1==c2

    c1.to_dot(base+".dot", True)
    os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))

