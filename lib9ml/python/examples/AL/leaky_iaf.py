import nineml.abstraction_layer as nineml

#parameters = ["Isyn", "gL", "vL", "theta", "V_reset", "C", "t_ref"]

subthreshold_regime = nineml.Regime(
    "dV/dt = (-gL*(V-vL) + Isyn)/C",
    transitions = nineml.On("V> theta",
                            do=["t_spike = t", "V = V_reset",
                                nineml.SpikeOutputEvent],
                            to="refractory_regime"),
    name="subthreshold_regime"
    )

refractory_regime = nineml.Regime(
    transitions = nineml.On("t >= t_spike + t_ref",
                            to=subthreshold_regime),
    name="refractory_regime"
    )

ports = [nineml.SendPort("V"),
         nineml.ReducePort("Isyn",op="+")]

c1 = nineml.Component("LeakyIAF", regimes = [subthreshold_regime, refractory_regime], ports=ports)


for eqn in c1.odes:
    print "Eqn:",eqn

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
