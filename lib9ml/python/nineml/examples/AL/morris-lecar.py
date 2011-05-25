"""

The Morris-Lecar Model

Authors:
- G4 (A. Davison, Y. Le Franc, E. Muller, I. Raikov
- Gif-sur-Yvette, CNRS-UNIC Salle de Vie, Dec 3rd, 2010.

"""


import nineml.abstraction_layer as nineml

regimes = [
    nineml.Regime(
        "dV/dt = (g_l*(V_l - V) + I_ca(V) + I_k(V,W) + Isyn)/C",
        "dW/dt = lambda_W(V)*(W_inf(V) - W)",
        "M_inf(V) := 0.5*(1.0+tanh((V-V1)/V2))",
        "W_inf(V) := 0.5*(1.0+tanh((V-V3)/V4))",
        "lambda_W(V) := phi*cosh((V-V3)/(2.0*V4))",
        "I_ca(V) := g_ca*M_inf(V)*(V_ca-V)",
        "I_k(V,W) := g_k*W*(V_k-V)",
        transitions = nineml.On("V > theta",do=[nineml.SpikeOutputEvent]),
        name="subthreshold_regime"
    )]


ports = [nineml.SendPort("V"),
         nineml.ReducePort("Isyn",op="+")]


c1 = nineml.Component("Morris-Lecar", regimes = regimes )



# write to file object f if defined
try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:
    import os

    base = "morris-lecar"
    c1.write(base+".xml")
    c2 = nineml.parse(base+".xml")
    assert c1==c2

    c1.to_dot(base+".dot")
    os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))
