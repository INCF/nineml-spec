"""
Script for generating a single-compartment Hodgkin-Huxley cell in NineML XML format.

Andrew Davison, 2010
"""

from nineml.abstraction_layer import *
import os

aliases = [
    "q10 := 3.0**((celsius - 6.3)/10.0)",  # temperature correction factor
    "alpha_m(V) := -0.1*(V+40.0)/(exp(-(V+40.0)/10.0) - 1.0)",  # m
    "beta_m(V) := 4.0*exp(-(V+65.0)/18.0)",
    "mtau(V) := 1/(q10*(alpha_m(V) + beta_m(V)))",
    "minf(V) := alpha_m(V)/(alpha_m(V) + beta_m(V))",
    "alpha_h(V) := 0.07*exp(-(V+65.0)/20.0)",               # h
    "beta_h(V) := 1.0/(exp(-(V+35)/10.0) + 1.0)",
    "htau(V) := 1.0/(q10*(alpha_h(V) + beta_h(V)))",
    "hinf(V) := alpha_h(V)/(alpha_h(V) + beta_h(V))",
    "alpha_n(V) := -0.01*(V+55.0)/(exp(-(V+55.0)/10.0) - 1.0)", # n
    "beta_n(V) := 0.125*exp(-(V+65.0)/80.0)",
    "ntau(V) := 1.0/(q10*(alpha_n(V) + beta_n(V)))",
    "ninf(V) := alpha_n(V)/(alpha_n(V) + beta_n(V))",
    "gna(m,h) := gnabar*m*m*m*h",                       # 
    "gk(n) := gkbar*n*n*n*n",
    "ina(m,h,V) := gna(m,h)*(ena - V)",                 # currents
    "ik(n,V) := gk(n)*(ek - V)",
    "il(V) := gl*(el - V )"]

hh_regime = Regime(
    "dn/dt = (ninf(V)-n)/ntau(V)",
    "dm/dt = (minf(V)-m)/mtau(V)",
    "dh/dt = (hinf(V)-h)/htau(V)",
    "dV/dt = (ina(m,h,V) + ik(n,V) + il(V) + Isyn)/C",
    name="hh_regime",
    transitions=On("V > theta",do=[SpikeOutputEvent])
)

# the rest are not "parameters" but aliases, assigned vars, state vars, indep vars, ports, etc.
parameters = ['el', 'C', 'ek', 'ena', 'gkbar', 'gnabar', 'theta', 'gl','celsius',
              'Isyn']

ports = [SendPort("V"),
         ReducePort("Isyn",op="+")]

c1 = Component("Hodgkin-Huxley", parameters=parameters,
                      regimes=(hh_regime,),
                      aliases=aliases, ports=ports)

# write to file object f if defined
try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:

    base = "hh"
    c1.write(base+".xml")
    c2 = parse(base+".xml")
    assert c1==c2

    c1.to_dot(base+".dot")
    os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))
