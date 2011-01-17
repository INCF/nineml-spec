'''

Adaptive exponential integrate-and-fire neuron as described in
A. Destexhe, J COmput Neurosci 27: 493--506 (2009)

Author B. Kriener (Jan 2011)

## neuron model: aeIF

## variables:
## V: membrane potential
## w: adaptation variable

## parameters:
## C_m     # specific membrane capacitance [muF/cm**2] 
## g_L     # leak conductance [mS/cm**2]
## E_L     # resting potential [mV]
## Delta   # steepness of exponential approach to threshold [mV]
## V_T     # spike threshold [mV]
## S       # membrane area [mum**2]
## tau_ref # refractory time [ms]
## tau_w   # adaptation time constant
## a, b    # adaptation parameters [muS, nA]

'''

from nineml.abstraction_layer import *

parameters = ['C_m','g_L','E_L', 'Delta', 'V_T', 'S', 'tau_ref', 'tau_w', 'a', 'b']

aeIF = Component("aeIF", # begin Component    
                 regimes=[                   # begin regimes
                     Regime(                              # begin subthreshold regime
                            "dV/dt = -g_L*(V-E_L)/C_m + g_L*Delta*exp((V-V_T)/Delta-w/S)/C_m+ Isyn/C_m",
                            "dw/dt = (a*(V-E_L)-w)/tau_w",
                            transitions=On("V > V_T",
                                           do=["V = E_L",
                                               "w += b",
                                               SpikeOutputEvent],
                                           to="refractory-regime"),
                            name="subthreshold-regime"),  # end subthreshold regime
                     Regime(                              # begin refractory regime
                            transitions=On("t>=tspike+trefractory",
                                           to="subthreshold-regime"),
                            name="refractory-regime")     # end refractory regime
                           ],                # end regimes
                     ports=[ReducePort("Isyn", op="+")]
                 )        # end Component  



# write to file object f if defined

c1 = aeIF

try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:
    import os

    base = "aeIF"
    c1.write(base+".xml")
    c2 = parse(base+".xml")
    assert c1==c2

    c1.to_dot(base+".dot")
    os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))

