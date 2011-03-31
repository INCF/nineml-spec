import nineml.abstraction_layer as nineml
from brian import *
import brian.stdunits as units
import numpy


# Single Union hh model definition, which is Sequence and Event free.
hh = nineml.parse("hh.xml")

def component_to_native_brian(c):

    assert len(c.regime_map)==1, "brian supports 1 regime natively"

    # back substitute bindings which need bindings
    hh.backsub_bindings()
    # substitute bindings in equations
    hh.backsub_equations()

    eqns = []
    for e in hh.equations:
        # this fudges brian's unit system
        eqn = '%s = (  %s  )/__time_factor__ : 1.' % (e.lhs,e.rhs,)
        eqns+=[eqn]
    return eqns
        
# have a look at what equations we got    
eqns = component_to_native_brian(hh)
for e in eqns:
    print e


# User parameters (lets get units spec'd!)
# Units are "emulated" (hacked),
# so still not sure all units are correct

#area=20000*umetre**2

celsius = 6.3 # results in eqns as in Gerstner & Kistler
el = -54.4 # mv
ek = -77.0 # mv
ena = 50.0 # mv
gl = 0.3 # *msiemens/cm**2
gkbar = 36.0 # *msiemens/cm**2
gnabar = 120.0 #*msiemens/cm**2
C = 1.0 #*ufarad/cm**2
Isyn = 10.0 #*uA/cm**2

# Set unit of time (see unit fudging above)
__time_factor__ = 1.*ms

# Set temporal resolution
defaultclock.dt = 0.1*ms

init = {'V':-60.0,'m':0.0,'h':0.0,'n':0.0}

neuron=NeuronGroup(1, eqns, init=init, implicit=True, freeze=True, compile=False)
trace=StateMonitor(neuron, 'V', record=True)

run(100*ms)
plot(trace.times/ms,trace[0])
xlabel('t [ms]')
ylabel('V [mV]')
show()

