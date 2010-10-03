import nineml.abstraction_layer as nineml
import numpy

# A model which generates a fixed sequence of spikes
# Here we use a pre-generated poisson process as an example.

# define a poisson spike-train
rate = 10 # events per time unit
length = 1.0 # one time unit
isi = numpy.random.exponential(1.0/rate,size=(rate*length*2,))
# spike times
t = numpy.add.accumulate(isi)
spike_times = t[t<length]

# 

regimes = []
for i,t_spike in enumerate(spike_times):

    if (i+1)<len(spike_times):
        trans = [nineml.On("t>%f" % spike_times[i+1],to="spike_%d" % (i+1))]
    else:
        trans = []
            
    regimes.append(nineml.Union("t_spike = %f" % t_spike,
                               transitions=trans,name="spike_%d" % i))


c1 = nineml.Component("Spike Generator", [],
                      regimes=regimes)

# write to file object f if defined
try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:
    import os
    
    base = "spike_generator"
    c1.write(base+".xml")
    c2 = nineml.parse(base+".xml")
    assert c1==c2

    c1.to_dot(base+".dot")
    os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))
              
    
