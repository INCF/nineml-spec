import neuron
h = neuron.h

#neuron.load_mechanisms('.')

dummy = h.Section()

cell = h.LeakyIAF(0.5, sec=dummy)
cell.C = 1
cell.vL = -65
cell.Isyn = 0.5
cell.Vreset = -70
cell.theta = -50
cell.trefractory = 5
cell.gL = 0.01

def initialize():
    cell.V = -65

fih = h.FInitializeHandler(1, initialize)
cvode = h.CVode()
cvode.active(1)
       
vrec = h.Vector()
trec = h.Vector()
vrec.record(cell._ref_V)
trec.record(h._ref_t)
       
h.finitialize()
while h.t < 100.0:
    h.fadvance()
    

import pylab
#pylab.rcParams['interactive'] = True
pylab.plot(trec, vrec)
pylab.savefig("test_LeakyIAF.png")