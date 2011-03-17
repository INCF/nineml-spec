import neuron
h = neuron.h

#neuron.load_mechanisms('.')

dummy = h.Section()

cell = h.Izhikevich(0.5, sec=dummy)
cell.a = 0.02
cell.b = 0.2
cell.c = -50
cell.d = 2
cell.Isyn = 15
cell.theta = 0

def initialize():
    cell.V = -65
    cell.U = 0.2*cell.V

fih = h.FInitializeHandler(0, initialize)
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
pylab.savefig("test_Izhikevich.png")