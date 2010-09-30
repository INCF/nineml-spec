# encoding: utf-8
import neuron
h = neuron.h

#neuron.load_mechanisms('.')

dummy = h.Section()

cell = h.Hodgkin_Huxley(0.5, sec=dummy)
cell.Isyn = 0.1 # nA
cell.C = 1.0e-3 # nF
cell.gnabar = 0.12 # µS
cell.gkbar = 0.036
cell.gl = 0.0003
cell.ena = 50.0
cell.ek = -77.0
cell.el = -54.3
cell.celsius = 6.3
cell.q10 = 1.0 # this should be assigned, not parameter
cell.theta = -20.0


def initialize():
    cell.V = cell.el
    cell.m = 0.1694
    cell.h = 0.2446
    cell.n = 0.4863

fih = h.FInitializeHandler(1, initialize)
cvode = h.CVode()
cvode.active(0)
       
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
pylab.savefig("test_HH.png")