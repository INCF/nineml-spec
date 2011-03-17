# encoding: utf-8

import neuron
h = neuron.h

#neuron.load_mechanisms('.')

dummy = h.Section()

cell = h.LeakyIAF(0.5, sec=dummy)
cell.C = 1         # nF
cell.vL = -65      # mV
cell.Isyn = 0.5    # nA
cell.V_reset = -70 # mV
cell.theta = -50   # mV
cell.t_ref = 5     # ms
cell.gL = 0.01     # µS
v_init = -65

def initialize():
    cell.V = v_init
    cell.t_spike = -1e9

fih = h.FInitializeHandler(1, initialize)
cvode = h.CVode()
cvode.active(1)
cvode.condition_order(2)

vrec = h.Vector()
trec = h.Vector()
vrec.record(cell._ref_V)
trec.record(h._ref_t)

spike_times = h.Vector()
rec = h.NetCon(cell, None)
rec.record(spike_times)

h.finitialize()
while h.t < 100.0:
    h.fadvance()

import numpy

def time_to_spike(v_start):
    tau_m = cell.C/cell.gL
    x = cell.vL + cell.Isyn/cell.gL
    return tau_m*numpy.log((v_start - x)/(cell.theta - x))
spike1 = time_to_spike(v_init)
spike2 = spike1 + cell.t_ref + time_to_spike(cell.V_reset)
expected_spike_times = numpy.array([spike1, spike2])

spike_times = numpy.array(spike_times)

errors = (spike_times - expected_spike_times)/expected_spike_times
success = (errors < 0.001).all()

import pylab
#pylab.rcParams['interactive'] = True
pylab.plot(trec, vrec, 'bo-')
pylab.savefig("test_LeakyIAF.png")