# encoding: utf-8

from single_cell_current_injection import create_cell, configure, setup_recording, run, calculate_errors, plot
import neuron
h = neuron.h

dummy = h.Section()

parameters = {
    'C': 1,         # nF
    'vL': -65,      # mV
    'Isyn': 0.5,    # nA
    'V_reset': -70, # mV
    'theta': -50,   # mV
    't_ref': 5,     # ms
    'gL': 0.01,     # µS
}
cell = create_cell("LeakyIAF", parameters, dummy)

v_init = -65
def initialize():
    cell.V = v_init
    cell.t_spike = -1e9
fih, cvode = configure(cell, initialize)
vrec, trec, spike_times, rec = setup_recording(cell)
run(100.0)

import numpy
def time_to_spike(v_start):
    tau_m = cell.C/cell.gL
    x = cell.vL + cell.Isyn/cell.gL
    return tau_m*numpy.log((v_start - x)/(cell.theta - x))
spike1 = time_to_spike(v_init)
spike2 = spike1 + cell.t_ref + time_to_spike(cell.V_reset)
expected_spike_times = numpy.array([spike1, spike2])

success, errors = calculate_errors(spike_times, expected_spike_times)

plot("test_leaky_iaf.png", trec, vrec)
