
from single_cell_current_injection import create_cell, configure, setup_recording, run, calculate_errors, plot
import neuron
h = neuron.h

dummy = h.Section()

parameters = {
    'C': 10,
    'V1': -1.2,
    'Isyn': 100,
    'phi': 0.04,
    'V2': 18,
    'g_l': 2,
    'g_ca': 4.4,
    'V3': 2,
    'V4': 30,
    'g_k': 8,
    'theta': 20,
    'V_l': -60,
    'V_ca': 120,
    'V_k': -84,
}
cell = create_cell("Morris_Lecar", parameters, dummy)

def initialize():
    cell.V = -65
    cell.W = 0

#fih, cvode = configure(cell, initialize)
fih = h.FInitializeHandler(1, initialize)
cvode = h.CVode()
cvode.active(1)
cvode.condition_order(2)

vrec, trec, spike_times, rec = setup_recording(cell)
run(100.0)

import numpy

expected_spike_times = numpy.array([ 8.19,  77.90])

success, errors = calculate_errors(spike_times, expected_spike_times)

plot("test_morris-lecar.png", trec, vrec)