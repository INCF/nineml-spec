
from single_cell_current_injection import create_cell, configure, setup_recording, run, calculate_errors, plot
import neuron
h = neuron.h

dummy = h.Section()

parameters = {
    'a': 0.02,
    'b': 0.2,
    'c': -50,
    'd': 2,
    'Isyn': 15,
    'theta': 0,
}
cell = create_cell("Izhikevich", parameters, dummy)

def initialize():
    cell.V = -65
    cell.U = 0.2*cell.V

#fih, cvode = configure(cell, initialize)
fih = h.FInitializeHandler(1, initialize)
cvode = h.CVode()
cvode.active(1)

vrec, trec, spike_times, rec = setup_recording(cell)
run(100.0)

import numpy

expected_spike_times = numpy.array( # I have no idea if these values are correct
  [  2.12834641,   3.17733172,   4.31376038,   5.52657414,
     6.85904596,   8.33813478,   9.97419301,  11.85470981,
    14.10067134,  17.04985314,  50.2182681 ,  51.81893091,
    53.63051808,  55.78022398,  58.53731759,  63.57952919,
    97.27780952,  98.87851621])

success, errors = calculate_errors(spike_times, expected_spike_times)

plot("test_izhikevich.png", trec, vrec)