
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
fih = h.FInitializeHandler(0, initialize)
cvode = h.CVode()
cvode.active(1)

vrec, trec, spike_times, rec = setup_recording(cell)
run(100.0)

import numpy

expected_spike_times = numpy.array( # I have no idea if these values are correct
    [  4.39370897e-07,   2.09438417e+00,   4.73028132e+00,
       8.96999878e+00,   4.27825706e+01,   4.43832185e+01,
       4.61947833e+01,   4.83444533e+01,   5.11014716e+01,
       5.61429396e+01,   8.98450254e+01,   9.14458020e+01,
       9.32575612e+01,   9.54075431e+01,   9.81652146e+01])

success, errors = calculate_errors(spike_times, expected_spike_times)

plot("test_izhikevich.png", trec, vrec)