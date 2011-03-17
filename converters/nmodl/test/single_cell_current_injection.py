

import neuron
h = neuron.h
import pylab
import numpy

def create_cell(name, parameters, section):
    cell = getattr(h, name)(0.5, sec=section)
    for param, value in parameters.items():
        setattr(cell, param, value)
    return cell        

def configure(cell, initialize):
    fih = h.FInitializeHandler(1, initialize)
    cvode = h.CVode()
    cvode.active(1)
    cvode.condition_order(2)    
    return fih, cvode

def setup_recording(cell):
    vrec = h.Vector()
    trec = h.Vector()
    vrec.record(cell._ref_V)
    trec.record(h._ref_t)
    
    spike_times = h.Vector()
    rec = h.NetCon(cell, None)
    rec.record(spike_times)
    return vrec, trec, spike_times, rec

def run(duration):
    h.finitialize()
    while h.t < duration:
        h.fadvance()
        
def calculate_errors(spike_times, expected_spike_times):
    spike_times = numpy.array(spike_times)
    errors = (spike_times - expected_spike_times)/expected_spike_times
    success = (errors < 0.001).all()
    return success, errors

def plot(filename, trec, vrec):
    #pylab.rcParams['interactive'] = True
    pylab.plot(trec, vrec, 'bo-')
    pylab.savefig(filename)