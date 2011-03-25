# encoding: utf-8

from neuron import h
from util import configure, run, BaseTestCase

SYN_TYPES = {  # argument 1 for net_receive
    'exc': 0,
    'inh': 1,
}

class TestCase(BaseTestCase):

    def __init__(self, name, parameters, initial_values, expected_spike_times):
        BaseTestCase.__init__(self, name, parameters, initial_values, expected_spike_times)
        self.stimuli = {}
        self.add_stimulation("exc", 5, 10, 0.04)
        self.add_stimulation("inh", 17, 30, 0.05)

    def add_stimulation(self, label, start, interval, weight):
        stim = h.NetStim(0.5, sec=self.section)
        stim.noise = 0
        stim.start = start
        stim.interval = interval
        stim.number = 1e12
        delay = 1.0
        nc = h.NetCon(stim, self.cell, 0.5, delay, weight)
        assert nc.weight[0] == weight
        nc.weight[1] = SYN_TYPES[label]
        self.stimuli[label] = (stim, nc)

    def setup_recording(self):
        self.Vm = h.Vector()
        self.gE = h.Vector()
        self.gI = h.Vector()
        self.times = h.Vector()
        self.Vm.record(self.cell._ref_V)
        self.gE.record(self.cell._ref_gE)
        self.gI.record(self.cell._ref_gI)
        self.times.record(h._ref_t)
        
        self.spike_times = h.Vector()
        self.source = h.NetCon(self.cell, None)
        self.source.record(self.spike_times)


mechanism = "IF_cond_exp"
parameters = p = {
    'cm': 1,         # nF
    'v_rest': -65,      # mV
    'i_offset': 0.0,    # nA
    'v_reset': -70, # mV
    'v_thresh': -60,   # mV
    'tau_refrac': 5,     # ms
    'tau_m': 10.0,     # ms
    'tau_syn_E': 2.0,
    'tau_syn_I': 5.0,
    'e_rev_E': 0,
    'e_rev_I': -70,
}
initial_values = {'V': -65, 'gE': 0, 'gI': 0, 't_spike': -1e9}
expected_output = [38.0033, 77.5482]

if __name__ == '__main__':
    configure()
    test = TestCase(mechanism, parameters, initial_values, expected_output)
    run(100.0)
    test.plot("test_if_cond_exp.png")
    