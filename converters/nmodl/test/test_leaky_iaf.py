# encoding: utf-8

from single_cell_current_injection import TestCase, configure, run
import numpy

mechanism = "LeakyIAF"
parameters = p = {
    'C': 1,         # nF
    'vL': -65,      # mV
    'Isyn': 0.5,    # nA
    'V_reset': -70, # mV
    'theta': -50,   # mV
    't_ref': 5,     # ms
    'gL': 0.01,     # µS
}
initial_values = {'V': -65, 't_spike': -1e9}

def time_to_spike(v_start):
    tau_m = p['C']/p['gL']
    x = p['vL'] + p['Isyn']/p['gL']
    return tau_m*numpy.log((v_start - x)/(p['theta'] - x))
spike1 = time_to_spike(initial_values['V'])
spike2 = spike1 + p['t_ref'] + time_to_spike(p['V_reset'])
expected_spike_times = numpy.array([spike1, spike2])


if __name__ == "__main__":
    configure()
    test = TestCase(mechanism, parameters, initial_values, expected_spike_times)
    run(100.0)
    test.plot("test_leaky_iaf.png")
    errors = test.calculate_errors()
    print test.success and "OK" or "FAIL"

