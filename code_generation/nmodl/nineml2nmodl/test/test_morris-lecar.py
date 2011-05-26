import numpy
from single_cell_current_injection import TestCase, configure, run

mechanism = "Morris_Lecar"
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
initial_values = {
    'V': -65,
    'W': 0
}
expected_output = numpy.array([ 8.19,  77.90]) # no idea if these are the correct values


if __name__ == "__main__":
    configure()
    test = TestCase(mechanism, parameters, initial_values, expected_output)
    run(100.0)
    test.plot("test_morris-lecar.png")
    errors = test.calculate_errors()
    print test.success and "OK" or "FAIL"
