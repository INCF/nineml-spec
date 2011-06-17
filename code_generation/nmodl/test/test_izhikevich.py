

from single_cell_current_injection import TestCase, configure, run

mechanism = "Izhikevich"
parameters = {
    'a': 0.02,
    'b': 0.2,
    'c': -50,
    'd': 2,
    'Isyn': 15,
    'theta': 0,
}
initial_values = {
    'V': -65,
    'U': 0.2 * -65 
}
expected_output = [# I have no idea if these values are correct
     2.12834641,   3.17733172,   4.31376038,   5.52657414,
     6.85904596,   8.33813478,   9.97419301,  11.85470981,
    14.10067134,  17.04985314,  50.2182681 ,  51.81893091,
    53.63051808,  55.78022398,  58.53731759,  63.57952919,
    97.27780952,  98.87851621]



if __name__ == "__main__":
    configure()
    test = TestCase(mechanism, parameters, initial_values, expected_output)
    run(100.0)
    test.plot("test_izhikevich.png")
    errors = test.calculate_errors()
    print test.success and "OK" or "FAIL"
