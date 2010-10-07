
import unittest
import nineml.abstraction_layer as nineml

import os, tempfile






class ComponentSymbolsTestCase(unittest.TestCase):

    def test_basic(self):

        bound = ['q']
        parameters = ['a','b','c','d','theta']
        in_ports = ['Isyn']
        #out_ports = ['spike']
        state_vars = ["V","U"]
        indep_vars = ["t"]

        regimes = [
            nineml.Union(
                "q := 20.0 + a*b",
                "dV/dt = 0.04*V*V + 5*V + 140.0 - U + Isyn",
                "dU/dt = a*(b*V - U)",
                transitions = [nineml.On("V > theta",to="suprathreshold_regime")],
                name="subthreshold_regime"
            ),
            nineml.Union(
                "V = c",
                "U += d",
                transitions = [nineml.On("true",to="subthreshold_regime")],
                name="suprathreshold_regime"
            )]


        c1 = nineml.Component("Izhikevich",
                                     regimes = regimes )

        assert c1.parameters == set(parameters+in_ports)
            
        assert c1.bound_symbols == set(bound)
        assert c1.independent_variables == set(indep_vars)
        assert c1.variables == set(state_vars+indep_vars)

        


def suite():

    suite = unittest.makeSuite(ComponentSymbolsTestCase,'test')
    return suite

if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
