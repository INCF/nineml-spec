
import unittest
import nineml.abstraction_layer as nineml
import os, tempfile


class ComponentSymbolsTestCase(unittest.TestCase):

    def test_basic(self):

        bound = ['q']
        parameters = ['a','b','c','d','theta','w']
        in_ports = ['Isyn']
        #out_ports = ['spike']
        state_vars = ["V","U"]
        indep_vars = ["t"]

        regimes = [
            nineml.Regime(
                "q := 20.0 + a*b + w",
                "dV/dt = 0.04*V*V + 5*V + 140.0 - U + Isyn",
                "dU/dt = a*(b*V - U)",
                transitions = [nineml.On("V > theta",do=["V = c","U += d"])],
                name="subthreshold_regime"
            ),
            ]

        ports = [nineml.ReducePort("Isyn",op="+")]

        c1 = nineml.Component("Izhikevich",
                                     regimes = regimes, ports=ports )

        #print parameters, c1.user_parameters
        assert c1.user_parameters == set(parameters)
            
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
