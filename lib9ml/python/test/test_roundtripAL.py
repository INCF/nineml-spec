
import unittest
import nineml.abstraction_layer as nineml


import os, tempfile


class RountripALTestCase(unittest.TestCase):

    def test_roundtrip(self):

        parameters = ["V", "U", "t", "spike", "Isyn",
                      "a", "b", "c", "d", "theta"]

        subthreshold_regime = nineml.Sequence(
            nineml.Union(
                "dV/dt = 0.04*V*V + 5*V + 140.0 - U + Isyn",
                "dU/dt = a*(b*V - U)",
                ),
            "spike = V > theta",
            nineml.On("V > theta", do=["tspike = t", "V=c","U+=d",
                                       nineml.SpikeOutputEvent]),
            name="subthreshold_regime"
            )
        
        component = nineml.Component("Izhikevich", parameters,
                                     initial_regime = subthreshold_regime )

        f = tempfile.TemporaryFile()
        component.write(f)
        f.seek(0)

        component1 = nineml.parse(f)
        f.close()

        assert component == component1




def suite():

    suite = unittest.makeSuite(RountripALTestCase,'test')
    return suite

if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
