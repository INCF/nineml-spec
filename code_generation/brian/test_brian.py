
import unittest


import os, tempfile


class Brian9MLTestCase(unittest.TestCase):

    def test_equations_examples(self):

        from brian.equations import Equations
        from brian import stdunits

        # parameter
        __time_factor__ = 1.*stdunits.ms

        # eqns
        e = Equations("""
        dV/dt = (-V/(c*tau))/__time_factor__ : 1.
        c = V**2 : 1.
        """)
        
        assert e._diffeq_names==['V']
        assert e._eq_names == ['c']


        e = Equations("""
        c = c+u : 1.
        """)
        
        assert e._diffeq_names==['V']
        assert e._eq_names == ['c']




        



def suite():

    suite = unittest.makeSuite(Brian9MLTestCase,'test')
    return suite

if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
