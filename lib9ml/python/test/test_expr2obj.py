""" Test expression to object mapping """

import unittest
import re
import nineml.abstraction_layer as nineml

# (expr, (lhs,op,rhs))
ODEs = [("dA_x/dt = -A/tau_r",("A_x","t","-A/tau_r")),
        ("  dB/dt=-B/tau_d",("B","t","-B/tau_d"))]

Assignments = [("gB = 1/(1 + mg_conc*eta*exp(-1*gamma*V))",("gB","1/(1 + mg_conc*eta*exp(-1*gamma*V))")),
               ("g = gB*gmax*(B-A)",("g","gB*gmax*(B-A)")),
               (" dA = dt",("dA","dt")),
               (" h = dA/dx", ("h","dA/dx"))]

Inplace = [("Isyn+=g*(E_rev-V)",("Isyn","+=","g*(E_rev-V)")),
           (" A += weight*factor",("A","+=","weight*factor")),
           (" A*=1.2",("A","*=","1.2")),
           (" B /= 1.45",("B","/=","1.45")),
           (" dA/=dt", ("dA","/=","dt"))]


all_good = Inplace+Assignments+ODEs

all_bad = ["B / = 1.45",
           " "]

        
p_eqn = re.compile(r"(?P<lhs>[a-zA-Z_]+[a-zA-Z_0-9]*(/?[a-zA-Z_]+[a-zA-Z_0-9]*)?)\s*(?P<op>[+\-*/]?=)\s*(?P<rhs>.*)")
p_ode_lhs = re.compile(r"(?:d)([a-zA-Z_]+[a-zA-Z_0-9]*)/(?:d)([a-zA-Z_]+[a-zA-Z_0-9]*)")



class E2OTestCase(unittest.TestCase):

    def test_donothing(self):
        """ Check that expr_to_obj leaves objects alone"""
        
        for t in ODEs:
            o = nineml.expr_to_obj(t[0])
            assert o == nineml.expr_to_obj(o)

        for t in Assignments:
            o = nineml.expr_to_obj(t[0])
            assert o == nineml.expr_to_obj(o)

        for t in Inplace:
            o = nineml.expr_to_obj(t[0])
            assert o == nineml.expr_to_obj(o)


    def test_odes(self):

        for t in ODEs:
            o = nineml.expr_to_obj(t[0])

            dep_var, indep_var, rhs = t[1]

            assert isinstance(o,nineml.ODE)

            assert o.rhs == rhs
            assert o.dependent_variable == dep_var
            assert o.bound_variable == indep_var


    def test_bad(self):

        for s in all_bad:
            m = p_eqn.match(s)
            self.assertRaises(ValueError, nineml.expr_to_obj, s)



    def test_assign(self):

        for t in Assignments:
            o = nineml.expr_to_obj(t[0])

            lhs, rhs = t[1]

            assert isinstance(o,nineml.Assignment)

            assert o.expr == rhs
            assert o.to == lhs


    def test_inplace(self):

        for t in Inplace:
            o = nineml.expr_to_obj(t[0])

            lhs, op, rhs = t[1]

            assert isinstance(o,nineml.Inplace)

            assert o.expr == rhs
            assert o.to == lhs
            assert o.op == op


        


def suite():

    suite = unittest.makeSuite(E2OTestCase,'test')
    return suite

if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
