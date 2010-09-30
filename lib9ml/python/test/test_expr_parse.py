
import unittest
import nineml.abstraction_layer as nineml
from nineml.expr_parse import expr_parse, NineMLMathParseError


import os, tempfile

# [expr, vars, functions]
expr_vars = [["-A/tau_r", ("A","tau_r"),()],
             ["V*V", ("V",),()],
             ["a*(b*V - U)", ("U","V","b","a"),()],
             [" 0.04*V*V + 5.0*V + 1. + 140.0 - U + Isyn", ("V","U","Isyn"),()],
             ["c",("c"),()],
             ["1",(),()],
             ["atan2(sin(x),cos(y))",("x","y"),("atan2","sin","cos")],
             ["1.*V",("V"),()],
             ["1.0",(),()],
             [".1",(),()],
             ["1/(1 + mg_conc*eta*exp(-1*gamma*V))", ("mg_conc","eta","gamma","V"),('exp',)],
             ["1 / ( 1 + mg_conc * eta *  exp( -1 * gamma*V))", ("mg_conc","eta","gamma","V"),('exp',)],
             ["1 / ( 1 + mg_conc * eta( ) *  exp ( -1 * gamma*V))", ("mg_conc","gamma","V"),('exp',"eta")],
             [".1 / ( 1.0 + mg_conc * eta() *  exp ( -1.0 * gamma*V))", ("mg_conc","gamma","V"),('exp',"eta")]]









class ExprParseTestCase(unittest.TestCase):

    def test_unmatchedparenthesis(self):

        self.assertRaises(NineMLMathParseError, expr_parse, "1 / (( 1 + mg_conc * eta *  exp ( -1 * gamma*V))")

        self.assertRaises(NineMLMathParseError, expr_parse, "1 / ( 1 + mg_conc * eta *  exp ( -1 * gamma*V)))")

        self.assertRaises(NineMLMathParseError, expr_parse, "1 / ( 1 + mg_conc * eta *  exp (( -1 * gamma*V))")

        self.assertRaises(NineMLMathParseError, expr_parse, "1..0")

        self.assertRaises(NineMLMathParseError, expr_parse, "..0")



        
    def test_getvf(self):
        """Test that we can get vars and funcs out"""
        for e in expr_vars:

            vars, funcs = expr_parse(e[0])
            assert set(vars)==set(e[1])
            assert set(funcs)==set(e[2])

            




def suite():

    suite = unittest.makeSuite(ExprParseTestCase,'test')
    return suite

if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
