
import unittest
import nineml.abstraction_layer as nineml
from nineml.cond_parse import cond_parse
from nineml.expr_parse import NineMLMathParseError, expr_parse


import os, tempfile

# [cond, vars, functions]
cond_vars = [["A > -A/tau_r", ("A","tau_r"),()],
             ["V > 1.0 && !(V<10.0)", ("V",),()],
             ["!!(V>10)",("V"),()],
             ["!!(V>10)",("V"),()],
             ["V>f(Vth)",("V","Vth"),('f',)],
             ["!(V>Vth)",("V","Vth"),()],
             ["!V>Vth",("V","Vth"),()],
             ["exp(V)>Vth",("V","Vth"),("exp",)],
             ["true",(),()],
             ["(V < (Vth+q)) && (t > t_spike)",("t_spike","t","q","Vth","V"),()],
             ["V < (Vth+q) && t > t_spike",("t_spike","Vth","q","V","t"),()],
             ["(true)",(),()],
             ["!true",(),()],
             ["!false",(),()],
             ["t >= tspike + trefractory",("t","tspike","trefractory"),()],
             ["true && !false",(),()]
             ]

bad_cond = ["V && || 20","V>&&V","(V<= V)<10",
            "1+2", "!1", "sin(V>10)", "exp(true)","true()"]         










class CondParseTestCase(unittest.TestCase):

    def test_unmatchedparenthesis(self):

        self.assertRaises(NineMLMathParseError, expr_parse, "true(")

        self.assertRaises(NineMLMathParseError, expr_parse, "V < (V+10")

        self.assertRaises(NineMLMathParseError, expr_parse, "1 / ( 1 + mg_conc * eta *  exp (( -1 * gamma*V))")

        self.assertRaises(NineMLMathParseError, expr_parse, "1..0")

        self.assertRaises(NineMLMathParseError, expr_parse, "..0")


    def test_bad_conditionals(self):

        for c in bad_cond:
            self.assertRaises(NineMLMathParseError, cond_parse, c)
            

    

        
    def test_getvf(self):
        """Test that we can get vars and funcs out"""
        for e in cond_vars:

            vars, funcs = cond_parse(e[0])
            assert set(vars)==set(e[1])
            assert set(funcs)==set(e[2])

            




def suite():

    suite = unittest.makeSuite(CondParseTestCase,'test')
    return suite

if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
