
import unittest
import nineml.abstraction_layer as nineml
from nineml.abstraction_layer.cond_parse import cond_parse, call_cond_func
from nineml.abstraction_layer.conditions import Condition
from nineml.abstraction_layer.expr_parse import NineMLMathParseError, expr_parse

import os, tempfile

# [cond, vars, functions]
cond_vars = [["A > -A/tau_r", ("A","tau_r"),()],
             ["V > 1.0 & !(V<10.0)", ("V",),()],
             ["!!(V>10)",("V"),()],
             ["!!(V>10)",("V"),()],
             ["V>exp(Vth)",("V","Vth"),('exp',)],
             ["!(V>Vth)",("V","Vth"),()],
             ["!V>Vth",("V","Vth"),()],
             ["exp(V)>Vth",("V","Vth"),("exp",)],
             ["true",(),()],
             ["(V < (Vth+q)) & (t > t_spike)",("t_spike","t","q","Vth","V"),()],
             ["V < (Vth+q) | t > t_spike",("t_spike","Vth","q","V","t"),()],
             ["(true)",(),()],
             ["!true",(),()],
             ["!false",(),()],
             ["t >= t_spike + tref",("t","t_spike","tref"),()],
             ["true & !false",(),()]
             ]

bad_cond = ["V & | 20","V>&&V","(V<= V)<10",
            "1+2", "!1", "sin(V>10)", "exp(true)","true()"]         


namespace = {
    "A":10,
    "tau_r":5,
    "V":20,
    "Vth":-50.0,
    "t_spike": 1.0,
    "q":11.0,
    "t":0.9,
    "tref":0.1
    }
    
return_values = [
True,
True,
True,
True,
True,
False,
False,
True,
True,
False,
False,
True,
False,
True,
False,
True
]






class CondParseTestCase(unittest.TestCase):

    def test_unmatchedparenthesis(self):

        self.assertRaises(NineMLMathParseError, expr_parse, "true(")

        self.assertRaises(NineMLMathParseError, expr_parse, "V < (V+10")

        self.assertRaises(NineMLMathParseError, expr_parse, "1 / ( 1 + mg_conc * eta *  exp (( -1 * gamma*V))")

        self.assertRaises(NineMLMathParseError, expr_parse, "1..0")

        self.assertRaises(NineMLMathParseError, expr_parse, "..0")


    def test_truefalse(self):

        s = cond_parse("true")
        assert s[0]==set() and s[1]==set()
        s = cond_parse("True")
        assert s[0]==set() and s[1]==set()


        s = cond_parse("False")
        assert s[0]==set() and s[1]==set()
        s = cond_parse("false")
        assert s[0]==set() and s[1]==set()


    def test_bad_conditionals(self):

        for c in bad_cond:
            self.assertRaises(NineMLMathParseError, cond_parse, c)
            

    

        
    def test_getvf(self):
        """Test that we can get vars and funcs out"""
        for i,e in enumerate(cond_vars):

            
            c = Condition(e[0])
            c.parse()
            vars = c.names
            funcs = c.funcs
            python_func = c.python_func()
            assert set(vars)==set(e[1])
            assert set(funcs)==set(e[2])
            r = call_cond_func(python_func,namespace)

            assert r==return_values[i]
            




def suite():

    suite = unittest.makeSuite(CondParseTestCase,'test')
    return suite

if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
