
import unittest
import nineml.abstraction_layer as nineml

import os, tempfile

# [expr, vars, functions]
aliases = [["q := -A/tau_r", "q",()],
            ["q(v,w) := v*w/10", "q", ("v","w")],
            ["alpha_n(V) := -0.01*(V+55)/(exp(-(V+55)/10) - 1)","alpha_n",("V",)],
            ["beta_n(V) := 0.125*exp(-(V+65)/80)","beta_n",("V",)],
            ["ntau(V) := 1/(q10*(alpha_n(V) + beta_n(V)))","ntau",("V",)],
            ["ninf(V) := alpha_n(V)/(alpha_n(V) + beta_n(V))","ninf",("V",)],
            ["gna(m,h) := gnabar*m*m*m*h","gna",("m","h")],
            ["gk(n) := gkbar*n*n*n*n","gk",("n",)]]
             

not_aliases = ["dn/dt = (ninf(V)-n)/ntau(V)",
                "il = gl*(V - el)",
                "ik = gk(n)*(V - ek)",
                "dV/dt = (ina + ik + il + Isyn)/C"]
                

class AliasLhsParseTestCase(unittest.TestCase):

    def test_alias_func_arg_parse(self):

        for b in aliases:
            assert nineml.Alias.match(b[0])
            symbol,args,rhs = nineml.Alias.pre_parse(b[0])
            assert symbol == b[1]
            #print args, b[2]
            assert args == b[2]

    def test_negative_matches(self):

        for nb in not_aliases:
            assert not nineml.Alias.match(nb)


    def test_failed_aliases(self):

        # these should work
        b = nineml.Alias("func(x,y)","x**2 + y")
        b = nineml.Alias("f","10")

        # func symbol in func arg
        self.assertRaises(ValueError, nineml.Alias, "func(func,x)","x**2 + func")

        # self reference
        self.assertRaises(ValueError, nineml.Alias, "f", "x**2 + f")

        # redefining math symbols
        self.assertRaises(ValueError, nineml.Alias, "e","10")
        


def suite():

    suite = unittest.makeSuite(AliasLhsParseTestCase,'test')
    return suite

if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
