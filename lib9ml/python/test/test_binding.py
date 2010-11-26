
import unittest
import nineml.abstraction_layer as nineml

import os, tempfile

# [expr, vars, functions]
bindings = [["q := -A/tau_r", "q",()],
            ["q(v,w) := v*w/10", "q", ("v","w")],
            ["alpha_n(V) := -0.01*(V+55)/(exp(-(V+55)/10) - 1)","alpha_n",("V",)],
            ["beta_n(V) := 0.125*exp(-(V+65)/80)","beta_n",("V",)],
            ["ntau(V) := 1/(q10*(alpha_n(V) + beta_n(V)))","ntau",("V",)],
            ["ninf(V) := alpha_n(V)/(alpha_n(V) + beta_n(V))","ninf",("V",)],
            ["gna(m,h) := gnabar*m*m*m*h","gna",("m","h")],
            ["gk(n) := gkbar*n*n*n*n","gk",("n",)]]
             

not_bindings = ["dn/dt = (ninf(V)-n)/ntau(V)",
                "il = gl*(V - el)",
                "ik = gk(n)*(V - ek)",
                "dV/dt = (ina + ik + il + Isyn)/C"]
                

class BindingLhsParseTestCase(unittest.TestCase):

    def test_binding_func_arg_parse(self):

        for b in bindings:
            assert nineml.Binding.match(b[0])
            symbol,args,rhs = nineml.Binding.pre_parse(b[0])
            assert symbol == b[1]
            #print args, b[2]
            assert args == b[2]

    def test_negative_matches(self):

        for nb in not_bindings:
            assert not nineml.Binding.match(nb)


    def test_failed_bindings(self):

        # these should work
        b = nineml.Binding("func(x,y)","x**2 + y")
        b = nineml.Binding("f","10")

        # func symbol in func arg
        self.assertRaises(ValueError, nineml.Binding, "func(func,x)","x**2 + func")

        # self reference
        self.assertRaises(ValueError, nineml.Binding, "f", "x**2 + f")

        # redefining math symbols
        self.assertRaises(ValueError, nineml.Binding, "e","10")
        


def suite():

    suite = unittest.makeSuite(BindingLhsParseTestCase,'test')
    return suite

if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
