""" This tests various features of regex required to automatically determine document element types
in nineml regimes."""


import unittest
import re

# (expr, (lhs,op,rhs))
ODEs = [("dA_x/dt = -A/tau_r",("dA_x/dt","=","-A/tau_r")),
        ("  dB/dt=-B/tau_d",("dB/dt","=","-B/tau_d"))]

Assignments = [("gB = 1/(1 + mg_conc*eta*exp(-1*gamma*V))",("gB","=","1/(1 + mg_conc*eta*exp(-1*gamma*V))")),
               ("g = gB*gmax*(B-A)",("g","=","gB*gmax*(B-A)")),
               (" dA = dt",("dA","=","dt")),
               (" h = dA/dx", ("h","=","dA/dx"))]

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



class ReTestCase(unittest.TestCase):

    def test_eqn(self):

        for t in all_good:
            s = t[0].strip()
            ans = t[1]
            m = p_eqn.match(s)
            if not m:
                print "Failed to match:", s
                assert False
            found = (m.group('lhs'), m.group('op'), m.group('rhs'))
            if found!=ans:
                print "Got wrong lhs,op,rhs parsing"
                print found
                print ans
                assert False


        for s in all_bad:
            m = p_eqn.match(s)
            assert not m
            
        

        #s.strip() to remove preceding and trailing whitespace
        
    def test_odes(self):

        for t in ODEs:
            s = t[0].strip()
            ans = t[1]
            m = p_eqn.match(s)
            if not m:
                print "Failed to match:", s
                assert False
            found = (m.group('lhs'), m.group('op'), m.group('rhs'))
            if found!=ans:
                print "Got wrong lhs,op,rhs parsing"
                print found
                print ans
                assert False

            lhs = m.group('lhs')
            
            # now parse the lhs for dVar/dX
            m = p_ode_lhs.match(lhs)
            assert m

            found = (m.group(1),m.group(2))

            ans = tuple([x[1:] for x in lhs.split('/')])

            if ans!=found:
                print "Got wrong dVar/dX splitting:"
                print found
                print ans
                assert False


        # sure not to match
        for t in Assignments+Inplace:
            s = t[0].strip()
            ans = t[1]
            m = p_eqn.match(s)
            if not m:
                print "Failed to match:", s
                assert False
            found = (m.group('lhs'), m.group('op'), m.group('rhs'))
            if found!=ans:
                print "Got wrong lhs,op,rhs parsing"
                print found
                print ans
                assert False

            lhs = m.group('lhs')
            
            # now parse the lhs for dVar/dX
            m = p_ode_lhs.match(lhs)
            assert not m
            
            



        


def suite():

    suite = unittest.makeSuite(ReTestCase,'test')
    return suite

if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
