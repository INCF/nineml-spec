
import unittest
import nineml.abstraction_layer as nineml

import os, tempfile



class ComponentTestCase(unittest.TestCase):

    def test_expressions(self):
        from nineml.abstraction_layer import expr_to_obj

        # no redefining or modifying math symbols
        self.assertRaises(ValueError, expr_to_obj,"pi:=11")
        self.assertRaises(ValueError, expr_to_obj,"x:=10+x")

        self.assertRaises(ValueError, expr_to_obj,"dpi/dt = 10+x")
        self.assertRaises(ValueError, expr_to_obj,"de/dt = 10+x")

        self.assertRaises(ValueError, expr_to_obj,"e = 10+x")
        self.assertRaises(ValueError, expr_to_obj,"pi = 10+x")

        self.assertRaises(ValueError, expr_to_obj,"pi += 10")
        self.assertRaises(ValueError, expr_to_obj,"e += 10")

        # undefined functions
        #self.assertRaises(ValueError, expr_to_obj,"U = WhatFunc(x)")


        # assignment self referencing detection
        e = expr_to_obj("U = U+1")
        assert e.self_referencing()
        
        e = expr_to_obj("U = V+1")
        assert not e.self_referencing()

    def test_alias_backsub(self):
        from nineml.abstraction_layer import expr_to_obj, get_args

        # Determine missing functions
        e = expr_to_obj("U(x,y):= exp(x) + y")
        assert list(e.missing_functions)==[]

        e = expr_to_obj("dA/dt = exp(x) + whatfunc(u)")
        assert list(e.missing_functions) == ['whatfunc']

        e = expr_to_obj("U = exp(x) + q(u,U)")
        assert list(e.missing_functions) == ['q']

        e = expr_to_obj("U(x,y):= exp(x) + _q10(y,x)")
        assert list(e.missing_functions) == ['_q10']

        e = expr_to_obj("U(x,y):= exp(x) + _q10(y,x)")
        b = expr_to_obj("_q10 := 20")
        self.assertRaises(ValueError, e.substitute_alias, b)

        e = expr_to_obj("U = exp(x) + _q10")
        e.substitute_alias(b)
        #print e.rhs
        assert e.rhs == "exp(x) + (20)"

        i,args = get_args("exp(exp(z)+1),cos(sin(q)-tan(z))) + 1/_q10(z,w)")
        assert args == ['exp(exp(z)+1)', 'cos(sin(q)-tan(z))']
        
    
        e = expr_to_obj("U(x,y):= exp(x) + _q10(y,x) + 1/_q10(z,w)")
        b = expr_to_obj("_q10(a,b) := a+b")
        e.substitute_alias(b)
        #print e.rhs
        assert e.rhs == "exp(x) + (y+x) + 1/(z+w)"

        e = expr_to_obj("dA/dt = exp(x) + _q10(exp(exp(z)+1),cos(sin(q)-tan(z))) + 1/_q10(z,w)")
        b = expr_to_obj("_q10(a,b) := a+b")
        e.substitute_alias(b)
        assert e.rhs == "exp(x) + (exp(exp(z)+1)+cos(sin(q)-tan(z))) + 1/(z+w)"

        # check that it does all levels of alias resolution (i.e. also in arguments of a alias)
        e = expr_to_obj("dA/dt = exp(x) + _q10(_q10(exp(z),1),cos(sin(q)-tan(z))) + 1/_q10(z,w)")
        b = expr_to_obj("_q10(a,b) := a+b")
        e.substitute_alias(b)
        assert e.rhs == "exp(x) + ((exp(z)+1)+cos(sin(q)-tan(z))) + 1/(z+w)"

        # catch number of args mismatch
        e = expr_to_obj("U(x,y):= exp(x) + _q10(y,x,z) + 1/_q10(z,w)")
        self.assertRaises(ValueError, e.substitute_alias, b)
        
        # check recursive alias is caught
        self.assertRaises(ValueError, expr_to_obj, "a(x) := a(x) + 1")

        # check
        b1 = expr_to_obj("v1(x) := exp(x)")
        b2 = expr_to_obj("p := e")
        b1.substitute_alias(b2)
        #print b1.rhs
        assert b1.rhs == "exp(x)"
        

        # check catch of alias rhs having no dependance on lhs args
        self.assertRaises(ValueError, expr_to_obj, "a(x) := exp(z) + 1")
        self.assertRaises(ValueError, expr_to_obj, "a(x,y,z) := exp(z) + x")
        b = expr_to_obj("a(x,y,z) := exp(z) + x + y")

        b1 = expr_to_obj("v1(x) := 1/v2(x+1,v3(x)+1) + v3(x)")
        b2 = expr_to_obj("v2(x,y) := v3(x)*y + 10")
        b3 = expr_to_obj("v3(x) := exp(x)")
        b2.substitute_alias(b3)
        #print b2.rhs
        assert b2.rhs == "(exp(x))*y + 10"
        b1.substitute_alias(b2)
        #print b1.rhs
        assert b1.rhs == "1/((exp(x+1))*v3(x)+1 + 10) + v3(x)"
        b1.substitute_alias(b3)
        #print b1.rhs
        assert b1.rhs == "1/((exp(x+1))*(exp(x))+1 + 10) + (exp(x))"
        #assert e.rhs == "exp(x) + ((exp(z)+1)+cos(sin(q)-tan(z))) + 1/(z+w)"



        # now to components

        aliases = [
            "v1(x) := 1/v2(x+1,v3(x)+1) + v3(x)",
            "v2(x,y) := v3(x)*y + 10",
            "v3(x) := exp(x)**2"
            ]
        r = nineml.Regime("dV/dt = 0.04*V*V + 5*V + 140.0 - U + Isyn + v1(v3(x))")

        c1 = nineml.Component("Izhikevich", regimes = [r], aliases=aliases )
        c1.backsub_aliases()
        c1.backsub_equations()

        bm = c1.aliases_map
        assert bm['v1'].rhs == "1/((exp(x+1)**2)*(exp(x)**2)+1 + 10) + (exp(x)**2)"
        assert bm['v2'].rhs == "(exp(x)**2)*y + 10"
        assert bm['v3'].rhs == "exp(x)**2"
        for e in c1.equations:
            assert e.rhs == "0.04*V*V + 5*V + 140.0 - U + Isyn + (1/((exp((exp(x)**2)+1)**2)*(exp((exp(x)**2))**2)+1 + 10) + (exp((exp(x)**2))**2))"

        # TODO More tests here ... Although the basic functionality is there,
        # as the code is custom written, some syntactic differences might still
        # cause problems.  An implementation using sympy might also be an option
        # to consider ...

    def test_ports_construction(self):

        # Check catches bad mode
        self.assertRaises(ValueError, nineml.AnalogPort,"q",mode="11")

        # Check catches op on non-reduce port
        self.assertRaises(ValueError, nineml.AnalogPort,"q",mode="send", op='+')

        # Check catches bad op for reduce port
        self.assertRaises(ValueError, nineml.AnalogPort,"q",mode="reduce", op='^')

        # No expressions for 'recv','reduce'
        for mode in ('recv','reduce'):
            self.assertRaises(ValueError, nineml.AnalogPort,"q = v**2",mode=mode)

        # Check symbol as expression ...
        p = nineml.AnalogPort("q = v**2",mode='send')
        assert p.symbol == 'q'
        assert p.expr.rhs == "v**2"

        # catch a alias expression ...
        self.assertRaises(ValueError, nineml.AnalogPort, "q := v**2",mode='send')
        # time_derivative
        self.assertRaises(ValueError, nineml.AnalogPort, "dq/dt = v**2",mode='send')
        # inplace
        self.assertRaises(ValueError, nineml.AnalogPort, "q += 10",mode='send')

    def test_find_port_expr_symbols(self):
        # check that symbols on rhs of a 'send' port expr
        # are considered for user parameters

        parameters = ['tau','E','q']
        
        regimes = [
            nineml.Regime(
                "dg/dt = -g/tau",
                transitions = nineml.On(nineml.SpikeInputEvent,do="g+=q")
                )]

        ports = [nineml.RecvPort("V"),
                 nineml.SendPort("Isyn = g(E-V)")]

        coba_syn = nineml.Component("CoBaSynapse", regimes = regimes, ports = ports)

        assert sorted(parameters) == sorted(coba_syn.parameters)
        

    def test_implicit_send_ports(self):
        # test that send ports are implicit for component variables
        r = nineml.Regime(
            "dV/dt = 0.04*V*V + 5*V + 140.0 - U + Isyn",
            transitions = [nineml.On(nineml.SpikeInputEvent,do="V+=10"),
                      nineml.On("V>Vth",do=["V=c","U+=d",nineml.SpikeOutputEvent])])

        c1 = nineml.Component("Izhikevich", regimes = [r], ports=[nineml.AnalogPort("V")] )
        ap = list(c1.analog_ports)

        assert sorted([p.symbol for p in ap]) == sorted(['V','U','t'])


    def test_ports_diverse(self):

        r = nineml.Regime(
            "dV/dt = 0.04*V*V + 5*V + 140.0 - U + Isyn",
            transitions = [nineml.On(nineml.SpikeInputEvent,do="V+=10"),
                      nineml.On("V>Vth",do=["V=c","U+=d",nineml.SpikeOutputEvent])])

        c1 = nineml.Component("Izhikevich", regimes = [r], ports=[nineml.AnalogPort("V")] )
        ep = list(c1.event_ports)
        assert len(ep)==2

        # test some port filtering, get with symb="V"
        ep = list(c1.filter_ports(symb="V"))
        assert len(ep)==1
        assert ep[0]==nineml.AnalogPort("V")

        # get all the event ports
        ep = list(c1.filter_ports(cls=nineml.EventPort))
        assert len(ep)==2
        assert nineml.SpikeInputEvent in ep
        assert nineml.SpikeOutputEvent in ep
        
        # get just the "recv" EventPort
        ep = list(c1.filter_ports(mode="recv", cls=nineml.EventPort))
        assert len(ep)==1
        assert ep[0]==nineml.SpikeInputEvent


        # check that Transition catches condition in mode="send"
        self.assertRaises(ValueError, nineml.On,nineml.SpikeOutputEvent,do="V+=10" )
        # check that it won't accept a simple Port
        self.assertRaises(ValueError, nineml.On,nineml.Port("hello",mode="recv"),do="V+=10" )
        # check that it won't accept an AnalogPort
        self.assertRaises(ValueError, nineml.On,nineml.AnalogPort("hello",mode="recv"),do="V+=10" )

        # user defined EventPort should be ok.
        e = nineml.On(nineml.EventPort("hello",mode="recv"),do="V+=10" )

        
        r = nineml.Regime(
            "_q10(V):=exp(V)",
            "dV/dt = 0.04*V*V + 5*V + 140.0 - U + Isyn",
            transitions = nineml.On(nineml.SpikeInputEvent,do="V+=10"))

        # ok to read from a alias, where function aliases are most interesting.
        # TODO: read from alias
        #c1 = nineml.Component("Izhikevich", regimes = [r], ports=[nineml.AnalogPort("_q10","send")] )
        # may not write to a alias
        self.assertRaises(ValueError,nineml.Component, "Izhikevich", regimes = [r], ports=[nineml.AnalogPort("_q10","recv")])

        # may not read from an undefined symbol
        self.assertRaises(ValueError,nineml.Component, "Izhikevich", regimes = [r], ports=[nineml.AnalogPort("_q11","send")])

        # Should be AnalogPort
        self.assertRaises(ValueError,nineml.Component, "Izhikevich", regimes = [r], ports=[nineml.Port("_q10","send")])

        # Should be AnalogPort
        self.assertRaises(ValueError,nineml.Component, "Izhikevich", regimes = [r], ports=[nineml.EventPort("_q10","send")])


        # EventPorts as nodes in Transitions

        # multiple EventPorts
        myeventport = nineml.EventPort('myeventport',mode="send")
        r = nineml.Regime(
            "dV/dt = 0.04*V*V + 5*V + 140.0 - U + Isyn",
            transitions = nineml.On("V>Vth",do=[nineml.SpikeOutputEvent,myeventport ]))

        c1 = nineml.Component("Izhikevich", regimes = [r] )
        ep = list(c1.event_ports)
        assert len(ep)==2

        assert ep[0]==nineml.SpikeOutputEvent
        assert ep[1]==myeventport


        # ok
        e = nineml.On("V>Vth", do=nineml.EventPort("hello",mode="send"))
        # not ok: do=EventPort cannot recv
        self.assertRaises(ValueError, nineml.On, "V>Vth", do=nineml.EventPort("hello",mode="recv"))



    def test_regime_basic(self):

        # no self-referencing Assignments in Regimes
        self.assertRaises(ValueError,nineml.Regime,"U = U+1")

            # no self-referencing Inplace ops in Regime
        self.assertRaises(ValueError,nineml.Regime,"U += 10")


    def test_regime_symbol_collision(self):

        self.assertRaises(ValueError,nineml.Regime,"dU/dt = -U", "dU/dt = -U +10" )
        
    def test_expression_interface(self):

        # Guarantee Expression interface:
        # e.rhs, e.lhs, e.to, e.as_expr()

        from nineml.abstraction_layer import expr_to_obj

        exprs = ["dU/dt = -U",
                 "U = 10",
                 "U := 20",
                 "U += 10",
                 "U(V) := exp(V)"]

        rhs_s = ["-U", "10","20","10","exp(V)"]
        lhs_s = ["dU/dt","U","U","U","U(V)"]
        to_s = ["U"]*5

        objs = map(expr_to_obj,exprs)

        for e,lhs in zip(objs,lhs_s):
            assert e.lhs == lhs

        for e,rhs in zip(objs,rhs_s):
            assert e.rhs == rhs

        for e,to in zip(objs,to_s):
            assert e.to == to

        for e,expr in zip(objs,exprs):
            assert e.as_expr()==expr
        

            

        


    def test_regime_transitions_with_target(self):

        # Test Regime.transitions_with_target
        
        # no transitions, we should have no targets
        u = nineml.Regime("dU/dt = -U")
        assert not list(u.transitions_with_target)

        # check we get a target with 'On' sugar
        u = nineml.Regime("dU/dt = -U", transitions=[nineml.On("V>10",to="test")])
        assert list(u.transitions_with_target)

        # test we have no target if we don't define one in the transition
        u = nineml.Regime("dU/dt = -U", transitions=[nineml.Transition("U+=10",condition="U>10")])
        assert not list(u.transitions_with_target)

        # test we get a target if we define it
        u = nineml.Regime("dU/dt = -U", transitions=[nineml.Transition("U+=10",condition="U>10",to="test")])
        assert list(u.transitions_with_target)


    def test_transition_construction(self):

        t = nineml.Transition(to=nineml.Reference(nineml.Regime,"test"), condition = "V>10", do="V=10")
        
        
    def test_transition(self):

        e = nineml.Transition("A+=10",condition="A>10")

        # Transition must have condition (otherwise it is not temporally sparse
        self.assertRaises(ValueError, nineml.Transition, "A+=10")

        # Transition conditional may not be true or false
        # as the former violates temporal sparsesness,
        # the latter neuters the Transition.
        self.assertRaises(ValueError, nineml.Transition, "A+=10", condition="true")
        self.assertRaises(ValueError, nineml.Transition, "A+=10", condition="false")

        # No TimeDerivatives in Transitions
        self.assertRaises(ValueError, nineml.Transition, "dA/dt = -A", condition="A>10")

        e = nineml.Transition("A+=10", condition="A>10", to="test")
        e = nineml.Transition("A+=10", condition="A>10", to=None)
        e = nineml.Transition("A+=10", condition="A>10", to=nineml.Reference(nineml.Regime,"test"))
        e = nineml.Transition("A+=10", condition="A>10", to=nineml.Reference(nineml.Regime,"test"))


        t = nineml.Transition(to=nineml.Reference(nineml.Regime,"test"), condition = "V>10")
                
        self.assertRaises(ValueError, nineml.Transition, "A+=10", condition="A>10", to=t)
        self.assertRaises(ValueError, nineml.Transition, "A+=10", condition="A>10", to=nineml.Reference(nineml.Transition,"test"))


    def test_component(self):
    
        r = nineml.Regime(
            "_q10(V):=exp(V)",
            "dV/dt = 0.04*V*V + 5*V + 140.0 - U + Isyn",
            transitions = nineml.On(nineml.SpikeInputEvent,do=["V+=10", "U+=d"]))

        #TODO: ok to read from a alias, where function aliases are most interesting.
        #p_q10 = nineml.AnalogPort("_q10(V)","send")
        #ports = [p_q10]
        c1 = nineml.Component("Izhikevich", regimes = [r] )

        # attribute lookup for ports and user parameters:

        #assert c1._q10 == p_q10
        # an implicit port
        assert c1.U.symbol == 'U'

        



def suite():

    suite = unittest.makeSuite(ComponentTestCase,'test')
    return suite

if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
