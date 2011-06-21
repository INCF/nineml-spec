
from nineml.utility import safe_dict

class UnequalException(Exception):
    def __init__(self, o1, o2, msg = "" ):
        Exception.__init__(self )
        self.msg = msg
        self.o1 = o1
        self.o2 = o2

    def __str__(self):
        s = self.msg
        s += '\nO1: %s' % str(self.o1)
        s += '\nO2: %s' % str(self.o2)
        return s




import types

def assert_equal( o1, o2, msg= '' ):
    
    if not isinstance(o1, (types.NoneType, basestring)):
        print o1, o2
        assert not (o1 is o2)
    
    assert type(o1) == type(o2)
    assert isinstance(o1, (basestring, types.NoneType))

    if o1 == o2: 
        return

    raise UnequalException( o1, o2, msg )



def assert_equal_list( o1, o2, msg= '', do_sort=True ):
    assert not o1 is o2
    assert type(o1) == type(o2)
    assert isinstance(o1, (tuple,list) )


    if sorted(o1) == sorted(o2): 
        return

    raise UnequalException( sorted(o1), sorted(o2), msg )






class ComponentEqualityChecker(object):

    @classmethod
    def check_equal(cls,  comp1, comp2, strict_aliases=True):
        """Forwarding Function :Easier Interface"""
        cls.check_equal_component(comp1, comp2, strict_aliases=strict_aliases)



    @classmethod
    def check_equal_component(cls, comp1, comp2, strict_aliases):

        # Check the component names are equal:
        assert_equal( comp1.name, comp2.name, 'Component Names' )
         
        
        #CHECK THE INTERFACE:
        # -------------------#
        # Parameters:
        p1Names = sorted([ p.name for p in comp1.parameters ])
        p2Names = sorted([ p.name for p in comp2.parameters ])
        assert_equal_list(p1Names, p2Names)


        # Analog Ports: Check Modes & reduce ops:
        ap1Dict = safe_dict([ (ap.name, ap) for ap in comp1.analog_ports ])
        ap2Dict = safe_dict([ (ap.name, ap) for ap in comp2.analog_ports ])
        assert_equal_list( ap1Dict.keys(), ap2Dict.keys() )
        for portname in ap1Dict.keys():
            assert_equal( ap1Dict[portname].mode, ap2Dict[portname].mode )
            assert_equal( ap1Dict[portname].reduce_op, ap2Dict[portname].reduce_op )

        # Event Ports: Check Modes & reduce ops:
        ev1Dict = safe_dict([ (ev.name, ev) for ev in comp1.event_ports ])
        ev2Dict = safe_dict([ (ev.name, ev) for ev in comp2.event_ports ])
        assert_equal_list( ev1Dict.keys(), ev2Dict.keys() )
        for portname in ev1Dict.keys():
            assert_equal( ev1Dict[portname].mode, ev2Dict[portname].mode )
            assert_equal( ev1Dict[portname].reduce_op, ev2Dict[portname].reduce_op )


        # CHECK THE SUBNAMESPACES AND PORT CONNECTIONS
        # ------------------------------------------- #

        # Recurse over subnamespaces:
        assert_equal_list( comp1.subnodes.keys(), comp2.subnodes.keys() )
        for subnamespace in comp1.subnodes.keys():
            subcomp1 = comp1.subnodes[subnamespace]
            subcomp2 = comp2.subnodes[subnamespace]
            check_equal_component( subcomp1, subcomp2 )

        # Port Connections:
        # Tuples are comparable, so lets make 2 lists of tuples and compare
        # them:
        pc1 = [ (src.loctuple, sink.loctuple) for (src,sink) in comp1.portconnections] 
        pc2 = [ (src.loctuple, sink.loctuple) for (src,sink) in comp2.portconnections]  
        assert_equal_list(pc1, pc2)



        # CHECK THE DYNAMICS
        # ------------------- #
        d1 = comp1.dynamics
        d2 = comp2.dynamics
        
        # Check Aliases:
        assert strict_aliases
        a1 = [ (a.lhs, a.rhs) for a in d1.aliases ]
        a2 = [ (a.lhs, a.rhs) for a in d2.aliases ]
        assert_equal_list(a1,a2)

        # State Variables:
        sv1Names = sorted([ sv.name for sv in d1.state_variables ])
        sv2Names = sorted([ sv.name for sv in d2.state_variables ])
        assert_equal_list(sv1Names, sv2Names)

        # Check Regimes:

        rgm1Dict = d1.regime_map
        rgm2Dict = d2.regime_map
        assert_equal_list( rgm1Dict.keys(), rgm2Dict.keys() )
        for regime_name in rgm1Dict.keys():
            rgm1 = rgm1Dict[regime_name]
            rgm2 = rgm2Dict[regime_name]
            cls.check_equal_regime( rgm1, rgm2)



    @classmethod
    def check_equal_regime(cls, rgm1, rgm2):

        assert_equal( rgm1.name, rgm2.name)
        
        # Check the OnEvents:
        on_event1dict = safe_dict([ (ev.src_port_name, ev) for ev in rgm1.on_events ])
        on_event2dict = safe_dict([ (ev.src_port_name, ev) for ev in rgm2.on_events ])
        assert_equal_list( on_event1dict.keys(), on_event2dict.keys() )
        for eventport in on_event1dict.keys():
            ev1 = on_event1dict[eventport]
            ev2 = on_event2dict[eventport]
            cls.check_equal_transitions(ev1, ev2)


        # Check the OnEvents:
        # [We use safe_dict, to ensure that we don't have any duplicate
        # condition.rhs ]
        on_condition1dict = safe_dict([ (cond.trigger.rhs, cond) for cond in rgm1.on_conditions ])
        on_condition2dict = safe_dict([ (cond.trigger.rhs, cond) for cond in rgm2.on_conditions ])
        assert_equal_list( on_condition1dict.keys(), on_condition2dict.keys() )
        for condition_trigger_rhs in on_condition1dict.keys():
            on_cond1 = on_condition1dict[condition_trigger_rhs]
            on_cond2 = on_condition2dict[condition_trigger_rhs]
            cls.check_equal_transitions(on_cond1, on_cond2)

        # Check the TimeDerivatives:
        time_deriv1s = [ (td.dependent_variable, td.rhs) for td in rgm1.time_derivatives ]
        time_deriv2s = [ (td.dependent_variable, td.rhs) for td in rgm2.time_derivatives ]
        assert_equal_list( time_deriv1s, time_deriv2s )




    @classmethod
    def check_equal_transitions(cls, trans1, trans2):

        # Check they are connecting the same named regions:
        assert_equal( trans1.source_regime.name, trans2.source_regime.name)
        assert_equal( trans1.target_regime.name, trans2.target_regime.name)

        # State Assignments:
        sa1 = [ (sa.lhs, sa.rhs) for sa in trans1.state_assignments ]
        sa2 = [ (sa.lhs, sa.rhs) for sa in trans2.state_assignments ]
        assert_equal_list( sa1, sa2 )

        # Output Events:
        op1 = [ op.port_name for op in trans1.event_outputs ]
        op2 = [ op.port_name for op in trans2.event_outputs ]
        assert_equal_list( sa1, sa2 )
        


