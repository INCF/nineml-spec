
from itertools import chain



class ComponentVisitor(object):
    def visit( self,obj,**kwargs):
        return obj.accept_visitor(self,**kwargs)
    
    def visit( self,obj,**kwargs):
        return obj.accept_visitor(self,**kwargs)




class InplaceActionVisitorDF(ComponentVisitor):

    def __init__(self, explicitly_require_action_overrides=True):
        self.explicitly_require_action_overrides = explicitly_require_action_overrides


    def visit_componentclass(self, component, **kwargs):
        self.action_componentclass(component, **kwargs)

        
        nodes = chain(component.parameters, component.analog_ports, component.event_ports )
        for p in nodes:
            p.accept_visitor(self, **kwargs)

        if component.dynamics:
            component.dynamics.accept_visitor(self,**kwargs)

        for subnode in component.subnodes.values():
            subnode.accept_visitor(self, **kwargs)

    

    def visit_dynamics(self, dynamics, **kwargs):
        self.action_dynamics(dynamics, **kwargs)
        nodes = chain(dynamics.regimes, dynamics.aliases, dynamics.state_variables)
        for p in nodes:
            p.accept_visitor(self, **kwargs)
        
    def visit_regime(self,regime, **kwargs):
        self.action_regime(regime, **kwargs)
        nodes = chain(regime.time_derivatives, regime.on_events, regime.on_conditions)
        for p in nodes:
            p.accept_visitor(self, **kwargs)

    def visit_statevariable(self, state_variable, **kwargs):
        self.action_statevariable( state_variable, **kwargs) 

    def visit_parameter(self, parameter, **kwargs):
        self.action_parameter(parameter, **kwargs)

    def visit_analogport(self, port, **kwargs):
        self.action_analogport(port, **kwargs)

    def visit_eventport(self, port, **kwargs):
        self.action_eventport(port, **kwargs)

    def visit_outputevent(self, output_event, **kwargs):
        self.action_outputevent(output_event, **kwargs)

    def visit_inputevent(self, input_event, **kwargs):
        self.action_inputevent(input_event, **kwargs)

    def visit_assignment(self, assignment, **kwargs):
        self.action_assignment(assignment, **kwargs)

    def visit_alias(self, alias, **kwargs):
        self.action_alias(alias, **kwargs)

    def visit_timederivative(self,ode,**kwargs):
        self.action__timederivative(ode, **kwargs)

    def visit_condition(self, condition, **kwargs):
        self.action_condition(condition, **kwargs)

    def visit_oncondition(self, on_condition, **kwargs):
        self.action_oncondition(on_condition, **kwargs)
        nodes = chain([on_condition.trigger], on_condition.event_outputs, on_condition.state_assignments)
        for p in nodes:
            p.accept_visitor(self, **kwargs)

    def visit_onevent(self, on_event, **kwargs):
        self.action_onevent(on_event, **kwargs)
        nodes = chain( on_event.event_outputs, on_event.state_assignments)
        for p in nodes:
            p.accept_visitor(self, **kwargs)



    def check_pass(self):
        if self.explicitly_require_action_overrides:
            assert False, "There is some over-riding missing"
        else:
            pass
        
    # To be overridden:
    def action_componentclass(self, component,  **kwargs):
        self.check_pass()
        
    def action_dynamics(self, dynamics, **kwargs):
        self.check_pass()
        
    def action_regime(self,regime,  **kwargs):
        self.check_pass()
        
    def action_statevariable(self, state_variable, **kwargs):
        self.check_pass()
        
    def action_parameter(self, parameter, **kwargs):
        self.check_pass()
        
    def action_analogport(self, port, **kwargs):
        self.check_pass()
        
    def action_eventport(self, port, **kwargs):
        self.check_pass()
        
    def action_outputevent(self, output_event, **kwargs):
        self.check_pass()
        
    #def action_inputevent(self, input_event, **kwargs):
    #    self.check_pass()
        
    def action_assignment(self, assignment, **kwargs):
        self.check_pass()
        
    def action_alias(self, alias, **kwargs):
        self.check_pass()
        
    def action__timederivative(self,ode, **kwargs):
        self.check_pass()
        
    def action_condition(self, condition, **kwargs):
        self.check_pass()
        
    def action_oncondition(self, on_condition, **kwargs):
        self.check_pass()
        
    def action_onevent(self, on_event, **kwargs):
        self.check_pass()


