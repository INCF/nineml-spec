
from itertools import chain



class ComponentVisitor(object):
    def Visit( self,obj,**kwargs):
        return obj.accept_visitor(self,**kwargs)
    
    def visit( self,obj,**kwargs):
        return obj.accept_visitor(self,**kwargs)




class InplaceActionVisitorDF(ComponentVisitor):

    def __init__(self, explicitly_require_action_overrides=True):
        self.explicitly_require_action_overrides = explicitly_require_action_overrides


    def VisitComponentClass(self, component, **kwargs):
        self.ActionComponentClass(component, **kwargs)

        
        nodes = chain(component.parameters, component.analog_ports, component.event_ports )
        for p in nodes:
            p.accept_visitor(self, **kwargs)

        if component.dynamics:
            component.dynamics.accept_visitor(self,**kwargs)

        for subnode in component.subnodes.values():
            subnode.accept_visitor(self, **kwargs)

    

    def VisitDynamics(self, dynamics, **kwargs):
        self.ActionDynamics(dynamics, **kwargs)
        nodes = chain(dynamics.regimes, dynamics.aliases, dynamics.state_variables)
        for p in nodes:
            p.accept_visitor(self, **kwargs)
        
    def VisitRegime(self,regime, **kwargs):
        self.ActionRegime(regime, **kwargs)
        nodes = chain(regime.time_derivatives, regime.on_events, regime.on_conditions)
        for p in nodes:
            p.accept_visitor(self, **kwargs)

    def VisitStateVariable(self, state_variable, **kwargs):
        self.ActionStateVariable( state_variable, **kwargs) 

    def VisitParameter(self, parameter, **kwargs):
        self.ActionParameter(parameter, **kwargs)

    def VisitAnalogPort(self, port, **kwargs):
        self.ActionAnalogPort(port, **kwargs)

    def VisitEventPort(self, port, **kwargs):
        self.ActionEventPort(port, **kwargs)

    def VisitOutputEvent(self, output_event, **kwargs):
        self.ActionOutputEvent(output_event, **kwargs)

    def VisitInputEvent(self, input_event, **kwargs):
        self.ActionInputEvent(input_event, **kwargs)

    def VisitAssignment(self, assignment, **kwargs):
        self.ActionAssignment(assignment, **kwargs)

    def VisitAlias(self, alias, **kwargs):
        self.ActionAlias(alias, **kwargs)

    def VisitTimeDerivative(self,ode,**kwargs):
        self.ActionODE(ode, **kwargs)

    def VisitCondition(self, condition, **kwargs):
        self.ActionCondition(condition, **kwargs)

    def VisitOnCondition(self, on_condition, **kwargs):
        self.ActionOnCondition(on_condition, **kwargs)
        nodes = chain([on_condition.trigger], on_condition.event_outputs, on_condition.state_assignments)
        for p in nodes:
            p.accept_visitor(self, **kwargs)

    def VisitOnEvent(self, on_event, **kwargs):
        self.ActionOnEvent(on_event, **kwargs)
        nodes = chain( on_event.event_outputs, on_event.state_assignments)
        for p in nodes:
            p.accept_visitor(self, **kwargs)



    def check_pass(self):
        if self.explicitly_require_action_overrides:
            assert False, "There is some over-riding missing"
        else:
            pass
        
    # To be overridden:
    def ActionComponentClass(self, component,  **kwargs):
        self.check_pass()
        
    def ActionDynamics(self, dynamics, **kwargs):
        self.check_pass()
        
    def ActionRegime(self,regime,  **kwargs):
        self.check_pass()
        
    def ActionStateVariable(self, state_variable, **kwargs):
        self.check_pass()
        
    def ActionParameter(self, parameter, **kwargs):
        self.check_pass()
        
    def ActionAnalogPort(self, port, **kwargs):
        self.check_pass()
        
    def ActionEventPort(self, port, **kwargs):
        self.check_pass()
        
    def ActionOutputEvent(self, output_event, **kwargs):
        self.check_pass()
        
    #def ActionInputEvent(self, input_event, **kwargs):
    #    self.check_pass()
        
    def ActionAssignment(self, assignment, **kwargs):
        self.check_pass()
        
    def ActionAlias(self, alias, **kwargs):
        self.check_pass()
        
    def ActionODE(self,ode, **kwargs):
        self.check_pass()
        
    def ActionCondition(self, condition, **kwargs):
        self.check_pass()
        
    def ActionOnCondition(self, on_condition, **kwargs):
        self.check_pass()
        
    def ActionOnEvent(self, on_event, **kwargs):
        self.check_pass()


