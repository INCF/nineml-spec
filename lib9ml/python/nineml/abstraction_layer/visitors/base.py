
from itertools import chain



class ComponentVisitor(object):
    def Visit( self,obj,**kwargs):
        return obj.AcceptVisitor(self,**kwargs)
    




class InplaceActionVisitorDF(ComponentVisitor):


    def VisitComponentClass(self, componentNodeCombined):
        self.ActionComponentClass(componentNodeCombined)

        comp = componentNodeCombined
        nodes = chain(comp.parameters, comp.analog_ports, comp.event_ports, )
        for p in nodes:
            p.AcceptVisitor(self)

        if comp.dynamics:
            comp.dynamics.AcceptVisitor(self)

        for subnode in componentNodeCombined.subnodes.values():
            subnode.AcceptVisitor(self)

    

    def VisitDynamics(self, dynamics):
        self.ActionDynamics(dynamics)
        nodes = chain(dynamics.regimes, dynamics.aliases, dynamics.state_variables)
        for p in nodes:
            p.AcceptVisitor(self)
        
    def VisitRegime(self,regime):
        self.ActionRegime(regime)
        nodes = chain(regime.time_derivatives, regime.on_events, regime.on_conditions)
        for p in nodes:
            p.AcceptVisitor(self)

    def VisitStateVariable(self, state_variable):
        self.ActionStateVariable( state_variable) 

    def VisitParameter(self, parameter):
        self.ActionParameter(parameter)

    def VisitAnalogPort(self, port, **kwargs):
        self.ActionAnalogPort(port)

    def VisitEventPort(self, port, **kwargs):
        self.ActionEventPort(port)

    def VisitOutputEvent(self, output_event, **kwargs):
        self.ActionOutputEvent(output_event)

    def VisitInputEvent(self, input_event, **kwargs):
        self.ActionInputEvent(input_event)

    def VisitAssignment(self, assignment, **kwargs):
        self.ActionAssignment(assignment)

    def VisitAlias(self, alias, **kwargs):
        self.ActionAlias(alias)

    def VisitODE(self,ode,**kwargs):
        self.ActionODE(ode)

    def VisitCondition(self, condition):
        self.ActionCondition(condition)

    def VisitOnCondition(self, on_condition):
        self.ActionOnCondition(on_condition)
        nodes = chain([on_condition.trigger], on_condition.event_outputs, on_condition.state_assignments)
        for p in nodes:
            p.AcceptVisitor(self)

    def VisitOnEvent(self, on_event, **kwargs):
        self.ActionOnEvent(on_event)
        nodes = chain( on_event.event_outputs, on_event.state_assignments)
        for p in nodes:
            p.AcceptVisitor(self)


    # To be overridden:
    def ActionComponentClass(self, component_node_combined):
        pass
    def ActionDynamics(self, dynamics):
        pass
    def ActionRegime(self,regime):
        pass
    def ActionStateVariable(self, state_variable):
        pass
    def ActionParameter(self, parameter):
        pass
    def ActionAnalogPort(self, port, **kwargs):
        pass
    def ActionEventPort(self, port, **kwargs):
        pass
    def ActionOutputEvent(self, output_event, **kwargs):
        pass
    def ActionInputEvent(self, input_event, **kwargs):
        pass
    def ActionAssignment(self, assignment, **kwargs):
        pass
    def ActionAlias(self, alias, **kwargs):
        pass
    def ActionODE(self,ode,**kwargs):
        pass
    def ActionCondition(self, condition):
        pass
    def ActionOnCondition(self, on_condition):
        pass
    def ActionOnEvent(self, on_event, **kwargs):
        pass


