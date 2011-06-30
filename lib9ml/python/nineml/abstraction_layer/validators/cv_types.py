
#import nineml.abstraction_layer as al
from nineml.abstraction_layer.visitors import ActionVisitor



class ComponentValidatorTypes(ActionVisitor):

    def __init__(self, component):
        self.visit(component)

    def action_componentclass(self, component):
        assert isinstance( component, nineml.al.ComponentClass )
        
    def action_dynamics(self, dynamics):
        assert isinstance( dynamics, nineml.al.Dynamics )

    def action_regime(self,regime):
        assert isinstance( regime, nineml.al.Regime )

    def action_statevariable(self, state_variable):
        assert isinstance( state_variable, nineml.al.StateVariable )

    def action_parameter(self, parameter):
        assert isinstance( parameter, nineml.al.Parameter )

    def action_analogport(self, port, **kwargs):
        assert isinstance( port, nineml.al.AnalogPort )

    def action_eventport(self, port, **kwargs):
        assert isinstance( port, nineml.al.EventPort )

    def action_outputevent(self, output_event, **kwargs):
        assert isinstance( output_event, nineml.al.OutputEvent )

    def action_assignment(self, assignment, **kwargs):
        assert isinstance( assignment, nineml.al.StateAssignment )

    def action_alias(self, alias, **kwargs):
        assert isinstance( alias, nineml.al.Alias )

    def action_timederivative(self,time_derivative,**kwargs):
        assert isinstance( time_derivative, nineml.al.TimeDerivative )

    def action_condition(self, condition):
        assert isinstance( condition, nineml.al.Condition )

    def action_oncondition(self, on_condition):
        assert isinstance( on_condition, nineml.al.OnCondition )

    def action_onevent(self, on_event, **kwargs):
        assert isinstance( on_event, nineml.al.OnEvent )
        
        
