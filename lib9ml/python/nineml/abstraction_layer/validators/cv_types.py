
import nineml.abstraction_layer as al
from nineml.abstraction_layer.visitors import ActionVisitor



class ComponentValidatorTypes(ActionVisitor):

    def __init__(self, component):
        self.visit(component)

    def action_componentclass(self, component):
        assert isinstance( component, al.ComponentClass )
        
    def action_dynamics(self, dynamics):
        assert isinstance( dynamics, al.Dynamics )

    def action_regime(self,regime):
        assert isinstance( regime, al.Regime )

    def action_statevariable(self, state_variable):
        assert isinstance( state_variable, al.StateVariable )

    def action_parameter(self, parameter):
        assert isinstance( parameter, al.Parameter )

    def action_analogport(self, port, **kwargs):
        assert isinstance( port, al.AnalogPort )

    def action_eventport(self, port, **kwargs):
        assert isinstance( port, al.EventPort )

    def action_outputevent(self, output_event, **kwargs):
        assert isinstance( output_event, al.OutputEvent )

    def action_assignment(self, assignment, **kwargs):
        assert isinstance( assignment, al.StateAssignment )

    def action_alias(self, alias, **kwargs):
        assert isinstance( alias, al.Alias )

    def action_timederivative(self,time_derivative,**kwargs):
        assert isinstance( time_derivative, al.TimeDerivative )

    def action_condition(self, condition):
        assert isinstance( condition, al.Condition )

    def action_oncondition(self, on_condition):
        assert isinstance( on_condition, al.OnCondition )

    def action_onevent(self, on_event, **kwargs):
        assert isinstance( on_event, al.OnEvent )
        
        
