
import nineml.abstraction_layer as al
from nineml.abstraction_layer.visitors import InplaceActionVisitorDF



class ComponentValidatorTypes(InplaceActionVisitorDF):

    def __init__(self, component):
        self.Visit(component)

    def ActionComponentClass(self, component):
        assert isinstance( component, al.ComponentClass )
        
    def ActionDynamics(self, dynamics):
        assert isinstance( dynamics, al.Dynamics )

    def ActionRegime(self,regime):
        assert isinstance( regime, al.Regime )

    def ActionStateVariable(self, state_variable):
        assert isinstance( state_variable, al.StateVariable )

    def ActionParameter(self, parameter):
        assert isinstance( parameter, al.Parameter )

    def ActionAnalogPort(self, port, **kwargs):
        assert isinstance( port, al.AnalogPort )

    def ActionEventPort(self, port, **kwargs):
        assert isinstance( port, al.EventPort )

    def ActionOutputEvent(self, output_event, **kwargs):
        assert isinstance( output_event, al.OutputEvent )

    def ActionInputEvent(self, input_event, **kwargs):
        assert isinstance( input_event, al.InputEvent )

    def ActionAssignment(self, assignment, **kwargs):
        assert isinstance( assignment, al.StateAssignment )

    def ActionAlias(self, alias, **kwargs):
        assert isinstance( alias, al.Alias )

    def ActionODE(self,ode,**kwargs):
        assert isinstance( ode, al.TimeDerivative )

    def ActionCondition(self, condition):
        assert isinstance( condition, al.Condition )

    def ActionOnCondition(self, on_condition):
        assert isinstance( on_condition, al.OnCondition )

    def ActionOnEvent(self, on_event, **kwargs):
        assert isinstance( on_event, al.OnEvent )
        
        