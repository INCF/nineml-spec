

import nineml.abstraction_layer as al
from nineml.abstraction_layer.visitors import InplaceActionVisitorDF
from nineml.utility import *


from itertools import chain


# Check that the sub-components stored are all of the
# right types:
class ComponentTypeChecker(InplaceActionVisitorDF):

    def ActionComponent(self, component):
        assert isinstance( component, al.models.ComponentNode )

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
        assert isinstance( assignment, al.Assignment )

    def ActionAlias(self, alias, **kwargs):
        assert isinstance( alias, al.Alias )

    def ActionODE(self,ode,**kwargs):
        assert isinstance( ode, al.ODE )

    def ActionCondition(self, condition):
        assert isinstance( condition, al.Condition )

    def ActionOnCondition(self, on_condition):
        assert isinstance( on_condition, al.OnCondition )

    def ActionOnEvent(self, on_event, **kwargs):
        assert isinstance( on_event, al.OnEvent )

    


# Check that the sub-components stored are all of the
# right types:
class ComponentPortChecker(InplaceActionVisitorDF):
    
    def __init__(self):
        self.recv_event_ports = []
        self.send_event_ports = []
        self.recv_analog_ports = []
        self.send_analog_ports = []

    def ActionComponent(self, component):
        assert isinstance( component, al.models.ComponentNode )
        
        # Check for name duplication:
        portNames = [ p.name for p in chain( component.event_ports, component.analog_ports )] 
        AssertNoDuplicates(portNames)

        self.recv_event_port_names = [ p.name for p in component.event_ports if p.mode=='recv']
        self.send_event_port_names = [ p.name for p in component.event_ports if p.mode=='send']
        self.recv_analog_port_names = [ p.name for p in component.analog_ports if p.mode=='recv']
        self.send_analog_port_names = [ p.name for p in component.analog_ports if p.mode=='send']

    def ActionComponentNodeCombined(self, component):
        assert isinstance( component, al.models.ComponentNodeCombined )
        
        # Check for name duplication:
        portNames = [ p.name for p in chain( component.event_ports, component.analog_ports )] 
        AssertNoDuplicates(portNames)

        self.recv_event_port_names = [ p.name for p in component.event_ports if p.mode=='recv']
        self.send_event_port_names = [ p.name for p in component.event_ports if p.mode=='send']
        self.recv_analog_port_names = [ p.name for p in component.analog_ports if p.mode=='recv']
        self.send_analog_port_names = [ p.name for p in component.analog_ports if p.mode=='send']

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
        print output_event, output_event.port
        print self.send_event_port_names
        assert output_event.port in self.send_event_port_names
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
