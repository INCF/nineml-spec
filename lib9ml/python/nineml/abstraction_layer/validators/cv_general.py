from nineml.abstraction_layer.validators.base import ComponentValidatorPerNamespace

from collections import defaultdict

# Check that the sub-components stored are all of the
# right types:
class ComponentValidatorTimeDerivativesAreDeclared(ComponentValidatorPerNamespace):
    """ Check for conflicts between Aliases, StateVariables, Parameters, and EventPorts,
        and analog input ports
    
        We do not need to check for comflicts with output AnalogPorts, since, these will use names. 
    """
     
    def __init__(self, component):
        ComponentValidatorPerNamespace.__init__(self, explicitly_require_action_overrides=False)
        self.sv_declared = defaultdict(list)
        self.time_derivatives_used = defaultdict(list)
            
        self.visit(component)
        
        for namespace,time_derivatives in self.time_derivatives_used.iteritems():
            for td in time_derivatives:
                assert td in self.sv_declared[namespace], 'StateVariable not declared: %s'%td
            
    def ActionStateVariable(self, state_variable, namespace, **kwargs):
        self.sv_declared[namespace].append(state_variable.name)
        
    def ActionODE(self, timederivative, namespace,**kwargs):
        self.time_derivatives_used[namespace].append(timederivative.dependent_variable)




class ComponentValidatorStateAssignmentsAreOnStateVariables(ComponentValidatorPerNamespace):
    """ Check for conflicts between Aliases, StateVariables, Parameters, and EventPorts,
        and analog input ports
    
        We do not need to check for comflicts with output AnalogPorts, since, these will use names. 
    """
     
    def __init__(self, component):
        ComponentValidatorPerNamespace.__init__(self, explicitly_require_action_overrides=False)
        self.sv_declared = defaultdict(list)
        self.state_assignments_lhses = defaultdict(list)
            
        self.visit(component)
        
        for namespace,state_assignments_lhs in self.state_assignments_lhses.iteritems():
            for td in state_assignments_lhs:
                assert td in self.sv_declared[namespace]
            
    def ActionStateVariable(self, state_variable, namespace, **kwargs):
        self.sv_declared[namespace].append(state_variable.name)
        
    def ActionStateAssignment(self, state_assignment, namespace,**kwargs):
        assert False
        self.state_assignments_lhses[namespace].append(state_assignment.lhs)

