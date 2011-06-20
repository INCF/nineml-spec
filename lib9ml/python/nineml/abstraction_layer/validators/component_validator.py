
from nineml.abstraction_layer.validators.cv_types import ComponentValidatorTypes
from nineml.abstraction_layer.validators.cv_ports import ComponentValidatorEventPorts
from nineml.abstraction_layer.validators.cv_ports import ComponentValidatorOutputAnalogPorts
from nineml.abstraction_layer.validators.cv_namingconflicts import ComponentValidatorLocalNameConflicts
from nineml.abstraction_layer.validators.cv_general import ComponentValidatorTimeDerivativesAreDeclared,\
    ComponentValidatorStateAssignmentsAreOnStateVariables


class ComponentValidator(object):
    
    @classmethod
    def validate_component(cls, component):
        
        
        ComponentValidatorTypes(component)
        
        
        ComponentValidatorLocalNameConflicts(component)
        
        ComponentValidatorEventPorts(component)
        ComponentValidatorOutputAnalogPorts(component)
        ComponentValidatorTimeDerivativesAreDeclared(component)
        ComponentValidatorStateAssignmentsAreOnStateVariables(component)
        
    