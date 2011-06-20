
from nineml.abstraction_layer.validators.cv_types import ComponentValidatorTypes
from nineml.abstraction_layer.validators.cv_ports import ComponentValidatorEventPorts
from nineml.abstraction_layer.validators.cv_ports import ComponentValidatorOutputAnalogPorts
from nineml.abstraction_layer.validators.cv_namingconflicts import ComponentValidatorLocalNameConflicts
from nineml.abstraction_layer.validators.cv_general import ComponentValidatorTimeDerivativesAreDeclared
from nineml.abstraction_layer.validators.cv_general import ComponentValidatorNoDuplicatedObjects
from nineml.abstraction_layer.validators.cv_general import ComponentValidatorAliasesAndStateVariablesHaveNoUnResolvedSymbols
from nineml.abstraction_layer.validators.cv_general import ComponentValidatorPortConnections
from nineml.abstraction_layer.validators.cv_general import ComponentValidatorStateAssignmentsAreOnStateVariables
from nineml.abstraction_layer.validators.cv_general import ComponentValidatorAliasesAreNotRecursive
from nineml.abstraction_layer.validators.cv_general import ComponentValidatorRegimeGraph

class ComponentValidator(object):
    
    @classmethod
    def validate_component(cls, component):

        from nineml.abstraction_layer.writers import TextWriter
        TextWriter.write(component, filename='/tmp/nineml.huntingduplication.txt')
        
        ComponentValidatorTypes(component)
        ComponentValidatorNoDuplicatedObjects(component)
        
        
        ComponentValidatorLocalNameConflicts(component)
        
        ComponentValidatorEventPorts(component)
        ComponentValidatorOutputAnalogPorts(component)
        ComponentValidatorTimeDerivativesAreDeclared(component)
        ComponentValidatorStateAssignmentsAreOnStateVariables(component)
        ComponentValidatorAliasesAreNotRecursive(component)
        ComponentValidatorAliasesAndStateVariablesHaveNoUnResolvedSymbols(component)
        ComponentValidatorPortConnections(component)
        ComponentValidatorRegimeGraph(component)
        
        
