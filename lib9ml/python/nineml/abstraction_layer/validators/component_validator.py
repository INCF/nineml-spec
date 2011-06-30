"""This file contains the ComponentValidator class for validating component"""

from nineml.utility import Settings

from cv_types import ComponentValidatorTypes
from cv_ports import ComponentValidatorEventPorts
from cv_ports import ComponentValidatorOutputAnalogPorts
from cv_namingconflicts import ComponentValidatorLocalNameConflicts
from cv_general import ComponentValidatorTimeDerivativesAreDeclared
from cv_general import ComponentValidatorNoDuplicatedObjects
#from cv_general import ComponentValidatorAliasesAndStateVariablesHaveNoUnResolvedSymbols
from cv_general import ComponentValidatorAssignmentsAliasesAndStateVariablesHaveNoUnResolvedSymbols
from cv_general import ComponentValidatorPortConnections
from cv_general import ComponentValidatorStateAssignmentsAreOnStateVariables
from cv_general import ComponentValidatorAliasesAreNotRecursive
from cv_general import ComponentValidatorRegimeGraph
from cv_general import ComponentValidatorRegimeOnlyHasOneHandlerPerEvent
from cv_general import ComponentValidatorCheckNoLHSAssignmentsToMathsNamespace
from cv_regimenames import ComponentValidatorDuplicateRegimeNames

class ComponentValidator(object):
    """Class for grouping all the component-validations tests together"""
    
    @classmethod
    def validate_component(cls, component):
        """ Tests a componentclass against a variety of tests, to verify its
        internal structure
        """


        if not Settings.enable_component_validation :
            import os
            assert os.getlogin() == 'hull', """Checking only mike turns off component-validation :) """
            print "  **** WARNING WARNIGN COMPONENT VALIDATION TURNRED OFF ****"
            return 

        #Check class structure:
        ComponentValidatorTypes(component)
        ComponentValidatorNoDuplicatedObjects(component)
        
        ComponentValidatorDuplicateRegimeNames(component)
        ComponentValidatorLocalNameConflicts(component)
        
        ComponentValidatorEventPorts(component)
        ComponentValidatorOutputAnalogPorts(component)
        ComponentValidatorTimeDerivativesAreDeclared(component)
        ComponentValidatorStateAssignmentsAreOnStateVariables(component)
        ComponentValidatorAliasesAreNotRecursive(component)
        ComponentValidatorAssignmentsAliasesAndStateVariablesHaveNoUnResolvedSymbols(component)
        ComponentValidatorPortConnections(component)
        ComponentValidatorRegimeGraph(component)
        ComponentValidatorRegimeOnlyHasOneHandlerPerEvent(component)
        ComponentValidatorCheckNoLHSAssignmentsToMathsNamespace(component)

        
        
