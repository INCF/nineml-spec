



import nineml.abstraction_layer as al
from nineml.abstraction_layer.visitors import ActionVisitor
from nineml.utility import *
from itertools import chain

from collections import defaultdict
from nineml.abstraction_layer.validators.base import ComponentValidatorPerNamespace






# Check that the sub-components stored are all of the
# right types:
class ComponentValidatorLocalNameConflicts(ComponentValidatorPerNamespace):
    """ Check for conflicts between Aliases, StateVariables, Parameters, and EventPorts,
        and analog input ports
    
        We do not need to check for comflicts with output AnalogPorts, since, these will use names. 
    """
     
    def __init__(self, component):
        ComponentValidatorPerNamespace.__init__(self, explicitly_require_action_overrides=False)
        self.symbols = defaultdict(list)

        self.visit(component)
    
    def check_comflicting_symbol(self, namespace, symbol):
        assert not symbol in self.symbols[namespace]
        self.symbols[namespace].append(symbol)
            
    def action_statevariable(self, state_variable, namespace, **kwargs):
        self.check_comflicting_symbol(namespace=namespace, symbol=state_variable.name)
        
    def action_parameter(self, parameter, namespace, **kwargs):
        self.check_comflicting_symbol(namespace=namespace, symbol=parameter.name)
        
    def action_analogport(self, port, namespace, **kwargs):
        if port.is_incoming():
            self.check_comflicting_symbol(namespace=namespace, symbol=port.name)
        
    def action_eventport(self, port, namespace, **kwargs):
        self.check_comflicting_symbol(namespace=namespace, symbol=port.name)
                
    def action_alias(self, alias, namespace, **kwargs):
        self.check_comflicting_symbol(namespace=namespace, symbol=alias.lhs)
        

        
    
    
