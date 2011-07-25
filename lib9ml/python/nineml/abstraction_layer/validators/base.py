
#from nineml.abstraction_layer.visitors import ActionVisitor

from ..visitors import ActionVisitor

class ComponentValidatorBase(object):
    
    def get_warnings(self):
        raise NotImplementedError()
    
    
   
    

class ComponentValidatorPerNamespace(ActionVisitor, ComponentValidatorBase):

    def __init__(self, explicitly_require_action_overrides=True):
        ActionVisitor.__init__(self, explicitly_require_action_overrides=explicitly_require_action_overrides)
        ComponentValidatorBase.__init__(self)
        
    
    # Over-ride this function, so we can extract out the 
    # namespace, then propogate this as a parameter.
    def visit_componentclass(self, component, **kwargs):
        namespace = component.get_node_addr()
        ActionVisitor.visit_componentclass(self, component, namespace=namespace)
        
    

