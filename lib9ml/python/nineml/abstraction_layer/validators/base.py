from nineml.abstraction_layer.visitors.base import ActionVisitor




class ComponentValidatorBase(object):
    
    def getWarnings(self):
        raise NotImplementedError()
    
    
    
    
    
    
    
    
    

class ComponentValidatorPerNamespace(ActionVisitor, ComponentValidatorBase):

    def __init__(self, explicitly_require_action_overrides=True):
        ActionVisitor.__init__(self, explicitly_require_action_overrides=explicitly_require_action_overrides)
        ComponentValidatorBase.__init__(self)
        
        
    #def Postaction_componentclass(self, component, namespace, **kwargs):
        #pass

    
    # Over-ride this function, so we can extract out the 
    # namespace, then propogate this as a parameter.
    def visit_componentclass(self, component, **kwargs):
        namespace = component.get_node_addr()
        ActionVisitor.visit_componentclass(self, component, namespace=namespace)
        #self.Postaction_componentclass(self, component, namespace, **kwargs)
        
    

