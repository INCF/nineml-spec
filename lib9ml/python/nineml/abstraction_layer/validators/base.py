from nineml.abstraction_layer.visitors.base import InplaceActionVisitorDF




class ComponentValidatorBase(object):
    
    def getWarnings(self):
        raise NotImplementedError()
    
    
    
    
    
    
    
    
    

class ComponentValidatorPerNamespace(InplaceActionVisitorDF, ComponentValidatorBase):

    def __init__(self, explicitly_require_action_overrides=True):
        InplaceActionVisitorDF.__init__(self, explicitly_require_action_overrides=explicitly_require_action_overrides)
        ComponentValidatorBase.__init__(self)
        
        
    #def Postaction_componentclass(self, component, namespace, **kwargs):
        #pass

    
    # Over-ride this function, so we can extract out the 
    # namespace, then propogate this as a parameter.
    def visit_componentclass(self, component, **kwargs):
        namespace = component.get_node_addr()
        InplaceActionVisitorDF.visit_componentclass(self, component, namespace=namespace)
        #self.Postaction_componentclass(self, component, namespace, **kwargs)
        
    

