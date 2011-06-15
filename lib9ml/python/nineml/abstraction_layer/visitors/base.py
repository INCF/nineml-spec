



class ComponentVisitor(object):
    def Visit( self,obj,**kwargs):
        return obj.AcceptVisitor(self,**kwargs)


