
# Base class for depth-first visitation:
class ModelVisitorDF(object):

    def Visit(self,model):
        model.AcceptVisitor(self)

    def AcceptComponentNode(self,node):
        self.VisitComponentNode(node)

    def AcceptModelNode(self,node):
        self.VisitModelNode(node)
        for sn in sorted(node.subnodes.values(), key=lambda sn:sn.getContainedNamespaceName() ):
            sn.AcceptVisitor(self)


    # To be overridden
    def VisitComponentNode(self, node):
        pass

    # To be overridden
    def VisitModelNode(self, node):
        pass





# Base class for visitation:
class ModelVisitorDF_NodeCollector(ModelVisitorDF):
    def __init__(self, model=None):
        self.nodes = []
        if model: self.Visit(model)

    def VisitComponentNode(self, node):
        self.nodes.append(node)
    def VisitModelNode(self, node):
        self.nodes.append(node)

    def __iter__(self):
        return iter(self.nodes)

class ModelVisitorDF_ComponentCollector(ModelVisitorDF):
    def __init__(self, model=None):
        self.components = []
        if model: self.Visit(model)

    def VisitComponentNode(self, node):
        self.components.append(node)

    def __iter__(self):
        return iter(self.components)


class ModelVisitorDF_ModelCollector(ModelVisitorDF):
    def __init__(self, model=None,include_root=True):
        assert include_root
        self.models = []
        if model: self.Visit(model)

    def VisitModelNode(self, node):
        self.models.append(node)

    def __iter__(self):
        return iter(self.components)

