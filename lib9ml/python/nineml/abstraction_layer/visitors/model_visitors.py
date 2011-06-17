
# Base class for depth-first visitation:
class ModelVisitorDF(object):

    def Visit(self,model):
        model.AcceptVisitor(self)

    def VisitComponentNode(self,node):
        assert False
        self.ActionComponentNode(node)

    def VisitModelNode(self,node):
        assert False
        self.ActionModelNode(node)
        for sn in sorted(node.subnodes.values(), key=lambda sn:sn.getContainedNamespaceName() ):
            sn.AcceptVisitor(self)

    def VisitComponentClass(self,node):
        self.ActionComponentClass(node)
        for sn in sorted(node.subnodes.values(), key=lambda sn:sn.getContainedNamespaceName() ):
            sn.AcceptVisitor(self)


    # To be overridden
    def ActionComponentNode(self, node):
        pass

    # To be overridden
    def ActionModelNode(self, node):
        pass





# Base class for visitation:
class ModelVisitorDF_NodeCollector(ModelVisitorDF):
    def __init__(self, model=None):
        self.nodes = []
        if model: self.Visit(model)

    def ActionComponentNode(self, node):
        assert False
        self.nodes.append(node)
    def ActionModelNode(self, node):
        assert False
        self.nodes.append(node)

    def __iter__(self):
        return iter(self.nodes)


    def ActionComponentClass(self, node):
        self.nodes.append(node)



class ModelVisitorDF_ComponentCollector(ModelVisitorDF):
    def __init__(self, model=None):
        self.components = []
        if model: self.Visit(model)

    def ActionComponentNode(self, node):
        assert False
        self.components.append(node)

    def __iter__(self):
        return iter(self.components)

    def ActionComponentClass(self, node):
        self.components.append(node)


class ModelVisitorDF_ModelCollector(ModelVisitorDF):
    def __init__(self, model=None,include_root=True):
        assert include_root
        self.models = []
        if model: self.Visit(model)

    def ActionModelNode(self, node):
        self.models.append(node)

    def __iter__(self):
        return iter(self.models)

    def ActionComponentClass(self, node):
        self.models.append(node)
