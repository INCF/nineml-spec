
class OutputEvent(object):
    def AcceptVisitor(self, visitor, **kwargs):
        return visitor.VisitOutputEvent(self, **kwargs)

    def __init__(self, port):
        self.port = port


class InputEvent(object):
    def AcceptVisitor(self, visitor, **kwargs):
        return visitor.VisitInputEvent(self, **kwargs)

    def __init__(self,port):
        self.port = port
