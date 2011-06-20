
class OutputEvent(object):
    def accept_visitor(self, visitor, **kwargs):
        """ |VISITATION| """
        return visitor.VisitOutputEvent(self, **kwargs)

    def __init__(self, port):
        self.port = port


class InputEvent(object):
    def accept_visitor(self, visitor, **kwargs):
        """ |VISITATION| """
        return visitor.VisitInputEvent(self, **kwargs)

    def __init__(self,port):
        self.port = port
