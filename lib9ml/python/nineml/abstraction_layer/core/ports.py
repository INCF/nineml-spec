
from nineml.helpers import curry


class Port(object):
    """ Base class for EventPort and AnalogPort, etc."""
    element_name = "port"
    modes = ('send','recv','reduce')
    reduce_op_map = {'add':'+', 'sub':'-', 'mul':'*', 'div':'/',
                     '+':'+', '-':'-', '*':'*', '/':'/'}

    def __init__(self, internal_symbol, mode='send', op=None):
        

        self.dimension="??" 
        self.symbol = internal_symbol
        self.mode = mode
        self.reduce_op = op
        
        if self.mode not in self.modes:
            raise ValueError, ("%s('%s')"+\
                  "specified undefined mode: '%s'") %\
                  (self.__class__.__name__, self.symbol, self.mode)
        if self.mode=='reduce':
            if self.reduce_op not in self.reduce_op_map.keys():
                raise ValueError, ("%s('%s')"+\
                      "specified undefined reduce_op: '%s'") %\
                      (self.__class__.__name__, self.symbol, str(self.reduce_op))

        if op and self.mode!="reduce":
            raise ValueError, "Port of mode!=reduce may not specify 'op'."
            
    
    @property
    def name(self):
        return self.symbol
    
    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.symbol == other.symbol and self.mode == other.mode\
               and self.reduce_op == other.reduce_op

    def __repr__(self):
        classstring = self.__class__.__name__ 
        opstr = ', op=%s'%self.reduce_op if self.reduce_op else ''
        return "%s('%s', mode='%s' %s)" % (classstring, self.symbol, self.mode, opstr)
                   
                   

    def encode(self, encoding):
        assert False
        return repr(self).encode(encoding)

    def as_expr(self):
        assert False
        return repr(self)





class AnalogPort(Port):
    """ Port which may be in a Regime """
    element_name = "AnalogPort"
    
    def AcceptVisitor(self, visitor, **kwargs):
        return visitor.VisitAnalogPort(self,**kwargs)


class EventPort(Port):
    """ Port which may be in an Event """
    element_name = "EventPort"
    
    def AcceptVisitor(self, visitor, **kwargs):
        return visitor.VisitEventPort(self,**kwargs)




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
    
    


# Syntactic sugar

ReducePort = curry(AnalogPort,mode="reduce")
RecvPort = curry(AnalogPort,mode="recv")
SendPort = curry(AnalogPort,mode="send")

RecvEventPort = curry(EventPort,mode="recv")
SendEventPort = curry(EventPort,mode="send")
