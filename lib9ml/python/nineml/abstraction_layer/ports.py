
from nineml.helpers import curry
from nineml.abstraction_layer.xmlns import *
from nineml.abstraction_layer.expressions import expr_to_obj, Assignment

class Port(object):
    """ Base class for EventPort and AnalogPort, etc."""
    element_name = "port"
    modes = ('send','recv','reduce')
    reduce_op_map = {'add':'+', 'sub':'-', 'mul':'*', 'div':'/',
                     '+':'+', '-':'-', '*':'*', '/':'/'}

    def __init__(self, internal_symbol, mode='send', op=None, expr=None):
        """  
        
        For AnalogPorts:

          if mode='send', internal_symbol may be an expression, e.g. "Isyn = g(E-V)"
          in which case the rhs is computed and made available as the port with symbol=lhs
          (in the example 'Isyn')


        """

        self.dimension="??" 

        self.expr = None
        self.symbol = internal_symbol
        # allow ports which expose an expression as a symbol
        # via passing e.g "Isyn = g(E-V)"
        if '=' in self.symbol:
            if not isinstance(self,AnalogPort):
                raise ValueError, "Port expression '%s' is valid only for AnalogPort" % internal_symbol
            if mode!="send":
                raise ValueError, "Port expression '%s' is valid only for mode='send'" % internal_symbol
            self.expr = expr_to_obj(internal_symbol)
            if not isinstance(self.expr, Assignment):
                raise ValueError, "Port expression '%s' is not a valid assignment" % internal_symbol
            self.symbol = self.expr.lhs


        # MH: Are we trying to express the expr twice?
        assert not (self.expr and expr)
        if expr:
            self.expr = expr
        

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
            
        # TODO: EventPort can also be reduce?  Then no op needed.

    
    

    
        
        


    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.symbol == other.symbol and self.mode == other.mode\
               and self.reduce_op == other.reduce_op and self.expr==other.expr

    def __repr__(self):
        
        classstring = self.__class__.__name__ 
        opstr = ', op=%s'%self.reduce_op if self.reduce_op else ''
        exprstr = ', expr= %s'%self.expr if self.expr else ''
        
        return "%s('%s', mode='%s' %s%s)" % (classstring, self.symbol, self.mode, opstr, exprstr)
                   
                   

    def encode(self, encoding):
        return repr(self).encode(encoding)

    def as_expr(self):
        return repr(self)

    @property
    def names(self):
        return []

    @property
    def name(self):
        return self.symbol

    def to_xml(self, **kwargs):
        if self.reduce_op:
            kwargs['op']=self.reduce_op
        if self.expr:
            kwargs['expression']=self.expr.rhs
        return E(self.element_name, symbol=self.symbol,
                 mode=self.mode, **kwargs)


    def AcceptVisitor(self,visitor,**kwargs):
        return visitor.VisitPort(self,**kwargs)

    #@property
    #def name(self):
    #    return self.element_name+"_"+self.symbol

    @classmethod
    def from_xml(cls,element):
        assert element.tag == NINEML+cls.element_name
        symbol = element.get("symbol")
        assert symbol
        mode = element.get("mode")
        reduce_op = element.get("op")
        expr = element.get("expression")
        if expr:
            symbol = "%s = %s" % (symbol,expr)
        return cls(symbol,mode,reduce_op)


class AnalogPort(Port):
    element_name = "AnalogPort"
    """ Port which may be in a Regime """
    
    
    def clone(self, prefix="", expr_prefix=None, prefix_excludes=[]):
        if expr_prefix is None:
            expr_prefix = prefix
        elif expr_prefix is False:
            expr_prefix = ""
        else:
            pass

        symbol = prefix + self.symbol if not self.symbol in prefix_excludes else self.symbol
        expr = self.expr.clone( prefix=expr_prefix,prefix_excludes=prefix_excludes ) if self.expr else None
        return AnalogPort(internal_symbol = symbol,
                          mode=self.mode, 
                          op=self.reduce_op, 
                          expr=expr
                          )
    def cloneOld(self, newname=None, expr_prefix=None):
        expr = self.expr.clone( prefix=expr_prefix ) if self.expr else None
        return AnalogPort(internal_symbol = self.symbol if not newname else newname,
                          mode=self.mode, 
                          op=self.reduce_op, 
                          expr=expr
                          )
     




class EventPort(Port):
    element_name = "EventPort"
    """ Port which may be in an Event """

    def is_bool(self):
        """ To match the Condition interface.  Event needs to check. """
        return False
    
    # Added by MH to make hierachical resolving code cleaner:
    def clone(self, prefix="", prefix_excludes=None):
        prefix_excludes = prefix_excludes if prefix_excludes else []
        assert not self.expr
        
        symbol = self.symbol if self.symbol in prefix_excludes else prefix + self.symbol
        return EventPort(internal_symbol=symbol, mode=self.mode, op=self.reduce_op )
     
    


class OutputEvent(object):
    def __init__(self, port):
        self.port = port

    def clone(self, prefix="", prefix_excludes=[]):
        portname = prefix + self.port if not self.port in prefix_excludes else self.port
        return OutputEvent(portname)

class InputEvent(object):
    def __init__(self,port):
        self.port
    
    def clone(self, prefix="", prefix_excludes=[]):
        portname = prefix + self.port if not self.port in prefix_excludes else self.port
        return OutputEvent(portname)
    
SpikeEventPort = EventPort('spike_output')
SpikeInputEvent = EventPort('spike_input', mode="recv")



SpikeOutputEvent = EventPort('spike_output')
SpikeInputEvent = EventPort('spike_input', mode="recv")


PreEvent = EventPort('spike_pre', mode="recv")
PostEvent = EventPort('spike_post', mode="recv")
PreEventRelay = EventPort('spike_pre_relay', mode="send")

# Syntactic sugar
ReducePort = curry(AnalogPort,mode="reduce")
RecvPort = curry(AnalogPort,mode="recv")
SendPort = curry(AnalogPort,mode="send")

# allows: RecvPort("V")
