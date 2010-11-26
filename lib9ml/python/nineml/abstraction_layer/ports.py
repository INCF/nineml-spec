
from nineml.helpers import curry
from nineml.abstraction_layer.xmlns import *
from nineml.abstraction_layer.expressions import expr_to_obj, Assignment

class Port(object):
    """ Base class for EventPort and AnalogPort, etc."""
    element_name = "port"
    modes = ('send','recv','reduce')
    reduce_op_map = {'add':'+', 'sub':'-', 'mul':'*', 'div':'/',
                     '+':'+', '-':'-', '*':'*', '/':'/'}

    def __init__(self, internal_symbol, mode='send', op=None):
        """

        For AnalogPorts:

          if mode='send', internal_symbol may be an expression, e.g. "Isyn = g(E-V)"
          in which case the rhs is computed and made available as the port with symbol=lhs
          (in the example 'Isyn')


        """

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
        if self.reduce_op:
            return "%s('%s', mode='%s', op='%s')" % \
                   (self.__class__.__name__, self.symbol, self.mode, self.reduce_op)
        else:
            return "%s('%s', mode='%s')" % (self.__class__.__name__, self.symbol, self.mode)

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
    element_name = "analog-port"
    """ Port which may be in a Regime """
    pass

class EventPort(Port):
    element_name = "event-port"
    """ Port which may be in an Event """

    def is_bool(self):
        """ To match the Condition interface.  Event needs to check. """
        return False


SpikeOutputEvent = EventPort('spike_output')
SpikeInputEvent = EventPort('spike_input', mode="recv")
PreEvent = EventPort('spike_pre', mode="recv")
PostEvent = EventPort('spike_post', mode="recv")

# Syntactic sugar
ReducePort = curry(AnalogPort,mode="reduce")
RecvPort = curry(AnalogPort,mode="recv")
SendPort = curry(AnalogPort,mode="send")

# allows: RecvPort("V")
