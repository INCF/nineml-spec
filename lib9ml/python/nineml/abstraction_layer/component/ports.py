
from nineml.helpers import curry


class Port(object):
    """ Base class for EventPort and AnalogPort.
    
    In general, a port has a ``name``, which can be used to reference it, 
    and a ``mode``, which specifies whether it sends or recieves information. 
    
    Generally, a send port can be connected to recieve port to allow different
    components to communicate. 
    
    In the case of an ``AnalogPort``, we have three
    modes, ``send``, ``recv`` and ``reduce``. ``send`` ports can be connected to
    any number of ``recv`` ports, but each ``recv`` port can only be connected
    to a single ``send`` port. In order to collect analog input from several
    ``send`` ports into a single port, we use a ``reduce`` port. A ``reduce``
    port also requires an additional parameter, ``op``, which specifies how to
    combine the information from the ports, for example, by adding thier values
    together, `+`. 
    
    For example, if we had several Hodgekin-Huxley channels on a neuron, we
    would want each one to have a ``send`` port, ``i`` containing the current
    passing through that type of channel. Then, we would have a single
    ``reduce`` port, ``I_in`` for example, with ``op='+'``, which would combine
    them together to calculate the voltage change in the neuron.

    """
    modes = ('send','recv','reduce')
    reduce_op_map = {'add':'+', 'sub':'-', 'mul':'*', 'div':'/',
                     '+':'+', '-':'-', '*':'*', '/':'/'}

    def __init__(self, name, mode='send', reduce_op=None):
        """ Port Constructor.

        :param name: The name of the port, as a `string`
        :param mode: The mode of the port, which should be a string as either,
            ``send``,``recv`` or ``reduce``.
        :param reduce_op: This should be ``None`` unless the mode is ``reduce``.
            If the mode is ``reduce``, then this must be a supported
            ``reduce_op``

        .. note::

            Currently support ``reduce_op`` s are: ``+``.

        """

        self.dimension="??" 
        self._name = name
        self.mode = mode
        self.reduce_op = reduce_op
        
        if self.mode not in self.modes:
            raise ValueError, ("%s('%s')"+\
                  "specified undefined mode: '%s'") %\
                  (self.__class__.__name__, self.symbol, self.mode)
        if self.mode=='reduce':
            if self.reduce_op not in self.reduce_op_map.keys():
                raise ValueError, ("%s('%s')"+\
                      "specified undefined reduce_op: '%s'") %\
                      (self.__class__.__name__, self.symbol, str(self.reduce_op))

        if reduce_op and self.mode!="reduce":
            raise ValueError, "Port of mode!=reduce may not specify 'op'."
            
    
    @property
    def name(self):
        return self._name

    @property
    def symbol(self):
        assert False
        return self._name
    

    def __repr__(self):
        classstring = self.__class__.__name__ 
        opstr = ', op=%s'%self.reduce_op if self.reduce_op else ''
        return "%s('%s', mode='%s' %s)" % (classstring, self.symbol, self.mode, opstr)
                   
                   





class AnalogPort(Port):
    def AcceptVisitor(self, visitor, **kwargs):
        return visitor.VisitAnalogPort(self,**kwargs)


class EventPort(Port):
    def AcceptVisitor(self, visitor, **kwargs):
        return visitor.VisitEventPort(self,**kwargs)




    
    


# Syntactic sugar

ReducePort = curry(AnalogPort,mode="reduce")
RecvPort = curry(AnalogPort,mode="recv")
SendPort = curry(AnalogPort,mode="send")

RecvEventPort = curry(EventPort,mode="recv")
SendEventPort = curry(EventPort,mode="send")
