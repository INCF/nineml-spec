"""This file defines the Port classes used in NineML"""


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
    _modes = ('send', 'recv', 'reduce')
    _reduce_op_map = {'add':'+', 'sub':'-', 'mul':'*', 'div':'/',
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

        self.dimension = "??" 
        self._name = name
        self._mode = mode
        self._reduce_op = reduce_op
        
        if self._mode not in Port._modes:
            raise ValueError, ("%s('%s')"+\
                  "specified undefined mode: '%s'") %\
                  (self.__class__.__name__, self.symbol, mode)
        if mode == 'reduce':
            if reduce_op not in Port._reduce_op_map.keys():
                raise ValueError, ("%s('%s')"+\
                      "specified undefined reduce_op: '%s'") %\
                      (self.__class__.__name__, name, str(reduce_op))

        if reduce_op and mode != "reduce":
            raise ValueError, "Port of mode!=reduce may not specify 'op'."
            
    
    @property
    def name(self):
        """The name of the port, local to the current component"""
        return self._name

    @property
    def mode(self):
        """The mode of the port. ['send','recv' or 'reduce'] """
        return self._mode
    
    @property
    def reduce_op(self):
        """The reduction operation of the port, if it is a 'reduce' port"""
        return self._reduce_op


    def __repr__(self):
        classstring = self.__class__.__name__ 
        opstr = ', op=%s'% (self.reduce_op or '' )
        return "%s('%s', mode='%s' %s)" % \
                    (classstring, self.name, self.mode, opstr)

    def is_incoming(self):
        """Returns True if the port's mode is 'recv' or 'reduce' """
        return self.mode in ('recv', 'reduce')

    def is_outgoing(self):
        """Returns True if the port's mode is 'send' """
        return not self.is_incoming()
                   
    @property
    def symbol(self):
        """Deprecated, do not use"""
        assert False
        return self._name





class AnalogPort(Port):
    """Analog Port
    
    An analog port represents a continuous input or output to/from a component.
    For example, this could be the membrane-voltage into a synapse component, or
    the current provided by a ion-channel. 

    """
    def accept_visitor(self, visitor, **kwargs):
        """ |VISITATION| """
        return visitor.visit_analogport(self, **kwargs)


class EventPort(Port):
    """Event Port
    
    An event port represents a port that can transmit and recieve discrete events at points
    in time. For example, an integrate-and-fire could 'send' events to notify
    other components that it had fired; or synapses could recieve events to
    notify them to provide current to a post-synaptic neuron. 

    """
    def accept_visitor(self, visitor, **kwargs):
        """ |VISITATION| """
        return visitor.visit_eventport(self, **kwargs)




    
    


# Syntactic sugar

ReducePort = curry(AnalogPort, mode="reduce")
RecvPort = curry(AnalogPort, mode="recv")
SendPort = curry(AnalogPort, mode="send")

RecvEventPort = curry(EventPort, mode="recv")
SendEventPort = curry(EventPort, mode="send")
