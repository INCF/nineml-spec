
class OutputEvent(object):
    """OutputEvent

    OutputEvents can occur during transitions, and correspond to 
    an event beng generated on the relevant EventPort port in 
    the component.
    """

    def accept_visitor(self, visitor, **kwargs):
        """ |VISITATION| """
        return visitor.visit_outputevent(self, **kwargs)

    def __init__(self, port_name):
        """OutputEvent Constructor

        :param port: The name of the output EventPort that should 
            transmit an event. An `EventPort` with a mode of 'send' must exist
            with a corresponding name in the component, otherwise a 
            ``NineMLRuntimeException`` will be raised.
        
        """

        self._port_name = port_name

    @property
    def port_name(self):
        '''Returns the name of the port'''
        return self._port_name



class InputEvent(object):
    """InputEvent

    Input Events are used to specify that a transition should occur
    that is triggered by a 'recv' or 'reduce' event port. 
    """

    def accept_visitor(self, visitor, **kwargs):
        """ |VISITATION| """
        return visitor.visit_inputevent(self, **kwargs)

    def __init__(self,port_name):
        """InputEvent Constructor

        :param port: The name of the input EventPort that should 
            causes the transition.  An `EventPort` with a mode of 'recv' or
            'reduce' must exist with a corresponding name in the component,
            otherwise a ``NineMLRuntimeException`` will be raised.

        """

        self._port_name = port_name


    @property
    def port_name(self):
        '''Returns the name of the port'''
        return self._port_name
