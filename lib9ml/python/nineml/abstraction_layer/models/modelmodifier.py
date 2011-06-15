
from nineml.abstraction_layer.visitors import InPlaceTransform
from nineml.utility import FilterExpectSingle

class ModelModifier(object):

    @classmethod
    def CloseAnalogPort( cls, component, port_name, value="0"):
       closer = InPlaceTransform( port_name, value ) 
       component.AcceptVisitor( closer)
       port = FilterExpectSingle( component.analog_ports, lambda ap: ap.name==port_name)
       component._analog_ports.remove( port  )

