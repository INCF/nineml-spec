
from nineml.abstraction_layer.visitors import InPlaceTransform
from nineml.utility import FilterExpectSingle
from nineml.abstraction_layer.util import check_flat_component

class ModelModifier(object):

    @classmethod
    @check_flat_component
    def CloseAnalogPort( cls, component, port_name, value="0"):
        # Subsitute the value in:
        component.AcceptVisitor( InPlaceTransform( port_name, value ) )

        # Remove it from the list of ports:
        port = FilterExpectSingle( component.analog_ports, lambda ap: ap.name==port_name)
        component._analog_ports.remove( port  )


    @classmethod
    @check_flat_component
    def CloseAllReducePorts(cls, component, exclude=None):

        for arp in component.query.analog_reduce_ports:
            if exclude and arp.name in exclude: continue
            ModelModifier.CloseAnalogPort(component=component, port_name=arp.name, value='0' )
    




