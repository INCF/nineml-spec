"""This file contains utility classes for modifying components"""

from nineml.abstraction_layer.visitors import ExpandPortDefinition
from nineml.utility import filter_expect_single
from nineml.abstraction_layer.util import check_flat_component

class ComponentModifier(object):
    """Utility classes for modifying components"""

    @classmethod
    @check_flat_component
    def close_analog_port( cls, component, port_name, value="0"):
        """Closes an incoming analog port by assigning its value to 0"""
        # Subsitute the value in:
        component.accept_visitor( ExpandPortDefinition( port_name, value ) )

        # Remove it from the list of ports:
        port = filter_expect_single( component.analog_ports, 
                                     lambda ap: ap.name==port_name)
        component._analog_ports.remove( port  )


    @classmethod
    @check_flat_component
    def close_all_reduce_ports(cls, component, exclude=None):
        """Closes all the ``reduce`` ports on a component by assigning them a
        value of 0"""

        for arp in component.query.analog_reduce_ports:
            if exclude and arp.name in exclude: 
                continue
            ComponentModifier.close_analog_port(component=component, 
                                                port_name=arp.name, 
                                                value='0' )

    @classmethod
    @check_flat_component
    def rename_port( cls, component, old_port_name, new_port_name ):
        """ Renames a port in a component """
        
        
        # Find the old port:
        port = filter_expect_single( component.analog_ports, 
                                     lambda ap: ap.name==old_port_name)
        port._name = new_port_name
    




