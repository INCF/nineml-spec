"""Utility functions for component core classes"""

from expressions import Alias, TimeDerivative, StateAssignment
import re
from nineml.exceptions import NineMLRuntimeError

# Wrapper for writing XML:
def parse(filename):
    """Left over from orignal Version. This will be deprecated"""

    from nineml.abstraction_layer.readers import XMLReader
    return XMLReader.read_component(filename)

class StrToExpr(object):
    """Class containing static methods for building Mathematical objects"""


    @classmethod
    def is_alias(cls, alias_string):
        """ Returns True if the string could be an alias"""
        return ':=' in alias_string

    @classmethod
    def alias(cls, alias_string):
        """Creates an Alias object from a string"""
        if not cls.is_alias(alias_string):
            errmsg = "Invalid Alias: %s" % alias_string
            raise NineMLRuntimeError(errmsg)

        lhs, rhs = alias_string.split(':=')
        return Alias( lhs = lhs.strip(), rhs = rhs.strip() )


    @classmethod
    def time_derivative(cls, time_derivative_string):
        """Creates an TimeDerivative object from a string"""
        # Note: \w = [a-zA-Z0-9_]
        tdre = re.compile(r"""\s* d(?P<dependent_var>[a-zA-Z][a-zA-Z0-9_]*)/dt
                           \s* = \s* 
                           (?P<rhs> .*) """, re.VERBOSE) 

        match = tdre.match(time_derivative_string)
        if not match:
            err = "Unable to load time derivative: %s" % time_derivative_string
            raise NineMLRuntimeError(err)
        dependent_variable =  match.groupdict()['dependent_var']
        rhs = match.groupdict()['rhs'] 
        return TimeDerivative( dependent_variable = dependent_variable, 
                               rhs= rhs )


    @classmethod
    def state_assignment(cls, state_assignment_string):
        """Creates an StateAssignment object from a string"""
        lhs, rhs = state_assignment_string.split('=')
        return StateAssignment( lhs=lhs, rhs=rhs )
