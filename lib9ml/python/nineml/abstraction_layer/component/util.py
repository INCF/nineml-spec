
from expressions import Alias, TimeDerivative, StateAssignment
import re
from nineml.exceptions import NineMLRuntimeError

# Wrapper for writing XML:
def parse(filename):
    from nineml.abstraction_layer.readers import XMLReader
    return XMLReader.read_component(filename)

class StrToExpr(object):


    @classmethod
    def is_alias(cls, alias_string):
        return ':=' in alias_string

    @classmethod
    def alias(cls, alias_string):
        if not cls.is_alias(alias_string):
            errmsg = "Invalid Alias: %s"%alias_string
            raise NineMLRuntimeError(errmsg)

        lhs,rhs = alias_string.split(':=')
        return Alias( lhs = lhs.strip(), rhs = rhs.strip() )


    @classmethod
    def time_derivative(cls, time_derivative_string):
        r = re.compile(r"""\s* d(?P<dependent_var>[a-zA-Z][a-zA-Z0-9_]*)/dt \s* = \s* (?P<rhs> .*) """, re.VERBOSE) 
        m = r.match(time_derivative_string)
        return TimeDerivative( dependent_variable = m.groupdict()['dependent_var'], rhs=m.groupdict()['rhs'] )


    @classmethod
    def state_assignment(cls, state_assignment_string):
        lhs,rhs = state_assignment_string.split('=')
        return StateAssignment( lhs=lhs, rhs=rhs )
