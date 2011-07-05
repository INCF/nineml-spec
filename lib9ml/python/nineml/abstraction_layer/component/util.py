"""Utility functions for component core classes"""



import re
from nineml.exceptions import NineMLRuntimeError
import nineml

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
        import nineml.abstraction_layer as al
        if not cls.is_alias(alias_string):
            errmsg = "Invalid Alias: %s" % alias_string
            raise NineMLRuntimeError(errmsg)

        lhs, rhs = alias_string.split(':=')
        return al.Alias( lhs = lhs.strip(), rhs = rhs.strip() )


    @classmethod
    def time_derivative(cls, time_derivative_string):
        """Creates an TimeDerivative object from a string"""
        import nineml.abstraction_layer as al
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
        return al.TimeDerivative( dependent_variable = dependent_variable, 
                               rhs= rhs )


    @classmethod
    def state_assignment(cls, state_assignment_string):
        import nineml.abstraction_layer as al
        """Creates an StateAssignment object from a string"""
        lhs, rhs = state_assignment_string.split('=')
        return al.StateAssignment( lhs=lhs, rhs=rhs )



class MathUtil(object):
    """Useful static methods for manipulating expressions that are string"""

    @classmethod
    def str_expr_replacement( cls, frm, to, expr_string, func_ok=False):
        """ replaces all occurences of name 'frm' with 'to' in expr_string
        ('frm' may not occur as a function name on the rhs) ...
        'to' can be an arbitrary string so this function can also be used for
        argument substitution.

        This function *will* substitute standard builtin symbols, for example
        ``e`` and ``sin``.

        Returns the resulting string. """


        # Escape the original string, so we can handle special 
        # characters.
        frm = re.escape(frm)


        # do replace using regex
        # this matches names, using lookahead and lookbehind to be sure we don't
        # match for example 'xp' in name 'exp' ...
        if func_ok:
            # func_ok indicates we may replace a function name
            p_func = re.compile(r"(?<![a-zA-Z_0-9])(%s)(?![a-zA-Z_0-9])" % frm)
        else:
            # this will not replace a function name even if its name matches
            # from due to the lookahead disallowing '('
            p_func = re.compile(r"(?<![a-zA-Z_0-9])(%s)(?![(a-zA-Z_0-9])" % frm)
        return p_func.sub(to, expr_string)

    @classmethod 
    def get_rhs_substituted(cls, expr_obj, namemap ):
        expr = expr_obj.rhs
        for frm,to in namemap.iteritems():
            expr =  MathUtil.str_expr_replacement( frm, to, expr, func_ok=False)
        return expr
        

    @classmethod 
    def get_prefixed_rhs_string(cls, expr_obj, prefix="", exclude=None ):
        """Prefixes variable names in a string. This will not toouch
        math-builtins such as ``pi`` and ``sin``, (i.e. neither standard constants
        not variables)"""
        
        expr = expr_obj.rhs
        for name in expr_obj.rhs_names:
            if exclude and name in exclude:
                continue
            expr = MathUtil.str_expr_replacement(name, prefix+name, expr)
        for func in expr_obj.rhs_funcs:
            if not nineml.maths.is_builtin_symbol(func):
                expr = MathUtil.str_expr_replacement(func, 
                                                     prefix+func, 
                                                     expr, 
                                                     func_ok=True)
        return expr


    @classmethod
    def is_single_symbol(cls, expr):
        """Returns ``True`` if the expression is a single symbol, possibly
        surrounded with white-spaces
        
        >>> is_single_symbol('hello')
        True

        >>> is_single_symbol('hello * world')
        False

        """

        expr = expr.strip()
        single_symbol = re.compile("^[a-zA-Z_]+[a-zA-Z_0-9]*$")
        m = single_symbol.match(expr) 
        return m is not None


