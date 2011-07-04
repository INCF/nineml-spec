
""" This module defines the namespace of functions and symbols available to 9ml
expressions"""


import numpy



#reserved_symbols = set(['t'])



#namespace = {
#    "exp":numpy.exp,
#    "sqrt":numpy.sqrt,
#    "sin":numpy.sin,
#    "cos":numpy.cos,
#    "atan2":numpy.arctan2,
#    "log":numpy.log,
#    "pi":numpy.pi,
#    "e":numpy.e
#    }
# TODO complete this list





_constants = set(['pi','e'])

_functions = set(['exp','sin','cos','log','log10','pow',
                'sinh','cosh','tanh','sqrt','mod','sum',
                'atan','asin','acos','asinh','acosh','atanh','atan2'])

_reserved_symbols = set(['t'])



def is_builtin_math_constant( constname ):
    assert isinstance( constname, basestring)
    return constname in _constants

def is_builtin_math_function( funcname ):
    assert isinstance( funcname, basestring)
    return funcname in _functions

def is_builtin_symbol(s):
    return is_builtin_math_constant(s) or is_builtin_math_function(s)

def get_builtin_symbols():
    return _constants | _functions 


def is_reserved(s):
    return s in _reserved_symbols

def is_valid_lhs_target(sym):
    
    return not( is_builtin_symbol(sym) or is_reserved(sym) )

def get_reserved_and_builtin_symbols():
    return get_builtin_symbols() | _reserved_symbols


str_to_npfunc_map = {
    "exp":numpy.exp,
    "sqrt":numpy.sqrt,
    "sin":numpy.sin,
    "cos":numpy.cos,
    "atan2":numpy.arctan2,
    "log":numpy.log,
    "pi":numpy.pi,
    "e":numpy.e
    }
