""" This module defines the namespace of functions and symbols available to 9ml expressions"""


import math, numpy


symbols = set(['pi','e'])

reserved_symbols = set(['t'])

functions = set(['exp','sin','cos','log','log10','pow','sinh','cosh','tanh','sqrt','mod','sum',
             'atan','asin','acos','asinh','acosh','atanh','atan2'])


namespace = {
    "exp":numpy.exp,
    "sqrt":numpy.sqrt,
    "sin":numpy.sin,
    "cos":numpy.cos,
    "atan2":numpy.arctan2,
    "log":numpy.log,
    "pi":numpy.pi,
    "e":numpy.e
    }
# TODO complete this list


def is_in_math_namespace( func ):
    return func in functions
