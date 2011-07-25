
""" This module defines the namespace of functions and symbols available to 9ml
expressions"""


import numpy





_constants = set(['pi','e'])

_functions = set(['exp','sin','cos','log','log10','pow',
                'sinh','cosh','tanh','sqrt','mod','sum',
                'atan','asin','acos','asinh','acosh','atanh','atan2'])

_reserved_symbols = set(['t'])



math_namespace_separator = '.'

# Specific Namespace:
_random_namespace = {
    'randn' :        numpy.random.randn,     
    'randint' :      numpy.random.randint,  

    # Taken from numpy documentations:
    # http://docs.scipy.org/doc/numpy/reference/routines.random.html

    'binomial':numpy.random.binomial,                   #binomial(n, p)
    'chisquare':numpy.random.chisquare,                 #chisquare(df)
    'mtrand.dirichlet':numpy.random.mtrand.dirichlet,   #mtrand.dirichlet(alpha)
    'exponential':numpy.random.exponential,             #exponential(dfnum, dfden)
    'f':numpy.random.f,                                 #f(dfnum, dfden)
    'gamma':numpy.random.gamma,                         #gamma(shape)
    'geometric':numpy.random.geometric,                 #geometric(p)
    'hypergeometric':numpy.random.hypergeometric,       #hypergeometric(ngood, nbad, nsample)
    'laplace':numpy.random.laplace,                     #laplace()
    'logistic':numpy.random.logistic,                   #logistic()
    'lognormal':numpy.random.lognormal,                 #lognormal()
    'logseries':numpy.random.logseries,                 #logseries(p)
    'negative_binomial':numpy.random.negative_binomial, #negative_binomial(n, p)
    'noncentral_chisquare':numpy.random.noncentral_chisquare, #noncentral_chisquare(df, nonc)
    'noncentral_f':numpy.random.noncentral_f,           #noncentral_f(dfnum, dfden, nonc)
    'normal':numpy.random.normal,                       #normal()
    'pareto':numpy.random.pareto,                       #pareto(a)
    'poisson':numpy.random.poisson,                     #poisson()
    'power':numpy.random.power,                         #power(a)
    'rayleigh':numpy.random.rayleigh,                   #rayleigh()
    'standard_cauchy':numpy.random.standard_cauchy,     #standard_cauchy()
    'standard_exponential':numpy.random.standard_exponential, #standard_exponential()
    'standard_gamma':numpy.random.standard_gamma,       #standard_gamma(shape)
    'standard_normal':numpy.random.standard_normal,     #standard_normal()
    'standard_t':numpy.random.standard_t,               #standard_t(df)
    'triangular':numpy.random.triangular,               #triangular(left, mode, right)
    'uniform':numpy.random.uniform,                     #uniform()
    'vonmises':numpy.random.vonmises,                   #vonmises(mu, kappa)
    'wald':numpy.random.wald,                           #wald(mean, scale)
    'weibull':numpy.random.weibull,                     #weibull(a)
    'zipf':numpy.random.zipf,                           #zipf(a)

    }

_math_namespaces = {
    'random': _random_namespace
        }






















def is_builtin_math_constant( constname ):
    assert isinstance( constname, basestring)
    return constname in _constants

def is_builtin_math_function( funcname ):
    assert isinstance( funcname, basestring)
    
    # Is it a standard mathematical function (cos,sin,etc)
    if funcname in _functions: return True

    # Is it a namespace:
    if math_namespace_separator in funcname:
        namespace, func = funcname.split(math_namespace_separator)
        if not namespace in _math_namespaces:
            err = 'Unrecognised math namespace: %s' % namespace
            raise NineMLMathParseError(err)
        if not func in _math_namespaces[namespace]:
            err = 'Unrecognised function in namespace: %s %s' % (namespace, func)
            raise NineMLMathParseError(err)
        return True

    # OK then, it is not built in.
    return False


def is_builtin_symbol(s):
    return is_builtin_math_constant(s) or is_builtin_math_function(s)

def get_builtin_symbols():

    builtins = _constants | _functions 
    for ns_name in _math_namespaces:
        for func in _math_namespaces[ns_name]:
            builtins.add( '%s%s%s' % (  ns_name,
                                        math_namespace_separator,
                                        func) )
    return builtins


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


def func_namespace_split(func_name):
    """Converts function names from 
    'namespace.func'
    to 
    'namespace','func'
    or None,'func' if it is not in a namespace
    """
    if not math_namespace_separator in func_name:
        return None, func_name
    
    toks = func_name.split(math_namespace_separator)
    if not len(toks) == 2:
        raise NineMLMathParseError('Invalid Namespace: %s'%toks)

    return toks




