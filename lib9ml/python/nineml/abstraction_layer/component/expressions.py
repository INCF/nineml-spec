"""This file defines mathematical classes and derived classes"""

import re
import itertools

#import math_namespace
from nineml.exceptions import NineMLRuntimeError
import util

import parse
#from expr_parse import expr_parse
import nineml



class RegimeElement(object):
    """ Base class for all things that can be elements of a regime """
    pass




class Expression(object):

    """ This is a base class for Expressions and Conditionals which provides
    the basic interface for parsing, yielding of python functions,
    C equivalents, alias and name substitution """

    def __init__(self, rhs):
        self._rhs = None
        self._rhs_names = None
        self._rhs_funcs = None

        self._set_rhs(rhs)

    # Subclasses can over-ride this, if need be.
    def _parse_rhs(self, rhs):
        # A temporary measure, this is until the parser is 
        # generalised to handle conditionals
        #return parse.expr_parse(rhs)
        return parse.expr(rhs)

    
    # If we assign to rhs, then we need to update the 
    # cached names and funcs:
    def _set_rhs(self, rhs):
        self._rhs = rhs
        self._rhs_names, self._rhs_funcs = self._parse_rhs(rhs)
        for name in self._rhs_names: 
            assert not name in self._rhs_funcs
        for func in self._rhs_funcs: 
            assert not func in self._rhs_names

    def _get_rhs(self):
        return self._rhs
    rhs = property(_get_rhs, _set_rhs)


    @property
    def rhs_names(self):
        return self._rhs_names
    
    @property
    def rhs_funcs (self):
        return self._rhs_funcs

    @property
    def rhs_atoms (self):
        return itertools.chain(self.rhs_names, self.rhs_funcs)

    def rhs_as_python_func(self, namespace=None):
        namespace = namespace or {}
        """ Returns a python callable which evaluates the expression in
        namespace and returns the result """

        return eval("lambda %s: %s" % (','.join(self.rhs_names), self.rhs), \
                math_namespace.namespace, namespace)


    def rhs_name_transform_inplace(self, name_map):
        """Replace atoms on the RHS with values in the name_map"""

        for name in name_map:
            replacment = name_map[name]
            self.rhs = util.MathUtil.str_expr_replacement(name, replacment, self.rhs)

        

    def substitute_alias(self, alias):
        """Substitute an alias into the rhs"""
        sub = "(%s)" % alias.rhs,

        self.rhs = util.MathUtil.str_expr_replacement(alias.lhs, sub, self.rhs)
        

    @property
    def rhs_missing_functions(self):
        """ yield names of functions in the rhs which are not in the math
        namespace"""
        for func in self.rhs_funcs:
            if func not in math_namespace.namespace:
                yield func

    def rhs_has_missing_functions(self):
        """ returns True if at least 1 function on the rhs is not in the math
        namespace"""
        return len(list(self.rhs_missing_functions)) != 0





# TO GO:
class Equation(Expression):
    def __init__(self, rhs):
        Expression.__init__(self, rhs)



class ExpressionWithLHS(Equation):
    # Sub-classes should over ride this, to allow 
    # proper-prefixing:
        
    def __init__(self, rhs):
        Equation.__init__(self, rhs)

    def name_transform_inplace(self, name_map):
        
        # Transform the lhs & rhs:
        self.lhs_name_transform_inplace( name_map )
        self.rhs_name_transform_inplace( name_map ) 

    @property
    def atoms(self):
        return itertools.chain(self.rhs_atoms, self.lhs_atoms )
    
    def lhs_name_transform_inplace(self, name_map):
        raise NotImplementedError()

    @property
    def lhs_atoms(self):
        raise NotImplementedError()






        
        
class ExpressionWithSimpleLHS(ExpressionWithLHS):

    def __init__(self, lhs, rhs):
        ExpressionWithLHS.__init__(self, rhs)

        
        if not util.MathUtil.is_single_symbol( lhs ):
            err = 'Expecting a single symbol on the LHS; got: %s' % lhs
            raise NineMLRuntimeError(err)
        if not nineml.maths.is_valid_lhs_target(lhs):
            err = 'Invalid LHS target: %s'%lhs
            raise NineMLRuntimeError(err)

        self._lhs = lhs.strip()
        

    @property
    def lhs(self):
        return self._lhs

    @property
    def lhs_atoms(self):
        return [self.lhs]

    def lhs_name_transform_inplace( self, name_map ):
        self._lhs = name_map.get(self.lhs, self.lhs) 
   



class Alias(ExpressionWithSimpleLHS, RegimeElement):
    """Aliases are a way of defining a variable local to a ``ComponentClass``, 
    in terms of its ``Parameters``, ``StateVariables`` and input ``Analog
    Ports``. ``Alias``es allow us to reduce the duplication of code in
    ComponentClass definition, and allow allow more complex outputs to
    ``AnalogPort`` than simply individual ``StateVariables``.
   
   When specified from a ``string``, an alias uses the notation ``:=``

    ``Alias``es can be defined in terms of other ``Alias``es, so for example, if
    we had ComponentClass representing a Hodgkin-Huxley style gating channel,
    which has a ``Property``, `reversal_potential`, and an input ``AnalogPort``,
    `membrane_voltage`, then we could define an ``Alias``:: 

        ``driving_force := reversal_potential - membrane_voltage``

    If the relevant ``StateVariables``, ``m`` and ``h``, for example were also
    defined, and a ``Parameter``, ``g_bar``, we could also define the current flowing
    through this channel as::
    
        current := driving_force * g * m * m * m * h

    This current could then be attached to an output ``AnalogPort`` for example.

    It is important to ensure that Alias definitions are not circular, for
    example, it is not valid to define two alias in terms of each other::

        a := b + 1
        b := 2 * a

    During code generation, we typically call ``ComponentClass.backsub_all()``.
    This method first expands each alias in terms of other aliases, such that
    each alias depends only on parameters, statevariables and input analogport.
    Next, it expands any alias definitions within time-derivatives,
    state-assignments, conditionals and output analog-ports.

    

    """

    def __init__(self, lhs, rhs):
        """ Constructor for an Alias 

        :param lhs: A `string` specifying the left-hand-side, i.e. the alias
            name. This should be a single `symbol`.
        :param rhs: A `string` specifying the right-hand-side. This should be a
            mathematical expression, expressed in terms of other aliases,
            state-variables, parameters and input-analogports local to the
            component.
        
        """
        ExpressionWithSimpleLHS.__init__(self, lhs, rhs)

    def __repr__(self):
        return "<Alias: %s := %s>" % (self.lhs, self.rhs)

    def accept_visitor(self, visitor, **kwargs):
        """ |VISITATION| """
        return visitor.visit_alias(self, **kwargs)









class StateAssignment(ExpressionWithSimpleLHS, RegimeElement):

    """Assignments represent a change that happens to the value of a
    ``StateVariable`` during a transition between regimes. 
    
    For example, in an integrate-and-fire neuron, we may want to reset the
    voltage back to zero, after it has reached a certain threshold. In this
    case, we would have an ``OnCondition`` object, that is triggered when
    ``v>vthres``. Attached to this OnCondition transition, we would attach an
    StateAssignment which sets ``v=vreset``.

    The left-hand-side symbol must be a state-variable of the component.

    """

    def __init__(self, lhs, rhs ):
        """StateAssignment Constructor
        
        :param lhs: A `string`, which must be a state-variable of the component.
        :param rhs: A `string`, representing the new value of the state after
            this assignment.
        
        """
        ExpressionWithSimpleLHS.__init__(self, lhs=lhs, rhs=rhs)

    def accept_visitor(self, visitor, **kwargs):
        """ |VISITATION| """
        return visitor.visit_assignment(self, **kwargs)
   

    def __repr__(self):
        return "StateAssignment('%s', '%s')" % (self.lhs, self.rhs)






            
        

class ODE(ExpressionWithLHS, RegimeElement):
    """ An ordinary, first order differential equation.
        
        .. note::
            
            These should not be created directly, this class is
            used as base class for ``TimeDerivative``

    """

    def __init__(self, dependent_variable, independent_variable, rhs): 

        ExpressionWithLHS.__init__(self, rhs)

        self._dependent_variable = dependent_variable
        self._independent_variable = independent_variable


    def __repr__(self):
        return "ODE(d%s/d%s = %s)" % (  self.dependent_variable,
                                        self.independent_variable,
                                        self.rhs)
    @property
    def lhs(self):
        """Return a string of the lhs of the form: 'dS/dt' """
        return "d%s/d%s" % (self.dependent_variable, self.independent_variable)

    @property
    def dependent_variable(self):
        """Return the dependent variable"""
        return self._dependent_variable

    @property
    def independent_variable(self):
        """Return the independent variable"""
        return self._independent_variable


    def lhs_name_transform_inplace( self, name_map ):
        """Replace atoms on the LHS with mapping in name_map """
        
        dep = self._dependent_variable
        self._dependent_variable = name_map.get(dep, dep) 

        indep = self._independent_variable
        self._independent_variable = name_map.get(indep, indep) 

    @property
    def lhs_atoms(self):
        return [self.independent_variable, self.dependent_variable ]


class TimeDerivative(ODE):

    """Represents a first-order, ordinary differential equation with respect to
    time.

    """
    
    def __init__(self, dependent_variable, rhs): 
        """Time Derivative Constructor
        
            :param dependent_variable: A `string` containing a single symbol,
                which is the dependent_variable. 
            :param rhs: A `string` containing the right-hand-side of the
                equation.
                
                
            For example, if our time derivative was:

            .. math::

                \\frac{dg}{dt} = \\frac{g}{gtau}
            
            Then this would be constructed as::

                TimeDerivative( dependent_variable='g', rhs='g/gtau' )

            
            """
        ODE.__init__( self, 
                      dependent_variable = dependent_variable, 
                      independent_variable = 't', 
                      rhs = rhs )

        
    def __repr__(self):
        return "TimeDerivative( d%s/dt = %s )" % \
                (self.dependent_variable, self.rhs)

    def accept_visitor(self, visitor, **kwargs):
        """ |VISITATION| """
        return visitor.visit_timederivative(self, **kwargs)

    









def expr_to_obj(s, name = None):
    """ Construct nineml objects from expressions """ 
    from util import StrToExpr

    #import re

    # Is our job already done?
    if isinstance(s, (RegimeElement)):
        return s

    # strip surrounding whitespace
    s = s.strip()

    # Do we have a alias?
    if StrToExpr.is_alias(s):
        return StrToExpr.alias(s)


    


    # re for an expression -> groups into lhs, op, rhs
    p_eqn = re.compile(r"(?P<lhs>[a-zA-Z_]+[a-zA-Z_0-9]*(/?[a-zA-Z_]+[a-zA-Z_0-9]*)?)\s*(?P<op>[+\-*/:]?=)\s*(?P<rhs>.*)")
    m = p_eqn.match(s)
    if not m:
        raise ValueError, "Not a valid nineml expression: %s" % s

    # get lhs, op, rhs
    lhs, op, rhs = [m.group(x) for x in ['lhs', 'op', 'rhs']]

    # do we have an TimeDerivative?
    # re for lhs for TimeDerivative
    p_ode_lhs = re.compile(r"(?:d)([a-zA-Z_]+[a-zA-Z_0-9]*)/(?:d)([a-zA-Z_]+[a-zA-Z_0-9]*)")
    m = p_ode_lhs.match(lhs)
    if m:
        if op!="=":
            raise ValueError, "TimeDerivative lhs, but op not '=' in %s" % s

        dep_var = m.group(1)
        indep_var = m.group(2)
        return TimeDerivative(dep_var, indep_var, rhs, name = name)

    ## Do we have an Inplace op?
    #if op in Inplace.op_name_map.keys():
    #    return Inplace(lhs,op,rhs, name = name)

    # Do we have an assignment?
    if op=="=":
        return StateAssignment(lhs, rhs, name = name)
        
    # If we get here, what do we have?
    raise ValueError, "Cannot map expr '%s' to a nineml Expression" % s


