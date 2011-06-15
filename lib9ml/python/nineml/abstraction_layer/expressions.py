
import re

from nineml.abstraction_layer.xmlns import *
from nineml.abstraction_layer import math_namespace
import itertools,re




class RegimeElement(object):
    """ Base class for all things that can be elements of a regime """
    pass






from nineml.abstraction_layer.expr_parse import expr_parse



#class MathStringUtils(object):
#    @classmethod
#    def prefix(cls, prefix="", exclude=[], expr=None):
#        assert expr
#        """ Applies a prefix to all names & funcs if not in math_namespace
#        returns new expr ... does not modify inplace
#
#        Exclude is a list of names (not functions) to be excluded from prefixing
#
#        If expr is None, the prefixing is computed for self.rhs and returned.
#        self.rhs is not modified.
#        
#        """
#
#        # names that are in math_symbol space do not show up in self.names
#        for name in self.names:
#            if name in exclude: continue
#            expr = Expression.name_replace(name,prefix+name,expr)
#        for func in self.funcs:
#            if func not in math_namespace.namespace:
#                expr = Expression.name_replace(func,prefix+func,expr, func_ok=True)
#        return expr





class Expression(object):

    """ This is a base class for Expressions and Conditionals which provides
    the basic interface for parsing, yielding of python functions,
    C equivalents, alias and name substitution """


    # Subclasses can over-ride this, if need be.
    def _parse_rhs(self,rhs):
        # A temporary measure, this is until the parser is 
        # generalised to handle conditionals
        return expr_parse(rhs)

    def set_rhs(self, rhs):
        self._rhs = rhs
        self._names, self._funcs = self._parse_rhs(rhs)
        for n in self._names: assert not n in self._funcs
        for n in self._funcs: assert not n in self._names

    def get_rhs(self):
        return self._rhs
    def get_names(self):
        return self._names
    def get_funcs(self):
        return self._funcs
    def get_atoms(self):
        return itertools.chain(self.names, self.funcs)

    rhs = property(get_rhs,set_rhs)
    names = property(get_names) 
    funcs = property(get_funcs)
    atoms = property(get_atoms)






    # Interface:
    def clone(self, prefix="", prefix_excludes=None, clone_name=True, prefix_name=False):
        prefix_excludes = [] if not prefix_excludes else prefix_excludes
        
        name = self.name if clone_name else None
        if name and prefix_name: name = prefix + name
        
        return self._clone(prefix=prefix,prefix_excludes=prefix_excludes,name=name)
        
        
    def _clone(self, prefix, prefix_excludes, name ):
        print "ERROR: _clone is not implemented in the subclass:", self.__class__.__name__
        raise NotImplementedError()
    
    def prefix(self, prefix="", exclude=[], expr=None):
        """ Applies a prefix to all names & funcs if not in math_namespace
        returns new expr ... does not modify inplace

        Exclude is a list of names (not functions) to be excluded from prefixing

        If expr is None, the prefixing is computed for self.rhs and returned.
        self.rhs is not modified.
        
        """

        # names that are in math_symbol space do not show up in self.names
        if expr==None:
            assert False
            expr = self.rhs
        for name in self.names:
            if name in exclude: continue
            expr = Expression.name_replace(name,prefix+name,expr)
        for func in self.funcs:
            if func not in math_namespace.namespace:
                expr = Expression.name_replace(func,prefix+func,expr, func_ok=True)
        return expr


    def python_func(self,namespace={}):
        """ Returns a python callable which evaluates the expression in namespace and returns the result """
        return eval("lambda %s: %s" % (','.join(self.names),self.rhs), math_namespace.namespace,namespace)



    # This should be over-ridden by subclasses:
    def name_transform_inplace(self, name_map):
        # Remap the rhs
        self.rhs_name_transform_inplace( name_map ) 

    # We do not provide a name_transform_clone(), because
    # this can be made more explicit by
    # expr.clone().name_transform_inplace()



    def rhs_name_transform_inplace(self, name_map):
        self.rhs = self.rhs_name_transform( name_map=name_map )     
    
    def rhs_name_transform(self, name_map):
        """
        Returns a string represenation of the rhs with
        expr symbol names replaced as follows:

        from_name->to_name

        Where nam_map should be dictionary like of the form:
        name_map[from]=to
        
        """

        rhs = self.rhs
        for name in name_map:
            rhs = Expression.name_replace(name,name_map[name],rhs)
        return rhs


    @classmethod
    def name_replace(cls,frm,to,rhs, func_ok=False):
        """ replaces all occurences of name 'frm' with 'to' in rhs (not self.rhs) ('frm' may not occur as a function name on the rhs) ...
        'to' can be an arbitrary string so this function can also be used for argument substitution.

        Does not write inplace to self.rhs, but returns the resulting string. """

        import re

        # do replace using regex
        # this matches names, using lookahead and lookbehind to be sure we don't
        # match for example 'xp' in name 'exp' ...
        if func_ok:
            # func_ok indicates we may replace a function name
            p_func = re.compile(r"(?<![a-zA-Z_0-9])(%s)(?![a-zA-Z_0-9])" % frm)
        else:
            # this will not replace a function name even if its name matches from
            # due to the lookahead disallowing '('
            p_func = re.compile(r"(?<![a-zA-Z_0-9])(%s)(?![(a-zA-Z_0-9])" % frm)
        return p_func.sub(to, rhs)
        

    def substitute_alias(self,b):
        # check b.name is not used as a function
        p_func = re.compile(r"(^|([ */+-,(]+))%s\(" % b.name)
        if p_func.search(self.rhs):
            raise ValueError, "substituting non-function alias '%s', found use in '%s' as function." % (b.name, self.rhs)
        self.rhs = Expression.name_replace(b.name,"(%s)" % b.rhs,self.rhs)
        
    @property
    def missing_functions(self):
        """ yield names of functions in the rhs which are not in the math namespace"""
        for f in self.funcs:
            if f not in math_namespace.namespace:
                yield f

    def has_missing_functions(self):
        """ returns True if at least 1 function on the rhs is not in the math namespace"""
        for f in self.funcs:
            if f not in math_namespace.namespace:
                return True
        return False






# TO GO:
class Equation(Expression):
    pass



class ExpressionWithLHS(Equation):
    # Sub-classes should over ride this, to allow 
    # proper-prefixing:
        
    def name_transform_inplace(self, name_map):
        # Transform the lhs & rhs:
        self.lhs_name_transform_inplace( name_map )
        self.rhs_name_transform_inplace( name_map ) 


    def get_atoms(self):
        return itertools.chain(self.names, self.funcs, self.get_lhs_atoms() )
    atoms = property(get_atoms)
    
    def lhs_name_transform_inplace(self, name_map):
        raise NotImplementedError()
    def get_lhs_atoms(self):
        raise NotImplementedError()
    def get_lhs():
        raise NotImplementedError() 

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.lhs == other.lhs and self.rhs == other.rhs





        
        
class ExpressionWithSimpleLHS(ExpressionWithLHS):

    def __init__(self, lhs, rhs):
        lhs=lhs.strip()
        single_symbol = re.compile("^[a-zA-Z_]+[a-zA-Z_0-9]*$")
        assert single_symbol.match( lhs ) 
        self.lhs = lhs
        self.rhs = rhs

    def get_lhs_atoms(self):
        return [self.lhs]

    def lhs_name_transform_inplace( self, name_map ):
        self.lhs = name_map.get(self.lhs,self.lhs) 
   

    # Syntactic Sugar - to remove:
    # -----------------------------
    @property
    def to(self):
        return self.lhs
    @property
    def name(self):
        return self.lhs
    





class Alias(ExpressionWithSimpleLHS, RegimeElement):
    
    element_name = "Alias"
    

    def __init__(self, lhs,rhs):
        ExpressionWithSimpleLHS.__init__(self, lhs, rhs)

    def __repr__(self):
        return "<Alias: %s := %s>" % (self.lhs,self.rhs)

    def _clone(self, prefix, prefix_excludes, name ):

        def doPrefix(atom):
            if a in prefix_excludes: return False
            if math_namespace.is_in_math_namespace(a): return False
            return True

        b = Alias( lhs = self.lhs, rhs = self.rhs )
        name_map = dict( [ (a, prefix+a) for a in self.atoms if doPrefix(a) ])
        b.name_transform_inplace( name_map = name_map )
        return b

    # XML Serialisation:
    def to_xml(self):
        return E(self.element_name,
                 E("math-inline", self.rhs),
                 name=self.lhs)

    def AcceptVisitor(self, visitor,**kwargs):
        return visitor.VisitAlias(self, **kwargs)


    @classmethod
    def from_xml(cls, element):
        return cls(element.get("name"), element.find(NINEML+"math-inline").text)


    # Deprecated - to remove:
    def as_expr(self):
        return "%s := %s" % (self.lhs, self.rhs)








class Assignment(ExpressionWithSimpleLHS, RegimeElement):
    element_name = "StateAssignment"
    n = 0

    def AcceptVisitor(self, visitor, **kwargs):
        return visitor.VisitAssignment(self, **kwargs)



        
    # Interface:
    def _clone(self, prefix, prefix_excludes, name ):
    
        to = self.to if self.to in prefix_excludes else prefix + self.to
        return Assignment( 
                    to = to,
                    expr = Expression.prefix(self,prefix=prefix,exclude=prefix_excludes,expr=self.rhs),
                    name = name
                    )
    
   
    def __init__(self, to, expr, name=None):
        ExpressionWithSimpleLHS.__init__(self, lhs=to, rhs=expr)


    def self_referencing(self):
        """ Returns True if the assignment is of the form U = f(U,...), otherwise False"""
        return self.to in self.names

    def __repr__(self):
        return "Assignment('%s', '%s')" % (self.to, self.rhs)

    def as_expr(self):
        return "%s = %s" % (self.lhs, self.rhs)

    def to_xml(self):
        return E(self.element_name,
                 E("math-inline", self.rhs),
                 name=self.name,
                 to=self.to)
                 
    @classmethod
    def from_xml(cls, element):
        assert element.tag == NINEML+cls.element_name
        math = element.find(NINEML+"math-inline").text
        return cls(to=element.get("to"), name=element.get("name"),
                   expr=math)






            
        



class ODE(Equation, RegimeElement):
    """ 
    Represents a first-order, ordinary differential equation.
    """
    element_name = "TimeDerivative"
    n = 0
    

    def _clone(self, prefix, prefix_excludes, name ):
    
        dep = self.dependent_variable if self.dependent_variable in prefix_excludes else prefix + self.dependent_variable
        indep = self.indep_variable if self.indep_variable in prefix_excludes else prefix + self.indep_variable
         
        return ODE( 
                    dependent_variable = dep,
                    indep_variable =     indep,
                    rhs = Expression.prefix(self,prefix=prefix,exclude=prefix_excludes,expr=self.rhs),
                    name = name
                    )

    
    def __init__(self, dependent_variable, indep_variable, rhs, name=None):
        self.dependent_variable = dependent_variable
        self.indep_variable = indep_variable
        self.rhs = rhs

        if self.dependent_variable in math_namespace.symbols:
            raise ValueError, "ODE '%s' redefines math symbols (such as 'e','pi')" % self.as_expr()

        self.name = name or ("ODE%d" % ODE.n)
        ODE.n += 1
        #self.parse()


        
    def __repr__(self):
        return "ODE(d%s/d%s = %s)" % (self.dependent_variable,
                                      self.indep_variable,
                                      self.rhs)

    def __eq__(self, other):
        from operator import and_

        if not isinstance(other, self.__class__):
            return False

        return reduce(and_, (self.name == other.name,
                             self.dependent_variable == other.dependent_variable,
                             self.indep_variable == other.indep_variable,
                             self.rhs == other.rhs))


    @property
    def lhs(self):
        return "d%s/d%s" % (self.dependent_variable, self.indep_variable)

    def _get_to(self):
        return self.dependent_variable
    def _set_to(self, name):
        self.dependent_variable = name
        
    to = property(fget=_get_to, fset=_set_to)
    
    def as_expr(self):
        return "d%s/d%s = %s" % (self.dependent_variable,
                                 self.indep_variable,
                                 self.rhs)

    
    def AcceptVisitor(self, visitor, **kwargs):
        return visitor.VisitODE(self,**kwargs)


    def to_xml(self):
        return E(self.element_name,
                 E("math-inline", self.rhs),
                 name=self.name,
                 dependent_variable=self.dependent_variable,
                 independent_variable = self.indep_variable)

    @classmethod
    def from_xml(cls, element):
        assert element.tag == NINEML+cls.element_name
        rhs = element.find(NINEML+"math-inline").text
        return cls(element.get("dependent_variable"),
                   element.get("independent_variable"),
                   rhs,
                   name=element.get("name"))
    







def expr_to_obj(s, name = None):
    """ Construct nineml objects from expressions """ 

    import re

    # Is our job already done?
    #if isinstance(s,(RegimeElement,Inplace)):
    if isinstance(s,(RegimeElement)):
        return s

    # strip surrounding whitespace
    s = s.strip()

    # Do we have a alias?
    if ":=" in s:
        lhs, rhs = [x.strip() for x in s.split(":=")]
        return Alias(lhs, rhs)

    #if Alias.match(s):
    #    lhs, rhs = [x.strip() for x in s.split(":=")]
    #    return Alias(lhs, rhs)

    # re for an expression -> groups into lhs, op, rhs
    p_eqn = re.compile(r"(?P<lhs>[a-zA-Z_]+[a-zA-Z_0-9]*(/?[a-zA-Z_]+[a-zA-Z_0-9]*)?)\s*(?P<op>[+\-*/:]?=)\s*(?P<rhs>.*)")
    # re for lhs for ODE
    p_ode_lhs = re.compile(r"(?:d)([a-zA-Z_]+[a-zA-Z_0-9]*)/(?:d)([a-zA-Z_]+[a-zA-Z_0-9]*)")

    


    m = p_eqn.match(s)
    if not m:
       
        raise ValueError, "Not a valid nineml expression: %s" % s

    # get lhs, op, rhs
    lhs, op, rhs = [m.group(x) for x in ['lhs','op','rhs']]

    # do we have an ODE?
    m = p_ode_lhs.match(lhs)
    if m:
        if op!="=":
            raise ValueError, "ODE lhs, but op not '=' in %s" % s

        dep_var = m.group(1)
        indep_var = m.group(2)
        return ODE(dep_var,indep_var,rhs, name = name)

    ## Do we have an Inplace op?
    #if op in Inplace.op_name_map.keys():
    #    return Inplace(lhs,op,rhs, name = name)

    # Do we have an assignment?
    if op=="=":
        return Assignment(lhs,rhs, name = name)
        
    # If we get here, what do we have?
    raise ValueError, "Cannot map expr '%s' to a nineml Expression" % s


