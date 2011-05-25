

from nineml.abstraction_layer.xmlns import *
from nineml.abstraction_layer import math_namespace

def get_args(s):
    """ return arguments of a function in a list,
    handling functions in the arguments. """

    # bracket level count
    bl = 0
    last_arg_end = 0
    args = []

    if s[0]==",":
        raise ValueError, "get_args: missing first arg."

    if s[0]==")":
        return 0,[]
    
    for i in xrange(len(s)):
        if s[i]=="(":
            bl+=1
        elif s[i]=="," and bl==0:
            args+=[s[last_arg_end:i].strip()]
            last_arg_end=i+1
        elif s[i]==")":
            if bl==0:
                args+=[s[last_arg_end:i].strip()]
                return i, args
            bl-=1



class RegimeElement(object):
    """ Base class for all things that can be elements of a regime """
    pass


class Expression(object):
    """ This is a base class for Expressions and Conditionals which provides
    the basic interface for parsing, yielding of python functions,
    C equivalents, binding and name substitution """

    def parse(self):
        """ parses and checks validity of rhs """
        from nineml.abstraction_layer.expr_parse import expr_parse

        self.names, self.funcs = expr_parse(self.rhs)

        # Parser now does this check
        
        #undef_funcs = funcs.difference(math_namespace.functions)
        #if undef_funcs:
        #    funcs.difference(math_namespace.functions)
        #    raise ValueError, "In expression '%s', undefined functions: %s" % \
        #          (e.as_expr(),repr(list(undef_funcs)))


    def python_func(self,namespace={}):
        """ Returns a python callable which evaluates the expression in namespace and returns the result """
        return eval("lambda %s: %s" % (','.join(self.names),self.rhs), math_namespace.namespace,namespace)


    def prefix(self, prefix="", exclude=[], expr=None):
        """ Applies a prefix to all names & funcs if not in math_namespace
        returns new expr ... does not modify inplace

        Exclude is a list of names (not functions) to be excluded from prefixing

        If expr is None, the prefixing is computed for self.rhs and returned.
        self.rhs is not modified.
        
        """

        # names that are in math_symbol space do not show up in self.names
        if expr==None:
            expr = self.rhs
        for name in self.names:
            if name in exclude: continue
            expr = Expression.name_replace(name,prefix+name,expr)
        for func in self.funcs:
            if func not in math_namespace.namespace:
                expr = Expression.name_replace(func,prefix+func,expr, func_ok=True)
        return expr



    


    def rhs_name_transform(self, name_map):
        """
        Returns a string represenation of the rhs with
        expr symbol names replaced as follows:

        from_name->to_name

        Where nam_map should be dictionary like of the form:
        name_map[from]=to
        
        """

        # names that are in math_symbol space do not show up in self.names
        expr = self.rhs
        for name in name_map:
            expr = Expression.name_replace(name,name_map[name],expr)

        return expr


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
        

    def substitute_binding(self,b):
        """ replaces all occurences of binding symbol or function with the binding rhs with arguments substituted """
        import re

        if b.args==():
            # check b.name is not used as a function
            p_func = re.compile(r"(^|([ */+-,(]+))%s\(" % b.name)
            if p_func.search(self.rhs):
                raise ValueError, "substituting non-function binding '%s', found use in '%s' as function." % (b.name, self.rhs)

            self.rhs = Expression.name_replace(b.name,"(%s)" % b.rhs,self.rhs)
        else:
            # binding is a function
            # find all occurences of start of the function
            # don't know where is the end yet, thats the job of get_args
            p_func = re.compile(r"(?<![a-zA-Z_0-9])%s\(" % b.name)

            # accumulate string parts here, to be joined at the end.
            parts = []
            i_start = 0
            i_end = 0
            m = p_func.search(self.rhs)
            while m:
                # add un-modified bit to parts
                parts+=[self.rhs[i_start:m.start()]]

                # traverse arguments 
                i, args = get_args(self.rhs[m.end():])
                i_start = m.end()+i+1

                # replace occurences in function arguments as well
                for j,arg in enumerate(args):
                    e = Expression()
                    e.rhs = arg
                    e.substitute_binding(b)
                    args[j]=e.rhs
                
                # this is the string to replace
                func = self.rhs[m.start():i_start]
                
                if not len(args) == len(b.args):
                    raise ValueError, "Substituting function binding: mis-match on number of function arguments.\n"+\
                          "Expression: '%s', Binding: '%s', on: '%s', arg: '%s', Binding args: '%s'" % (self.as_expr(), b.as_expr(),func,args,b.args)
                subs_expr = "(%s)" % b.rhs

                #subs_expr
                for frm,to in zip(b.args,args):
                    subs_expr = Expression.name_replace(frm,to,subs_expr)
                parts+=[subs_expr]

                # match next after the closing bracket of the function
                # this ensures recursive function calls don't get
                # replaced yet
                m = p_func.search(self.rhs,i_start)

            parts+=[self.rhs[i_start:]]
            self.rhs = "".join(parts)

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



    # Interface:
    def clone(self, prefix="", prefix_excludes=None, clone_name=True, prefix_name=False):
        prefix_excludes = [] if not prefix_excludes else prefix_excludes
        
        name = self.name if clone_name else None
        if name and prefix_name: name = prefix + name
        
        return self._clone(prefix=prefix,prefix_excludes=prefix_excludes,name=name)
        
        
    def _clone(self, prefix, prefix_excludes, name ):
        raise NotImplementedError()
        
        
        
        
        
        
    
    
    

class Binding(Expression, RegimeElement):
    # EM: In the context of NEST and GPU code generation, bindings make sense:
    #  They are constants, i.e. a binding which takes state vars or ports in rhs
    #  should throw an exception.
    #  Users can specify them manually for eash of short hands,
    #  but automatic symbolic symplification of the expressions may well produce
    #  new bindings which can be pre-calculated outside of the integration loop.
    #
    #  Let's keep this in mind, and keep Bindings as we move forward!
    
    element_name = "binding"
    
    def __init__(self, lhs,rhs):

        import re

        self.name, self.args, self.value = Binding.pre_parse(lhs + ":=" + rhs)

        if self.name in math_namespace.symbols:
            raise ValueError, "binding '%s' redefines math symbols (such as 'e','pi')" % self.as_expr()

        self.parse()

        if self.name in self.names:
            raise ValueError, "Binding expression '%s': may not self reference." % self.name

        # detect recursive binding
        if self.args and self.name in self.funcs:
            raise ValueError, "Binding expression '%s': is recursive." % self.as_expr()

        # check that rhs depends on all args
        if self.args and self.names.intersection(self.args)!=set(self.args):
            raise ValueError, "Binding expression '%s': rhs does not depend on some of the arguments ." % self.as_expr()

        # remove args from names 
        self.names.difference_update(self.args)

        if self.name in self.args:
            raise ValueError, "Binding expression '%s': function binding has argument symbol = binding symbol." % self.name


    def __repr__(self):
        return "<Binding: %s>" % self.as_expr()

    def prefix(self,prefix=""):
        return prefix+Expression.prefix(self,prefix,exclude=self.args, expr=self.as_expr())

    @classmethod
    def match(cls,s):
        """ Checks the syntax of the lhs to be that of a binding
        rhs parsing is not yet performed """

        try:
            cls.pre_parse(s)
        except ValueError:
            return False

        return True

    @classmethod
    def pre_parse(cls,s):
        """ Determines if the lhs is a symbol binding, or function binding

        If symbol:

        return symbol, (), rhs

        If function:

        return symbol, args, rhs
        where args is a tuple of function argument symbols
        """

        import re
        
        if s.count(':=')!=1:
            raise ValueError, "Invalid binding syntax. Must contain ':=' once"

        lhs,rhs = s.split(":=")

        lhs = lhs.strip()
        rhs = rhs.strip()

        p_binding_symbol = re.compile("^[a-zA-Z_]+[a-zA-Z_0-9]*$")

        # not a function
        if p_binding_symbol.match(lhs):
            return lhs,(),rhs

        # lhs matches a function?

        func_regex = r"[a-zA-Z_]+[a-zA-Z_0-9]*[ ]*\([ ]*([a-zA-Z_]+[a-zA-Z_0-9]*)([ ]*,[ ]*[a-zA-Z_]+[a-zA-Z_0-9]*)*[ ]*\)"

        p_binding_func = re.compile(func_regex)

        if not p_binding_func.match(lhs):
            raise ValueError, "Invalid binding lhs syntax '%s'. Not symbol binding, and not a function" % lhs

        symbol,rest = lhs.split("(")
        symbol = symbol.strip()

        args = rest.split(",")
        args[-1] = args[-1].replace(")","")
        args = [arg.strip() for arg in args]
        
        return symbol,tuple(args),rhs

    def get_rhs(self):
        return self.value

    def set_rhs(self,v):
        self.value = v

    rhs = property(get_rhs, set_rhs)

    @property
    def lhs(self):
        if self.args:
            return self.name+"("+", ".join(self.args)+")"
        else:
            return self.name

    @property
    def to(self):
        return self.name

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.name == other.name and self.value == other.value

    def to_xml(self):
        return E(self.element_name,
                 E("math-inline", self.value),
                 name=self.lhs)

    def as_expr(self):
        return "%s := %s" % (self.lhs, self.value)

    @classmethod
    def from_xml(cls, element):
        return cls(element.get("name"), element.find(NINEML+"math-inline").text)




            
class Equation(Expression):
    pass
        



class ODE(Equation, RegimeElement):
    """ 
    Represents a first-order, ordinary differential equation.
    """
    element_name = "ode"
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
        self.parse()


        
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
    



class Assignment(Equation, RegimeElement):
    element_name = "assignment"
    n = 0
        
    # Interface:
    def _clone(self, prefix, prefix_excludes, name ):
    
        to = self.to if self.to in prefix_excludes else prefix + self.to
        return Assignment( 
                    to = to,
                    expr = Expression.prefix(self,prefix=prefix,exclude=prefix_excludes,expr=self.rhs),
                    name = name
                    )
    
   
    def __init__(self, to, expr, name=None):
        self.to = to
        self.expr = expr
        self.name = name or ("Assignment%d" % Assignment.n)

        if self.to in math_namespace.symbols:
            raise ValueError, "Assignment '%s' redefines math symbols (such as 'e','pi')" % self.as_expr()

        Assignment.n += 1

        self.parse()


    def get_rhs(self):
        return self.expr
    def set_rhs(self,v):
        self.expr = v
    rhs = property(get_rhs, set_rhs)

    @property
    def lhs(self):
        return self.to

    def self_referencing(self):
        """ Returns True if the assignment is of the form U = f(U,...), otherwise False"""
        return self.to in self.names

    def __repr__(self):
        return "Assignment('%s', '%s')" % (self.to, self.expr)

    def as_expr(self):
        return "%s = %s" % (self.to,
                            self.expr)

    #def prefix(self,prefix=""):
    #    return ("%s = " % (prefix+self.to,)) + Expression.prefix(self,prefix)

    def __eq__(self, other):
        from operator import and_

        if not isinstance(other, self.__class__):
            return False

        return reduce(and_, (self.name == other.name,
                             self.to == other.to,
                             self.expr == other.expr))

    def to_xml(self):
        return E(self.element_name,
                 E("math-inline", self.expr),
                 name=self.name,
                 to=self.to)
                 
    @classmethod
    def from_xml(cls, element):
        assert element.tag == NINEML+cls.element_name
        math = element.find(NINEML+"math-inline").text
        return cls(to=element.get("to"), name=element.get("name"),
                   expr=math)



class Inplace(Equation):
    element_name = "inplace"
    n = 0
    op_name_map = {'+=':'Add','-=':'Sub','*=':'Mul','/=':'Div'}

    op = "+="
    
    def get_rhs(self):
        return self.expr
    def set_rhs(self,v):
        self.expr = v
    rhs = property(get_rhs, set_rhs)

    @property
    def lhs(self):
        return self.to

    def __init__(self, to, op, expr, name=None):
        
        self.to = to
        self.op = op

        # catch invalid ops and give the user feedback
        try:
            self.op_name = self.op_name_map[op]
        except KeyError:
            raise ValueError, "Unsupported inplace operation '%s', supported ops: %s" %(self.op_name, str(self.op_name_map))
        
        self.expr = expr

        if self.to in math_namespace.symbols:
            raise ValueError, "Inplace '%s' operates on math symbols (such as 'e','pi')" % self.as_expr()

        self.name = name or ("Inplace%s%d" % (self.op_name,Inplace.n))
        Inplace.n += 1
        self.parse()


    def __repr__(self):
        return "Inplace('%s', '%s', '%s')" % (self.to,self.op,self.expr)

    def as_expr(self):
        return "%s %s %s" % (self.to,self.op, self.expr)

    def as_assignment(self):
        expr = "%s %s %s" % (self.to, self.op[0], self.expr)
        return Assignment(self.to, expr, self.name)

    #def prefix(self,prefix=""):
    #    return ("%s %s " % (prefix+self.to,self.op)) + Expression.prefix(self,prefix)

    def __eq__(self, other):
        from operator import and_

        if not isinstance(other, self.__class__):
            return False

        return reduce(and_, (self.name == other.name,
                             self.to == other.to,
                             self.op == other.op,
                             self.expr == other.expr))

    def to_xml(self):
        return E(self.element_name,
                 E("math-inline", self.expr),
                 name=self.name,
                 to=self.to, op=self.op)
                 
    @classmethod
    def from_xml(cls, element):
        assert element.tag == NINEML+cls.element_name
        math = element.find(NINEML+"math-inline").text
        return cls(to=element.get("to"), op=element.get("op"), expr=math,
                   name=element.get("name"))

# factories for Inplace ops
def InplaceAdd(to,expr):
    return Inplace(to,'+=',expr)

def InplaceSub(to,expr):
    return Inplace(to,'-=',expr)

def InplaceMul(to,expr):
    return Inplace(to,'*=',expr)

def InplaceDiv(to,expr):
    return Inplace(to,'/=',expr)


def expr_to_obj(s, name = None):
    """ Construct nineml objects from expressions """ 

    import re

    # Is our job already done?
    if isinstance(s,(RegimeElement,Inplace)):
        return s

    # strip surrounding whitespace
    s = s.strip()

    # Do we have a binding?
    if Binding.match(s):
        lhs, rhs = [x.strip() for x in s.split(":=")]
        return Binding(lhs, rhs)

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

    # Do we have an Inplace op?
    if op in Inplace.op_name_map.keys():
        return Inplace(lhs,op,rhs, name = name)

    # Do we have an assignment?
    if op=="=":
        return Assignment(lhs,rhs, name = name)
        
    # If we get here, what do we have?
    raise ValueError, "Cannot map expr '%s' to a nineml Expression" % s


