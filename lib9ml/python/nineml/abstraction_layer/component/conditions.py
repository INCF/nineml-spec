import expressions
from expressions import Expression

class Condition(expressions.Expression):

    def AcceptVisitor(self, visitor, **kwargs):
        return visitor.VisitCondition(self, **kwargs)

    def __init__(self, rhs):
        Expression.__init__(self,rhs)


    def _parse_rhs(self,rhs):
        from cond_parse import cond_parse
        return cond_parse(rhs)
        


    def is_bool(self):
        """ Checks if conditions is pure bool: True, False"""
        if self.names==set() and self.funcs==set():
            val = self.python_func()()
            if val==False:
                return True
            else:
                assert val==True
                return True

    def rhs_as_python_func(self, namespace={}):
        """ Returns a python callable which evaluates the expression in namespace and returns the result """

        # overriding Expression.python_func
        import math_namespace

        
        expr = self.rhs
        # fix syntax to match python
        expr = expr.replace('&', ' and ')
        expr = expr.replace('|', ' or ')
        expr = expr.replace('!',' not ')
        # use name_replace, as we don't want to replace 'mytrue' to 'myTrue'
        expr = self.name_replace('true','True',expr)
        expr = self.name_replace('false','False',expr)

        return eval("lambda %s: %s" % (','.join(self.names),expr), math_namespace.namespace, namespace)

    def as_expr(self):
        assert False
        """ This is to behave as an expr"""
        return self.rhs

    def __repr__(self):
        return "Condition('%s')" % (self.rhs)





def cond_to_obj(cond_str):

    if isinstance(cond_str,Condition):
        return cond_str

    elif cond_str == None:
        return None

    elif isinstance(cond_str,str):
        return Condition(cond_str.strip())

    raise ValueError, "Condition: expected None, str, or Condition object"

    
