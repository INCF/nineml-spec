import expressions
from expressions import Expression

class Condition(expressions.Expression):

    def accept_visitor(self, visitor, **kwargs):
        """ |VISITATION| """
        return visitor.visit_condition(self, **kwargs)

    def __init__(self, rhs):
        Expression.__init__(self, rhs)


    def _parse_rhs(self, rhs):
        #from cond_parse import cond_parse
        import parse
        return parse.cond(rhs)
        


    #def is_bool(self):
    #    """ Checks if conditions is pure bool: True, False"""
    #    if self.names==set() and self.funcs==set():
    #        val = self.rhs_as_python_func()()
    #        if val==False:
    #            return True
    #        else:
    #            assert val==True
    #            return True

    def rhs_as_python_func(self, namespace={}):
        """ Returns a python callable which evaluates the expression in
        namespace and returns the result """

        # overriding Expression.python_func
        import math_namespace

        
        expr = self.rhs
        # fix syntax to match python
        expr = expr.replace('&', ' and ')
        expr = expr.replace('|', ' or ')
        expr = expr.replace('!',' not ')
        # use name_replace, as we don't want to replace 'mytrue' to 'myTrue'
        expr = self.name_replace('true', 'True', expr)
        expr = self.name_replace('false', 'False', expr)

        return eval("lambda %s: %s" % (','.join(self.names), expr), math_namespace.namespace, namespace)


    def __repr__(self):
        return "Condition('%s')" % (self.rhs)





def cond_to_obj(cond_str):

    if isinstance(cond_str, Condition):
        return cond_str

    elif cond_str == None:
        return None

    elif isinstance(cond_str, str):
        return Condition(cond_str.strip())

    raise ValueError, "Condition: expected None, str, or Condition object"

    
