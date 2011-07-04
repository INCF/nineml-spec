
import nineml
import expressions
from expressions import Expression

class Condition(expressions.Expression):

    def accept_visitor(self, visitor, **kwargs):
        """ |VISITATION| """
        return visitor.visit_condition(self, **kwargs)

    def __init__(self, rhs):
        Expression.__init__(self, rhs)


    def _parse_rhs(self, rhs):
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

    #def rhs_as_python_func(self, namespace=None):
    #    """ Returns a python callable which evaluates the expression in
    #    namespace and returns the result """
    #    namespace = namespace or {}

    #    return eval("lambda %s: %s" % (','.join(self.rhs_names), self.rhs), \
    #            nineml.maths.str_to_npfunc_map, namespace)
    #            #math_namespace.namespace, namespace)



    def rhs_as_python_func(self, namespace={}):
        """ Returns a python callable which evaluates the expression in
        namespace and returns the result """


        
        
        import util 
        rhs = self.rhs

        rhs = rhs.replace('!',' not ')
        rhs = rhs.replace('&',' and ')
        rhs = rhs.replace('|',' or ')

        name_map = {
            'true':'True',
            'false':'False'
                }

        for frm,to in name_map.iteritems():
            rhs = util.MathUtil.str_expr_replacement(frm, to, rhs)

        lmda_str = "lambda %s: %s" % (','.join(self.rhs_names), rhs) 
        return eval(lmda_str, nineml.maths.str_to_npfunc_map, namespace)

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

    
