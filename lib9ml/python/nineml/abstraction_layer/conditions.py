from nineml.abstraction_layer import expressions

class Condition(expressions.Expression):

    def __init__(self, cond):

        self.cond = cond
        self.parse()

    def parse(self):
        """ parses and checks validity of condtional """
        # overrides Expression.parse

        from nineml.abstraction_layer.cond_parse import cond_parse
        self.names, self.funcs = cond_parse(self.cond)

    def is_bool(self):
        """ Checks if conditions is pure bool: True, False"""
        
        if self.names==set() and self.funcs==set():
            val = self.python_func()()
            if val==False:
                return True
            else:
                assert val==True
                return True

    def python_func(self, namespace={}):
        """ Returns a python callable which evaluates the expression in namespace and returns the result """

        # overriding Expression.python_func
        from nineml.abstraction_layer import math_namespace

        
        expr = self.cond
        # fix syntax to match python
        expr = expr.replace('&', ' and ')
        expr = expr.replace('|', ' or ')
        expr = expr.replace('!',' not ')
        # use name_replace, as we don't want to replace 'mytrue' to 'myTrue'
        expr = self.name_replace('true','True',expr)
        expr = self.name_replace('false','False',expr)

        return eval("lambda %s: %s" % (','.join(self.names),expr), math_namespace.namespace, namespace)

    def as_expr(self):
        """ This is to behave as an expr"""
        return self.cond

    @property
    def rhs(self):
        """ This is to behave as an Expression"""
        return self.cond
        

    def __repr__(self):
        return "Condition('%s')" % (self.cond)


    def encode(self, encoding):
        return repr(self).encode(encoding)

    def __eq__(self, other):
        from operator import and_

        if not isinstance(other, self.__class__):
            return False

        return self.cond == other.cond


##     def to_xml(self):
##         return E(self.element_name,
##                  E("conditional-inline", self.cond),
##                  name=self.name)
                 
##     @classmethod
##     def from_xml(cls, element):
##         assert element.tag == NINEML+cls.element_name
##         math = element.find(NINEML+"conditional-inline").text
##         return cls(math, name=element.get("name"))



def cond_to_obj(cond_str):

    if isinstance(cond_str,Condition):
        return cond_str

    elif cond_str == None:
        return None

    elif isinstance(cond_str,str):
        return Condition(cond_str.strip())

    raise ValueError, "Condition: expected None, str, or Condition object"

    
