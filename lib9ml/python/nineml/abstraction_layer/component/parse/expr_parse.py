# Based on PLY calc.py example:
# -----------------------------------------------------------------------------
#
# A simple calculator with variables.   This is from O'Reilly's
# "Lex and Yacc", p. 63.
#
# Class-based example contributed to PLY by David McNab.
#
# Modified to use new-style classes.   Test case.
# -----------------------------------------------------------------------------





import ply.lex as lex
import ply.yacc as yacc
import os

#import nineml.maths.math_namespace
import nineml


def call_expr_func(expr_func, ns):
    args = []
    for var in expr_func.func_code.co_varnames:
        try:
            args.append(ns[var])
        except KeyError:
            raise KeyError, "call_expr_func: namespace missing variable '%s'" % var
    return expr_func(*args)


class Parser(object):
    """
    Base class for a lexer/parser that has the rules defined as methods
    """
    tokens = ()
    precedence = ()


    def __init__(self, **kw):
        self.debug = kw.get('debug', 0)
        self.names = { }
        try:
            modname = os.path.split(os.path.splitext(__file__)[0])[1] + "_" + self.__class__.__name__
        except:
            modname = "parser"+"_"+self.__class__.__name__

        from nineml.utility import LocationMgr
        self.debugfile = LocationMgr.getTmpDir() + modname + ".dbg"
        self.tabmodule = LocationMgr.getTmpDir() + modname + "_" + "parsetab"

        #print self.debugfile, self.tabmodule

        # Build the lexer and parser
        lex.lex(module=self, debug=self.debug)
        yacc.yacc(module=self,
                  debug=self.debug,
                  debugfile=self.debugfile,
                  tabmodule=self.tabmodule)

    def parse(self,expr):
        from nineml.abstraction_layer.component.parse import NineMLMathParseError
        self.names = []
        self.funcs = []
        try:
            yacc.parse(expr)
        except NineMLMathParseError, e:
            raise NineMLMathParseError, str(e)+" Expression was: '%s'" % expr

        # remove names from the math_namespace
        #from nineml.maths
        self.names = set(self.names)
        #self.names.difference_update(math_namespace.namespace)
        self.names.difference_update( nineml.maths.get_builtin_symbols() )

        return self.names, set(self.funcs)



    
class CalcExpr(Parser):

    tokens = (
        'NAME','NUMBER',
        'PLUS','MINUS','EXP', 'TIMES','DIVIDE',
        'LPAREN','RPAREN','LFUNC', 'COMMA'
        )

    # Tokens

    t_PLUS    = r'\+'
    t_MINUS   = r'-'
    t_EXP     = r'\*\*'
    t_TIMES   = r'\*'
    t_DIVIDE  = r'/'
    t_LPAREN  = r'\('
    t_RPAREN  = r'\)'
    t_NAME    = r'[a-zA-Z_][a-zA-Z0-9_]*'
    t_LFUNC    = r'[a-zA-Z_][a-zA-Z0-9_]*[ ]*\('
    t_COMMA   = r','


    def t_NUMBER(self, t):
        r'(\d*\.\d+)|(\d+\.\d*)|(\d+)'
        try:
            t.value = float(t.value)
        except ValueError:
            from nineml.abstraction_layer.component.parse import NineMLMathParseError
            raise NineMLMathParseError, "Invalid number %s" % t.value
        return t

    t_ignore = " \t"

    
    def t_error(self, t):
        from nineml.abstraction_layer.component.parse import NineMLMathParseError
        raise NineMLMathParseError, "Illegal character '%s' in '%s'" % (t.value[0],t)

    precedence = (
        ('left','PLUS','MINUS'),
        ('left','TIMES','DIVIDE'),
        ('left', 'EXP'),
        ('right','UMINUS'),
        ('left','LFUNC'),
        )

    def p_statement_expr(self, p):
        'statement : expression'
        pass

    def p_expression_binop(self, p):
        """
        expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression EXP expression
        """
        pass

    def p_expression_uminus(self, p):
        'expression : MINUS expression %prec UMINUS'
        pass

    def p_func(self,p):
        """expression : LFUNC expression RPAREN\n | LFUNC RPAREN
                        | LFUNC expression COMMA expression RPAREN
                        | LFUNC expression COMMA expression COMMA expression RPAREN
        """
        # EM: Supports up to 3 args.  Don't know how to support N.
        
        # check that function name is known
        func_name = p[1][:-1].strip()
        #if func_name not in math_namespace.namespace:
        #    raise NineMLMathParseError, "Undefined function '%s'" % func_name
        self.funcs.append(func_name)

    def p_expression_group(self, p):
        'expression : LPAREN expression RPAREN'
        pass

    def p_expression_number(self, p):
        'expression : NUMBER'
        pass

    def p_expression_name(self, p):
        'expression : NAME'
        self.names.append(p[1])

    def p_error(self, p):
        from nineml.abstraction_layer.component.parse import NineMLMathParseError
        if p:
            raise NineMLMathParseError, "Syntax error at '%s'" % p.value
        else:
            raise NineMLMathParseError, "Syntax error at EOF, probably unmatched parenthesis."


def expr_parse(rhs):
    """ Parses an expression rhs, i.e. no "=, +=, -=, etc." in the expr
    and returns var names and func names as sets """

    calc = CalcExpr()
    return calc.parse(rhs)
    
if __name__ == '__main__':
    calc = CalcExpr()
    p = calc.parse("1 / (( 1 + mg_conc * eta *  exp ( -1 * gamma*V))")
    print p
