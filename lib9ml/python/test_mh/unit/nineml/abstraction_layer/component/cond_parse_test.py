

# Automatically Generated Testing Skeleton Template:
import warnings
import unittest
import nineml




# Testing Skeleton for function:


def test_cond_parse():
    # Signature: name(conditional)
	# Parses a conditinal expression 
	# and returns var names and func names as sets 
    #from nineml.abstraction_layer.component.cond_parse import cond_parse
    warnings.warn('Tests not implemented')
    # raise NotImplementedError()







# Testing Skeleton for class: CalcCond

class CalcCond_test(object):
    
    def test_Constructor(self):
        pass


    def test_p_boolean_bool(self):
        # Signature: name(self, p)
		# boolean : BOOL
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_p_boolean_conditional(self):
        # Signature: name(self, p)
		# boolean : expression CONDITIONAL expression
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_p_boolean_group(self):
        # Signature: name(self, p)
		# boolean : LPAREN boolean RPAREN
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_p_boolean_logical(self):
        # Signature: name(self, p)
		# boolean : boolean LOGICAL boolean
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_p_boolean_not(self):
        # Signature: name(self, p)
		# boolean : NOT boolean %prec UNOT
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_p_conditional(self):
        # Signature: name(self, p)
		# conditional : boolean
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_p_error(self):
        # Signature: name(self, p)
		# No Docstring
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_p_expression_binop(self):
        # Signature: name(self, p)
		# expression : expression PLUS expression
		#           | expression MINUS expression
		#           | expression TIMES expression
		#           | expression DIVIDE expression
		#           | expression EXP expression
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_p_expression_group(self):
        # Signature: name(self, p)
		# expression : LPAREN expression RPAREN
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_p_expression_name(self):
        # Signature: name(self, p)
		# expression : NAME
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_p_expression_number(self):
        # Signature: name(self, p)
		# expression : NUMBER
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_p_expression_uminus(self):
        # Signature: name(self, p)
		# expression : MINUS expression %prec UMINUS
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_p_func(self):
        # Signature: name(self, p)
		# expression : LFUNC expression RPAREN
		# | LFUNC RPAREN
		#                        | LFUNC expression COMMA expression RPAREN
		#                        | LFUNC expression COMMA expression COMMA expression RPAREN
		#        
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_parse(self):
        # Signature: name(self, expr)
		# No Docstring
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_precedence(self):
        # Signature: name
		# tuple() -> empty tuple
		# tuple(iterable) -> tuple initialized from iterable's items
		# 
		# If the argument is a tuple, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_start(self):
        # Signature: name
		# str(object) -> string
		# 
		# Return a nice string representation of the object.
		# If the argument is a string, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_t_COMMA(self):
        # Signature: name
		# str(object) -> string
		# 
		# Return a nice string representation of the object.
		# If the argument is a string, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_t_CONDITIONAL(self):
        # Signature: name
		# str(object) -> string
		# 
		# Return a nice string representation of the object.
		# If the argument is a string, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_t_DIVIDE(self):
        # Signature: name
		# str(object) -> string
		# 
		# Return a nice string representation of the object.
		# If the argument is a string, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_t_EXP(self):
        # Signature: name
		# str(object) -> string
		# 
		# Return a nice string representation of the object.
		# If the argument is a string, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_t_LFUNC(self):
        # Signature: name(self, t)
		# [a-zA-Z_][a-zA-Z0-9_]*[ ]*\(
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_t_LOGICAL(self):
        # Signature: name
		# str(object) -> string
		# 
		# Return a nice string representation of the object.
		# If the argument is a string, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_t_LPAREN(self):
        # Signature: name
		# str(object) -> string
		# 
		# Return a nice string representation of the object.
		# If the argument is a string, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_t_MINUS(self):
        # Signature: name
		# str(object) -> string
		# 
		# Return a nice string representation of the object.
		# If the argument is a string, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_t_NAME(self):
        # Signature: name(self, t)
		# [a-zA-Z_][a-zA-Z0-9_]*
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_t_NOT(self):
        # Signature: name
		# str(object) -> string
		# 
		# Return a nice string representation of the object.
		# If the argument is a string, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_t_NUMBER(self):
        # Signature: name(self, t)
		# (\d*\.\d+)|(\d+\.\d*)|(\d+)
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_t_PLUS(self):
        # Signature: name
		# str(object) -> string
		# 
		# Return a nice string representation of the object.
		# If the argument is a string, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_t_RPAREN(self):
        # Signature: name
		# str(object) -> string
		# 
		# Return a nice string representation of the object.
		# If the argument is a string, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_t_TIMES(self):
        # Signature: name
		# str(object) -> string
		# 
		# Return a nice string representation of the object.
		# If the argument is a string, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_t_error(self):
        # Signature: name(self, t)
		# No Docstring
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_t_ignore(self):
        # Signature: name
		# str(object) -> string
		# 
		# Return a nice string representation of the object.
		# If the argument is a string, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_tokens(self):
        # Signature: name
		# tuple() -> empty tuple
		# tuple(iterable) -> tuple initialized from iterable's items
		# 
		# If the argument is a tuple, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import CalcCond
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: Parser

class Parser_test(object):
    
    def test_Constructor(self):
        pass


    def test_parse(self):
        # Signature: name(self, expr)
		# No Docstring
        #from nineml.abstraction_layer.component.cond_parse import Parser
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_precedence(self):
        # Signature: name
		# tuple() -> empty tuple
		# tuple(iterable) -> tuple initialized from iterable's items
		# 
		# If the argument is a tuple, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import Parser
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_tokens(self):
        # Signature: name
		# tuple() -> empty tuple
		# tuple(iterable) -> tuple initialized from iterable's items
		# 
		# If the argument is a tuple, the return value is the same object.
        #from nineml.abstraction_layer.component.cond_parse import Parser
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








