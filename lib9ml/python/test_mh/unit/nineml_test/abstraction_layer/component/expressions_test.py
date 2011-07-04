

# Automatically Generated Testing Skeleton Template:
import warnings
import unittest
import nineml








# Testing Skeleton for class: Alias

class Alias_test(unittest.TestCase):
    


    def test_accept_visitor(self):
        # Signature: name(self, visitor, **kwargs)
		# |VISITATION| 
        #from nineml.abstraction_layer.component.expressions import Alias
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()







# Testing Skeleton for class: Expression

class Expression_test(unittest.TestCase):


    def test_Valid(self):
        from nineml.abstraction_layer import Expression
        # rhs, expt_vars, expt_funcs, result, values
        valid_rhses = [
                ( ('a'),                ('a'),                (), 5,            {'a':5}, ), 
                ( ('b'),                ('b'),                (), 7,            {'b':7}, ),
                ( ('a+b'),              ('a','b'),            (), 13,           {'a':12,'b':1} ),
                ( ('1./(alpha+2*beta)'),  ('alpha','beta'),   (), 0.2,          {'alpha':1,'beta':2} ),
                ( ('pi'),                (),                  (), 3.14159265,   {} ),
                ]

        
        for rhs, exp_var, exp_func, exp_res, params in valid_rhses:
            e = Expression(rhs)
            self.assertEquals( set( e.rhs_names), set( exp_var) )
            self.assertEquals( set( e.rhs_funcs), set( exp_func) )
            self.assertAlmostEqual( e.rhs_as_python_func()(**params), exp_res, places=4 )
            


    
    def test_Constructor(self):
        pass


    def test_rhs(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.expressions import Expression
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_rhs_as_python_func(self):
        # Signature: name(self, namespace=None)
		# No Docstring
        #from nineml.abstraction_layer.component.expressions import Expression
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_rhs_atoms(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.expressions import Expression
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_rhs_funcs(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.expressions import Expression
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()

    def test_rhs_missing_functions(self):
        # Signature: name
		# yield names of functions in the rhs which are not in the math
		# namespace
        #from nineml.abstraction_layer.component.expressions import Expression
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_rhs_name_transform_inplace(self):
        # Signature: name(self, name_map)
		# Replace atoms on the RHS with values in the name_map
        #from nineml.abstraction_layer.component.expressions import Expression
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_rhs_names(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.expressions import Expression
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_substitute_alias(self):
        # Signature: name(self, alias)
		# Substitute an alias into the rhs
        #from nineml.abstraction_layer.component.expressions import Expression
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: ExpressionWithLHS

class ExpressionWithLHS_test(unittest.TestCase):
    

    def test_atoms(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.expressions import ExpressionWithLHS
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()

    def test_lhs_name_transform_inplace(self):
        # Signature: name(self, name_map)
		# No Docstring
        #from nineml.abstraction_layer.component.expressions import ExpressionWithLHS
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()

    def test_name_transform_inplace(self):
        # Signature: name(self, name_map)
		# No Docstring
        #from nineml.abstraction_layer.component.expressions import ExpressionWithLHS
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_lhs_atoms(self):
        warnings.warn('Tests not implemented')







# Testing Skeleton for class: ExpressionWithSimpleLHS

class ExpressionWithSimpleLHS_test(unittest.TestCase):
    
    def test_lhs(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.expressions import ExpressionWithSimpleLHS
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_lhs_atoms(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.expressions import ExpressionWithSimpleLHS
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_lhs_name_transform_inplace(self):
        # Signature: name(self, name_map)
		# No Docstring
        #from nineml.abstraction_layer.component.expressions import ExpressionWithSimpleLHS
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_name_transform_inplace(self):
        # Signature: name(self, name_map)
		# No Docstring
        #from nineml.abstraction_layer.component.expressions import ExpressionWithSimpleLHS
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: StateAssignment

class StateAssignment_test(unittest.TestCase):
    


    def test_accept_visitor(self):
        # Signature: name(self, visitor, **kwargs)
		# |VISITATION| 
        #from nineml.abstraction_layer.component.expressions import StateAssignment
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_lhs(self):
        warnings.warn('Tests not implemented')
        








# Testing Skeleton for class: TimeDerivative

class TimeDerivative_test(unittest.TestCase):
    

    def test_accept_visitor(self):
        # Signature: name(self, visitor, **kwargs)
		# |VISITATION| 
        #from nineml.abstraction_layer.component.expressions import TimeDerivative
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_atoms(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.expressions import TimeDerivative
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_dependent_variable(self):
        # Signature: name
		# Return the dependent variable
        #from nineml.abstraction_layer.component.expressions import TimeDerivative
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_independent_variable(self):
        # Signature: name
		# Return the independent variable
        #from nineml.abstraction_layer.component.expressions import TimeDerivative
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()




    def test_lhs_atoms(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.expressions import TimeDerivative
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_lhs_name_transform_inplace(self):
        # Signature: name(self, name_map)
		# Replace atoms on the LHS with mapping in name_map 
        #from nineml.abstraction_layer.component.expressions import TimeDerivative
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_name_transform_inplace(self):
        # Signature: name(self, name_map)
		# No Docstring
        #from nineml.abstraction_layer.component.expressions import TimeDerivative
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()









