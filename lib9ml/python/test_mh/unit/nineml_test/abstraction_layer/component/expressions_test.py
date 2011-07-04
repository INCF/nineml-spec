

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
        
        
        import numpy
        expr_vars = [["-A/tau_r", ("A","tau_r"),()],
             ["V*V", ("V",),()],
             ["a*(b*V - U)", ("U","V","b","a"),()],
             [" 0.04*V*V + 5.0*V + 1. + 140.0 - U + Isyn", ("V","U","Isyn"),()],
             ["c",("c"),()],
             ["1",(),()],
             ["atan2(sin(x),cos(y))",("x","y"),("atan2","sin","cos")],
             ["1.*V",("V"),()],
             ["1.0",(),()],
             [".1",(),()],
             ["1/(1 + mg_conc*eta*exp(-1*gamma*V))", ("mg_conc","eta","gamma","V"),('exp',)],
             ["1 / ( 1 + mg_conc * eta *  exp( -1 * gamma*V))", ("mg_conc","eta","gamma","V"),('exp',)],
             ["1 / ( 1 + mg_conc * sin(0.5) *  exp ( -1 * gamma*V))", ("mg_conc","gamma","V"),('exp',"sin")],
             [".1 / ( 1.0 + mg_conc * sin(V) *  exp ( -1.0 * gamma*V))", ("mg_conc","gamma","V"),('exp',"sin")],
             ["sin(w)",("w"),("sin",)]]

        namespace = {
            "A": 10.0,
            "tau_r": 11.0,
            "V":-70.0,
            "a": 1.2,
            "b": 3.0,
            "U": -80.0,
            "Isyn": 2.0,
            "c": 10.0,
            "mg_conc":1.0,
            "eta":2.0,
            "gamma":-20.0,
            "x":1.0,
            "y":1.0,
            "w":numpy.arange(10)
            }

        return_values = [-0.909090909091, 4900.0,-156.0,69.0,10.0,1,1.0,-70.0,1.0,0.1,1.0,1.0,1.0,0.1, numpy.sin(namespace['w'])]
                
        for i,(expr, expt_vars, expt_funcs) in enumerate(expr_vars):
            c = Expression(expr)
            self.assertEqual( set(c.rhs_names), set(expt_vars) )
            self.assertEqual( set(c.rhs_funcs), set(expt_funcs) )
                
            python_func = c.rhs_as_python_func(namespace=namespace)
            param_dict = dict([ (v, namespace[v]) for v in expt_vars]) 

            v = return_values[i]- python_func(**param_dict) 
            self.assertAlmostEqual( numpy.dot(v,v), 0 )

        

    def test_rhs_name_transform_inplace(self):
        # Signature: name(self, name_map)
		# Replace atoms on the RHS with values in the name_map
        from nineml.abstraction_layer import Expression

        e = Expression( "V/(1 + mg_conc*eta*exp(-1*gamma*V*V)) * sin(V)" )
        e.rhs_name_transform_inplace({'V':'VNEW'})
        self.assertEquals( e.rhs, "VNEW/(1 + mg_conc*eta*exp(-1*gamma*VNEW*VNEW)) * sin(VNEW)" )










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









