

# Automatically Generated Testing Skeleton Template:
import warnings
import unittest
import nineml




# Testing Skeleton for function:


class TestOn(unittest.TestCase):

    def test_On(self):
        # Signature: name(trigger, do=None, to=None)
		# No Docstring
        #from nineml.abstraction_layer.component.dynamics import On
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()





# Testing Skeleton for function:


class TestDoOnCondition(unittest.TestCase):

    def test_DoOnCondition(self):
        # Signature: name(condition, do=None, to=None)
		# No Docstring
        #from nineml.abstraction_layer.component.dynamics import DoOnCondition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()





# Testing Skeleton for function:


class TestDoOnEvent(unittest.TestCase):

    def test_DoOnEvent(self):
        # Signature: name(input_event, do=None, to=None)
		# No Docstring
        #from nineml.abstraction_layer.component.dynamics import DoOnEvent
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()





# Testing Skeleton for function:


class Testdo_to_assignments_and_events(unittest.TestCase):

    def test_do_to_assignments_and_events(self):
        # Signature: name(doList)
		# No Docstring
        #from nineml.abstraction_layer.component.dynamics import do_to_assignments_and_events
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()







# Testing Skeleton for class: Dynamics

class Dynamics_test(unittest.TestCase):
    
    def test_Constructor(self):
        pass


    def test_accept_visitor(self):
        # Signature: name(self, visitor, **kwargs)
		# |VISITATION| 
        #from nineml.abstraction_layer.component.dynamics import Dynamics
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_aliases(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.dynamics import Dynamics
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_aliases_map(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.dynamics import Dynamics
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_regime_map(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.dynamics import Dynamics
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_regimes(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.dynamics import Dynamics
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_state_variables(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.dynamics import Dynamics
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_transitions(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.dynamics import Dynamics
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: OnCondition

class OnCondition_test(unittest.TestCase):
    
    def test_Constructor(self):
        pass


    def test_accept_visitor(self):
        # Signature: name(self, visitor, **kwargs)
		# |VISITATION| 
        #from nineml.abstraction_layer.component.dynamics import OnCondition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_event_outputs(self):
        # Signature: name
		# Events that happen when this transitions occurs
        #from nineml.abstraction_layer.component.dynamics import OnCondition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_set_source_regime(self):
        # Signature: name(self, source_regime)
		# Internal method, used during component construction.
		# 
		# Used internally by the ComponentClass objects after all objects
		# have be constructed, in the ``_ResolveTransitionRegimeNames()`` method.
		# This is because when we build Transitions, the Regimes that they refer
		# to generally are not build yet, so are refered to by strings. This
		# method is used to set the source ``Regime`` object. We check that the name
		# of the object set is the same as that previously expected.
        #from nineml.abstraction_layer.component.dynamics import OnCondition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_set_target_regime(self):
        # Signature: name(self, target_regime)
		# Internal method, used during component construction.
		# 
		# See ``set_source_regime``
        #from nineml.abstraction_layer.component.dynamics import OnCondition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_source_regime(self):
        # Signature: name
		# Returns the source regime of this transition.
		# 
		# .. note::
		# 
		#     This method will only be available after the ComponentClass
		#     containing this transition has been built. See ``set_source_regime``
        #from nineml.abstraction_layer.component.dynamics import OnCondition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_source_regime_name(self):
        # Signature: name
		# DO NOT USE: Internal function. Use `source_regime.name` instead.
		#         
        #from nineml.abstraction_layer.component.dynamics import OnCondition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_state_assignments(self):
        # Signature: name
		# An ordered list of StateAssignments that happen when this
		# transitions occurs
        #from nineml.abstraction_layer.component.dynamics import OnCondition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_target_regime(self):
        # Signature: name
		# Returns the target regime of this transition.
		# 
		# .. note::
		# 
		#     This method will only be available after the ComponentClass
		#     containing this transition has been built. See ``set_source_regime``
        #from nineml.abstraction_layer.component.dynamics import OnCondition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_target_regime_name(self):
        # Signature: name
		# DO NOT USE: Internal function. Use `target_regime.name` instead.
		#         
        #from nineml.abstraction_layer.component.dynamics import OnCondition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_trigger(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.dynamics import OnCondition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: OnEvent

class OnEvent_test(unittest.TestCase):
    
    def test_Constructor(self):
        pass


    def test_accept_visitor(self):
        # Signature: name(self, visitor, **kwargs)
		# |VISITATION| 
        #from nineml.abstraction_layer.component.dynamics import OnEvent
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_event_outputs(self):
        # Signature: name
		# Events that happen when this transitions occurs
        #from nineml.abstraction_layer.component.dynamics import OnEvent
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_set_source_regime(self):
        # Signature: name(self, source_regime)
		# Internal method, used during component construction.
		# 
		# Used internally by the ComponentClass objects after all objects
		# have be constructed, in the ``_ResolveTransitionRegimeNames()`` method.
		# This is because when we build Transitions, the Regimes that they refer
		# to generally are not build yet, so are refered to by strings. This
		# method is used to set the source ``Regime`` object. We check that the name
		# of the object set is the same as that previously expected.
        #from nineml.abstraction_layer.component.dynamics import OnEvent
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_set_target_regime(self):
        # Signature: name(self, target_regime)
		# Internal method, used during component construction.
		# 
		# See ``set_source_regime``
        #from nineml.abstraction_layer.component.dynamics import OnEvent
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_source_regime(self):
        # Signature: name
		# Returns the source regime of this transition.
		# 
		# .. note::
		# 
		#     This method will only be available after the ComponentClass
		#     containing this transition has been built. See ``set_source_regime``
        #from nineml.abstraction_layer.component.dynamics import OnEvent
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_source_regime_name(self):
        # Signature: name
		# DO NOT USE: Internal function. Use `source_regime.name` instead.
		#         
        #from nineml.abstraction_layer.component.dynamics import OnEvent
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_src_port_name(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.dynamics import OnEvent
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_state_assignments(self):
        # Signature: name
		# An ordered list of StateAssignments that happen when this
		# transitions occurs
        #from nineml.abstraction_layer.component.dynamics import OnEvent
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_target_regime(self):
        # Signature: name
		# Returns the target regime of this transition.
		# 
		# .. note::
		# 
		#     This method will only be available after the ComponentClass
		#     containing this transition has been built. See ``set_source_regime``
        #from nineml.abstraction_layer.component.dynamics import OnEvent
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_target_regime_name(self):
        # Signature: name
		# DO NOT USE: Internal function. Use `target_regime.name` instead.
		#         
        #from nineml.abstraction_layer.component.dynamics import OnEvent
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: Regime

class Regime_test(unittest.TestCase):
    
    def test_Constructor(self):
        pass


    def test_accept_visitor(self):
        # Signature: name(self, visitor, **kwargs)
		# |VISITATION| 
        #from nineml.abstraction_layer.component.dynamics import Regime
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_add_on_condition(self):
        # Signature: name(self, on_condition)
		# Add an OnCondition transition which leaves this regime
		# 
		# If the on_condition object has not had its target regime name
		# set in the constructor, or by calling its ``set_target_regime_name()``, 
		# then the target is assumed to be this regime, and will be set
		# appropriately.
		# 
		# The source regime for this transition will be set as this regime.
        #from nineml.abstraction_layer.component.dynamics import Regime
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_add_on_event(self):
        # Signature: name(self, on_event)
		# Add an OnEvent transition which leaves this regime
		# 
		# If the on_event object has not had its target regime name
		# set in the constructor, or by calling its ``set_target_regime_name()``, 
		# then the target is assumed to be this regime, and will be set
		# appropriately.
		# 
		# The source regime for this transition will be set as this regime.
        #from nineml.abstraction_layer.component.dynamics import Regime
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_get_next_name(self):
        # Signature: name(cls)
		# Return the next distinct autogenerated name
		#         
        #from nineml.abstraction_layer.component.dynamics import Regime
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_name(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.dynamics import Regime
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_on_conditions(self):
        # Signature: name
		# Returns all the transitions out of this regime trigger by
		# conditions
        #from nineml.abstraction_layer.component.dynamics import Regime
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_on_events(self):
        # Signature: name
		# Returns all the transitions out of this regime trigger by events
        #from nineml.abstraction_layer.component.dynamics import Regime
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_time_derivatives(self):
        # Signature: name
		# Returns the state-variable time-derivatives in this regime.
		# 
		# .. note::
		# 
		#     This is not guarenteed to contain the time derivatives for all the
		#     state-variables specified in the component. If they are not defined,
		#     they are assumed to be zero in this regime.
        #from nineml.abstraction_layer.component.dynamics import Regime
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_transitions(self):
        # Signature: name
		# Returns all the transitions leaving this regime.
		# 
		# Returns an iterator over both the on_events and on_conditions of this
		# regime
        #from nineml.abstraction_layer.component.dynamics import Regime
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: StateVariable

class StateVariable_test(unittest.TestCase):
    
    def test_Constructor(self):
        pass


    def test_accept_visitor(self):
        # Signature: name(self, visitor, **kwargs)
		# |VISITATION| 
        #from nineml.abstraction_layer.component.dynamics import StateVariable
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_name(self):
        # Signature: name
		# No Docstring
        #from nineml.abstraction_layer.component.dynamics import StateVariable
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: Transition

class Transition_test(unittest.TestCase):
    
    def test_Constructor(self):
        pass


    def test_event_outputs(self):
        # Signature: name
		# Events that happen when this transitions occurs
        #from nineml.abstraction_layer.component.dynamics import Transition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_set_source_regime(self):
        # Signature: name(self, source_regime)
		# Internal method, used during component construction.
		# 
		# Used internally by the ComponentClass objects after all objects
		# have be constructed, in the ``_ResolveTransitionRegimeNames()`` method.
		# This is because when we build Transitions, the Regimes that they refer
		# to generally are not build yet, so are refered to by strings. This
		# method is used to set the source ``Regime`` object. We check that the name
		# of the object set is the same as that previously expected.
        #from nineml.abstraction_layer.component.dynamics import Transition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_set_target_regime(self):
        # Signature: name(self, target_regime)
		# Internal method, used during component construction.
		# 
		# See ``set_source_regime``
        #from nineml.abstraction_layer.component.dynamics import Transition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_source_regime(self):
        # Signature: name
		# Returns the source regime of this transition.
		# 
		# .. note::
		# 
		#     This method will only be available after the ComponentClass
		#     containing this transition has been built. See ``set_source_regime``
        #from nineml.abstraction_layer.component.dynamics import Transition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_source_regime_name(self):
        # Signature: name
		# DO NOT USE: Internal function. Use `source_regime.name` instead.
		#         
        #from nineml.abstraction_layer.component.dynamics import Transition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_state_assignments(self):
        # Signature: name
		# An ordered list of StateAssignments that happen when this
		# transitions occurs
        #from nineml.abstraction_layer.component.dynamics import Transition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_target_regime(self):
        # Signature: name
		# Returns the target regime of this transition.
		# 
		# .. note::
		# 
		#     This method will only be available after the ComponentClass
		#     containing this transition has been built. See ``set_source_regime``
        #from nineml.abstraction_layer.component.dynamics import Transition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_target_regime_name(self):
        # Signature: name
		# DO NOT USE: Internal function. Use `target_regime.name` instead.
		#         
        #from nineml.abstraction_layer.component.dynamics import Transition
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








