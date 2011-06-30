

# Automatically Generated Testing Skeleton Template:
import warnings
import unittest
import nineml




# Testing Skeleton for function:


def test_On():
    # Signature: name(trigger, do=None, to=None)
	# No Docstring

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test_DoOnCondition():
    # Signature: name(condition, do=None, to=None)
	# No Docstring

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test_DoOnEvent():
    # Signature: name(input_event, do=None, to=None)
	# No Docstring

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test_do_to_assignments_and_events():
    # Signature: name(doList)
	# No Docstring

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()







# Testing Skeleton for class: Dynamics

class Dynamics_test(object):
    
    def test_Constructor(self):
        pass


    def test_accept_visitor(self):
        # Signature: name(self, visitor, **kwargs)
		# |VISITATION| 
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_aliases(self):
        # Signature: name
		# No Docstring
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_aliases_map(self):
        # Signature: name
		# No Docstring
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_regime_map(self):
        # Signature: name
		# No Docstring
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_regimes(self):
        # Signature: name
		# No Docstring
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_state_variables(self):
        # Signature: name
		# No Docstring
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_transitions(self):
        # Signature: name
		# No Docstring
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: OnCondition

class OnCondition_test(object):
    
    def test_Constructor(self):
        pass


    def test_accept_visitor(self):
        # Signature: name(self, visitor, **kwargs)
		# |VISITATION| 
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_event_outputs(self):
        # Signature: name
		# Events that happen when this transitions occurs
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
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_set_target_regime(self):
        # Signature: name(self, target_regime)
		# Internal method, used during component construction.
		# 
		# See ``set_source_regime``
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
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_source_regime_name(self):
        # Signature: name
		# DO NOT USE: Internal function. Use `source_regime.name` instead.
		#         
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_state_assignments(self):
        # Signature: name
		# An ordered list of StateAssignments that happen when this
		# transitions occurs
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
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_target_regime_name(self):
        # Signature: name
		# DO NOT USE: Internal function. Use `target_regime.name` instead.
		#         
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_trigger(self):
        # Signature: name
		# No Docstring
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: OnEvent

class OnEvent_test(object):
    
    def test_Constructor(self):
        pass


    def test_accept_visitor(self):
        # Signature: name(self, visitor, **kwargs)
		# |VISITATION| 
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_event_outputs(self):
        # Signature: name
		# Events that happen when this transitions occurs
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
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_set_target_regime(self):
        # Signature: name(self, target_regime)
		# Internal method, used during component construction.
		# 
		# See ``set_source_regime``
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
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_source_regime_name(self):
        # Signature: name
		# DO NOT USE: Internal function. Use `source_regime.name` instead.
		#         
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_src_port_name(self):
        # Signature: name
		# No Docstring
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_state_assignments(self):
        # Signature: name
		# An ordered list of StateAssignments that happen when this
		# transitions occurs
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
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_target_regime_name(self):
        # Signature: name
		# DO NOT USE: Internal function. Use `target_regime.name` instead.
		#         
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: Regime

class Regime_test(object):
    
    def test_Constructor(self):
        pass


    def test_accept_visitor(self):
        # Signature: name(self, visitor, **kwargs)
		# |VISITATION| 
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
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_get_next_name(self):
        # Signature: name(cls)
		# Return the next distinct autogenerated name
		#         
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_name(self):
        # Signature: name
		# No Docstring
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_on_conditions(self):
        # Signature: name
		# Returns all the transitions out of this regime trigger by
		# conditions
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_on_events(self):
        # Signature: name
		# Returns all the transitions out of this regime trigger by events
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
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_transitions(self):
        # Signature: name
		# Returns all the transitions leaving this regime.
		# 
		# Returns an iterator over both the on_events and on_conditions of this
		# regime
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: StateVariable

class StateVariable_test(object):
    
    def test_Constructor(self):
        pass


    def test_accept_visitor(self):
        # Signature: name(self, visitor, **kwargs)
		# |VISITATION| 
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_name(self):
        # Signature: name
		# No Docstring
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: Transition

class Transition_test(object):
    
    def test_Constructor(self):
        pass


    def test_event_outputs(self):
        # Signature: name
		# Events that happen when this transitions occurs
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
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_set_target_regime(self):
        # Signature: name(self, target_regime)
		# Internal method, used during component construction.
		# 
		# See ``set_source_regime``
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
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_source_regime_name(self):
        # Signature: name
		# DO NOT USE: Internal function. Use `source_regime.name` instead.
		#         
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_state_assignments(self):
        # Signature: name
		# An ordered list of StateAssignments that happen when this
		# transitions occurs
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
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_target_regime_name(self):
        # Signature: name
		# DO NOT USE: Internal function. Use `target_regime.name` instead.
		#         
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








