

# Automatically Generated Testing Skeleton Template:
import warnings
import unittest
import nineml






# Testing Skeleton for class: NamespaceAddress

class NamespaceAddress_test(object):
    
    def test_Constructor(self):
        pass


    def test_concat(self):
        # Signature: name(cls, *args)
		# Concatenates all the Namespace Addresses.
		# 
		# This method take all the arguments supplied, converts each one into a
		# namespace object, then, produces a new namespace object which is the
		# concatentation of all the arugements namespaces.
		# 
		# For example:
		# 
		# >>> NamespaceAddress.concat('first.second','third.forth','fifth.sixth')
		#     NameSpaceAddress: '/first/second/third/forth/fifth/sixth'
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_create_root(self):
        # Signature: name(cls)
		# Returns a empty (root) namespace address
		# 
		# 
		# >>> nineml.abstraction_layer.NamespaceAddress.create_root()
		# NameSpaceAddress: '//'    
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_get_local_name(self):
        # Signature: name(self)
		# Returns the local reference; i.e. the last field in the 
		# address, as a ``string``
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_get_parent_addr(self):
        # Signature: name(self)
		# Return the address of an namespace higher
		# 
		# >>> a = NamespaceAddress('level1.level2.level3')
		# >>> a
		# NameSpaceAddress: '/level1/level2/level3/'
		# >>> a.get_parent_addr()
		# NameSpaceAddress: '/level1/level2/'
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_get_str_prefix(self):
        # Signature: name(self, join_char='_')
		# Returns the same as ``getstr``, but prepends the ``join_char`` to
		# the end of the string, so that the string can be used to prefix
		# variables.
		# 
		# :param join_char: The character used to join the levels in the address.
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_get_subns_addr(self):
        # Signature: name(self, component_name)
		# Returns the address of a subcomponent at this address.
		# 
		# For example:
		# 
		# >>> a = NamespaceAddress('level1.level2.level3')
		# >>> a.get_subns_addr('subcomponent')
		# NameSpaceAddress: '/level1/level2/level3/subcomponent/'
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_getstr(self):
        # Signature: name(self, join_char='_')
		# Returns the namespace address as a string.
		# 
		# :param join_char: The character used to join the levels in the address.
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








