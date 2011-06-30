

# Automatically Generated Testing Skeleton Template:
import warnings
import unittest
import nineml




# Testing Skeleton for function:


def test_check_list_contain_same_items():
    # Signature: name(lst1, lst2, desc1='', desc2='', ignore=[], desc='')
	# No Docstring

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test_filter_expect_single():
    # Signature: name(lst, func=None, error_func=None)
	# Find a single element matching a predicate in a list.
	# 
	# This is a syntactic-sugar function ``_filter`` and ``expect_single``
	# in a single call.
	# 
	#  Returns::
	# 
	#      expect_single( _filter(lst, func), error_func )
	# 
	# 
	#  This is useful when we want to find an item in a sequence with a certain 
	#  property, and expect there to be only one.
	#  
	#  Examples:
	# 
	#  >>> find_smith = lambda s: s.split()[-1] == 'Smith'
	#  >>> filter_expect_single( ['John Smith','Tim Jones'], func=find_smith )  #doctest: +NORMALIZE_WHITESPACE 
	#  'John Smith'

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test_expect_single():
    # Signature: name(lst, error_func=None)
	# Retrieve a single element from an iterable.
	# 
	# This function tests whether an iterable contains just a single element and
	# if so returns that element. Otherwise it raises an Exception.
	#     
	# :param lst: An iterable
	# 
	# :param error_func: An exception object or a callable. ``error_func`` will be
	#     raised or called in case there is not exactly one element in ``lst``. If
	#     ``error_func`` is ``None``, a ``NineMLRuntimeError`` exception will be
	#     raised.
	# 
	# 
	# :rtype: the element in the list, ``lst[0]``, provided ``len(lst)==1``
	# 
	# 
	# 
	# >>> expect_single( ['hello'] )
	# 'hello'
	# 
	# >>> expect_single( [1] )
	# 1
	# 
	# >>> expect_single( [] ) #doctest: +SKIP
	# NineMLRuntimeError: expect_single() recieved an iterable of length: 0
	# 
	# >>> expect_single( [None,None] ) #doctest: +SKIP
	# NineMLRuntimeError: expect_single() recieved an iterable of length: 2
	# 
	# >>> expect_single( [], lambda: raise_exception( RuntimeError('Aggh') ) #doctest: +SKIP
	# RuntimeError: Aggh
	# 
	# >>> #Slightly more tersly:
	# >>> expect_single( [], RuntimeError('Aggh') ) #doctest: +SKIP
	# RuntimeError: Aggh

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test_flatten_first_level():
    # Signature: name(nested_list)
	# Flattens the first level of an iterable, ie
	# 
	# >>> flatten_first_level( [ ['This','is'],['a','short'],['phrase'] ] ) #doctest: +NORMALIZE_WHITESPACE 
	# ['This', 'is', 'a', 'short', 'phrase'] 

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test_invert_dictionary():
    # Signature: name(dct)
	# Takes a dictionary mapping (keys => values) and returns a 
	# new dictionry mapping (values => keys).
	# i.e. given a dictionary::
	# 
	#     {k1:v1, k2:v2, k3:v3, ...} 
	# 
	# it returns a dictionary::
	# 
	#     {v1:k1, v2:k2, v3:k3, ...} 
	# 
	# It checks to make sure that no values are duplicated before converting.

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test_assert_no_duplicates():
    # Signature: name(lst, error_func=None)
	# Check for duplicates in a sequence.
	# 
	# This function checks that a list contains no duplicates, by casting the list
	# to a set and comparing the lengths.
	# 
	# It raises an `NineMLRuntimeError` if the lengths are not equal.

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test_file_sha1_hexdigest():
    # Signature: name(filename)
	# Returns the SHA1 hex-digest of a file

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test_filter_by_type():
    # Signature: name(lst, acceptedtype)
	# Find all the objects of a certain type in a list
	# 
	# This is a syntactic sugar function, which returns a list of all the
	# objects in a iterable for which  ``isinstance(o,acceptedtype) == True``

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test_filter_discrete_types():
    # Signature: name(lst, acceptedtypes)
	# Creates a dictionary mapping types to objects of that type.
	# 
	# Starting with a list of object, and a list of types, this returns a
	# dictionary mapping each type to a list of objects of that type.
	# 
	# For example::
	# 
	#     >>> import types
	#     >>> filter_discrete_types( ['hello',1,2,'world'], ( basestring, types.IntType) ) #doctest: +NORMALIZE_WHITESPACE 
	#     {<type 'basestring'>: ['hello', 'world'], <type 'int'>: [1, 2]}
	# 
	# 
	# The function checks that each object is mapped to exactly one type

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test_join_norm():
    # Signature: name(*args)
	# No Docstring

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test_restore_sys_path():
    # Signature: name(func)
	# Decorator used to restore the sys.path
	# to the value it was before the function call.
	# This is useful for loading modules.

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test_safe_dict():
    # Signature: name(vals)
	# Create a dict, like dict(), but ensure no duplicate keys are given!
	# [Python silently allows dict( [(1:True),(1:None)] ) !!

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test_safe_dictionary_merge():
    # Signature: name(dictionaries)
	# Safely merge multiple dictionaries into one
	# 
	# Merges an iterable of dictionaries into a new single dictionary,
	# checking that there are no key collisions
	# 
	# >>> safe_dictionary_merge( [ {1:'One',2:'Two'},{3:'Three'} ] ) #doctest: +NORMALIZE_WHITESPACE 
	# {1: 'One', 2: 'Two', 3: 'Three'} 
	# 
	# >>> safe_dictionary_merge( [ {1:'One',2:'Two'},{3:'Three',1:'One'} ] ) #doctest: +NORMALIZE_WHITESPACE +IGNORE_EXCEPTION_DETAIL +SKIP
	# NineMLRuntimeError: Key Collision while merging dictionarys

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test__dispatch_error_func():
    # Signature: name(error_func, default_error=NineMLRuntimeError())
	# Internal function for dispatching errors. 
	# 
	# This was seperated out because it happens in a lot of utility functions

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()





# Testing Skeleton for function:


def test__filter():
    # Signature: name(lst, func=None)
	# Filter a list according to a predicate.
	# 
	# Takes a sequence [o1,o2,..] and returns a list contains those which 
	# are not `None` and satisfy the predicate `func(o)`
	# 
	# :param lst: Input iterable
	# :param func: Predicate function. If ``none``, this function always returns ``True``
	# 
	# 
	# Implementation::
	# 
	#     if func:
	#         return  [ l for l in lst if l is not None and func(l) ]
	#     else:
	#         return  [ l for l in lst if l is not None]
	#     
	# Examples:
	# 
	# >>> _filter( ['hello','world'] )         #doctest: +NORMALIZE_WHITESPACE 
	# ['hello', 'world']
	# 
	# 
	# >>> _filter( ['hello',None,'world'] )    #doctest: +NORMALIZE_WHITESPACE 
	# ['hello', 'world']
	# 
	# >>> _filter( [None,] )                   #doctest: +NORMALIZE_WHITESPACE 
	# []

    warnings.warn('Tests not implemented')
    # raise NotImplementedError()







# Testing Skeleton for class: LocationMgr

class LocationMgr_test(object):
    
    def test_Constructor(self):
        pass


    def test_StdAppendToPath(self):
        # Signature: name(cls)
		# No Docstring
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_getCatalogDir(self):
        # Signature: name(cls)
		# No Docstring
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_getComponentDir(self):
        # Signature: name(cls)
		# No Docstring
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_getRootDir(self):
        # Signature: name(cls)
		# No Docstring
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_getTmpDir(self):
        # Signature: name(cls)
		# No Docstring
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_std_append_to_path_called(self):
        # Signature: name
		# bool(x) -> bool
		# 
		# Returns True when the argument x is true, False otherwise.
		# The builtins True and False are the only two instances of the class bool.
		# The class bool is a subclass of the class int, and cannot be subclassed.
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


    def test_temp_dir(self):
        # Signature: name
		# str(object) -> string
		# 
		# Return a nice string representation of the object.
		# If the argument is a string, the return value is the same object.
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








# Testing Skeleton for class: Settings

class Settings_test(object):
    
    def test_Constructor(self):
        pass


    def test_enable_component_validation(self):
        # Signature: name
		# bool(x) -> bool
		# 
		# Returns True when the argument x is true, False otherwise.
		# The builtins True and False are the only two instances of the class bool.
		# The class bool is a subclass of the class int, and cannot be subclassed.
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()








