"""This module contains general purpose utility functions for simplifying list
analysis.
"""


# This is to make 'join' available in other modules:
from os.path import join as Join


from os.path import dirname, normpath
import sys

import itertools


from nineml.exceptions import internal_error, raise_exception
from nineml.exceptions import NineMLRuntimeError



def _dispatch_error_func(error_func, default_error=NineMLRuntimeError() ):
    """Internal function for dispatching errors. 
    
    This was seperated out because it happens in a lot of utility functions
    """

    if error_func:
        if isinstance(error_func, Exception):
            raise error_func
        elif isinstance(error_func, basestring):
            raise NineMLRuntimeError(error_func)
        else:
            error_func()
            internal_error('error_func failed to raise Exception')
    else:
        if isinstance(default_error, Exception):
            raise default_error
        elif isinstance(default_error, basestring):
            raise NineMLRuntimeError(default_error)
        else:
            default_error()
            internal_error('default_error failed to raise Exception')








def expect_single(lst, error_func = None):
    """Retrieve a single element from an iterable.

    This function tests whether an iterable contains just a single element and
    if so returns that element. Otherwise it raises an Exception.
        
    :param lst: An iterable

    :param error_func: An exception object or a callable. ``error_func`` will be
        raised or called in case there is not exactly one element in ``lst``. If
        ``error_func`` is ``None``, a ``NineMLRuntimeError`` exception will be
        raised.


    :rtype: the element in the list, ``lst[0]``, provided ``len(lst)==1``
   

    
    >>> expect_single( ['hello'] )
    'hello'

    >>> expect_single( [1] )
    1

    >>> expect_single( [] ) #doctest: +SKIP
    NineMLRuntimeError: expect_single() recieved an iterable of length: 0

    >>> expect_single( [None,None] ) #doctest: +SKIP
    NineMLRuntimeError: expect_single() recieved an iterable of length: 2

    >>> expect_single( [], lambda: raise_exception( RuntimeError('Aggh') ) #doctest: +SKIP
    RuntimeError: Aggh

    >>> #Slightly more tersly:
    >>> expect_single( [], RuntimeError('Aggh') ) #doctest: +SKIP
    RuntimeError: Aggh

    """
    lst = list(lst)

    # Good case:
    if len(lst) == 1:
        return lst[0]

    # Bad case: our list doesn't contain just one element
    errmsg  = 'expect_single() recieved an iterable of length: %d' % len(lst)
    errmsg += '\n  - List Contents:' + str(lst) + '\n'
    _dispatch_error_func(error_func, NineMLRuntimeError(errmsg) )





def _filter(lst, func=None):
    """Filter a list according to a predicate.

    Takes a sequence [o1,o2,..] and returns a list contains those which 
    are not `None` and satisfy the predicate `func(o)`

    :param lst: Input iterable
    :param func: Predicate function. If ``none``, this function always returns ``True``


    Implementation::

        if func:
            return  [ l for l in lst if l is not None and func(l) ]
        else:
            return  [ l for l in lst if l is not None]
        
    Examples:

    >>> _filter( ['hello','world'] )         #doctest: +NORMALIZE_WHITESPACE 
    ['hello', 'world']


    >>> _filter( ['hello',None,'world'] )    #doctest: +NORMALIZE_WHITESPACE 
    ['hello', 'world']

    >>> _filter( [None,] )                   #doctest: +NORMALIZE_WHITESPACE 
    []

    """

    if func:
        return  [ l for l in lst if l is not None and func(l) ]
    else:
        return  [ l for l in lst if l is not None]



def filter_expect_single(lst, func=None, error_func=None):
    """Find a single element matching a predicate in a list.
    
       This is a syntactic-sugar function ``_filter`` and ``expect_single``
       in a single call.
    
        Returns::
    
            expect_single( _filter(lst, func), error_func )


        This is useful when we want to find an item in a sequence with a certain 
        property, and expect there to be only one.
        
        Examples:

        >>> find_smith = lambda s: s.split()[-1] == 'Smith'
        >>> filter_expect_single( ['John Smith','Tim Jones'], func=find_smith )  #doctest: +NORMALIZE_WHITESPACE 
        'John Smith'

    """
    return expect_single( _filter(lst, func), error_func )



def filter_by_type(lst, acceptedtype):
    """ Find all the objects of a certain type in a list
    
        This is a syntactic sugar function, which returns a list of all the
        objects in a iterable for which  ``isinstance(o,acceptedtype) == True``

    """
    return _filter( lst, lambda x: isinstance(x, acceptedtype))


def filter_discrete_types(lst, acceptedtypes):
    """Creates a dictionary mapping types to objects of that type.
    
    Starting with a list of object, and a list of types, this returns a
    dictionary mapping each type to a list of objects of that type.

    For example::

        >>> import types
        >>> filter_discrete_types( ['hello',1,2,'world'], ( basestring, types.IntType) ) #doctest: +NORMALIZE_WHITESPACE 
        {<type 'basestring'>: ['hello', 'world'], <type 'int'>: [1, 2]}


    The function checks that each object is mapped to exactly one type
    """

    res = dict( [ (a, []) for a in acceptedtypes ] )
    for obj in lst:
        obj_type = filter_expect_single( acceptedtypes, 
                                         lambda at: isinstance(obj, at), 
                                         error_func= '%s could not be mapped to a single type'%obj ) 
        res[obj_type].append(obj)
    return res


def assert_no_duplicates(lst, error_func=None):
    """Check for duplicates in a sequence.
    
    This function checks that a list contains no duplicates, by casting the list
    to a set and comparing the lengths.

    It raises an `NineMLRuntimeError` if the lengths are not equal.
    """

    if len(lst) != len( set(lst) ):
        
        # Find the duplication:
        seen_items = set()
        for i in lst:
            if i in seen_items:
                duplication = i
                break
            else:
                seen_items.add(i)
        
        _dispatch_error_func( error_func, 
                              "Unxpected duplication found: %s \n in %s" %( str(i) ,  str(lst)) ) 



def invert_dictionary(dct):
    """Takes a dictionary mapping (keys => values) and returns a 
    new dictionry mapping (values => keys).
    i.e. given a dictionary::

        {k1:v1, k2:v2, k3:v3, ...} 

    it returns a dictionary::
    
        {v1:k1, v2:k2, v3:k3, ...} 

    It checks to make sure that no values are duplicated before converting.
    """

    assert_no_duplicates( dct.values() )
    return dict( zip(dct.values(), dct.keys()) )


def flatten_first_level( nested_list ):
    """Flattens the first level of an iterable, ie
        
        >>> flatten_first_level( [ ['This','is'],['a','short'],['phrase'] ] ) #doctest: +NORMALIZE_WHITESPACE 
        ['This', 'is', 'a', 'short', 'phrase'] 

    """
    return list( itertools.chain(*nested_list) )

def safe_dictionary_merge( dictionaries ):
    """Safely merge multiple dictionaries into one
    
    Merges an iterable of dictionaries into a new single dictionary,
    checking that there are no key collisions
    
    >>> safe_dictionary_merge( [ {1:'One',2:'Two'},{3:'Three'} ] ) #doctest: +NORMALIZE_WHITESPACE 
    {1: 'One', 2: 'Two', 3: 'Three'} 

    >>> safe_dictionary_merge( [ {1:'One',2:'Two'},{3:'Three',1:'One'} ] ) #doctest: +NORMALIZE_WHITESPACE +IGNORE_EXCEPTION_DETAIL +SKIP
    NineMLRuntimeError: Key Collision while merging dictionarys

    """
    kv_pairs = list( itertools.chain(*[d.iteritems() for d in dictionaries]))
    keys, values  = zip( *kv_pairs )
    assert_no_duplicates(keys, 'Key collision while merging dictionarys') 
    return dict(kv_pairs)






#TODO: DOCUMENT THESE:

def join_norm(*args):
    return normpath( Join(*args))


class LocationMgr(object):

    @classmethod
    def getRootDir(cls):
        localDir = dirname( __file__ )
        rootDir = join_norm( localDir, "../../../../") 
        return rootDir

    @classmethod
    def getCatalogDir(cls):
        return join_norm( cls.getRootDir(), "catalog/" )

    @classmethod
    def StdAppendToPath(cls):
        root = cls.getRootDir()
        sys.path.append(Join(root, "lib9ml/python/examples/AL"))
        sys.path.append(Join(root, "code_generation/nmodl"))     






def check_list_contain_same_items(lst1, lst2, desc1="", desc2="", ignore=[],
        desc=""):
    set1 = set(lst1)
    set2 = set(lst2)

    for i in ignore:
        set1.discard(i)
        set2.discard(i)


    # Are the lists subsets of each other.
    if set1.issubset( set2 ) and set2.issubset( set1 ):
        return

    errmsg =  "Lists were suppose to contain the same elements, but don't!!" 
    if desc: errmsg += '\n' + desc
    errmsg += "\n1: [%s]: %s"%(desc1, sorted(set1) )
    errmsg += "\n2: [%s]: %s"%(desc2, sorted(set2) )
    errmsg += "\nElements in : 1 (not 2): %s"% (sorted( set1-set2 )  )
    errmsg += "\nElements in : 2 (not 1): %s"% (sorted( set2-set1 )  )
    raise NineMLRuntimeError(errmsg)








def safe_dict( vals ):
    """ Create a dict, like dict(), but ensure no duplicate keys are given!
    [Python silently allows dict( [(1:True),(1:None)] ) !!""" 
    d = {}
    for k,v in vals:
        if k in vals:
            err = 'safe_dict() failed with duplicated keys: %s'%k
            raise NineMLRuntimeError(err)
        d[k] = v
    assert len(vals) == len(d)
    return d
