
from os.path import dirname, join, normpath

from os.path import join as Join
import sys

import itertools


from nineml.exceptions import internal_error, raise_exception, NineMLRuntimeError





def expect_single(lst, error_func = None):
    """Function to test whether an iterable contains just a single element
    and if so return that element. Otherwise raises an Exception.
        
    Keyword arguments:

    :param lst: An iterable
    :param error_func: An exception object or a callable. ``error_func`` will be raised or called in case there is not exactly one element in ``lst``
    :rtype: the element in the list, ``lst[0]``, provided ``len(lst)==1``
    
    
    """
    lst = list(lst)

    # Good case:
    if len(lst) == 1:
        return lst[0]

    # Bad case: our list doesn't contain one
    if error_func:
        if isinstance(error_func, Exception):
            raise error_func
        else:
            error_func()
            internal_error('error_func failed to raise Exception')
    else:
        errmsg  = 'expect_single() recieved an iterable of length: %d\n' %len(lst)
        errmsg += '  - List Contents:' + str(lst) + '\n'
        raise NineMLRuntimeError(errmsg)




expect_single( ['d','sd'], error_func =  NineMLRuntimeError() ) #'lambda : raise_exception( 'AGGGGH' ) )



def filter_expect_single(lst, func= lambda x: x is None):

    return expect_single( filter(lst, func) )

def filter(lst,func):
    return  [ l for l in lst if l and func(l) ]

def filter_by_type(lst, acceptedtype):
    return filter( lst, lambda x: isinstance(x,acceptedtype))


def filter_discrete_types(lst, acceptedtypes):
    # Starting with a list, splits into a dictionary which maps class types to
    # a list of objects of that type.
    res = {}
    for a in acceptedtypes:
        res[a] = filter( lst, lambda x: isinstance(x,a))

    nCounted = sum([ len(l) for l in res.values()])
    if nCounted != len(lst):
        print 'Initial/Final: %d/%d'%( len(lst), nCounted)
        print 'Object Types:', [ type(o) for o in lst]
        print 'Class Counts', [ (a, len( res[a])) for a in acceptedtypes ] 
        assert False
    return res



def assert_no_duplicates(lst):
    assert len(lst) == len( set(lst) )





def join_norm(*args):
    return normpath( join(*args))


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
        sys.path.append(join(root, "lib9ml/python/examples/AL"))
        sys.path.append(join(root, "code_generation/nmodl"))     





def invert_dictionary(d):
    # Check for duplicated values:
    assert len( set(d.values()) ) == len( d.values() )
    return dict( zip(d.values(), d.keys()) )


def flatten_first_level( nestedList ):
    return list( itertools.chain(*nestedList) )

def safe_dictionary_merge( dictionaries ):
    newDict = {}
    for d in dictionaries:
        for k,v in d.iteritems():
            assert not k in newDict
            newDict[k] = v
    return newDict

