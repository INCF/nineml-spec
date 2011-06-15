
from os.path import dirname, join, normpath

from os.path import join as Join
import sys




def ExpectSingle(lst):
    if len(lst) != 1:
        print "Filter Expect Single: ",lst
        assert False

    assert len(lst) == 1
    return lst[0]


def FilterExpectSingle(lst, func= lambda x: x is None):

    return ExpectSingle( Filter(lst, func) )

def Filter(lst,func):
    return  [ l for l in lst if l and func(l) ]

def FilterType(lst, acceptedtype):
    return Filter( lst, lambda x: isinstance(x,acceptedtype))









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



import itertools


def invertDictionary(d):
    # Check for duplicated values:
    assert len( set(d.values()) ) == len( d.values() )
    return dict( zip(d.values(), d.keys()) )


def flattenFirstLevel( nestedList ):
    return list( itertools.chain(*nestedList) )

def safe_dictionary_merge( dictionaries ):
    newDict = {}
    for d in dictionaries:
        for k,v in d.iteritems():
            assert not k in newDict
            newDict[k] = v
    return newDict

