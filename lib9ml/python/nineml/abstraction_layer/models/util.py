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

