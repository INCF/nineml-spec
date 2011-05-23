
def invertDictionary(d):
    # Check for duplicated values:
    assert len( set(d.values()) ) == len( d.values() )
    return dict( zip(d.values(), d.keys()) )
