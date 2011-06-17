

class NineMLRuntimeError(Exception):
    pass


def internal_error(s):
    assert False, 'INTERNAL ERROR:' + s

def raise_exception( exception = None ):
    if exception:
        if isinstance(exception, basestring):
            raise NineMLRuntimeError(exception)
        else:
            raise exception
    else:
        raise NineMLRuntimeError()



