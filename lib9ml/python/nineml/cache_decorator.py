

_cache = {}


def im_cache_decorator(f):
    """ Instance Method caching decorator """

    import types
    assert type(f)==types.MethodType, "Must decorate an instance method."

    # TODO, this is not quite right because 
    def new_f(*args,**kwargs):
        cache_key = ('.'.join([f.__module__,f.im_self]),f.__name__,args,tuple(kwargs.iteritems()))
        try:
            val = _cache[cache_key]
        except KeyError:
            # no cached value found
            # call the func
            val = f(*args,**kwargs)
            _cache[cache_key] = val

        # return cached or new value
        return val

    # return function wrapped with cache
    return new_f


def dict_to_tuple_recursive(di):
    """ Turns dicts to tuples recursivly (if dicts inside dict)"""
    import copy
    # shallow copy so i can replace
    d = copy.copy(di)
    for k,v in d.iteritems():
        if isinstance(v,list):
            v = list_to_tuple_recursive(v)
        elif isinstance(v,dict):
            v = dict_to_tuple_recursive(v)
        d[k]=v
    # freeze
    return tuple(d.iteritems())

def list_to_tuple_recursive(li):
    import copy
    l = copy.copy(li)
    for i,v in enumerate(l):
        if isinstance(v,list):
            v = list_to_tuple_recursive(v)
        elif isinstance(v,dict):
            v = dict_to_tuple_recursive(v)
        l[i]=v
    # freeze
    return tuple(l)


def freeze(o):
    """ Turns lists and dicts to tuples recursivly with the hope that the result is hashable"""
    if isinstance(o,dict):
        return dict_to_tuple_recursive(o)
    elif isinstance(o,list):
        return list_to_tuple_recursive(o)
    else:
        return o
    

def clear_cache():
    _cache = {}

def cache_decorator(f):
    """ caching decorator """

    import types

    def new_f(*args,**kwargs):
        cache_key = ('.'.join([f.__module__,f.func_name]),args,freeze(kwargs))
        try:
            val = _cache[cache_key]
        except KeyError:
            # no cached value found
            # call the func
            val = f(*args,**kwargs)
            _cache[cache_key] = val

        # return cached or new value
        return val

    # return function wrapped with cache
    return new_f
