

_cache = {}



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





def im_cache_decorator(f):
    """ Instance Method caching decorator

    This is better than cache_decorator for instance methods:
    - the __cache__ per instance is freed when the instance is freed
    - it is easier to invalidate the cache on a per function basis.

    This caches in the instance in a instance attribute

    __cache__

    assigning instance.__cache__={}

    will reset the cache

    assigning instance.__cache__['f'] = {}
    will reset the cache for instance.f

    """

    import types

    def new_f(self, *args,**kwargs):

        # create a cache in the class for this function
        # if it does not already exist

        if hasattr(self,'__cache__'):
            cache = self.__cache__.get(f.func_name)
        else:
            self.__cache__ = {}
            cache = None

        cache_key = (args,freeze(kwargs))

        # if no cache yet for the func
        # proceed to evaluate and return
        if not cache:
            val = f(self, *args,**kwargs)
            cache = {cache_key:val}
            self.__cache__[f.func_name] = cache
            return val

        # there is a cache, but for these args,kwargs?
        try:
            val = cache[cache_key]
        except KeyError:
            # no cached value found
            # call the func
            val = f(self, *args,**kwargs)
            cache[cache_key] = val

        # return cached or new value
        return val
    new_f.__name__ = '__cached__'+f.__name__

    # return function wrapped with cache
    return new_f




