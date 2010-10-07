
import unittest
import nineml.abstraction_layer as nineml
from nineml.cache_decorator import cache_decorator, _cache, freeze, clear_cache


import os, tempfile




    





class CacheDecorTestCase(unittest.TestCase):

    def test_method_basic_args(self):

        # test class
        class Y(object):
            @cache_decorator
            def f(self, *args):
                return list(args)

        def f():
            pass

        clear_cache()

        y = Y()
        args = (10,11,12)
        kwargs = {}
        val = y.f(*args)
        assert val == list(args)
        cache_key = ('.'.join([f.__module__,f.func_name]),(y,)+args,freeze(kwargs))
        assert val == _cache[cache_key]

        # change cached value:
        new_val = 'hello'
        _cache[cache_key] = new_val

        # we get the new cached val, don't we?
        assert y.f(*args)==new_val


    def test_method_kwargs(self):

        # test class
        class Y(object):
            @cache_decorator
            def f(self, *args, **kwargs):
                d = {}
                d['__args__'] = args
                d.update(kwargs)
                return d

        def f():
            pass

        clear_cache()
        
        y = Y()
        args = (10,11,12)
        kwargs = {'q':20,'foo':{}}
        val = y.f(*args,**kwargs)
        e_val = {'__args__':args}
        e_val.update(kwargs)
        assert val == e_val
        cache_key = ('.'.join([f.__module__,f.func_name]),(y,)+args,freeze(kwargs))
        assert val == _cache[cache_key]

        # change cached value:
        new_val = 'hello'
        _cache[cache_key] = new_val

        # we get the new cached val, don't we?
        assert y.f(*args,**kwargs)==new_val


    def test_func_kwargs(self):

        # test class

        @cache_decorator
        def f(*args, **kwargs):
            d = {}
            d['__args__'] = args
            d.update(kwargs)
            return d

        def q():
            pass

        clear_cache()
        args = (10,11,12)
        kwargs = {'q':20,'foo':{}}
        val = f(*args,**kwargs)
        e_val = {'__args__':args}
        e_val.update(kwargs)
        assert val == e_val
        cache_key = ('.'.join([q.__module__,'f']),args,freeze(kwargs))
        assert val == _cache[cache_key]

        # change cached value:
        new_val = 'hello'
        _cache[cache_key] = new_val

        # we get the new cached val, don't we?
        assert f(*args,**kwargs)==new_val
        
    def test_collision(self):

        # test class
        class Y(object):
            state = 1.0
            @cache_decorator
            def f(self, *args, **kwargs):
                d = {}
                d['__args__'] = args
                d.update(kwargs)
                d['state'] = self.state
                return d

        # test class
        class Z(object):
            @cache_decorator
            def f(self, *args, **kwargs):
                d = {}
                d['_args_'] = args
                d.update(kwargs)
                return d

        y = Y()
        args1 = (10,11,12)
        kwargs = {'q':20,'foo':{}}
        e_valy = {'__args__':args1}
        e_valy.update(kwargs)
        e_valy['state'] = y.state

        z = Z()
        e_valz = {'_args_':args1}
        e_valz.update(kwargs)


        valy = y.f(*args1,**kwargs)
        valz = z.f(*args1,**kwargs)

        # classes with same function name stay distinct in cache

        assert y.f(*args1,**kwargs)==e_valy
        assert z.f(*args1,**kwargs)==e_valz

        # instances of same class stay distinct in cache

        yy = Y()
        yy.state = 2.0
        valyy = yy.f(*args1,**kwargs)

        e_valyy = {'__args__':args1}
        e_valyy.update(kwargs)
        e_valyy['state'] = yy.state

        assert yy.f(*args1,**kwargs) == e_valyy
        assert y.f(*args1,**kwargs)==e_valy
        assert z.f(*args1,**kwargs)==e_valz

        assert z.f(*args1,**kwargs)==e_valz
        assert y.f(*args1,**kwargs)==e_valy
        assert yy.f(*args1,**kwargs) == e_valyy


def suite():

    suite = unittest.makeSuite(CacheDecorTestCase,'test')
    return suite

if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
