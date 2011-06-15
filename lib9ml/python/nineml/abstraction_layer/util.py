
import core

def check_flat_component( func ):
    def newfunc(*args,**kwargs):
        print args
        print kwargs
        if 'component' in kwargs:
            component = kwargs['component']
            assert isinstance( component, core.ComponentNodeCombined)
            assert component.isflat()
        else:
            assert False

        return func(*args,**kwargs)
    return newfunc 
