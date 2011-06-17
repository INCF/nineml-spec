
import component

def check_flat_component( func ):
    def newfunc(*args,**kwargs):
        print args
        print kwargs
        if 'component' in kwargs:
            comp = kwargs['component']
            assert isinstance( comp, component.ComponentClass)
            assert comp.isflat()
        else:
            assert False

        return func(*args,**kwargs)
    return newfunc 
