import os
import sys

from nineml.exceptions import NineMLRuntimeError

def restore_sys_path( func ):
    def newfunc(*args,**kwargs):
        oldpath = sys.path[:]
        try:
            return func(*args, **kwargs)
        finally:
            sys.path = oldpath
    return newfunc


@restore_sys_path
def load_py_module( filename ):
    if not os.path.exists(filename):
        print os.getcwd()
        raise NineMLRuntimeError('File does not exist %s'%filename )

    dirname,fname = os.path.split( filename )
    sys.path = [dirname] + sys.path

    module_name =  fname.replace('.py','')
    module_name_short = module_name
    
    module = __import__( module_name )
    return module
        
    



class TestableComponent(object):

    functor_name = 'get_component'
    metadata_name = 'ComponentMetaData'

    def __repr__(self):
        s = 'Testable Component from %s [MetaData=%s]'%(self.filename, self.has_metadata)
        return s

    def has_metadata(self):
        return self.metadata != None
    
    def __call__(self):
        return self.component_functor()

    def __init__(self, filename):
       cls = TestableComponent 

       self.filename = filename
       self.mod = load_py_module(filename )

       # Get the component functor:
       if not cls.functor_name in self.mod.__dict__.keys():
           err = """Can't load TestableComponnet from %s"""% self.filename
           err+= """Can't find required method: %s""" % cls.functor_name
           raise NineMLRuntimeError(err)

       self.component_functor = self.mod.__dict__[cls.functor_name]
    
       # Check the functor will actually return us an object:
       try:
            c = self.component_functor()
       except Exception, e:
           print e
           raise NineMLRuntimeError('component_functor() threw an exception')

       from nineml.abstraction_layer import ComponentClass
       if not isinstance(c, ComponentClass):
            raise NineMLRuntimeError('Functor does not return Component Class')

       # Try and get the meta-data
       self.metadata = None
       if cls.metadata_name in self.mod.__dict__.keys():
           self.metadata = self.mod.__dict__[cls.metadata_name]


    
        
    
