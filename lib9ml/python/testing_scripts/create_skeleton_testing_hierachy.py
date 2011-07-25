import inspect
from collections import defaultdict 
import os

from Cheetah.Template import Template

modules = []
classes = []
functions = []
classes_in_file = defaultdict( list ) 


def is_object_in_module(o):
    module = inspect.getmodule(o)
    if not module:
        return False
    if not module.__name__.startswith('nineml'):
        return False
    return True

def inspect_class(c):
    if is_object_in_module(c):
        classes.append(c)

def inspect_function(f):
    if is_object_in_module(f):
        functions.append(f)

def inspect_module(m):
    modules.append(m)
    for name, data in inspect.getmembers(m):
        if name == '__builtins__':
            continue
        if name.startswith('__'):
            continue
        if inspect.isbuiltin(data):
            continue
        

        if inspect.ismodule(data):
            if data in modules:
                continue
            inspect_module(data)
            
        if inspect.isclass(data):
            if data in classes:
                continue
            inspect_class(data)

        if inspect.isfunction(data):
            if data in functions:
                continue
            inspect_function(data)
    



import sys
sys.setrecursionlimit(50)
sys.path.insert(0, '/home/hull/junk')

src_package = 'nineml'
root_dir = '/home/hull/src/nineml-svn-model-tree/lib9ml/python/'
root_testing_dir = '/home/hull/src/nineml-svn-model-tree/lib9ml/python/test_mh/unit'

m =  __import__(src_package)
inspect_module(m)


print classes
print functions





def get_local_testing_filename( o ):
    f = inspect.getfile(o)
    f = f.replace(root_dir,'')
    print f
    f = f.replace('.pyc','.py')
    assert f.count('.py') == 1
    f = f.replace('.py','_test.py')
    f = f.replace('__init__','INIT')
    return f












test_file_templ = """

# Automatically Generated Testing Skeleton Template:
import warnings
import unittest
import nineml


#for $func_block in $func_blocks:
$func_block
#end for


#for $cls_block in $class_blocks:
$cls_block
#end for


"""


test_func_templ = """

# Testing Skeleton for function:

#set name,value,doc, fsig = $to_test

class Test${name}(unittest.TestCase):

    def test_${name}(self):
        # Signature: name${fsig}
        #
${doc}
        $import_stmt
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


"""


test_class_templ = """

# Testing Skeleton for class: $cls.__name__

class ${test_classname}(unittest.TestCase):
    
    def test_Constructor(self):
        pass


#for name,value,doc, fsig in $to_test:
    def test_${name}(self):
        # Signature: name${fsig}
        #
${doc}
        $import_stmt
        warnings.warn('Tests not implemented')
        # raise NotImplementedError()


#end for



"""



def get_package_import(obj):
    mod = inspect.getmodule(obj)
    if not mod:
        return "# [Can't find module]"
   
    pkg = mod.__name__#.replace(root_dir,'') 
    return "#from %s import %s"%(pkg, obj.__name__)

# Represents a Test File:
class TestingFile(object):
    def __init__(self, filename, root_testing_dir):
        self.root_testing_dir = root_testing_dir
        self.filename = filename
        self.classes = []
        self.funcs = []
    
    @property 
    def full_path(self):
        return os.path.join( self.root_testing_dir, self.filename )

    def add_test_class(self, cls):
        self.classes.append(cls)

    def add_test_func(self, func):
        self.funcs.append(func)

    
    
    def get_class_skeleton_text(self, cls):
        def get_doc(obj):
            prefix = '\t\t# '
            if not data.__doc__:
                return prefix + 'No Docstring'

            d = inspect.cleandoc( data.__doc__ ) 
            return '\n'.join([ prefix + l for l in d.splitlines() ] )
        
        to_test = []
        for name,data in  inspect.getmembers(cls):
            if name.startswith('_'):
                continue

            fsig = ''
            try:
                fsig = inspect.formatargspec(*inspect.getargspec(data))
            except:
                pass

            to_test.append( (name,data,get_doc(data), fsig) )

        context = { 'cls':cls,
                    'test_classname': cls.__name__ + '_test',
                    'to_test': to_test,
                    'import_stmt':get_package_import(cls),

                } 
        t = Template(test_class_templ, context).respond()
        return t
        



    def get_func_skeleton_text(self, func):
        def get_doc(obj):
            prefix = '\t\t# '
            if not obj.__doc__:
                return prefix + 'No Docstring'

            d = inspect.cleandoc( obj.__doc__ ) 
            return '\n'.join([ prefix + l for l in d.splitlines() ] )
       

        fsig = ''
        try:
            fsig = inspect.formatargspec(*inspect.getargspec(func))
        except:
            pass

        to_test = (func.__name__,func,get_doc(func), fsig) 

        context = { 'func':func,
                    'test_classname': func.__name__ + '_test',
                    'to_test': to_test,
                    'import_stmt':get_package_import(func),
                } 

        t = Template(test_func_templ, context).respond()
        return t



    
    def create_skeleton( self, suffix='' ):
        full_path = self.full_path + suffix
        if os.path.exists( full_path ):
        #if False and os.path.exists( full_path ):
            print 'Not Overwriting file:', full_path
            return

        dirname,filename = os.path.split(full_path)
        if not os.path.exists(dirname):
            print 'Creating Directory:', dirname
            os.makedirs(dirname)

        package_init = dirname + '/__init__.py'
        if not os.path.exists(package_init):
            open(package_init,'w').close()

        func_blocks  = [self.get_func_skeleton_text(f) for f in self.funcs]
        class_blocks = [self.get_class_skeleton_text(c) for c in self.classes]
        
    
        print 'Creating Skeleton File:', full_path
        context = {'class_blocks':class_blocks, 'func_blocks': func_blocks } 
        f_text = Template( test_file_templ, context ).respond() 

        f = open( full_path, 'w')
        f.write( f_text )
        f.close()



files_to_create = {}

for c in classes:
    f = get_local_testing_filename(c)
    if not f in files_to_create:
        files_to_create[f] = TestingFile(f, root_testing_dir)
    files_to_create[f].add_test_class(c)
    


for func in functions:
    f = get_local_testing_filename(func)
    if not f in files_to_create:
        files_to_create[f] = TestingFile(f, root_testing_dir)
    files_to_create[f].add_test_func(func)

print 'Done'




for testing_file in files_to_create.values():
    print testing_file.full_path, testing_file.filename
    for cls in testing_file.classes:
        print ' -- C:', cls
    for func in testing_file.funcs:
        print ' -- F:', func

    testing_file.create_skeleton()
