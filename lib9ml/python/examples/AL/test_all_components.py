
import os
import glob
import sys
import shutil
import hashlib

import nineml.abstraction_layer as al
from nineml.abstraction_layer import readers
from nineml.abstraction_layer import writers
from nineml.abstraction_layer import validators
from nineml.abstraction_layer import flattening
from nineml.abstraction_layer import component_modifiers

# This is so we can use nineml2nmodl files
from nineml.utility import LocationMgr
LocationMgr.StdAppendToPath()


build_dir = 'build/'
output = 'output/'


def main():
    src_dir = 'components/'

    print
    print ' Testing all Components in: ', src_dir

    component_files = glob.glob( src_dir + '/*.py')
    for src_file in component_files:
        test_component_file( src_file )

    

def file_sha1_sum(filename):
    f = open(filename ,'rb')
    return hashlib.sha1(f.read()).hexdigest()



def get_component_functor( filename ):
    module_name =  filename.replace('.py','').replace('//','/').replace('/','.')
    module_name_short = module_name.split('.')[-1]
    

    try:
        print "  -- Trying to import: %s"% module_name
        module = __import__( module_name )
        
        print "  -- Trying to find get_component() method."
        module_contents = module.__dict__[module_name_short]
        fnc = module_contents.get_component

        print '  -- Found get_component()'
        return fnc

    except Exception, e:
        print '***** ERROR: Failed to import module ********'
        print
        print e




def test_component_file( filename ):
    if filename.endswith('__init__.py'):
        return

    print
    print '  Looking in file:', filename

    component_functor = get_component_functor(filename)


    if not component_functor:
        return
    print '  -- Instantiating Component'
    component = component_functor()
    assert component
    
    print '  -- Clearing the build_dir: %s'%build_dir
    if os.path.exists(build_dir):
        shutil.rmtree(build_dir)
    os.mkdir(build_dir)


    # Run some tests:
    test_one_and_a_half_trips(component)
    test_write_dot(component)
    test_write_mod(component)


    
    #Save all the output files:
    






def test_one_and_a_half_trips(component):
    print '  -- Testing One and a half trips...'

    if not component.is_flat():
        component = flatten(component)


    xmlfile1 = build_dir + component.name + '1.xml'
    xmlfile2 = build_dir + component.name + '2.xml'


    print '    -- Saving Component To XML:', xmlfile1
    writers.XMLWriter.write( component, xmlfile1 ) 

    print '    -- Loading Component To XML.'
    reloaded_comp = readers.XMLReader.read(xmlfile1)

    print '    -- Checking Components are identical'
    validators.ComponentEqualityChecker.check_equal( component, reloaded_comp)

    print '    -- Saving Reloaded Component to XML',xmlfile2
    writers.XMLWriter.write( reloaded_comp, xmlfile2 ) 

    print '    -- Checking the SHA1 Checksum of the two xml files:'
    hash1 = file_sha1_sum(xmlfile1)
    hash2 = file_sha1_sum(xmlfile2)
    print '      -->', hash1
    print '      -->', hash2

    if hash1 != hash2: 
        raise ValueError('XML files are different. This may not be an error but please report it to the developers')
    


def test_write_dot(component):
    print '  -- Writing Component to .dot'
    dotfile = build_dir + component.name + '.dot'
    writers.DotWriter.write(component, dotfile)

    print '  -- Building .dot -> pdf, svg, png'
    writers.DotWriter.build(dotfile, output_types=['pdf','svg','png'])


def test_write_mod(component):

    component_modifiers.ComponentModifier.close_all_reduce_ports(component=component) 

    print '  -- Writing Component to .mod'
    modfilename = build_dir + component.name + '.mod'
    modfilename = modfilename.replace('-','_')
    writers.ModFileWriter.write( component = component, filename=modfilename )
    writers.ModFileWriter.compile_modfiles(build_dir) 









main()



