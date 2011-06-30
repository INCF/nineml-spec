
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


from nineml.abstraction_layer.testing_utils import TestableComponent

from nineml.abstraction_layer.testing_utils import TestXMLWriteReadWrite
from nineml.abstraction_layer.testing_utils import TestWriteDot


# This is so we can use nineml2nmodl files
from nineml.utility import file_sha1_hexdigest





def clear_and_recreate_dir(dir_name):
    print '  -- Clearing the build_dir: %s'%dir_name
    if os.path.exists(dir_name):
        shutil.rmtree(dir_name)
    os.mkdir(dir_name)



def main():
    build_dir = 'build/'
    output_dir = 'output/'
    src_dir = 'components_done/'

    print 'Clearing output directory: %s' % output_dir
    clear_and_recreate_dir(output_dir)

    print ' Testing all Components in: %s'% src_dir

    for src_file in glob.glob( src_dir + '/*.py'):

        # Clear the build-dir
        clear_and_recreate_dir(build_dir)

        # Load the file:
        t = TestableComponent(filename)

        # Run some tests:
        TestXMLWriteReadWrite.test(t, build_dir=build_dir)
        TestWriteDot.test(t, build_dir=build_dir)

        if t.has_metadata():
            if t.metadata.is_neuron_model:
                test_write_mod(t)

        #Save all the output files:
        
        shutil.move(build_dir, output_dir)
        shutil.move( os.path.join(output_dir,build_dir),
                os.path.join(output_dir,srcfile.replace('.py','') ) )
        print '  Everything Ran Fine'
        print '  -------------------'
    







def test_component_file( filename ):
    pass
    
    






#def test_one_and_a_half_trips(testable_component):
#    component = testable_component()
#    print '  -- Testing One and a half trips...'
#
#    if not component.is_flat():
#        component = flatten(component)
#
#
#    xmlfile1 = build_dir + component.name + '1.xml'
#    xmlfile2 = build_dir + component.name + '2.xml'
#
#
#    print '    -- Saving Component To XML:', xmlfile1
#    writers.XMLWriter.write( component, xmlfile1 ) 
#
#    print '    -- Loading Component To XML.'
#    reloaded_comp = readers.XMLReader.read(xmlfile1)
#
#    print '    -- Checking Components are identical'
#    validators.ComponentEqualityChecker.check_equal( component, reloaded_comp)
#
#    print '    -- Saving Reloaded Component to XML',xmlfile2
#    writers.XMLWriter.write( reloaded_comp, xmlfile2 ) 
#
#    print '    -- Checking the SHA1 Checksum of the two xml files:'
#    hash1 = file_sha1_hexdigest(xmlfile1)
#    hash2 = file_sha1_hexdigest(xmlfile2)
#    print '      -->', hash1
#    print '      -->', hash2
#
#    if hash1 != hash2: 
#        raise ValueError('XML files are different. This may not be an error but please report it to the developers')
#    
#
#
#def test_write_dot(testable_component):
#    component = testable_component()
#    print '  -- Writing Component to .dot'
#    dotfile = build_dir + component.name + '.dot'
#    writers.DotWriter.write(component, dotfile)
#
#    print '  -- Building .dot -> pdf, svg, png'
#    writers.DotWriter.build(dotfile, output_types=['pdf','svg','png'])


def test_write_mod(testable_component):
    component = testable_component()

    from nineml.utility import LocationMgr
    LocationMgr.StdAppendToPath()
    component_modifiers.ComponentModifier.close_all_reduce_ports(component=component) 

    print '  -- Writing Component to .mod'
    modfilename = build_dir + component.name + '.mod'
    modfilename = modfilename.replace('-','_')
    writers.ModFileWriter.write( component = component, filename=modfilename )
    writers.ModFileWriter.compile_modfiles(build_dir) 









main()



