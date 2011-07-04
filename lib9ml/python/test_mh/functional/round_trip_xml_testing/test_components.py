#!/usr/bin/python


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


from nineml.abstraction_layer.testing_utils import std_pynn_simulation



def clear_and_recreate_dir(dir_name):
    print '  -- Clearing the build_dir: %s'%dir_name
    if os.path.exists(dir_name):
        shutil.rmtree(dir_name)
    os.mkdir(dir_name)



def main(src=None):

    
    build_dir = 'build/'
    output_dir = 'output/'

    print 'Clearing output directory: %s' % output_dir
    clear_and_recreate_dir(output_dir)


    #single_file_mode = os.path.isfile(src)

    if src:
        print ' Testing Component: %s'% src
        src_files = [src]
    else:
        print ' Testing all Components.'
        src_files = TestableComponent.list_available()
        #src_files = glob.glob( src + '/*.py')


    for src_file in src_files:

        # Clear the build-dir
        clear_and_recreate_dir(build_dir)
        clear_and_recreate_dir('nineml_mechanisms')

        # Load the file:
        print '  -- Loading from file: %s' % src_file
        t = TestableComponent(src_file)

        # Run some tests:
        TestXMLWriteReadWrite.test(t, build_dir=build_dir)
        TestWriteDot.test(t, build_dir=build_dir)

        if t.has_metadata():
            if t.metadata.is_neuron_model:
                test_write_mod(t)
        
            if src:
                flg = 'supports_test_pynn_neuron_std'
                if t.metadata.__dict__.get(flg, False):
                    test_pynn_neuron_std(t)

        #Save all the output files:
        
        shutil.move(build_dir, output_dir)
        shutil.move( os.path.join(output_dir,build_dir),
                os.path.join(output_dir,src_file.replace('.py','') ) )
        print '  Everything Ran Fine'
        print '  -------------------'
    







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




def test_pynn_neuron_std(testable_component):
    t = testable_component 
    
    flg = 'supports_test_pynn_neuron_std'
    assert t.metadata.__dict__.get(flg, False)

    std_pynn_simulation( 
                test_component = testable_component(),
                parameters = t.metadata.parameters, 
                initial_values = t.metadata.initial_values, 
                synapse_components = t.metadata.synapse_components, 
                records = t.metadata.records
                         )



















if len(sys.argv) == 1:
    main()

elif len(sys.argv) == 2:
    main(src=sys.argv[1])

else:
    raise NineMLRuntimeError('Invalid Usage: test_all_components.py [src]')
