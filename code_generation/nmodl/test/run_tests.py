"""
Run tests for NMODL generation from 9ML

Each example 9ML neuron component is converted to NMODL, a test-script run
using that mechanism and the results compared to expected values.
"""

from __future__ import with_statement
import sys, os
import shutil
sys.path.append('..')
nineml2nmodl = __import__("nineml2nmodl")
from subprocess import Popen, PIPE

#models = ["izhikevich", "morris_lecar", "leaky_iaf", "if_cond_exp"]
models = ["izhikevich",] 

import nineml


test_dir = 'test_files'
output_dir = test_dir


# Remove and recreate the old test-dir:
if os.path.exists(test_dir):
    shutil.rmtree(test_dir)
os.makedirs(output_dir)


# generate NMODL files
for model in models:
    # run the example script, saving the XML to file
    tc = nineml.abstraction_layer.testing_utils.TestableComponent(model)

    component = tc()
    #nineml.al.component_modifiers.ComponentModifier.close_all_reduce_ports(component)
    nineml.al.component_modifiers.ComponentModifier.remap_port_to_parameter(component, 'Isyn')

    nineml2nmodl.write_nmodldirect(component=component,
                                  mod_filename='%s/%s.mod'%(test_dir, model) )


    

# run nrnivmodl
p = Popen("nrnivmodl", shell=True, stdout=None, stderr=None, cwd=output_dir)
result = p.wait()
assert result == 0, "nrnivmodl failed"

# run test scripts
cwd = os.getcwd()
os.chdir(output_dir)
#from single_cell_current_injection import TestCase, configure, run
from util import configure, run
configure()
tests = {}
for model in models:
    test = __import__("test_%s" % model)
    tests[model] = test.TestCase(test.mechanism, test.parameters, test.initial_values, test.expected_output)
run(100.0)

print "\n" + "="*26
for model, test in tests.items():
    print "%-20s %s" % (model, test.success and " OK " or "FAIL")
    if not test.success:
        print ' -- ', test.calculate_errors()
    
    test.plot("test_%s.png" % model)

os.chdir(cwd)    
