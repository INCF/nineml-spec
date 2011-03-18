"""
Run tests for NMODL generation from 9ML

Each example 9ML neuron component is converted to NMODL, a test-script run
using that mechanism and the results compared to expected values.
"""

from __future__ import with_statement
import sys, os
sys.path.append('..')
nineml2nmodl = __import__("9ml2nmodl")
from subprocess import Popen, PIPE

models = ["izhikevich", "morris-lecar", "leaky_iaf", ]
#models = ["morris-lecar"]

output_dir = "test_files"
if not os.path.exists(output_dir):
    os.makedirs(output_dir)

# generate NMODL files
for model in models:
    # run the example script, saving the XML to file
    xml_file = os.path.join(output_dir, "%s.xml" % model)
    with open(xml_file, 'w') as f:
        execfile('../../../lib9ml/python/examples/AL/%s.py' % model, {'f': f})
    # parse the XML and convert to NMODL
    nineml2nmodl.write_nmodl(xml_file)

# run nrnivmodl
p = Popen("nrnivmodl", shell=True, stdout=None, stderr=None, cwd=output_dir)
result = p.wait()

# run test scripts
cwd = os.getcwd()
os.chdir(output_dir)
from single_cell_current_injection import TestCase, configure, run
configure()
tests = {}
for model in models:
    test = __import__("test_%s" % model)
    tests[model] = TestCase(test.mechanism, test.parameters, test.initial_values, test.expected_spike_times)
run(100.0)

print "\n" + "="*26
for model, test in tests.items():
    print "%-20s %s" % (model, test.success and " OK " or "FAIL")
    test.plot("test_%s.png" % model)

os.chdir(cwd)    
