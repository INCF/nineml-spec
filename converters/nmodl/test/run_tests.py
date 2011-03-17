

#for each example script in ../../lib9ml/python/examples:
#    execute with f defined (as in nineml tests)
    #d = dict(f=f)
    #execfile(os.path.join('../examples/AL',e),d)
    # could probably cut-out the XML-generation part, and go directly from the final component
    # which seems always to be called "c1"
    # it might be easier if the examples were auto-generated from the tests,
    # rather than the other way round
    
    # run ../9ml2nmodl.write_nmodl, storing results in user-provided output directory (smt integration)
    # run nrnivmodl in that directory
    # run test script associated with the example

from __future__ import with_statement
import sys, os
sys.path.append('..')
nineml2nmodl = __import__("9ml2nmodl")
from subprocess import Popen, PIPE


output_dir = "test_files"
if not os.path.exists(output_dir):
    os.makedirs(output_dir)

# run the example script, saving the XML to file
xml_file = os.path.join(output_dir, "leaky_iaf.xml")
with open(xml_file, 'w') as f:
    execfile('../../../lib9ml/python/examples/AL/leaky_iaf.py', {'f': f})

# parse the XML and convert to NMODL
nineml2nmodl.write_nmodl(xml_file)

# run nrnivmodl
p = Popen("nrnivmodl", shell=True, stdout=None, stderr=None, cwd=output_dir)
result = p.wait()

# run test script
cwd = os.getcwd()
os.chdir(output_dir)
execfile("../test_LeakyIAF.py")
os.chdir(cwd)
assert success
