import nineml
from nineml.abstraction_layer.testing_utils import RecordValue, TestableComponent
from nineml.abstraction_layer import ComponentClass
from nineml.abstraction_layer.testing_utils import std_pynn_simulation

# Load the Component:
coba1_base = TestableComponent('hierachical_iaf_1coba')
coba1 = coba1_base()

# Write the component back out to XML
nineml.al.writers.XMLWriter.write(coba1, 'TestOut_Coba1.xml')
nineml.al.writers.DotWriter.write(coba1, 'TestOut_Coba1.dot')
nineml.al.writers.DotWriter.build('TestOut_Coba1.dot')





# Simulate the Neuron:
records = [
    RecordValue(what='iaf_V', tag='V', label='V'), 
    RecordValue(what='regime', tag='Regime', label='Regime'), 
        ]

parameters = nineml.al.flattening.ComponentFlattener.flatten_namespace_dict({
'cobaExcit_tau':5.0, 
'cobaExcit_vrev':0, 
'iaf_cm':1, 
'iaf_gl':50, 
'iaf_taurefrac':8, 
'iaf_vreset':-60, 
'iaf_vrest':-60, 
'iaf_vthresh':-40
 })

initial_values = {
        'iaf_V': parameters['iaf_vrest'],
        'tspike': -1e99,
        'regime': 1002,
            }

res = std_pynn_simulation( test_component = coba1,
                    parameters = parameters, 
                    initial_values = initial_values,
                    synapse_components = [('cobaExcit','q')],
                    synapse_weights=15.0,
                    records = records,
                    sim_time=250,
                    syn_input_rate=100
                   )




