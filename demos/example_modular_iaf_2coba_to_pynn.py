
"""
Example of using a cell type defined in 9ML with pyNN.neuron
"""


#from std_pynn_simulation import std_pynn_simulation, RecordValue


from nineml.abstraction_layer.testing_utils import TestableComponent
from nineml.abstraction_layer.testing_utils import std_pynn_simulation, RecordValue 

test_component = TestableComponent('hierachical_iaf_2coba')()

parameters = {
    'iaf.cm': 1.0,
    'iaf.gl': 50.0,
    'iaf.taurefrac': 5.0,
    'iaf.vrest': -65.0,
    'iaf.vreset': -65.0,
    'iaf.vthresh': -50.0,
    'cobaExcit.tau': 2.0,
    'cobaInhib.tau': 5.0,
    'cobaExcit.vrev': 0.0,
    'cobaInhib.vrev': -70.0,
}


initial_values = {
    'iaf_V': parameters['iaf.vrest'],
    'tspike': -1e99,
    'regime': 1002,
        }


synapse_components = [
    ( 'cobaInhib', 'q' ),
    ( 'cobaExcit', 'q' ),
    ]


records = [
    RecordValue( what='iaf_V',       tag='Voltage [mV]',     label='Membrane Voltage' ),
    RecordValue( what='cobaInhib_g', tag='Conductance [ns]', label='cobaInhib-g' ),
    RecordValue( what='cobaExcit_g', tag='Conductance [ns]', label='cobaExcit-g' ),
    RecordValue( what='regime',      tag='Regime',           label='Regime' ),
        ]


std_pynn_simulation( test_component = test_component,
                     parameters = parameters, 
                     initial_values = initial_values, 
                     synapse_components = synapse_components, 
                     records = records
                     )



