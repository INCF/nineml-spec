
"""
Example of using a cell type defined in 9ML with pyNN.neuron
"""

from nineml.abstraction_layer.example_models import  get_hierachical_iaf_nmda
from nineml.abstraction_layer.testing_utils import std_pynn_simulation, RecordValue 

test_component = get_hierachical_iaf_nmda()

parameters = {
    'iaf.cm': 1.0,
    'iaf.gl': 50.0,
    'iaf.taurefrac': 5.0,
    'iaf.vrest': -65.0,
    'iaf.vreset': -65.0,
    'iaf.vthresh': -50.0,
    # NMDA parameters from Gertner's book, pg 53.
    'nmda.taur': 3.0, # ms
    'nmda.taud': 40.0, # ms
    'nmda.gmax': 1.2, #nS
    'nmda.E': 0.0,
    'nmda.gamma': 0.062, #1/mV
    'nmda.mgconc': 1.2, # mM
    'nmda.beta': 3.57 #mM
}

initial_values = {
    'iaf_V': parameters['iaf.vrest'],
    'tspike': -1e99,
    'regime': 1002,
        }

synapse_components = [
    ( 'nmda',  'weight' ),
    ( 'cobaExcit', 'q' ),
    ]



records = [
    RecordValue( what='iaf_V',       tag='Voltage [mV]',     label='Membrane Voltage' ),
    RecordValue( what='nmda_g',      tag='Conductance [ns]', label='NMDA-g' ),
    RecordValue( what='cobaExcit_g', tag='Conductance [ns]', label='cobaexcit-g' ),
    RecordValue( what='regime',      tag='Regime',           label='Regime' ),
        ]


std_pynn_simulation( test_component = test_component,
                     parameters = parameters, 
                     initial_values = initial_values, 
                     synapse_components = synapse_components, 
                     records = records
                     )

           



