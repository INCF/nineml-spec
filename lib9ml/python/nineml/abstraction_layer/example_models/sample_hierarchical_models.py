

from nineml.abstraction_layer.testing_utils import TestableComponent


def get_iaf():
    return TestableComponent('iaf')()

def get_coba():
    return TestableComponent('coba_synapse')()

def nmda():
    return TestableComponent('nmda')()

def get_hierachical_iaf_2coba():
    return TestableComponent('hierachical_iaf_2coba')()

def get_hierachical_iaf_3coba():
    return TestableComponent('hierachical_iaf_3coba')()

def get_hierachical_iaf_nmda():
    return TestableComponent('hierachical_iaf_nmda')()
    
