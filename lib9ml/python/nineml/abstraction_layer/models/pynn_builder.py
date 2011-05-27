
"""
Example of using a cell type defined in 9ML with pyNN.neuron
"""


import sys
from os.path import abspath, realpath, join

#root = abspath(join(realpath(nineml.__path__[0]), "../../.."))

from pyNN.neuron.nineml import nineml_cell_type
import pyNN.neuron.nineml as pyNN_nrn_9ml




class _mh_build_nineml_celltype(type):
    """
    Metaclass for building NineMLCellType subclasses
    """
    def __new__(cls, name, bases, dct):
        
        
        #Extract Parameters Back out from Dict:
        nineml_model = dct['nineml_model']
        synapse_components = dct['synapse_components']

        # Reduce the model:                    
        from nineml.abstraction_layer import models               
        reduction_process = models.ModelToSingleComponentReducer(nineml_model, componentname=name)
        reduced_component = reduction_process.reducedcomponent
        
        
        
        # New:
        dct["combined_model"] = reduction_process.reducedcomponent
        dct["default_parameters"] = dict( (name, 1.0) for name in reduced_component.parameters )
        dct["default_initial_values"] = dict((name, 0.0) for name in reduced_component.state_variables)
        dct["synapse_types"] = [syn.namespace for syn in synapse_components] 
        dct["standard_receptor_type"] = (dct["synapse_types"] == ('excitatory', 'inhibitory'))
        dct["injectable"] = True # need to determine this. How??
        dct["conductance_based"] = True # how to determine this??
        dct["model_name"] = name
        
        
        dct["recordable"] = [port.name for port in reduced_component.analog_ports] + ['spikes', 'regime']
        dct["weight_variables"] = dict([ (syn.namespace,syn.namespace+'_'+syn.weight_connector ) for syn in synapse_components ]) #{'cobaInhib':'cobaInhib_q', 'cobaExcit':'cobaExcit_q',}
        
        
        pyNN_nrn_9ml.logger.debug("Creating class '%s' with bases %s and dictionary %s" % (name, bases, dct))
        # generate and compile NMODL code, then load the mechanism into NEUORN
        pyNN_nrn_9ml._compile_nmodl(reduced_component, dct["weight_variables"],hierarchical_mode=True) # weight variables should really be stored within combined_model
        return type.__new__(cls, name, bases, dct)
        
        





def create_celltypeclass_from_model( name, nineml_model, synapse_components ):
    dct = {'nineml_model':nineml_model, 'synapse_components':synapse_components } 
    return _mh_build_nineml_celltype(name, (pyNN_nrn_9ml.NineMLCellType,), dct)
                        





class CoBaSyn(object):
    def __init__(self, namespace, weight_connector):
        self.namespace = namespace
        self.weight_connector = weight_connector

