
"""
Example of using a cell type defined in 9ML with pyNN.neuron
"""


import sys
from os.path import abspath, realpath, join
import nineml
#from nineml.nmodl.nineml2nmodl import weight_variables
root = abspath(join(realpath(nineml.__path__[0]), "../../.."))
sys.path.append(join(root, "lib9ml/python/examples/AL"))
sys.path.append(join(root, "code_generation/nmodl"))                
leaky_iaf = __import__("leaky_iaf")
coba_synapse = __import__("coba_synapse")
import pyNN.neuron as sim
from pyNN.neuron.nineml import nineml_cell_type

import pyNN.neuron.nineml as pyNN_nrn_9ml


from pyNN.utility import init_logging

from copy import deepcopy

from collections import namedtuple

init_logging(None, debug=True)
sim.setup(timestep=0.1, min_delay=0.1)



#
#class OLD_mh_build_nineml_celltype(type):
#    """
#    Metaclass for building NineMLCellType subclasses
#    """
#    def __new__(cls, name, bases, dct):
#        
#        
#        # join the neuron and synapse components into a single component
#        combined_model = dct["neuron_model"]
#        for label in dct["synapse_models"].keys():
#            port_map = dct["port_map"][label]
#            port_map = pyNN_nrn_9ml._add_prefix(dct["synapse_models"][label], label, port_map)
#            dct["weight_variables"][label] = label + "_" + dct["weight_variables"][label]
#            combined_model = pyNN_nrn_9ml.join(combined_model,
#                                  dct["synapse_models"][label],
#                                  port_map,
#                                  name=name)
#            
#        
#        dct["combined_model"] = combined_model
#        # set class attributes required for a PyNN cell type class
#        dct["default_parameters"] = dict((name, 1.0)
#                                      for name in combined_model.parameters)
#        dct["default_initial_values"] = dict((name, 0.0)
#                                          for name in combined_model.state_variables)
#        dct["synapse_types"] = dct["synapse_models"].keys() #really need an ordered dict
#        dct["injectable"] = True # need to determine this. How??
#        dct["recordable"] = [port.name for port in combined_model.analog_ports] + ['spikes', 'regime']
#        dct["standard_receptor_type"] = (dct["synapse_types"] == ('excitatory', 'inhibitory'))
#        dct["conductance_based"] = True # how to determine this??
#        dct["model_name"] = name
#        pyNN_nrn_9ml.logger.debug("Creating class '%s' with bases %s and dictionary %s" % (name, bases, dct))
#        # generate and compile NMODL code, then load the mechanism into NEUORN
#        pyNN_nrn_9ml._compile_nmodl(combined_model, dct["weight_variables"]) # weight variables should really be stored within combined_model
#        return type.__new__(cls, name, bases, dct)
#    
#    
#
#def OLDmh_nineml_cell_type(name, neuron_model, port_map={}, weight_variables={}, **synapse_models):
#    """
#    Return a new NineMLCellType subclass.
#    """
#    return _mh_build_nineml_celltype(name, (pyNN_nrn_9ml.NineMLCellType,),
#                                  {'neuron_model': neuron_model,
#                                   'synapse_models': synapse_models,
#                                   'port_map': port_map,
#                                   'weight_variables': weight_variables})
#    
#



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
        reduction_process = models.ModelToSingleComponentReducer(nineml_model)
        reduced_component = reduction_process.reducedcomponent
        
        #reduced_component.regimes
        
        
        
        models.dump_reduced(component=reduced_component, filename="testNRNCodeGen.txt")
        
        reduced_component.write("/tmp/model1.xml")
        c = nineml.abstraction_layer.parse("/tmp/model1.xml")
        
        
        #import sys
        #sys.exit(0)
        
        
        # New:
        dct["combined_model"] = reduction_process.reducedcomponent
        dct["default_parameters"] = dict( (name, 1.0) for name in reduced_component.parameters )
        dct["default_initial_values"] = dict((name, 0.0) for name in reduced_component.state_variables)
        dct["synapse_types"] = ['cobaExcit','cobaInhib']
        dct["standard_receptor_type"] = (dct["synapse_types"] == ('excitatory', 'inhibitory'))
        dct["injectable"] = True # need to determine this. How??
        dct["conductance_based"] = True # how to determine this??
        dct["model_name"] = name
        
        
        dct["recordable"] = [port.name for port in reduced_component.analog_ports] + ['spikes', 'regime']
        dct["weight_variables"] = {'cobaInhib':'cobaInhib_q', 'cobaExcit':'cobaExcit_q',}
        
        
        pyNN_nrn_9ml.logger.debug("Creating class '%s' with bases %s and dictionary %s" % (name, bases, dct))
        # generate and compile NMODL code, then load the mechanism into NEUORN
        pyNN_nrn_9ml._compile_nmodl(reduced_component, dct["weight_variables"],hierarchical_mode=True) # weight variables should really be stored within combined_model
        return type.__new__(cls, name, bases, dct)
        
        
        
#        
#        #OLD:
#        
#        # join the neuron and synapse components into a single component
#        combined_model = dct["model"]
#        for label in dct["synapse_models"].keys():
#            port_map = dct["port_map"][label]
#            port_map = pyNN_nrn_9ml._add_prefix(dct["synapse_models"][label], label, port_map)
#            dct["weight_variables"][label] = label + "_" + dct["weight_variables"][label]
#            combined_model = pyNN_nrn_9ml.join(combined_model,
#                                  dct["synapse_models"][label],
#                                  port_map,
#                                  name=name)
#            
#        
#        dct["combined_model"] = combined_model
#        # set class attributes required for a PyNN cell type class
#        dct["default_parameters"] = dict( (name, 1.0) for name in combined_model.parameters )
#        dct["default_initial_values"] = dict((name, 0.0) for name in combined_model.state_variables)
#        dct["synapse_types"] = dct["synapse_models"].keys() #really need an ordered dict
#        
#        dct["recordable"] = [port.name for port in combined_model.analog_ports] + ['spikes', 'regime']
#        
#        dct["standard_receptor_type"] = (dct["synapse_types"] == ('excitatory', 'inhibitory'))
#        
#        dct["injectable"] = True # need to determine this. How??
#        dct["conductance_based"] = True # how to determine this??
#        dct["model_name"] = name
#        
#        pyNN_nrn_9ml.logger.debug("Creating class '%s' with bases %s and dictionary %s" % (name, bases, dct))
#        # generate and compile NMODL code, then load the mechanism into NEUORN
#        pyNN_nrn_9ml._compile_nmodl(combined_model, dct["weight_variables"],hierachical_mode=True) # weight variables should really be stored within combined_model
#        return type.__new__(cls, name, bases, dct)
#    
#    
#
##def mh_nineml_cell_type(name, neuron_model, port_map={}, weight_variables={}, **synapse_models):
##    """
##    Return a new NineMLCellType subclass.
##    """
##    return _mh_build_nineml_celltype(name, (pyNN_nrn_9ml.NineMLCellType,),
##                                  {'neuron_model': neuron_model,
##                                   'synapse_models': synapse_models,
##                                   'port_map': port_map,
##                                   'weight_variables': weight_variables})





def create_celltypeclass_from_model( name, nineml_model, synapse_components ):
    
    
    dct = {'nineml_model':nineml_model, 'synapse_components':synapse_components } 
                                 
    """
    Return a new NineMLCellType subclass.
    """
    return _mh_build_nineml_celltype(name, (pyNN_nrn_9ml.NineMLCellType,), dct)
                        





class CoBaSyn(object):
    def __init__(self, namespace, weight_connector):
        self.namespace = namespace
        self.weight_connector = weight_connector




import testmodels
testModel = testmodels.get_iaf_2coba()


celltype = create_celltypeclass_from_model(
                                        name = "iaf_2coba",
                                        nineml_model = testModel,
                                        #membrane_voltage = "iaf.V",
                                        #membrane_synaptic_conductance = 'iaf.gSynapticInput',
                                        synapse_components = [
                                            CoBaSyn( namespace='cobaExcit',  weight_connector='q' ),
                                            CoBaSyn( namespace='cobaInhib',  weight_connector='q' ),
                                                   ]
                                        )



parameters = {
    'iaf.C': 1.0,
    'iaf.gL': 50.0,
    'iaf.t_ref': 5.0,
    'iaf.theta': -50.0,
    'iaf.vL': -65.0,
    'iaf.Vreset': -65.0,
    'cobaExcit.tau': 2.0,
    'cobaInhib.tau': 5.0,
    'cobaExcit.E': 0.0,
    'cobaInhib.E': -70.0,
    
    'bf':123

}



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

#parameters['cobaExcit.vrev']



newParams = {}
for k,v in parameters.iteritems():
    newParams[ k.replace(".",'_') ] = v
parameters = newParams
    

    
    
## OLD VERSION ##
######################
#celltype_cls = mh_nineml_cell_type("if_cond_exp",
#                                leaky_iaf.c1,
#                                port_map={
#                                    'excitatory': [('V', 'V'), ('Isyn', 'Isyn')],
#                                    'inhibitory': [('V', 'V'), ('Isyn', 'Isyn')]
#                                },
#                                weight_variables={
#                                    'excitatory': 'q',
#                                    'inhibitory': 'q'
#                                },
#                                excitatory=coba_synapse.c1,
#                                inhibitory=deepcopy(coba_synapse.c1),
#                                   )
#
#parameters = {
#    'C': 1.0,
#    'gL': 50.0,
#    't_ref': 5.0,
#    'excitatory_tau': 2.0,
#    'inhibitory_tau': 5.0,
#    'excitatory_E': 0.0,
#    'inhibitory_E': -70.0,
#    'theta': -50.0,
#    'vL': -65.0,
#    'V_reset': -65.0
#}



celltype_cls = celltype



cells = sim.Population(1, celltype_cls, parameters)
cells.initialize('iaf_V', parameters['iaf_vrest'])
cells.initialize('tspike', -1e99) # neuron not refractory at start
cells.initialize('regime', 1002) # temporary hack

input = sim.Population(2, sim.SpikeSourcePoisson, {'rate': 100})

connector = sim.OneToOneConnector(weights=1.0, delays=0.5)


#conn = [sim.Projection(input[0:1], cells, connector, target='cobaExcit'),
#        sim.Projection(input[1:2], cells, connector, target='cobaInhib')]


conn = [sim.Projection(input[1:2], cells, connector, target='cobaExcit'),
        sim.Projection(input[0:1], cells, connector, target='cobaInhib')]




cells._record('iaf_V')
cells._record('cobaExcit_g')
cells._record('cobaInhib_g')
cells.record()

sim.run(100.0)

cells.recorders['iaf_V'].write("Results/nineml_neuron.V", filter=[cells[0]])
cells.recorders['cobaExcit_g'].write("Results/nineml_neuron.g_exc", filter=[cells[0]])
cells.recorders['cobaInhib_g'].write("Results/nineml_neuron.g_inh", filter=[cells[0]])


t = cells.recorders['iaf_V'].get()[:,1]
v = cells.recorders['iaf_V'].get()[:,2]
gInh = cells.recorders['cobaInhib_g'].get()[:,2]
gExc = cells.recorders['cobaExcit_g'].get()[:,2]

import pylab
pylab.plot(t,v)
pylab.suptitle("From Tree-Model Pathway")
pylab.show()

sim.end()




#SynapseDetails = namedtuple( 'SynapseDetails', ['label', 'conductance_connector', 'weight_connector', 'mode'] )







