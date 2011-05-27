"""
Example of using a cell type defined in 9ML with pyNN.neuron
"""


class HOCIntercept(object):
    def __init__(self, h):
        self.__dict__['h'] = h
        
    def __getattr__(self, name):
        print "HOC:__getattr__: ", name
        return getattr( self.__dict__['h'], name )
    def __setattr__(self, name, value):
        print "HOC:__setattr__: ", name
        return setattr( self.__dict__['h'], name, value )
    
    def __call__(self, *args,**kwargs):
        print "HOC:__Call__: ", args, kwargs
        return self.__dict__['h'](*args,**kwargs)
    
    

import sys
from os.path import abspath, realpath, join
import nineml
#from nineml.nmodl.nineml2nmodl import weight_variables
root = abspath(join(realpath(nineml.__path__[0]), "../../.."))
sys.path.append(join(root, "lib9ml/python/examples/AL"))
sys.path.append(join(root, "code_generation/nmodl"))                
leaky_iaf = __import__("leaky_iaf")
coba_synapse = __import__("coba_synapse")

import neuron

hIntercept = HOCIntercept(neuron.h)
neuron.h = hIntercept


#hIntercept = HOCIntercept(sim.simulator.h)
#sim.simulator.h = hIntercept



import pyNN.neuron as sim


    








from pyNN.neuron.nineml import nineml_cell_type

import pyNN.neuron.nineml as pyNN_nrn_9ml


from pyNN.utility import init_logging

from copy import deepcopy

from collections import namedtuple

init_logging(None, debug=True)
sim.setup(timestep=0.1, min_delay=0.1)




























class _mh_build_nineml_celltype(type):
    """
    Metaclass for building NineMLCellType subclasses
    """
    def __new__(cls, name, bases, dct):
        print "In metaclass constructor"
        print ""
        
        
        # join the neuron and synapse components into a single component
        combined_model = dct["neuron_model"]
        for label in dct["synapse_models"].keys():
            port_map = dct["port_map"][label]
            port_map = pyNN_nrn_9ml._add_prefix(dct["synapse_models"][label], label, port_map)
            dct["weight_variables"][label] = label + "_" + dct["weight_variables"][label]
            combined_model = pyNN_nrn_9ml.join(combined_model,
                                  dct["synapse_models"][label],
                                  port_map,
                                  name=name)
            
        
        dct["combined_model"] = combined_model
        # set class attributes required for a PyNN cell type class
        dct["default_parameters"] = dict((name, 1.0)
                                      for name in combined_model.parameters)
        dct["default_initial_values"] = dict((name, 0.0)
                                          for name in combined_model.state_variables)
        dct["synapse_types"] = dct["synapse_models"].keys() #really need an ordered dict
        dct["injectable"] = True # need to determine this. How??
        dct["recordable"] = [port.name for port in combined_model.analog_ports] + ['spikes', 'regime']
        dct["standard_receptor_type"] = (dct["synapse_types"] == ('excitatory', 'inhibitory'))
        dct["conductance_based"] = True # how to determine this??
        dct["model_name"] = name
        pyNN_nrn_9ml.logger.debug("Creating class '%s' with bases %s and dictionary %s" % (name, bases, dct))
        # generate and compile NMODL code, then load the mechanism into NEUORN
        pyNN_nrn_9ml._compile_nmodl(combined_model, dct["weight_variables"]) # weight variables should really be stored within combined_model
        return type.__new__(cls, name, bases, dct)
    
    

def mh_nineml_cell_type(name, neuron_model, port_map={}, weight_variables={}, **synapse_models):
    """
    Return a new NineMLCellType subclass.
    """
    return _mh_build_nineml_celltype(name, (pyNN_nrn_9ml.NineMLCellType,),
                                  {'neuron_model': neuron_model,
                                   'synapse_models': synapse_models,
                                   'port_map': port_map,
                                   'weight_variables': weight_variables})
    
    
    
celltype_cls = mh_nineml_cell_type("if_cond_exp",
                                leaky_iaf.c1,
                                port_map={
                                    'excitatory': [('V', 'V'), ('Isyn', 'Isyn')],
                                    'inhibitory': [('V', 'V'), ('Isyn', 'Isyn')]
                                },
                                weight_variables={
                                    'excitatory': 'q',
                                    'inhibitory': 'q'
                                },
                                excitatory=coba_synapse.c1,
                                inhibitory=deepcopy(coba_synapse.c1),
                                   )

parameters = {
    'C': 1.0,
    'gL': 50.0,
    't_ref': 5.0,
    'excitatory_tau': 2.0,
    'inhibitory_tau': 5.0,
    'excitatory_E': 0.0,
    'inhibitory_E': -70.0,
    'theta': -50.0,
    'vL': -65.0,
    'V_reset': -65.0
}


#print "Class Type:", celltype_cls, type( celltype_cls )
#import sys
#sys.exit(0)

#c = celltype_cls()

    

cells = sim.Population(1, celltype_cls, parameters)
cells.initialize('V', parameters['vL'])
cells.initialize('t_spike', -1e99) # neuron not refractory at start
cells.initialize('regime', 1002) # temporary hack

input = sim.Population(2, sim.SpikeSourcePoisson, {'rate': 100})

connector = sim.OneToOneConnector(weights=1.0, delays=0.5)

print "========BUILDING CONNECTIONS=========="
conn = [sim.Projection(input[0:1], cells, connector, target='excitatory'),
        sim.Projection(input[1:2], cells, connector, target='inhibitory')]
print "========DONE BUILDING CONNECTIONS====="


cells._record('V')
cells._record('excitatory_g')
cells._record('inhibitory_g')
cells.record()

print "========run()================"
sim.run(100.0)
print "========run() done =========="


cells.recorders['V'].write("Results/nineml_neuron.V", filter=[cells[0]])
cells.recorders['excitatory_g'].write("Results/nineml_neuron.g_exc", filter=[cells[0]])
cells.recorders['inhibitory_g'].write("Results/nineml_neuron.g_inh", filter=[cells[0]])


t = cells.recorders['V'].get()[:,1]
v = cells.recorders['V'].get()[:,2]
gInh = cells.recorders['inhibitory_g'].get()[:,2]
gExc = cells.recorders['excitatory_g'].get()[:,2]

import pylab
pylab.suptitle("From Original Pathway")
pylab.plot(t,v)
pylab.show()

sim.end()
