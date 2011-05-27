
"""
Example of using a cell type defined in 9ML with pyNN.neuron
"""




import sys
from os.path import abspath, realpath, join
import nineml

root = abspath(join(realpath(nineml.__path__[0]), "../../.."))
sys.path.append(join(root, "lib9ml/python/examples/AL"))
sys.path.append(join(root, "code_generation/nmodl"))     
           

from nineml.abstraction_layer.example_models import  get_hierachical_iaf_nmda
from nineml.abstraction_layer.models import ModelToSingleComponentReducer

import pyNN.neuron as sim
import pyNN.neuron.nineml as pyNNml

from pyNN.utility import init_logging


init_logging(None, debug=True)
sim.setup(timestep=0.1, min_delay=0.1)


testModel = get_hierachical_iaf_nmda()


celltype_cls = pyNNml.nineml_celltype_from_model(
                                        name = "iaf_nmda",
                                        nineml_model = testModel,
                                        synapse_components = [
                                            pyNNml.CoBaSyn( namespace='nmda',  weight_connector='weight' ),
                                                   ]
                                        )

parameters = {
    'iaf.cm': 1.0,
    'iaf.gl': 50.0,
    'iaf.taurefrac': 5.0,
    'iaf.vrest': -65.0,
    'iaf.vreset': -65.0,
    'iaf.vthresh': -50.0,
    # NMDA parameters from Gertner's book, pg 53.
    'nmda.tau_r': 3.0, # ms
    'nmda.tau_d': 40.0, # ms
    'nmda.gmax': 1.2, #nS
    'nmda.E': 0.0,
    'nmda.gamma': 0.062, #1/mV
    'nmda.mg_conc': 1.2, # mM
    'nmda.beta': 3.57 #mM
}


parameters = ModelToSingleComponentReducer.flatten_namespace_dict( parameters )


cells = sim.Population(1, celltype_cls, parameters)
cells.initialize('iaf_V', parameters['iaf_vrest'])
cells.initialize('tspike', -1e99) # neuron not refractory at start
cells.initialize('regime', 1002) # temporary hack

input = sim.Population(1, sim.SpikeSourcePoisson, {'rate': 100})

connector = sim.OneToOneConnector(weights=1.0, delays=0.5)


conn = [sim.Projection(input[0:1], cells, connector, target='nmda')]


cells._record('iaf_V')
cells._record('nmda_gSyn')
cells.record()

sim.run(100.0)

cells.recorders['iaf_V'].write("Results/nineml_neuron.V", filter=[cells[0]])
cells.recorders['nmda_gSyn'].write("Results/nineml_neuron.g_nmda", filter=[cells[0]])


t = cells.recorders['iaf_V'].get()[:,1]
v = cells.recorders['iaf_V'].get()[:,2]
gNMDA = cells.recorders['nmda_gSyn'].get()[:,2]

import pylab
pylab.plot(t,v)
pylab.suptitle("From Tree-Model Pathway")
pylab.show()

sim.end()

