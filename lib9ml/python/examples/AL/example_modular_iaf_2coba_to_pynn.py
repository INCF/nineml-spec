
"""
Example of using a cell type defined in 9ML with pyNN.neuron
"""




import sys
from os.path import abspath, realpath, join
import nineml

root = abspath(join(realpath(nineml.__path__[0]), "../../.."))
sys.path.append(join(root, "lib9ml/python/examples/AL"))
sys.path.append(join(root, "code_generation/nmodl"))     
           

from nineml.abstraction_layer.example_models import  get_hierachical_iaf_2coba, get_coba
from nineml.abstraction_layer.flattening import ModelToSingleComponentReducer

import pyNN.neuron as sim
import pyNN.neuron.nineml as pyNNml

from pyNN.utility import init_logging


init_logging(None, debug=True)
sim.setup(timestep=0.1, min_delay=0.1)




from nineml.abstraction_layer.componentmodifiers import ModelModifier
from nineml.abstraction_layer.flattening import reduce_to_single_component
from nineml.abstraction_layer.writers import dump_reduced
from nineml.abstraction_layer.component_checker import ComponentTypeChecker, ComponentPortChecker



testModel = get_hierachical_iaf_2coba()


flat = reduce_to_single_component( testModel, componentname = 'iaf_2coba' )

leave_open = ['V','t']
for arp in flat.query.analog_reduce_ports:
    if arp.name in leave_open: continue
    print 'Closing ARP Port:', arp
    ModelModifier.CloseAnalogPort(component=flat, port_name=arp.name, value='0' )
    
ComponentTypeChecker().Visit( flat )
ComponentPortChecker().Visit( flat )

flat.backsub_aliases()
flat.backsub_equations()


celltype_cls = pyNNml.nineml_celltype_from_model(
                                        name = "iaf_2coba",
                                        nineml_model = flat,
                                        synapse_components = [
                                            pyNNml.CoBaSyn( namespace='cobaExcit',  weight_connector='q' ),
                                            pyNNml.CoBaSyn( namespace='cobaInhib',  weight_connector='q' ),
                                                   ]
                                        )

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


parameters = ModelToSingleComponentReducer.flatten_namespace_dict( parameters )


cells = sim.Population(1, celltype_cls, parameters)
cells.initialize('iaf_V', parameters['iaf_vrest'])
cells.initialize('tspike', -1e99) # neuron not refractory at start
cells.initialize('regime', 1002) # temporary hack

input = sim.Population(2, sim.SpikeSourcePoisson, {'rate': 100})

#connector = sim.OneToOneConnector(weights=1.0, delays=0.5)
connector = sim.OneToOneConnector(weights=20.0, delays=0.5)


conn = [sim.Projection(input[0:1], cells, connector, target='cobaExcit'),
        sim.Projection(input[1:2], cells, connector, target='cobaInhib')]


cells._record('iaf_V')
cells._record('cobaExcit_g')
cells._record('cobaInhib_g')
cells._record('regime')
cells.record()

sim.run(100.0)

cells.recorders['iaf_V'].write("Results/nineml_neuron.V", filter=[cells[0]])
cells.recorders['regime'].write("Results/nineml_neuron.regime", filter=[cells[0]])
cells.recorders['cobaExcit_g'].write("Results/nineml_neuron.g_exc", filter=[cells[0]])
cells.recorders['cobaInhib_g'].write("Results/nineml_neuron.g_inh", filter=[cells[0]])


t = cells.recorders['iaf_V'].get()[:,1]
v = cells.recorders['iaf_V'].get()[:,2]
regime = cells.recorders['regime'].get()[:,2]
gInh = cells.recorders['cobaInhib_g'].get()[:,2]
gExc = cells.recorders['cobaExcit_g'].get()[:,2]

import pylab
pylab.subplot(311)
pylab.plot(t,v)
pylab.subplot(312)
pylab.plot(t,gInh)
pylab.plot(t,gExc)
pylab.subplot(313)
pylab.plot(t,regime)
pylab.ylim( (999,1005) )
pylab.suptitle("From Tree-Model Pathway")
pylab.show()

sim.end()

