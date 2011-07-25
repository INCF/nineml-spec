
"""
Example of using a cell type defined in 9ML with pyNN.neuron
"""




def run(plot_and_show=True):
    import sys
    from os.path import abspath, realpath, join
    import numpy
    import nineml

    root = abspath(join(realpath(nineml.__path__[0]), "../../.."))
    sys.path.append(join(root, "lib9ml/python/examples/AL"))
    sys.path.append(join(root, "code_generation/nmodl"))     
    sys.path.append(join(root, "code_generation/nest2"))       
               

    #from nineml.abstraction_layer.example_models import  get_hierachical_iaf_3coba
    from nineml.abstraction_layer.testing_utils import TestableComponent
    from nineml.abstraction_layer.flattening import  ComponentFlattener

    import pyNN.neuron as sim
    import pyNN.neuron.nineml as pyNNml

    from pyNN.utility import init_logging


    init_logging(None, debug=True)
    sim.setup(timestep=0.1, min_delay=0.1)


    #test_component = get_hierachical_iaf_3coba()
    test_component = TestableComponent('hierachical_iaf_3coba')()

    from nineml.abstraction_layer.writers import DotWriter
    DotWriter.write(test_component, 'test1.dot')
    

    from nineml.abstraction_layer.writers import XMLWriter
    XMLWriter.write(test_component, 'iaf_3coba.xml')


    celltype_cls = pyNNml.nineml_celltype_from_model(
                                            name = "iaf_3coba",
                                            nineml_model = test_component,
                                            synapse_components = [
                                                pyNNml.CoBaSyn( namespace='AMPA',  weight_connector='q' ),
                                                pyNNml.CoBaSyn( namespace='GABAa',  weight_connector='q' ),
                                                pyNNml.CoBaSyn( namespace='GABAb',  weight_connector='q' ),
                                                       ]
                                            )

    parameters = {
        'iaf.cm': 1.0,
        'iaf.gl': 50.0,
        'iaf.taurefrac': 5.0,
        'iaf.vrest': -65.0,
        'iaf.vreset': -65.0,
        'iaf.vthresh': -50.0,
        'AMPA.tau': 2.0,
        'GABAa.tau': 5.0,
        'GABAb.tau': 50.0,
        'AMPA.vrev': 0.0,
        'GABAa.vrev': -70.0,
        'GABAb.vrev': -95.0,

    }


    parameters = ComponentFlattener.flatten_namespace_dict( parameters )


    cells = sim.Population(1, celltype_cls, parameters)
    cells.initialize('iaf_V', parameters['iaf_vrest'])
    cells.initialize('tspike', -1e99) # neuron not refractory at start
    cells.initialize('regime', 1002) # temporary hack

    input = sim.Population(3, sim.SpikeSourceArray)

    numpy.random.seed(12345)
    input[0].spike_times = numpy.add.accumulate(numpy.random.exponential(1000.0/100.0, size=1000))
    input[1].spike_times = numpy.add.accumulate(numpy.random.exponential(1000.0/20.0, size=1000))
    input[2].spike_times = numpy.add.accumulate(numpy.random.exponential(1000.0/50.0, size=1000))

    connector = sim.OneToOneConnector(weights=1.0, delays=0.5)


    conn = [sim.Projection(input[0:1], cells, connector, target='AMPA'),
            sim.Projection(input[1:2], cells, connector, target='GABAa'),
            sim.Projection(input[2:3], cells, connector, target='GABAb')]


    cells._record('iaf_V')
    cells._record('AMPA_g')
    cells._record('GABAa_g')
    cells._record('GABAb_g')
    cells.record()

    sim.run(100.0)

    cells.recorders['iaf_V'].write("Results/nineml_neuron.V", filter=[cells[0]])
    cells.recorders['AMPA_g'].write("Results/nineml_neuron.g_exc", filter=[cells[0]])
    cells.recorders['GABAa_g'].write("Results/nineml_neuron.g_gabaA", filter=[cells[0]])
    cells.recorders['GABAb_g'].write("Results/nineml_neuron.g_gagaB", filter=[cells[0]])


    t = cells.recorders['iaf_V'].get()[:,1]
    v = cells.recorders['iaf_V'].get()[:,2]
    gInhA = cells.recorders['GABAa_g'].get()[:,2]
    gInhB = cells.recorders['GABAb_g'].get()[:,2]
    gExc = cells.recorders['AMPA_g'].get()[:,2]

    if plot_and_show:
        import pylab
        pylab.subplot(211)
        pylab.plot(t,v)
        pylab.ylabel('voltage [mV]')
        pylab.suptitle("AMPA, GABA_A, GABA_B")
        pylab.subplot(212)
        pylab.plot(t,gInhA,label='GABA_A')
        pylab.plot(t,gInhB, label='GABA_B')
        pylab.plot(t,gExc, label='AMPA')
        pylab.ylabel('conductance [nS]')
        pylab.xlabel('t [ms]')
        pylab.legend()

        pylab.show()

    sim.end()



if __name__ == '__main__':
    run(plot_and_show = True)
else:
    run(plot_and_show = False)
