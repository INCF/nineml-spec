

import sys
from os.path import abspath, realpath, join


def std_pynn_simulation( test_component, parameters, initial_values,
        synapse_components, records, plot=True,sim_time=100.,synapse_weights=1.0, syn_input_rate=100):

    import nineml
    nineml.utility.LocationMgr.StdAppendToPath()

    from nineml.abstraction_layer.flattening import ComponentFlattener

    import pyNN.neuron as sim
    import pyNN.neuron.nineml as pyNNml
    from pyNN.neuron.nineml import CoBaSyn

    from pyNN.utility import init_logging

    init_logging(None, debug=True)
    sim.setup(timestep=0.01, min_delay=0.1)


    synapse_components_ML = [ CoBaSyn( namespace=ns,  weight_connector=wc ) for (ns,wc) in synapse_components ]


    celltype_cls = pyNNml.nineml_celltype_from_model(
                                            name = test_component.name,
                                            nineml_model = test_component,
                                            synapse_components = synapse_components_ML,
                                            )



    parameters = ComponentFlattener.flatten_namespace_dict( parameters )
    initial_values = ComponentFlattener.flatten_namespace_dict( initial_values )


    cells = sim.Population(1, celltype_cls, parameters)

    # Set Initial Values:
    for state, state_initial_value in initial_values.iteritems():
        cells.initialize( state, state_initial_value )


    # For each synapse type, create a spike source:
    if synapse_components:
        input = sim.Population(len(synapse_components), sim.SpikeSourcePoisson, {'rate': syn_input_rate})
        connector = sim.OneToOneConnector(weights=synapse_weights, delays=0.5)

        conn = []
        for i,(ns, weight_connector) in enumerate(synapse_components):
            proj = sim.Projection(input[i:i+1], cells, connector, target=ns),
            conn.append( proj )


    # Setup the Records:
    for record in records:
        cells._record(record.what)

    cells.record()

    #Run the simulation:
    sim.run(sim_time)


    if len(records) == 0:
        assert False


    # Write the Results to a file:
    for record in records:
        cells.recorders[record.what].write("Results/nineml_%s"%record.what, filter=[cells[0]])

    # Plot the values:

    t = cells.recorders[ records[0].what ].get()[:,1]

    result_traces = {}
    for record in records:
        result_traces[ record.what ] = cells.recorders[ record.what ].get()[:,2]

    # Create a list of the tags:
    tags = []
    for record in records:
        if not record.tag in tags:
            tags.append( record.tag )
 
    
    # Plot the graphs:
    if plot:
        import pylab
        nGraphs = len(tags) 

        # Plot the Records:
        for graphIndex, tag in enumerate(tags):
            pylab.subplot(nGraphs,1, graphIndex+1)
            
            for r in records:
                if r.tag != tag:
                    continue
                pylab.plot(t, result_traces[r.what], label=r.label)
            
            pylab.ylabel(tag)
            pylab.legend()

        #Plot the spikes:
        #pylab.subplot(nGraphs,1, len(tags)+1)
        #t_spikes = cells[0:1].getSpikes()[:1]
        #pylab.plot( [1,3],[1,3],'x'  )
        #print t_spikes
        #if t_spikes:
        #    pylab.scatter( t_spikes, t_spikes )
        


        # Add the X axis to the last plot:
        pylab.xlabel('t [ms]')

        #pylab.suptitle("From Tree-Model Pathway")
        pylab.show()

    sim.end()


    return t, result_traces
