Simulating
==========


Interfacing to directly to NEURON
---------------------------------

It is possible to export NineML to .mod files, for use in NEURON simulations.
This can be done directly from the commandline, with the commandline tool::

    $nineml2nmodl.py myninemlfile.xml

If there is a single component in this file, this script will generate out a
single file ``myninemlfile.mod``, containing a mod-file which represents the
dynamics of this neuron. If there are multiple components in this XML file, then
``nineml2nmodl.py`` will produce a mod-file for each one, in the form
``myninemlfile_comp1.mod``, ``myninemlfile_comp2.mod``, etc.

This code can also be generated from python, using the method::
    
    def write_nmodldirect(component, mod_filename, weight_variables={}, hierarchical_mode=False):

where ``nineml_file`` is the filename to be written to; ``component`` is the
component we want to create the mod-file from.




Interfacing to PyNN (NEST & NEURON)
-----------------------------------

To use a component with pyNN; we construct the component as we have been doing
previously; then we construct a class that pyNN can use for simulation. If we
are using the neuron-backend, this internally takes care of creating and
compiling the relevant mod-file for the simulation, and if we are using the NEST
back-end, it will automatically create the relevant module. 

.. note::
    
    See example XX for more about how to use NineML with NEURON.

The interface to NineML is consistent accross back-ends; using with pyNN/Neuron
or pyNN/NEST, the code will look like this ::
    
    
    #Either:
    import pyNN.nest as sim
    import pyNN.nest.nineml as pyNNml
        
    #Or:
    import pyNN.neuron as sim
    import pyNN.neuron.nineml as pyNNml


    # ...
    # PyNN Initialisation:
    # [omitted] 
    # ...

    # Create a component; it can be hierachical.
    test_component = get_hierachical_iaf_3coba()

    celltype_cls = pyNNml.nineml_celltype_from_model(
                                            name = "iaf_3coba",
                                            nineml_model = test_component,
                                            synapse_components = [
                                                pyNNml.CoBaSyn( namespace='AMPA',  weight_connector='q' ),
                                                pyNNml.CoBaSyn( namespace='GABAa',  weight_connector='q' ),
                                                pyNNml.CoBaSyn( namespace='GABAb',  weight_connector='q' ),
                                                       ]
                                            )

    parameters = ComponentFlattener.flatten_namespace_dict(
    {
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

    })


    parameters = ( parameters )


    cells = sim.Population(1, celltype_cls, parameters)

    # Finish building simulation.....







