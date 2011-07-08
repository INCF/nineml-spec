

NineML Python API
=================

.. currentmodule:: nineml.abstraction_layer


Overview
---------

NineML is an API designed for specifying the dynamics and connectivity of neural
simulations; in particular for large-scale simulations of many point neurons.

Typically, point-neurons are currently simulated by writing a either a custom
simulation in a general purpose programming language, (such as Python, Matlab) 
or by writing a model for a particular simulator (NEURON/NEST/BRIAN). As models
of neuronal dynamics and connectivity become more and more complex, writing a
simulations from scratch in python or Matlab can become more and more complex; taking
time to debug and producing hard to find bugs. On the other-hand, writing simulator
specific models can reduce some of this duplication, but means the model will
only run on a single simulator.


NineML tries to mitigate some of these problems by providing an language for
defining smaller components of a simulation in a language-independent way, and
provides pathways for generating code for various simulators from this
description.



*Abstraction* and *User* Layers
--------------------------------

In NineML, the definition of a component is split into two parts; 

Abstraction Layer
    Components on this layer can be thought of as parameterised models. For
    example, we could specify a general integrate-and-fire neuron, with a firing
    threshold, ``V_Threshold`` and a reset voltage ``V_Reset``. We are able to
    define the dynamics of the neuron in terms of these parameters.

User Layer
    In order to simulate a network, we need to take the ``parameterised`` models from
    the ``Abstraction Layer``, fill in the parameters, and specify the
    connectivity between the components. For example, we might specify for our
    neurons that ``V_Threshold`` was -45mV and ``V_Reset`` was -60mV.


The flow for a simulation using NineML would look like:

.. image::
    _static/images/build/AL_UL_Overview.png



An obvious question is `Why do this?!`

For a single, relatively simple simulation, it may not be worth the effort!
But imagine we are modelling a (relatively simple) network of neurons, which 
contains 5 different types of neurons. The neurons synapse onto each other,
and there are 3 different classes of synapses, with different models for 
their dynamics. If we were to implement this naively, we could potentially
copy and paste the same code 15 times, *for each simulator*. By factoring out
basic functionality, we make our workflow much more manageable.

There is more about the NineML design and specification :doc:`here</src_rst/overview/nineml_overview>` 





NineML Abstraction Layer & XML
-------------------------------

:doc:`NineML Implementation and XML</nineml_al_implementation_and_xml>` 

.. toctree::
    :maxdepth: 2 
    
    nineml_al_implementation_and_xml
    xml_spec


Python NineML
--------------

Python-NineML is the python implementation of the NineML specification.

.. todo::
    
    Status of the python NineML Libraries


.. toctree::
    :maxdepth: 2

    python_nineml
    src_rst/developers/developer


* To start using Python NineML, we recommend you start with :doc:`The Python-NineML User Documentation</python_nineml>` 

* For Python NineML developers, see the :doc:`Python-NineML Developer Documentation</src_rst/developers/developer>` 
