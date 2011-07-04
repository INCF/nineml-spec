.. NineML AbstractionLayer API documentation master file, created by
   sphinx-quickstart on Wed Jun 15 16:54:44 2011.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.



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
simulations from scratch in python or matlab can become more and more complex; taking
time to debug and producing hard to find bugs. On the otherhand, writing simulator
specific models can reduce some of this duplication, but means the model will
only run on a single simulator.

Some other common problems in computational neuroscience:
* How do I share my model with others?
* How do I change parameters in my model, without copying-&-pasting lots of code?


NineML tries to mitigate some of these problems by providing 


Some of the problems facing Computational Modellers at the moment are:

* How do I share my model with others?
* 


* There are already many languages (python, Matlab, Neuron, NEST) we can use for simulating point-neurons, so why
  another?

*



NineML aims to  






Extensibility and Combinmetrics.


* What is NineML

* What do the python bindings allow you to do


What sort of things can I model with NineML
--------------------------------------------


Installation
------------


.. toctree::
   :maxdepth: 3

   src_rst/tutorials/installation/installation
   


Abstraction Layer
------------------


.. toctree::
   :maxdepth: 3

   src_rst/tutorials/al/al


User Layer
----------





Examples
--------

.. toctree::
   :maxdepth: 3
   
   src_rst/examples/examples_al_python




Futher Information
------------------


More information for :doc:`developers<developer>` 


