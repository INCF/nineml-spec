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



Some common problems for modellers are:

 * How do I share my model with others?
 * How do I change parameters in my model, without copying-&-pasting lots of code?
 * How do I reuse parts of my model, without copying-&-pasting lots of code?


NineML tries to mitigate some of these problems by providing an language for
defining smaller components of a simulation in a language-independent way, and
provides pathways for generating code for various simulators from this
description.



The 2 *Layers*
----------------

In NineML, the definition of a component is split into two parts; 

Abstraction Layer
    Components on this layer can be thought of as parameterised models. For
    example, we could specify a general integrate-and-fire neuron, with a firing
    threshold, ``V_Threshold`` and a reset voltage ``V_Reset``. We are able to
    define the dynamics of the neuron in terms of these parameters.

User Layer
    In order to simulate a network, we need to take the ``parameterised`` models from
    the ``Abstraction Layer``, fill in the parameters, and specify the
    connectivity between the components. 


The flow for a simulation using NineML would look like:

.. image::
    _static/images/build/AL_UL_Overview.png



An obvious question is `Why do we do this?!` At first glance it looks like we
are making our simulation much more complex - For a single, relatively simple simulation, it hardly
seems worth the effort!



But imagine we are modelling a (relatively simple) network of neurons, which 
contains 5 different types of neurons. The neurons synapse onto each other,
and there are 3 different classes of synapses, with different models for 
thier dynamics. If we were to implement this naively, we could potentially
copy and paste tge 





Reducing Duplication: Changing Parameters
    We would like to know the effects of adjusting the 'c' parameter on the
    dynamics of a neuron for example. By defining separating the parameters from the
    component dynamics definition, we do not need to respecify the component
    dynamics for each simulation, just the values of parameters on the
    user layer. 


More Complex Neuron Definitions: Regimes and Transitions
    The dynamics of the neurons in this simulation are relatively simple.
    Imagining that we now have a neuron which can be in two ``regimes``; 
    a *regular* regime and a *refractory* regime, which it enters for a time
    after a spike. 

Composing Components: 
    Imagining our network model contains 

    
    












Workflow Example
----------------


To make this a bit more concrete, we use an example from, Eugene Izhikevich, in
which 1000 point neurons are connected together and simulated. 
The simulation is given in Matlab:

http://www.izhikevich.org/publications/net.m


.. code-block:: matlab


    % Created by Eugene M. Izhikevich, February 25, 2003
    % Excitatory neurons    Inhibitory neurons
    Ne=800;                 Ni=200;
    re=rand(Ne,1);          ri=rand(Ni,1);
    a=[0.02*ones(Ne,1);     0.02+0.08*ri];
    b=[0.2*ones(Ne,1);      0.25-0.05*ri];
    c=[-65+15*re.^2;        -65*ones(Ni,1)];
    d=[8-6*re.^2;           2*ones(Ni,1)];
    S=[0.5*rand(Ne+Ni,Ne),  -rand(Ne+Ni,Ni)];

    v=-65*ones(Ne+Ni,1);    % Initial values of v
    u=b.*v;                 % Initial values of u
    firings=[];             % spike timings

    for t=1:1000            % simulation of 1000 ms
      I=[5*randn(Ne,1);2*randn(Ni,1)]; % thalamic input
      fired=find(v>=30);    % indices of spikes
      firings=[firings; t+0*fired,fired];
      v(fired)=c(fired);
      u(fired)=u(fired)+d(fired);
      I=I+sum(S(:,fired),2);
      v=v+0.5*(0.04*v.^2+5*v+140-u+I); % step 0.5 ms
      v=v+0.5*(0.04*v.^2+5*v+140-u+I); % for numerical
      u=u+a.*(b.*v-u);                 % stability
    end;
    plot(firings(:,1),firings(:,2),'.');



Using NineML, we would first define a parameterised Izhikevich neuron component in the
`Abstraction Layer` as:

.. code-block:: python 


    comp= ComponentClass(   name = "Izhikevich", 
                            regimes = [
                                Regime(
                                    "dV/dt = 0.04*V*V + 5*V + 140.0 - U + Isyn",
                                    "dU/dt = a*(b*V - U)",

                                    transitions = On("V > theta",
                                                     do =["V = c", 
                                                          "U = U + d", 
                                                          OutputEvent('spike') ] )       
                                    )],
                            analog_ports = [ 
                                SendPort("V"),
                                ReducePort("Isyn",reduce_op="+") ],

                            parameters = ['a','b','c','d','theta']
                                )

Now; we are able to 

[In this case we have used the NineML-Python API, but we could have equivalent
used the Scheme or XML specifications]










NineML aims to  






Extensibility and Combinmetrics.


* What is NineML

* What do the python bindings allow you to do


What sort of things can I model with NineML
--------------------------------------------



What Other Tools/Languages Should I consider?
----------------------------------------------



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


