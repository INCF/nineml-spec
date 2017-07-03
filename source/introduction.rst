Introduction
============

The increasing diversity of neuronal network models and the
software/hardware platforms used to simulate them presents a significant
challenge for sharing, replicability and reusability of models in
computational neuroscience. To address this problem, we propose a common
description language, *Network Interchange for Neuroscience Modeling
Language (NineML)*, to facilitate the exchange neuronal network models
between researchers and simulator platforms.

NineML is based on a common object model describing the different
elements of neuronal network models. It was initiated and supported by
the `International Neuroinformatics Coordinating Facility (INCF)
(http://www.incf.org) <http://www.incf.org>`__ as part of the
`Multiscale Modeling
Program <https://www.incf.org/activities/our-programs/modeling/people>`__,
and has benefitted from wide-ranging input from computational
neuroscientists, simulator developers and developers of
simulator-independent languages (e.g. NeuroML, PyNN) (see
[sec:task\_force])
[Goddard2001]_, [Gleeson2010]_, [Davison2008]_.

Scope
-----

The purpose of NineML is to provide a computer language for succinct and
unambiguous description of computational neuroscience models of neuronal
networks. NineML is intended to describe the network architecture,
parameters and equations that govern the dynamics of a neuronal network,
without taking into account model implementation details such as
numerical integration methods.

The following neuronal network objects can be described in NineML,

1. spiking and non-spiking neurons

#. synapses

   a. Post-synaptic membrane current mechanisms
   
   #. Short-term synaptic dynamics (depression, facilitation)
   
   #. Long-term synaptic modifications (STDP, learning, etc.)
   
   #. Gap-junctions
   
#. populations of neurons

#. synaptic projections between populations of neurons

Design considerations
---------------------

As one of the goals of NineML is to provide a means to exchange models
between simulator platforms, it is important to maintain a clear
distinction between the role of NineML and the role of a simulator.
Therefore, NineML only contains the necessary information to describe
the model not how to simulate it, although suggestions can be supplied
in annotations to the model (see [sec:Annotations_Section]). For example,
NineML should specify the neuron membrane equation to solve, but not how
to solve it. In addition, for implementation and performance reasons, it
is important to keep the language layer “close” to the simulator – such
that the language layer is not responsible for maintaining separate
representations of all the instantiated elements in the network.

A NineML object model representation can take multiple forms. A program
can employ a concrete representation of the NineML objects in a specific
programming language, convert an internal model representation to and
from the NineML XML schema, or use code generation to produce a model
representation for a target simulation environment. It is important to
note that the NineML XML schema is isomorphic to the NineML object
model.

The design of NineML is divided into two semantic layers:

#. An **Abstraction Layer** that provides the core concepts and
   mathematical descriptions with which model variables and state update
   rules are explicitly described in *parametrized* form, and

#. A **User Layer** that provides a syntax to specify the instantiation
   and the value of parameters of all these components of a network
   model.

Since the User Layer provides the instantiation and parametrization of
model elements that have been defined in the Abstraction Layer, the two
layers should share a complementary and compatible design philosophy.
Which aspects of a model are defined in the Abtraction Layer and which
are in the User Layer Layer are clearly defined (each element type
belongs to only one layer with the exception of units and dimensions).
In order to simplify their interpretation and maintain compatibility
with a wide range of data formats (e.g. JSON, Python objects), NineML
documents are not sensitive to the order that objects appear in.

Identifiers
-----------

Elements are identified by *names*, which are unique in the scope they
are enclosed by (either within a component class or in the document
scope of the file). For a name to be a valid NineML identifier, it must
meet the requirements for a `ANSI C89
identifiers <http://msdn.microsoft.com/en-us/library/e7f8y25b.aspx>`__.
Additionally, identifiers are not permitted to begin or end with an
underscore character (i.e. ‘\_’) to allow special variables to be
defined in the same scope as identified variables/objects in generated
code.

NineML identifiers are case-sensitive in the sense that they must be
referred to with the same case as they are defined. However, two
identifiers that are identical with the exception of case, e.g.
‘v\_threshold’ and ‘v\_Threshold’, are not permitted within the same
scope. Identifiers used within component classes also cannot be the same
(case-insensitive) as one of the built-in symbols or functions (see
:ref:`MathInline`).
