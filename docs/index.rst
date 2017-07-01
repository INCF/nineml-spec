|image|

--------------

| **Network Interchange for Neuroscience Modeling Language (NineML)**
| **Specification**
| NineML Committee
| Version: 1.0.1

--------------

**Editors:**

-  Thomas G. Close

-  Alexander J. Cope

-  Andrew P. Davison

-  Jochen M. Eppler

-  Erik De Schutter

-  Ivan Raikov

-  Paul Richmond

| **Acknowledgments:**
| We would like to thank the former INCF NineML Task Force members for
  their contributions to the text and the concepts presented in this
  document. In particular: A. Gorchetchnikov, M. Hull, Y. Le Franc, P.
  Gleeson, E. Muller, R. Cannon, Birgit Kriener, Subhasis Ray and S.
  Hill.

| This document is under the Common Creative license BY-NC-SA:
| http://creativecommons.org/licenses/by-nc-sa/3.0/

|image|

**Date:**

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

#. spiking and non-spiking neurons

#. synapses

   #. Post-synaptic membrane current mechanisms

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
in annotations to the model (see [sec:AnnotationsSection]). For example,
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
MathInline).

Document layout
===============

NineML documents must be enclosed within an NineML element, which should
be in the ’http://nineml.net/9ML/1.0’ XML namespace.

NineML
------

+----------------+-----------------------------+----------+
| Attribute      | Type/Format                 | Required |
+----------------+-----------------------------+----------+
| xmlns          | ‘http://nineml.net/9ML/1.0’ | yes      |
+----------------+-----------------------------+----------+
| Child elements | Multiplicity                | Required |
+----------------+-----------------------------+----------+
| Component      | set                         | no       |
| ComponentClass | set                         | no       |
| Unit           | set                         | no       |
| Dimension      | set                         | no       |
| Population     | set                         | no       |
| Projection     | set                         | no       |
| Selection      | set                         | no       |
+----------------+-----------------------------+----------+

Seven *document-level* elements are allowed to reside directly within
NineML elements: Component, ComponentClass, Unit, Dimension, Population,
Projection and Selection. Each element should be uniquely identified by
its *name* attribute within the scope of the document (see ).

Unit and Dimension elements must be defined within the document they are
referenced, whereas the remaining element types can also be referenced
from other NineML documents (see Reference and Definition).

Xmlns attribute
~~~~~~~~~~~~~~~

The *xmlns* attribute is required and should refer to the
`URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__\ corresponding
to the correct NineML version, which for version 1.0 is
‘http://nineml.net/9ML/1.0’ (see http://www.w3.org/TR/REC-xml-names/).

Units and Dimensions
====================

Dimensions are associated with parameters, analog ports and state
variables in component class definitions. Each dimension can give rise
to a family of unit declarations, each of which has the same
dimensionality but a different multiplier. For example, typical units
for a quantity with dimensionality voltage include millivolts
(multiplier = :math:`10^{-3}`), microvolts (multiplier =
:math:`10^{-6}`) and volts (multiplier = 1). To express a dimensional
quantity both a numerical factor and a unit are required.

Except where physical constants are required, abstraction layer
definitions generally only contain references to dimensions and are
independent of any particular choice of units. Conversely, the user
layer only refers to units. Internally, dimensional quantities are to be
understood as rich types with a numerical factor and exponents for each
of the base dimensions. They are independent of the particular choice of
units by which they are assigned.

Dimension
---------

+-----------+-------------+----------+
| Attribute | Type/Format | Required |
+-----------+-------------+----------+
| name      | identifier  | yes      |
| m         | int         | no       |
| l         | int         | no       |
| t         | int         | no       |
| i         | int         | no       |
| n         | int         | no       |
| k         | int         | no       |
| j         | int         | no       |
+-----------+-------------+----------+

Dimension objects are constructed values from the powers for each of the
seven SI base units: length (*l*), mass (*m*), time (*t*), electric
current (*i*), temperature (*k*), luminous intensity (*l*) and amount of
substance (*n*). For example, acceleration has dimension :math:`lt^{-2}`
and voltage is :math:`ml^2t^3i^{-1}`. Dimension objects must be declared
in the top-level scope of the NineML document where they are referenced.

Name attribute
~~~~~~~~~~~~~~

Each Dimension requires a *name* attribute, which should be a valid and
uniquely identify the Dimension in current the scope.

M attribute
~~~~~~~~~~~

The *m* attribute specifies the power of the mass dimension in the
Dimension. If omitted the power is zero.

L attribute
~~~~~~~~~~~

The *l* attribute specifies the power of the length dimension in the
Dimension. If omitted the power is zero.

T attribute
~~~~~~~~~~~

The *t* attribute specifies the power of the time dimension in the
Dimension. If omitted the power is zero.

I attribute
~~~~~~~~~~~

The *i* attribute specifies the power of the current dimension in the
Dimension. If omitted the power is zero.

N attribute
~~~~~~~~~~~

The *n* attribute specifies the power of the amount-of-substance
dimension in the Dimension. If omitted the power is zero.

K attribute
~~~~~~~~~~~

The *k* attribute specifies the power of the temperature dimension in
the Dimension. If omitted the power is zero.

J attribute
~~~~~~~~~~~

The *j* attribute specifies the power of the luminous-intensity
dimension in the Dimension. If omitted the power is zero.

Unit
----

+-----------+----------------+----------+
| Attribute | Type/Format    | Required |
+-----------+----------------+----------+
| symbol    |                | yes      |
| dimension | Dimension@name | yes      |
| power     |                | no       |
| offset    |                | no       |
+-----------+----------------+----------+

Unit objects specify the dimension multiplier and the offset of a unit
with respect to a defined Dimension object. Unit objects must be
declared in the top-level scope of the NineML documents where they are
referenced.

Symbol attribute
~~~~~~~~~~~~~~~~

Each Unit requires a *symbol* attribute, which should be a valid and
uniquely identify the Unit in current the scope.

Dimension attribute
~~~~~~~~~~~~~~~~~~~

Each Unit requires a *dimension* attribute. This attribute specifies the
dimension of the units and should refer to the name of a Dimension
element in the document scope.

Power attribute
~~~~~~~~~~~~~~~

Each Unit requires a *power* attribute. This attribute specifies the
relative scale of the units compared to the equivalent SI units in
powers of ten. If omitted the power is zero.

Offset attribute
~~~~~~~~~~~~~~~~

A Unit can optionally have an *offset* attribute. This attribute
specifies the zero offset of the unit scale. For example,

.. code-block:: xml

    <Unit name="degC" dimension="temperature" power="0" offset="273.15"/>

If omitted, the offset is zero.

Component Classes and Parameters
================================

The main building block of the Abstraction Layer is the ComponentClass.
The ComponentClass is intended to package together a collection of
objects that relate to the definition of a model (e.g. cells, synapses,
synaptic plasticity rules, random spike trains, inputs). All equations
and event declarations that are part of particular entity model, such as
neuron model, belong in a single ComponentClass. A ComponentClass can be
used to represent either a specific model of a neuron or a composite
model, including synaptic mechanisms.

The interface is the *external* view of the ComponentClass that defines
what inputs and outputs the component exposes to other ComponentClass
elements and the parameters that can be set for the ComponentClass. The
interface consists of instances of ports and Parameter (see
[fig:component\_class\_overview]).

.. figure:: figures/component_simple.pdf
   :alt: ComponentClass Overview
   :width: 12.00000cm

   ComponentClass Overview

As well as being able to specify the communication of continuous values,
ComponentClass elements are also able to specify the emission and the
reception of events. Events are discrete notifications that are
transmitted over event ports. Since Event ports have names, saying that
we transmit ‘event1’ for example would mean transmitting an event on the
EventPort called ‘event1’. Events can be used for example to signal
action potential firing.

ComponentClass
--------------

+----------------------------------------------+--------------+----------+
| Attribute                                    | Type/Format  | Required |
+----------------------------------------------+--------------+----------+
| name                                         | identifier   | yes      |
+----------------------------------------------+--------------+----------+
| Child elements                               | Multiplicity | Required |
+----------------------------------------------+--------------+----------+
| Parameter                                    | set          | no       |
|                                              |              |          |
| AnalogSendPort                               | set          | no       |
|                                              |              |          |
| AnalogReceivePort                            | set          | no       |
| AnalogReducePort                             | set          | no       |
| EventSendPort                                | set          | no       |
| EventReceivePort                             | set          | no       |
| [Dynamics,ConnectionRule,RandomDistribution] | singleton    | yes      |
+----------------------------------------------+--------------+----------+

A ComponentClass is composed of:

-  Parameter objects for the ComponentClass, which specify which values
   are required to be provided in the User Layer.

-  An unordered collection of port objects, which either publish or read
   state variables or derived values published from other components in
   the case of analog send and receive ports, or emit events or listen
   for events emitted from components. EventSendPort and
   EventReceivePort objects raise and listen for events passed between
   dynamic components.

-  A ‘main’ block, which specifies the nature of the component class:

   -  Dynamics, the component class defines a dynamic element such as
      neutron or post-synaptic response.

   -  ConnectionRule, the component class defines a rule by which
      populations are connected in projections.

   -  RandomDistribution, the component class defines random
      distribution.

Name attribute
~~~~~~~~~~~~~~

Each ComponentClass requires a *name* attribute, which should be a valid
and uniquely identify the ComponentClass in the document scope.

Parameter
---------

+-----------+----------------+----------+
| Attribute | Type/Format    | Required |
+-----------+----------------+----------+
| name      | identifier     | yes      |
| dimension | Dimension@name | yes      |
+-----------+----------------+----------+

Parameter objects are placeholders for numerical values within a
ComponentClass. They define particular qualities of the model, such as
the firing threshold, reset voltage or the decay time constant of a
synapse model. By definition, Parameters are set at the start of the
simulation, and remain constant throughout.

Name attribute
~~~~~~~~~~~~~~

Each Parameter requires a *name* attribute, which is a valid and
uniquely identifies the Parameter within the ComponentClass.

Dimension attribute
~~~~~~~~~~~~~~~~~~~

Parameter elements must have a *dimension* attribute. This attribute
specifies the dimension of the units of the quantity that is expected to
be passed to the Parameter and should refer to the name of a Dimension
element in the document scope. For a dimensionless parameters a
Dimension with all attributes of power 0 can be used.

Mathematical Expressions
========================

As of NineML version 1.0, only inline mathematical expressions, which
have similar syntax to the ANSI C89 standard, are supported. In future
versions it is envisaged that inline expressions will be either
augmented or replaced with MathML (http://mathml.org) expressions.

MathInline
----------

+-------------------------+----------+
| Body                    | Required |
+-------------------------+----------+
| Inline-maths expression | yes      |
+-------------------------+----------+


MathInline blocks are used to specify mathematical expressions.
Depending on the context, MathInline blocks should return an expression
that evaluates to either a (when used as the trigger for OnCondition
objects) or a (when used as a right-hand-side for Alias, TimeDerivative
and StateAssignment objects). All numbers/variables in inline maths
expressions are assumed to be .

Body
~~~~

The following arithmetic operators are supported in all inline maths
expressions and have the same interpretation and precedence levels as in
the ANSI C89 standard,

-  Addition ``+``

-  Subtraction ``-``

-  Division ``/``

-  Multiplication ``*``

The following inequality and logical operators are only supported in
inline maths expressions within Trigger elements. They also have the
same interpretation and precedence levels as in ANSI C89 standard.

-  Greater than ``>``

-  Lesser than ``<``

-  Logical And: ``&&``

-  Logical Or: ``||``

-  Logical Not: ``!``

The following functions are built in and are defined as per ANSI C89:

-  ``exp(x)``

-  ``sin(x)``

-  ``cos(x)``

-  ``log(x)``

-  ``log10(x)``

-  ``pow(x, p)``

-  ``sinh(x)``

-  ``cosh(x)``

-  ``tanh(x)``

-  ``sqrt(x)``

-  ``atan(x)``

-  ``asin(x)``

-  ``acos(x)``

-  ``asinh(x)``

-  ``acosh(x)``

-  ``atanh(x)``

-  ``atan2(x)``

The following symbols are built in, and cannot be redefined,

-  pi

-  t

where :math:`pi` is the mathematical constant :math:`\pi`, and :math:`t`
is the elapsed simulation time within a Dynamics block.

The following random distributions are available in StateAssignment
elements via the ``random`` namespace, :

-  ``random.uniform`` (see http://uncertml.org/distributions/uniform)

-  ``random.normal`` (see http://uncertml.org/distributions/normal)

-  ``random.binomial(N,P)`` (see
   http://uncertml.org/distributions/binomial)

-  ``random.poisson(L)`` (see http://uncertml.org/distributions/poisson)

-  ``random.exponential(L)`` (see
   http://uncertml.org/distributions/exponential)

Alias
-----

+----------------+--------------+----------+
| Attribute      | Type/Format  | Required |
+----------------+--------------+----------+
| name           | identifier   | yes      |
+----------------+--------------+----------+
| Child elements | Multiplicity | Required |
+----------------+--------------+----------+
| MathInline     | singleton    | yes      |
+----------------+--------------+----------+

An alias corresponds to an alternative name for a variable or part of an
expression.

**Aliases** are motivated by two use cases:

-  **substitution**: rather than writing long expressions for functions
   of state variables, we can split the expressions into a chain of
   Alias objects, e.g.

   ::

       m_alpha = (alphaA + alphaB * V)/(alphaC + exp((alphaD + V / alphaE)))
       m_beta = (betaA + betaB * V)/(betaC + exp((betaD + V / betaE)))
       minf = m_alpha / (m_alpha + m_beta)
       mtau = 1.0 / (m_alpha + m_beta)
       dm/dt = (1 / C) * (minf - m) / mtau

   In this case, ``m_alpha``, ``m_beta``, ``minf`` and ``mtau`` are all
   alias definitions. There is no reason we couldn’t expand our
   :math:`\mathrm{d}m/\mathrm{d}t` description out to eliminate these
   intermediate Alias objects, but the expression would be very long and
   difficult to read.

-  **Accessing intermediate variables**: if we would like to communicate
   a value other than a simple StateVariable to another ComponentClass.
   For example, if we have a component representing a neuron, which has
   an internal StateVariable, ‘V’, we may be interested in transmitting
   a current, for example :math:`i=g*(E-V)`.

Name attribute
~~~~~~~~~~~~~~

Each Alias requires a *name* attribute, which is a valid and uniquely
identifies the Alias from all other elements in the ComponentClass.

Constant
--------

+-----------+-------------+----------+
| Attribute | Type/Format | Required |
+-----------+-------------+----------+
| name      | identifier  | yes      |
| units     | Unit@name   | yes      |
+-----------+-------------+----------+
| Body      |             | Required |
+-----------+-------------+----------+
| ``float`` |             | yes      |
+-----------+-------------+----------+

Constant objects are used to specify physical constants such as the
Ideal Gas Constant (i.e. 8.314462175
JK\ :math:`^{-1}`\ mol\ :math:`^{-1}`) or Avogadro’s number (i.e.
6.0221412927\ :math:`\times`\ 10\ :math:`^{23}`\ mol\ :math:`^{-1}`),
and to convert unit dimensions between abstract mathematical quantities.

The use of Constant elements to hold fixed model parameters is *strongly
discouraged* since this breaks the division of semantic layers
(abstraction and user), which is a key feature of NineML (see
[sec:scope]).

Name attribute
~~~~~~~~~~~~~~

Each Constant requires a *name* attribute, which should be a valid and
uniquely identify the Dimension in current the scope.

Units attribute
~~~~~~~~~~~~~~~

Each Constant requires a *units* attribute. The *units* attribute
specifies the units of the property and should refer to the name of a
Unit element in the document scope.

Body
~~~~

Any valid numeric value, including shorthand scientific notation e.g.
1e-5 (:math:`1\times10^{-5}`).

Ports
=====

Ports allow components to communicate with each other during a
simulation. Ports can either transmit discrete events or continuous
streams of analog data. Events are typically used to transmit and
receive spikes between neutron model, whereas analog ports can be used
to model injected current and gap junctions between neuron models.

Ports are divided into sending, EventSendPort and AnalogSendPort, and
receiving objects, EventReceivePort, AnalogReceivePort and
AnalogReducePort. With the exception of AnalogReducePort objects, each
receive port must be connected to exactly one matching (i.e.
analog\ :math:`\to`\ analog, event\ :math:`\to`\ event) send port, where
as a send port can be connected any number of receive ports.
AnalogReducePort objects can be connected to any number of
AnalogSendPort objects; the values of the connected ports are then
“reduced” to a single data stream using the *operator* provided to the
AnalogReducePort.

AnalogSendPort
--------------

+-----------+----------------------------+----------+
| Attribute | Type/Format                | Required |
+-----------+----------------------------+----------+
| name      | [StateVariable,Alias]@name | yes      |
| dimension | Dimension@name             | yes      |
+-----------+----------------------------+----------+

AnalogSendPort objects allow variables from the current component to be
published externally so they can be read by other ComponentClass
objects. Each AnalogSendPort can be connected to multiple
AnalogReceivePort and AnalogReducePort objects.

Name attribute
~~~~~~~~~~~~~~

Each AnalogSendPort requires a *name* attribute, which should refer to a
StateVariable or Alias within the current ComponentClass.

Dimension attribute
~~~~~~~~~~~~~~~~~~~

Each AnalogSendPort requires a *dimension* attribute. This attribute
specifies the dimension of the units of the quantity that is expected to
be passed through the AnalogSendPort and should refer to the name of a
Dimension element in the document scope.

AnalogReceivePort
-----------------

+-----------+----------------+----------+
| Attribute | Type/Format    | Required |
+-----------+----------------+----------+
| name      | identifier     | yes      |
| dimension | Dimension@name | yes      |
+-----------+----------------+----------+

AnalogReceivePorts allow variables that have been published externally
to be used within the current component. Each AnalogReceivePort must be
connected to exactly *one* AnalogSendPort.

Name attribute
~~~~~~~~~~~~~~

Each AnalogReceivePort requires a *name* attribute, which is a valid and
uniquely identifies the AnalogReceivePort from all other elements in the
ComponentClass.

Dimension attribute
~~~~~~~~~~~~~~~~~~~

Each AnalogReceivePort requires a *dimension* attribute. This attribute
specifies the dimension of the units of the quantity that is expected to
be passed through the AnalogReceivePort and should refer to the name of
a Dimension element in the document scope.

AnalogReducePort
----------------

+-----------+----------------+----------+
| Attribute | Type/Format    | Required |
+-----------+----------------+----------+
| name      | identifier     | yes      |
| dimension | Dimension@name | yes      |
| operator  | +              | yes      |
+-----------+----------------+----------+

Reduce ports can receive data from any number of AnalogSendPort objects
(including none). An AnalogReducePort takes an additional operator
compared to an AnalogReceivePort, operator, which specifies how the data
from multiple analog send ports should be combined to produce a single
value. Currently, the only supported operation is :math:`+`, which
calculates the sum of the incoming port values.

The motivation for AnalogReducePort is that it allows us to make our
ComponentClass definitions more general. For example, if we are defining
a neuron, we would define an AnalogReducePort called *InjectedCurrent*.
This allows us to write the membrane equation for that neuron as
:math:`\mathrm{d}V/\mathrm{d}t = (1/C) * InjectedCurrent`.

Then, when we connect this neuron to synapses, current-clamps, etc, we
simply need to connect the send ports containing the currents of these
ComponentClasses to the *InjectedCurrent* reduce port, without having to
change our original ComponentClass definitions.

Name attribute
~~~~~~~~~~~~~~

Each AnalogReducePort requires a *name* attribute, which is a valid and
uniquely identifies the AnalogReducePort from all other elements in the
ComponentClass.

Dimension attribute
~~~~~~~~~~~~~~~~~~~

Each AnalogReducePort requires a *dimension* attribute. This attribute
specifies the dimension of the units of the quantity that is expected to
be communicated through the AnalogReducePort and should refer to the
name of a Dimension element in the document scope.

Operator attribute
~~~~~~~~~~~~~~~~~~

Each AnalogReducePort requires an *operator* attribute. The operator
reduces the connected inputs to a single value at each time point. For
example the following port,

.. code-block:: xml

    <AnalogReducePort name="total_membrane_current" dimension="current" operator="+"/>

will take all of the electrical currents that have been connected to it
via AnalogSendPorts and sum them to get the total current passing
through the membrane.

EventSendPort
-------------

+-----------+-------------+----------+
| Attribute | Type/Format | Required |
+-----------+-------------+----------+
| name      | identifier  | yes      |
+-----------+-------------+----------+

An EventSendPort specifies a channel over which events can be
transmitted from a component. Each EventSendPort can be connected any
number of EventReceivePort objects.

Name attribute
~~~~~~~~~~~~~~

Each EventSendPort requires a *name* attribute, which is a valid and
uniquely identifies the EventSendPort from all other elements in the
ComponentClass.

EventReceivePort
----------------

+-----------+-------------+----------+
| Attribute | Type/Format | Required |
+-----------+-------------+----------+
| name      | identifier  | yes      |
+-----------+-------------+----------+

An EventReceivePort specifies a channel over which events can be
received by a component. Each EventReceivePort must be connected to
exactly *one* EventSendPort.

Name attribute
~~~~~~~~~~~~~~

Each EventReceivePort requires a *name* attribute, which is a valid and
uniquely identifies the EventReceivePort from all other elements in the
ComponentClass.

Dynamic Regimes
===============

Dynamics blocks define the dynamic equations of models such as neurons,
post-synaptic responses or plasticity of synaptic weights. In Dynamics
blocks, state variables are evolved by one or more sets of ordinary
differential equations (ODE). Each set of equations is called a regime,
and only one regime can be active at a particular point in time. The
currently active regime can be changed by a transition event, which is
represented as a logical expression on the state variables. When the
logical expression evaluates to true, the transition must occur.

[fig:simple\_regime\_graph] illustrates a hypothetical transition graph
for a system with three state variables, :math:`X`, :math:`Y` and
:math:`Z`, which transitions between three ODE regimes, *regime1*,
*regime2* and *regime3*. At any time, the model will be in one and only
one of these regimes, and the state variables will evolve according to
the ODE of that regime.

.. figure:: figures/SimpleRegimeGraph.png
   :alt: The dynamics block for an example component.
   :width: 14.00000cm

   The dynamics block for an example component.

Dynamics
--------

+----------------+--------------+----------+
| Child elements | Multiplicity | Required |
+----------------+--------------+----------+
| StateVariable  | set          | no       |
| Regime         | set          | yes      |
| Alias          | set          | no       |
| Constant       | set          | no       |
+----------------+--------------+----------+

The Dynamics block represents the *internal* mechanisms governing the
behaviour of the component. These dynamics are based on ordinary
differential equations (ODE) but may contain non-linear transitions
between different ODE regimes. The regime graph (e.g.
[fig:simple\_regime\_graph]) must contain at least one Regime element,
and contain no regime islands. At any given time, a component will be in
a single regime, and can change which regime it is in through
transitions.

StateVariable
-------------

+-----------+----------------+----------+
| Attribute | Type/Format    | Required |
+-----------+----------------+----------+
| name      | identifier     | yes      |
| dimension | Dimension@name | yes      |
+-----------+----------------+----------+

The state of the model is defined by a set of StateVariable objects. The
value of a StateVariable can change in two ways:

    -  continuously through TimeDerivative elements (in Regime
       elements), which define how the StateVariable evolves over time,
       e.g. :math:`dX/dt=1-X`.

    -  discretely through StateAssignment (in OnCondition or OnEvent
       transition elements), which make discrete changes to a
       StateVariable value, e.g. :math:`X = X + 1`.

Name attribute
~~~~~~~~~~~~~~

Each StateVariable requires a *name* attribute, which is a valid and
uniquely identifies the StateVariable from all other elements in the
ComponentClass.

Dimension attribute
~~~~~~~~~~~~~~~~~~~

Each StateVariable requires a *dimension* attribute. This attribute
specifies the dimension of the units of the quantities that
StateVariable is expected to be initialised and updated with and should
refer to the name of a Dimension element in the document scope.

Regime
------

+----------------+--------------+----------+
| Attribute      | Type/Format  | Required |
+----------------+--------------+----------+
| name           | identifier   | yes      |
+----------------+--------------+----------+
| Child elements | Multiplicity | Required |
+----------------+--------------+----------+
| TimeDerivative | set          | no       |
| OnCondition    | set          | no       |
| OnEvent        | set          | no       |
+----------------+--------------+----------+

A Regime element represents a system of ODEs in time on StateVariable.
As such, Regime defines how the state variables change (propagate in
time) between subsequent transitions.

Name attribute
~~~~~~~~~~~~~~

Each Regime requires a *name* attribute, which is a valid and uniquely
identifies the Regime from all other elements in the ComponentClass.

TimeDerivative
--------------

+----------------+--------------------+----------+
| Attribute      | Type/Format        | Required |
+----------------+--------------------+----------+
| variable       | StateVariable@name | yes      |
+----------------+--------------------+----------+
| Child elements | Multiplicity       | Required |
+----------------+--------------------+----------+
| MathInline     | singleton          | yes      |
+----------------+--------------------+----------+

TimeDerivative elements contain a mathematical expression for the
right-hand side of the ODE

.. math:: \frac{\mathrm{d} variable}{\mathrm{d} t} = expression

 which can contain of references to any combination of StateVariable,
Parameter, AnalogReceivePort, AnalogReducePort and Alias elements with
the exception of aliases that are derived from RandomDistribution
components. Therefore, only one TimeDerivative element is allowed per
StateVariable per Regime. If a TimeDerivative for a StateVariable is not
defined in a Regime, it is assumed to be zero.

Variable attribute
~~~~~~~~~~~~~~~~~~

Each TimeDerivative requires a *variable* attribute. This should refer
to the name of a StateVariable in the ComponentClass. Only one
TimeDerivative is allowed per *variable* in each Regime.

Transitions
===========

The currently active dynamic regime can be changed via transitions.
Transitions have instantaneous temporal extent (i.e. they are
event-like). There are two types of transitions, condition-triggered
transitions (see OnCondition), which are evoked when an associated
trigger expression becomes true, or event-triggered transitions (see
OnEvent), which are evoked when an associated event port receives an
event from an external component. Multiple state assignments can be
defined and multiple events can be sent within a single transition
block.

During either type of transition three instantaneous actions can occur:

-  The component transitions to a target regime (can be the same as the
   current regime)

-  State variables can be assigned new values (see StateAssignment)

-  The component can send events (see OutputEvent).

There is no order defined in transitions; this means that the order of
resolution of state assignments can be ambiguous. If, for example, we
have two transitions, T1 and T2, originating from the same Regime, in
which T1 contains the state assignment *V=V+1* and T2 contains the
assignment *V=V\*V*, and both transitions are triggered simultaneously,
then there is no guarantee about the value of V. It is left to the user
to ensure such situations do not occur. Implementations should emit a
warning when they are detected.

OnCondition
-----------

+-----------------+--------------+----------+
| Attribute       | Type/Format  | Required |
+-----------------+--------------+----------+
| targetRegime    | Regime@name  | no       |
+-----------------+--------------+----------+
| Child elements  | Multiplicity | Required |
+-----------------+--------------+----------+
| Trigger         | singleton    | yes      |
| StateAssignment | set          | no       |
| OutputEvent     | set          | no       |
+-----------------+--------------+----------+

OnCondition blocks are activated when the mathematical expression in the
Trigger block becomes true. They are typically used to model spikes in
spiking neuron models, potentially emitting spike events and/or
transitioning to an explicit refractory regime.

TargetRegime attribute
~~~~~~~~~~~~~~~~~~~~~~

An OnEvent can have a *targetRegime* attribute, which should refer to
the name of a Regime element in the ComponentClass that the dynamics
block will transition to when the trigger condition is met. If the
*targetRegime* attribute is omitted the regime will transition to
itself.

OnEvent
-------

+-----------------+-----------------------+----------+
| Attribute       | Type/Format           | Required |
+-----------------+-----------------------+----------+
| targetRegime    | Regime@name           | no       |
| port            | EventReceivePort@name | yes      |
+-----------------+-----------------------+----------+
| Child elements  | Multiplicity          | Required |
+-----------------+-----------------------+----------+
| StateAssignment | set                   | no       |
| OutputEvent     | set                   | no       |
+-----------------+-----------------------+----------+

OnEvent blocks are activated when the dynamics component receives an
event from an external component on the port the OnEvent element is
“listening” to. They are typically used to model the transient response
to spike events from incoming synaptic connections.

*Cascading* of events, i.e. events triggering subsequent events, are
permitted, which in theory could be recursive through components
depending on their connectivity. It is the user’s responsibility to
ensure that infinite recursion does not occur with zero delay.
Implementations may decide to terminate after a given number of
recursive cascades of zero delay (say 1000) to prevent infinite loops,
but such limits should be modifiable by the user.

Port attribute
~~~~~~~~~~~~~~

Each OnEvent requires a *port* attribute. This should refer to the name
of an EventReceivePort in the ComponentClass interface.

TargetRegime attribute
~~~~~~~~~~~~~~~~~~~~~~

OnEvent can have a *targetRegime* attribute, which should refer to the
name of a Regime element in the ComponentClass that the dynamics block
will transition to when the OnEvent block is triggered by an incoming
event. If the *targetRegime* attribute is omitted the regime will
transition to itself.

Trigger
-------

+----------------+--------------+----------+
| Child elements | Multiplicity | Required |
+----------------+--------------+----------+
| MathInline     | singleton    | yes      |
+----------------+--------------+----------+

Trigger objects define when an OnCondition transition should occur. The
MathInline block of a Trigger can contain any arbitrary combination of
‘and’, ‘or’ and ‘negation’ *logical operations* (‘:math:`\&\&`’,
‘:math:`||`’ and ‘:math:`!`’ respectively) on the result of pure
inequality *relational operations* (‘:math:`>`’ and ‘:math:`<`’), which
follow the syntax and semantics of ANSI C89. The inequality expression
may contain references to StateVariable, AnalogReceivePort,
AnalogReducePort, Parameter and Alias elements, with the exception of
Alias elements derived from random distributions. The OnCondition block
is triggered when the boolean result of the Trigger statement changes
from *false* to *true*.

StateAssignment
---------------

+----------------+--------------------+----------+
| Attribute      | Type/Format        | Required |
+----------------+--------------------+----------+
| variable       | StateVariable@name | yes      |
+----------------+--------------------+----------+
| Child elements | Multiplicity       | Required |
+----------------+--------------------+----------+
| MathInline     | singleton          | yes      |
+----------------+--------------------+----------+

StateAssignment elements allow discontinuous changes in the value of
state variables. Only one state assignment is allowed per variable per
transition block. The assignment expression may contain references to
StateVariable, AnalogReceivePort, AnalogReducePort, Parameter and Alias
elements, including Alias elements derived from random distributions.
State assignments are typically used to reset the membrane voltage after
an outgoing spike event or update post-synaptic response states after an
incoming spike event.

Variable attribute
~~~~~~~~~~~~~~~~~~

Each StateAssignment requires a *variable* attribute. This should refer
to the name of a StateVariable in the ComponentClass. Only one
StateAssignment is allow per *variable* in each OnEvent or OnCondition
block.

OutputEvent
-----------

+-----------+--------------------+----------+
| Attribute | Type/Format        | Required |
+-----------+--------------------+----------+
| port      | EventSendPort@name | yes      |
+-----------+--------------------+----------+

OutputEvent elements specify events to be raised during a transition.
They are typically used to raise spike events from within OnCondition
elements.

Port attribute
~~~~~~~~~~~~~~

Each OutputEvent requires a *port* attribute. This should refer to the
name of an EventSendPort in the ComponentClass interface.

Random Distributions
====================

Values for a property across all elements in a container (e.g. cells in
a population, post-synaptic responses, plasticity rules or delays in a
projection) can be defined as a random distribution by a Component
within a RandomValue element. A random distribution component must
parameterize a ComponentClass with a RandomDistribution block; the
component class defines the random distribution family (e.g. normal,
cauchy, gamma, etc...). As of version 1.0, the only random distributions
available to the user are those defined in the standard library,
however, derived distributions are planned for future versions.

RandomDistribution
------------------

+------------------+-----------------------------------------------------------------+----------+
| Attribute        | Type/Format                                                     | Required |
+------------------+-----------------------------------------------------------------+----------+
| standard_library | `URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__ | yes      |
+------------------+-----------------------------------------------------------------+----------+

The names and parameters of the random distribution in the standard
library match the UncertML definitions that can be found at
http://www.uncertml.org/distributions. The subset of the UncertML
distributions that should be implemented are by NineML compliant
packages are,

-  BernoulliDistribution

-  BetaDistribution

-  BinomialDistribution

-  CauchyDistribution

-  ChiSquareDistribution

-  DirichletDistribution

-  ExponentialDistribution

-  FDistribution

-  GammaDistribution

-  GeometricDistribution

-  HypergeometricDistribution

-  LaplaceDistribution

-  LogisticDistribution

-  LogNormalDistribution

-  MultinomialDistribution

-  NegativeBinomialDistribution

-  NormalDistribution

-  ParetoDistribution

-  PoissonDistribution

-  UniformDistribution

-  WeibullDistribution

Standard\_library attribute
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The *standard\_library* attribute is required and should point to a
`URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__\ in the
http://www.uncertml.org/distributions/ directory.

Network Connectivity
====================

The connection rule for cells in the source and destination populations
of a Projection (i.e. the rule that determines which source cells are
connected to which destination cells) is defined by a connection-rule
component within the Connectivity element of the Projection. This
component must parameterize a ComponentClass with a ConnectionRule
block, which describes the connection algorithm. As of version 1.0, the
only connection rules available to the user are those defined in the
standard library (e.g. all-to-all, one-to-one, probabilistic, etc...),
however, custom connectivity rules are planned for future versions.

ConnectionRule
--------------

+------------------+-----------------------------------------------------------------+----------+
| Attribute        | Type/Format                                                     | Required |
+------------------+-----------------------------------------------------------------+----------+
| standard_library | `URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__ | yes      |
+------------------+-----------------------------------------------------------------+----------+

| Connection rules must be one of 6 standard library types,
  *all-to-all*, *one-to-one*, *probabilistic*, *explicit*,
| *random-fan-out* and *random-fan-in*, provided to the
  *standard\_libarary* attribute.

Standard\_library attribute
~~~~~~~~~~~~~~~~~~~~~~~~~~~

| The *standard\_library* attribute is required and should point to the
  `URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__\ in
  the
| `http://nineml.net/9ML/1.0/connectionrules/ <http://nineml.net/9ML/1.0/\-connectionrules/>`__
  directory that corresponds to the desired connection rule.

All cells in the source population are connected to all cells in the
destination population.

Each cell in the source population is connected to the cell in the
destination population with the corresponding index. Note that this
requires that the source and destination populations be the same size.

All cells in the source population are connected to cells in the
destination population with a probability defined by a parameter, which
should be named *probability*. The properties supplied to the
*probability* parameter should either be a SingleValue representing the
probability of a connection between all source and destination cell
pairs, or a ArrayValue or ExternalArrayValue of size :math:`M{\times}N`,
where :math:`M` and :math:`N` are the size of the source and destination
populations respectively. For array probabilities, the data in the
ArrayValue or ExternalArrayValue are ordered by the indices

.. math:: i_{\mathrm{prob}} = i_{\mathrm{source}} * N_{\mathrm{dest}} + i_{\mathrm{dest}}

where :math:`i_{\mathrm{prob}}`, :math:`i_{\mathrm{source}}` and
:math:`i_{\mathrm{dest}}` are the indices of the probability entry, and
the source and destination cells respectively, and
:math:`N_{\mathrm{dest}}` is the size of the destination population.

Cells in the source population are connected to cells in the destination
population as specified by an explicit arrays. The source and
destination are defined via parameters, which should be named
*sourceIndicies* and *destinationIndicies* parameters respectively.

The properties supplied to the *sourceIndicies* parameter should be a
ArrayValue or ExternalArrayValue drawn from the set
:math:`\{1,\ldots,M\}` where :math:`M` is the size of the source
population and be the same length as the property supplied to the
*target-indices* parameter.

The properties supplied to the *destinationIndicies* parameter should be
a ArrayValue or ExternalArrayValue drawn from the set
:math:`\{1,\ldots,N\}` where :math:`N` is the size of the source
population and be the same length as the property supplied to the
*source-indices* parameter.

Each cell in the source population is connected to a fixed number of
randomly selected cells in the destination population. The number of
cells is specified by the parameter *number*. The property supplied to
the *number* parameter should be a SingleValue.

Each cell in the destination population is connected to a fixed number
of randomly selected cells in the source population. The number of cells
is specified by the parameter *number*. The property supplied to the
*number* parameter should be a SingleValue.

Components and Properties
=========================

Component
---------

+----------------------+--------------+----------+
| Attribute            | Type/Format  | Required |
+----------------------+--------------+----------+
| name                 | identifier   | yes      |
+----------------------+--------------+----------+
| Child elements       | Multiplicity | Required |
+----------------------+--------------+----------+
| Definition,Prototype | singleton    | yes      |
| Property             | set          | no       |
+----------------------+--------------+----------+

Component elements instantiate Abstraction Layer component classes by
providing properties for each of the parameters defined the class. Each
Component is linked to a ComponentClass class by a Definition element,
which locates the component class. A Component that instantiates a
ComponentClass directly must supply matching Property elements for each
Parameter in the ComponentClass. Alternatively, a Component can inherit
a ComponentClass and set of Property elements from an existing component
by substituting the Definition for a Prototype element, which locates
the reference Component. In this case, only the properties that differ
from the reference component need to be specified.

Name attribute
~~~~~~~~~~~~~~

Each Component requires a *name* attribute, which should be a valid and
uniquely identify the Component from all other elements in the document
scope.

Definition
----------

+---------------------+-----------------------------------------------------------------+----------+
| Attribute           | Type/Format                                                     | Required |
+---------------------+-----------------------------------------------------------------+----------+
| url                 | `URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__ | no       |
+---------------------+-----------------------------------------------------------------+----------+
| Body                |                                                                 | Required |
+---------------------+-----------------------------------------------------------------+----------+
| ComponentClass@name |                                                                 | yes      |
+---------------------+-----------------------------------------------------------------+----------+

The Definition element establishes a link between a User Layer component
and Abstraction Layer ComponentClass. This ComponentClass can be located
either in the current document or in another file if a *url* attribute
is provided.

Url attribute
~~~~~~~~~~~~~

If the ComponentClass referenced by the definition element is defined
outside the current document, the *url* attribute specifies a
`URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__\ for the
file which contains the ComponentClass definition. If the *url*
attribute is omitted the ComponentClass is referenced from the current
document.

Body
~~~~

The name of the ComponentClass to be referenced ComponentClass needs to
be provided in the body of the Definition element.

Prototype
---------

+----------------+-----------------------------------------------------------------+----------+
| Attribute      | Type/Format                                                     | Required |
+----------------+-----------------------------------------------------------------+----------+
| url            | `URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__ | no       |
+----------------+-----------------------------------------------------------------+----------+
| Body           |                                                                 | Required |
+----------------+-----------------------------------------------------------------+----------+
| Component@name |                                                                 | yes      |
+----------------+-----------------------------------------------------------------+----------+

The Prototype element establishes a link to an existing User Layer
Component, which defines the ComponentClass and default properties of
the Component. The reference Component can be located either in the
current document or in another file if a *url* attribute is provided.

Url attribute
~~~~~~~~~~~~~

If the prototype Component is defined outside the current file, the
*URL* attribute specifies a
`URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__\ for the
file which contains the prototype Component. If the *url* attribute is
omitted the Component is referenced from the current document.

Body
~~~~

The name of the Component to be referenced Component needs to be
provided in the body of the Prototype element.

Property
--------

+---------------------------------------------------------+----------------+----------+
| Attribute                                               | Type/Format    | Required |
+---------------------------------------------------------+----------------+----------+
| name                                                    | Parameter@name | yes      |
| units                                                   | Unit@symbol    | yes      |
+---------------------------------------------------------+----------------+----------+
| Child elements                                          | Multiplicity   | Required |
+---------------------------------------------------------+----------------+----------+
| [SingleValue,ArrayValue,ExternalArrayValue,RandomValue] | singleton      | yes      |
+---------------------------------------------------------+----------------+----------+

Property elements provide values for the parameters defined in the
ComponentClass of the Component. Their *name* attribute should match the
name of the corresponding Parameter element in the ComponentClass. The
Property should be provided units that match the dimensionality of the
corresponding Parameter definition.

Name attribute
~~~~~~~~~~~~~~

Each Property requires a *name* attribute. This should refer to the name
of a Parameter in the corresponding ComponentClass of the Component.

Units attribute
~~~~~~~~~~~~~~~

Each Property element requires a *units* attribute. The *units*
attribute specifies the units of the quantity and should refer to the
name of a Unit element in the document scope. For a dimensionless units
a Unitwith no SI dimensions can be used. The SI dimensions of the
Unitshould match the SI dimensions of the corresponding Parameter.

Reference
---------

+-----------+-----------------------------------------------------------------+----------+
| Attribute | Type/Format                                                     | Required |
+-----------+-----------------------------------------------------------------+----------+
| url       | `URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__ | no       |
+-----------+-----------------------------------------------------------------+----------+
| Body      |                                                                 | Required |
+-----------+-----------------------------------------------------------------+----------+
| \*@name   |                                                                 | yes      |
+-----------+-----------------------------------------------------------------+----------+

Reference elements are used to locate User Layer elements in the
document scope of the current separate documents. In most cases, User
Layer elements (with the exception of Population elements supplied to
Projection) can be specified inline, i.e. within the element they are
required. However, it is often convenient to define a component in the
document scope as this allows it to be reused at different places within
the model. The *url* attribute can be used to reference a component in a
separate document, potentially one published online in a public
repository (e.g.
`ModelDB <http://senselab.med.yale.edu/modeldb/ListByModelName.asp?c=19&lin=-1>`__
or `Open Source Brain <http://www.opensourcebrain.org/>`__).

Url attribute
~~~~~~~~~~~~~

The *url* attribute specifies a
`URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__\ for the
file which contains the User Layer element to be referenced. If the
*url* attribute is omitted the element is referenced from the current
document.

Body
~~~~

The name of the User Layer element to be referenced should be included
in the body of the Reference element.

Values
======

In NineML, “values” are arrays that implicitly grow to fill the size of
the container (i.e. Population or Projection) they are located within.
Values can be one of four types

-  SingleValue, a consistent value across the container

-  ArrayValue, an explicit array defined in NineML

-  ExternalArrayValue, an explicit array defined in text (space
   delimited) or HDF5 format.

-  RandomValue, an array of values derived from a random distribution.

SingleValue
-----------

+-------------+----------+
| Body        | Required |
+-------------+----------+
| ``integer`` | yes      |
+-------------+----------+

A SingleValue element represents an array filled with a single value.

Body
~~~~

Any valid numeric value in `ANSI
C89 <http://en.wikipedia.org/wiki/ANSI_C>`__, including shorthand
scientific notation e.g. 1e-5 (:math:`1\times10^{-5}`).

ArrayValue
----------

+----------------+--------------+----------+
| Child elements | Multiplicity | Required |
+----------------+--------------+----------+
| ArrayValueRow  | set          | no       |
+----------------+--------------+----------+

ArrayValue elements are used to represent an explicit array of values in
XML. ArrayValue elements contain a set of ArrayValueRow elements (i.e.
unordered, since they are explicitly ordered by their *index*
attribute). Since XML is significantly slower to parse than plain text
and binary formats it is not recommended to use ArrayValue for large
arrays, preferring ExternalArrayValue instead.

ArrayValueRow
-------------

+-------------+-------------+----------+
| Attribute   | Type/Format | Required |
+-------------+-------------+----------+
| index       | ``integer`` | yes      |
+-------------+-------------+----------+
| Body        |             | Required |
+-------------+-------------+----------+
| ``integer`` |             | yes      |
+-------------+-------------+----------+

ArrayValueRow elements represent the numerical values of the explicit
ArrayValue element.

Index attribute
~~~~~~~~~~~~~~~

The *index* attribute specifies the index of the ArrayValueRow in the
ArrayValue. It must be non-negative, unique amongst the set of
ArrayValueRow@index in the list, and the set of indices must be
contiguous for a single ArrayValue.

Body
~~~~

Any valid numeric value in `ANSI
C89 <http://en.wikipedia.org/wiki/ANSI_C>`__, including shorthand
scientific notation e.g. 1e-5 (:math:`1\times10^{-5}`).

ExternalArrayValue
------------------

+------------+-------------------------------------------------------------------+----------+
| Attribute  | Type/Format                                                       | Required |
+------------+-------------------------------------------------------------------+----------+
| url        | `URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__   | yes      |
| mimeType   | `MIME  type <http://en.wikipedia.org/wiki/Internet_media_type>`__ | yes      |
| columnName | Data column name in external file                                 | yes      |
+------------+-------------------------------------------------------------------+----------+

ExternalArrayValue elements are used to explicitly define large arrays
of values. The array data are not stored in XML (which is slow to parse)
but more efficient text or binary `HDF5
(http://www.hdfgroup.org/HDF5/) <http://www.hdfgroup.org/HDF5/>`__
formats. As of version 1.0, the data in the external files are stored as
dense or arrays. However, sparse-array formats are planned for future
versions.

The *columnName* attribute of the ExternalArrayValue elements allows
multiple arrays of equal length (and therefore typically relating to the
same container) to be stored in the same external file.

Url attribute
~~~~~~~~~~~~~

The *url* attribute specifies the
`URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__\ of the
external data file.

MimeType attribute
~~~~~~~~~~~~~~~~~~

The *mimetype* attribute specifies the data format for the external
value list in the `MIME
type <http://en.wikipedia.org/wiki/Internet_media_type>`__ syntax.
Currently, only two formats are supported
``application/vnd.nineml.valuelist.text`` and
``application/vnd.nineml.valuelist.hdf5``.

-  ``application/vnd.nineml.externalvaluearray.text`` - an ASCII text
   file with a single row of white-space separated column names,
   followed by arbitrarily many white-space separated data rows of
   numeric values. Each numeric value is associated with the column name
   corresponding to the same index the along the row. Therefore, the
   number of items in each row must be the same.

-  ``application/vnd.nineml.externalvaluearray.hdf5`` - a
   `HDF5 <http://www.hdfgroup.org/HDF5/>`__ data file containing a
   single level of named members of or type.

ColumnName attribute
~~~~~~~~~~~~~~~~~~~~

Each ExternalArrayValue must have a *columnName* attribute, which refers
to a column header in the external data file.

RandomValue
-----------

+-----------------------+--------------+----------+
| Child elements        | Multiplicity | Required |
+-----------------------+--------------+----------+
| [Component,Reference] | singleton    | yes      |
+-----------------------+--------------+----------+

RandomValue elements represent arrays of values drawn from random
distributions, which are defined by a Componentelements. The size of the
generated array is determined by the size of the container (i.e.
Population or Projection) the RandomValue is nested within.

Populations
===========

Population
----------

+----------------+--------------+----------+
| Attribute      | Type/Format  | Required |
+----------------+--------------+----------+
| name           | identifier   | yes      |
+----------------+--------------+----------+
| Child elements | Multiplicity | Required |
+----------------+--------------+----------+
| Size           | singleton    | yes      |
| Cell           | singleton    | yes      |
+----------------+--------------+----------+

A Population defines a set of dynamic components of the same class. The
size of the set is specified by the Size element. The properties of the
dynamic components are generated from value types, which can be constant
across the population, randomly distributed or individually specified
(see [sec:Values]).

Name attribute
~~~~~~~~~~~~~~

Each Population requires a *name* attribute, which should be a valid and
uniquely identify the Population from all other elements in the document
scope.

Cell
----

+-----------------------+--------------+----------+
| Child elements        | Multiplicity | Required |
+-----------------------+--------------+----------+
| [Component,Reference] | singleton    | yes      |
+-----------------------+--------------+----------+

The Cell element specifies the dynamic components that will make up the
population. The Component can be defined inline or via a Reference
element.

Size
----

+------+----------+
| Body | Required |
+------+----------+
| int  | yes      |
+------+----------+

The number of cells in the population is specified by the integer
provided in the body of the Size element. In future versions this may be
extended to allow the size of a population to be derived from other
features of the Population.

Body
~~~~

The text of the Size element contains an representing the size of the
population.

Projections
===========

Projections define the synaptic connectivity between two populations,
the post-synaptic response of the connections, the plasticity rules that
modulate the post-synaptic response and the transmission delays.
Synaptic and plasticity dynamic components are created if the connection
rule determines there is a connection between a particular source and
destination cell pair. The synaptic and plasticity components are then
connected to and from explicitly defined ports of the cell components in
the source and projection populations

SingleValue and RandomValue elements used in properties of a projection
(in the Connectivity, Response, Plasticity and Delay elements) take the
size of the number of connections made. Explicitly array values,
ArrayValue and ExternalArrayValue, are only permitted with connection
rules (as defined by the Connectivity element) where the number of
connections is predetermined (i.e. *one-to-one*, *all-to-all* and
*explicit*). Explicit arrays are ordered by the indices

.. math:: i_{\mathrm{value}} = i_{\mathrm{source}} * N_{\mathrm{dest}} + i_{\mathrm{dest}}

 where :math:`i_{\mathrm{value}}`, :math:`i_{\mathrm{source}}` and
:math:`i_{\mathrm{dest}}` are the indices of the array entry, and the
source and destination cells respectively, and :math:`N_{\mathrm{dest}}`
is the size of the destination population. Value indices that do not
correspond to connected pairs are omitted, and therefore the arrays are
the same size as the number of connections.

Projection
----------

+----------------+--------------+----------+
| Attribute      | Type/Format  | Required |
+----------------+--------------+----------+
| name           | identifier   | yes      |
+----------------+--------------+----------+
| Child elements | Multiplicity | Required |
+----------------+--------------+----------+
| Source         | singleton    | yes      |
| Destination    | singleton    | yes      |
| Connectivity   | singleton    | yes      |
| Response       | singleton    | yes      |
| Plasticity     | singleton    | no       |
| Delay          | singleton    | yes      |
+----------------+--------------+----------+

The Projection element contains all the elements that define a
projection between two populations and should be uniquely identified in
the scope of the document.

Name attribute
~~~~~~~~~~~~~~

Each Projection requires a *name* attribute, which should be a valid and
uniquely identify the Projection from all other elements in the document
scope.

Connectivity
------------

+----------------+--------------+----------+
| Child elements | Multiplicity | Required |
+----------------+--------------+----------+
| Component      | singleton    | yes      |
+----------------+--------------+----------+

Each Connectivity element contains a Component, which defines the
connection pattern of the cells in the source population to cells in the
destination population (i.e. binary ‘connected’ or ‘not connected’
decisions). For each connection that is specified, a synapse, consisting
of a post-synaptic response and plasticity dynamic components, is
created to model the synaptic interaction between the cells.

Source
------

+-----------------------+--------------+----------+
| Child elements        | Multiplicity | Required |
+-----------------------+--------------+----------+
| [Component,Reference] | singleton    | yes      |
| FromDestination       | set          | no       |
| FromPlasticity        | set          | no       |
| FromResponse          | set          | no       |
+-----------------------+--------------+----------+

The Source element specifies the pre-synaptic population or selection
(see Selection) of the projection and all the port connections it
receives. The source population is specified via a Reference element
since it should not be defined within the Projection. The source
population can receive incoming port connections from the post-synaptic
response (see FromResponse), the plasticity rule (see FromPlasticity) or
the post-synaptic population directly (see FromDestination). Connections
with these ports are only made if the Connectivitydetermines that the
source and destination cells should be connected.

Destination
-----------

+-----------------------+--------------+----------+
| Child elements        | Multiplicity | Required |
+-----------------------+--------------+----------+
| [Component,Reference] | singleton    | yes      |
| FromSource            | set          | no       |
| FromPlasticity        | set          | no       |
| FromResponse          | set          | no       |
+-----------------------+--------------+----------+

The Destination element specifies the post-synaptic or selection (see
Selection) population of the projection and all the port connections it
receives. The destination population is specified via a Reference
element since it should not be defined within the Projection. The source
population can receive incoming port connections from the post-synaptic
response (see FromResponse), the plasticity rule (see FromPlasticity) or
the pre-synaptic population directly (see FromSource). Connections with
these ports are only made if the Connectivitydetermines that the source
and destination cells should be connected.

Response
--------

+-----------------------+--------------+----------+
| Child elements        | Multiplicity | Required |
+-----------------------+--------------+----------+
| [Component,Reference] | singleton    | yes      |
| FromSource            | set          | no       |
| FromDestination       | set          | no       |
| FromPlasticity        | set          | no       |
+-----------------------+--------------+----------+

The Response defines the effect on the post-synaptic cell dynamics of an
incoming synaptic input. The additional dynamics are defined by a
Componentelement, which can be defined inline or referenced. For static
connections (i.e. those without a Plasticity element), the magnitude of
the response (i.e. synaptic weight) is typically passed as a property of
the Response element.

The post-synaptic response dynamics can receive incoming port
connections from the plasticity rule (see FromPlasticity) or the pre or
post synaptic populations (see FromSource and FromDestination). The
post-synaptic response object is implicitly created and connected to
these ports if the Connectivitydetermines that the source and
destination cells should be connected.

Plasticity
----------

+-----------------------+--------------+----------+
| Child elements        | Multiplicity | Required |
+-----------------------+--------------+----------+
| [Component,Reference] | singleton    | yes      |
| FromSource            | set          | no       |
| FromDestination       | set          | no       |
| FromResponse          | set          | no       |
+-----------------------+--------------+----------+

The Plasticity element describes the dynamic processes that modulate the
dynamics of the post-synaptic response, typically the magnitude of the
response (see [sec:Response]). If the synapse is not plastic the
Plasticity element can be omitted.

The plasticity dynamics can receive incoming port connections from the
post-synaptic response rule (see FromResponse) or the pre or post
synaptic populations (see FromSource and FromDestination). The
plasticity object is implicitly created and connected to these ports if
the Connectivitydetermines that the source and destination cells should
be connected.

FromSource
----------

+-----------+------------------------------------------------------------+----------+
| Attribute | Type/Format                                                | Required |
+-----------+------------------------------------------------------------+----------+
| sender    | [AnalogSendPort,EventSendPort]@name                        | yes      |
| receiver  | [AnalogReceivePort,EventReceivePort,AnalogReducePort]@name | yes      |
+-----------+------------------------------------------------------------+----------+

The FromSource element specifies a port connection to the projection
component (either the destination cell, post-synaptic response or
plasticity dynamics) inside which it is inserted from the source cell
dynamics.

Sender attribute
~~~~~~~~~~~~~~~~

Each FromSource element requires a *sender* attribute. This should refer
to the name of a AnalogSendPort or EventSendPort in the Cellof the
source population. The transmission mode of the port (i.e. analog or
event) should match that of the port referenced by the *receiver*
attribute.

Receiver attribute
~~~~~~~~~~~~~~~~~~

| Each FromSource element requires a *receiver* attribute. This should
  refer to the name of a AnalogReceivePort, EventReceivePort or
  AnalogReducePort in the Componentin the enclosing
| Source/Destination/Plasticity/Response element. The transmission mode
  of the port (i.e. analog or event) should match that of the port
  referenced by the *sender* attribute.

FromDestination
---------------

+-----------+------------------------------------------------------------+----------+
| Attribute | Type/Format                                                | Required |
+-----------+------------------------------------------------------------+----------+
| sender    | [AnalogSendPort,EventSendPort]@name                        | yes      |
| receiver  | [AnalogReceivePort,EventReceivePort,AnalogReducePort]@name | yes      |
+-----------+------------------------------------------------------------+----------+

The FromDestination element specifies a port connection to the
projection component (either the source cell, post-synaptic response or
plasticity dynamics) inside which it is inserted from the destination
cell dynamics.

Sender attribute
~~~~~~~~~~~~~~~~

Each FromDestination element requires a *sender* attribute. This should
refer to the name of a AnalogSendPort or EventSendPort in the Cellof the
source population. The transmission mode of the port (i.e. analog or
event) should match that of the port referenced by the *receiver*
attribute.

Receiver attribute
~~~~~~~~~~~~~~~~~~

| Each FromDestination element requires a *receiver* attribute. This
  should refer to the name of a AnalogReceivePort, EventReceivePort or
  AnalogReducePort in the Componentin the enclosing
| Source/Destination/Plasticity/Response element. The transmission mode
  of the port (i.e. analog or event) should match that of the port
  referenced by the *sender* attribute.

FromPlasticity
--------------

+-----------+------------------------------------------------------------+----------+
| Attribute | Type/Format                                                | Required |
+-----------+------------------------------------------------------------+----------+
| sender    | [AnalogSendPort,EventSendPort]@name                        | yes      |
| receiver  | [AnalogReceivePort,EventReceivePort,AnalogReducePort]@name | yes      |
+-----------+------------------------------------------------------------+----------+

The FromPlasticity element specifies a port connection to the projection
component (either the source cell, destination cell or post-synaptic
response dynamics) inside which it is inserted from the plasticity
dynamics.

Sender attribute
~~~~~~~~~~~~~~~~

Each FromPlasticity element requires a *sender* attribute. This should
refer to the name of a AnalogSendPort or EventSendPort in the
Cell->Componentof the source population. The transmission mode of the
port (i.e. analog or event) should match that of the port referenced by
the *receiver* attribute.

Receiver attribute
~~~~~~~~~~~~~~~~~~

Each FromPlasticity element requires a *receiver* attribute. This should
refer to the name of a AnalogReceivePort, EventReceivePort or
AnalogReducePort in the Componentin the enclosing Source/Destination/
Plasticity/Response element. The transmission mode of the port (i.e.
analog or event) should match that of the port referenced by the
*sender* attribute.

FromResponse
------------

+-----------+------------------------------------------------------------+----------+
| Attribute | Type/Format                                                | Required |
+-----------+------------------------------------------------------------+----------+
| sender    | [AnalogSendPort,EventSendPort]@name                        | yes      |
| receiver  | [AnalogReceivePort,EventReceivePort,AnalogReducePort]@name | yes      |
+-----------+------------------------------------------------------------+----------+

The FromResponse element specifies a port connection to the projection
component (either the source cell, destination cell or plasticity
dynamics) inside which it is inserted from the post-synaptic response
dynamics.

Sender attribute
~~~~~~~~~~~~~~~~

Each FromResponse element requires a *sender* attribute. This should
refer to the name of a AnalogSendPort or EventSendPort in the
Cell->Componentof the source population. The transmission mode of the
port (i.e. analog or event) should match that of the port referenced by
the *receiver* attribute.

Receiver attribute
~~~~~~~~~~~~~~~~~~

| Each FromResponse element requires a *receiver* attribute. This should
  refer to the name of a AnalogReceivePort, EventReceivePort or
  AnalogReducePort in the Componentin the enclosing Source/Destination/
| Plasticity/Response element. The transmission mode of the port (i.e.
  analog or event) should match that of the port referenced by the
  *sender* attribute.

Delay
-----

+---------------------------------------------------------+--------------+----------+
| Attribute                                               | Type/Format  | Required |
+---------------------------------------------------------+--------------+----------+
| units                                                   | Unit@symbol  | yes      |
+---------------------------------------------------------+--------------+----------+
| Child elements                                          | Multiplicity | Required |
+---------------------------------------------------------+--------------+----------+
| [SingleValue,ArrayValue,ExternalArrayValue,RandomValue] | singleton    | yes      |
+---------------------------------------------------------+--------------+----------+

In version 1.0, the Delay element specifies the delay between the
pre-synaptic cell port and both the Plasticityand Response. In future
versions, it is planned to include the delay directly into the
port-connection objects (i.e. FromSource, FromDestination, etc...) to
allow finer control of the delay between the different components.

Units attribute
~~~~~~~~~~~~~~~

The *units* attribute specifies the units of the delay and should refer
to the name of a Unit element in the document scope. The Unitshould be
temporal, i.e. have :math:`t=1` and all other SI dimensions set to 0.

Selections: combining populations and subsets
=============================================

Selections are designed to allow sub and super-sets of cell populations
to be projected to/from other populations (or selections thereof). In
version 1.0, the only supported operation is the concatenation of
multiple populations into super-sets but in future versions it is
planned to provide “slicing” operations to select sub sets of
populations.

Selection
---------

+----------------+--------------+----------+
| Attribute      | Type/Format  | Required |
+----------------+--------------+----------+
| name           | identifier   | yes      |
+----------------+--------------+----------+
| Child elements | Multiplicity | Required |
+----------------+--------------+----------+
| Concatenate    | singleton    | yes      |
+----------------+--------------+----------+

The Selection element contains the operations that are used to select
the cells to add to the selection.

Name attribute
~~~~~~~~~~~~~~

Each Selection requires a *name* attribute, which should be a valid and
uniquely identify the Selection from all other elements in the document
scope.

Concatenate
-----------

+----------------+--------------+----------+
| Child elements | Multiplicity | Required |
+----------------+--------------+----------+
| Item           | set          | yes      |
+----------------+--------------+----------+

The Concatenate element is used to add populations to a selection. It
contains a set of Item elements which reference the Population elements
to be concatenated. The order of the Item elements does not effect the
order of the concatenation, which is determined by the *index* attribute
of the Item elements. The set of Item@\ *index* attributes must be
non-negative, contiguous, not contain any duplicates and contain the
index 0 (i.e. :math:`i=0,\ldots,N-1`).

Item
----

+---------------------------------+--------------+----------+
| Attribute                       | Type/Format  | Required |
+---------------------------------+--------------+----------+
| index                           | ``integer``  | yes      |
+---------------------------------+--------------+----------+
| Child elements                  | Multiplicity | Required |
+---------------------------------+--------------+----------+
| Reference(Population,Selection) | singleton    | yes      |
+---------------------------------+--------------+----------+

Each Item element references as a Population or Selection element and
specifies their order in the concatenation.

Index attribute
~~~~~~~~~~~~~~~

Each Item requires a *index* attribute. This attribute specifies the
order in which the Populations in the Selection are concatenated and
thereby the indices of the cells within the combined Selection.

Annotations
===========

Annotations are provided to add semantic information about the model,
preserving structure that is lost during conversion from an extended
format to core NineML, and provide suggestions for the simulation of the
model. It is highly recommended to add references to all publications on
which the model or property values are based in the annotations. For
adding semantic structure to the model it is recommended to use the
`Resource Description Framework (RDF) <http://www.w3.org/RDF/>`__
although it not a strict requirement.

In order to be compliant with the NineML specification any tool handling
NineML descriptions must preserve all existing annotations, except where
a user explicitly edits/deletes them. In future versions of this section
will be expanded to include suggested formats for commonly used
annotations.

Annotations
-----------

+----------------+--------------+----------+
| Child elements | Multiplicity | Required |
+----------------+--------------+----------+
| \*             | set          | no       |
+----------------+--------------+----------+

The Annotations element is the top-level of the annotations attached to
a NineML element. They can be included within any NineML element (User
Layer and Abstraction Layer) and any valid XML is allowed within them.

Examples
========

Izhikevich Model
----------------

In this first example, we are describing how to represent the Izhikevich
model in NineML [Izhikevich2003]_. The model is
composed of single ComponentClass, containing a single Regime,
*subthresholdRegime*, and two state variables, :math:`U` & :math:`V`.

The ODEs defined for the Regime are:

.. math::

   \begin{aligned}
   \frac{dV}{dt} &= 0.04*V*V + 5*V + 140.0 - U + i_{\mathrm{synapse}} + i_{\mathrm{injected}}  \\
   \frac{dU}{dt} &= a * ( b* V -U )\end{aligned}

The ComponentClass has a single OnCondition transition, is triggered
when :math:`V>theta`. When triggered, It causes an Event called
*spikeOutput* to be emitted, and two StateAssignments to be made:

.. math::

   \begin{aligned}
   U &\leftarrow U + d \\
   V &\leftarrow c\end{aligned}

The target-regime of the OnCondition transition is not declared
explicitly in the XML, implying that the target-regime is the same as
the source-regime, i.e. *subthresholdRegime*.

The RegimeGraph is shown in Figure [fig:EX1\_RegimeGraph]

.. figure:: figures/example_IzRegimeTransGraph.pdf
   :alt: RegimeGraph for the XML model in this section.
   :width: 8.00000cm

   RegimeGraph for the XML model in this section.

Using this Abstraction Layer definition, as well as suitable parameters
from the user layer;
:math:`a=0.02, b=0.2, c=-65, d= 8, i_{\mathrm{injected}}= 5.0`, we can
simulate this, giving output as shown in Figure [fig:Ex1\_Output].

In Figure [fig:Ex1\_Output], we can see the value of the StateVariable
:math:`V` over time. We can also see that when the value of
:math:`V>theta` triggers the condition, we emit a spike, and the
StateAssignment of :math:`V \leftarrow c` resets the value of :math:`V`.
The corresponding Abstraction Layer XML description for this model is
the following:

.. code-block:: xml

    <?xml version="1.0" encoding='UTF-8'?>
    <NineML xmlns="http://nineml.net/9ML/1.0"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://nineml.net/9ML/1.0/NineML_v1.0.xsd">
        <ComponentClass name="IzhikevichCell">
            <Parameter name="a" dimension="per_time"/>
            <Parameter name="c" dimension="voltage"/>
            <Parameter name="b" dimension="per_voltage"/>
            <Parameter name="d" dimension="dimensionless"/>
            <Parameter name="theta" dimension="voltage"/>
            <Parameter name="iInj" dimension="current"/>
            <AnalogReducePort name="iSyn" operator="+" dimension="current"/>
            <AnalogSendPort name="V" dimension="voltage"/>
            <EventPort name="spikeOutput" mode="send"/>
            <Dynamics>
                <StateVariable name="V" dimension="voltage"/>
                <StateVariable name="U" dimension="dimensionless"/>
                <Regime name="subthresholdRegime">
                    <TimeDerivative variable="U">
                        <MathInline>a*(b*V - U)</MathInline>
                    </TimeDerivative>
                    <TimeDerivative variable="V">
                        <MathInline
                            >(0.04*V*V/unitV + 5*V + (140.0 - U)*unitV + (iSyn + iInj)*unitR)/unitT</MathInline>
                    </TimeDerivative>
                    <OnCondition>
                        <Trigger>
                            <MathInline>V &gt; theta </MathInline>
                        </Trigger>
                        <StateAssignment variable="V" >
                            <MathInline>c</MathInline>
                        </StateAssignment>
                        <StateAssignment variable="U" >
                            <MathInline>U+d</MathInline>
                        </StateAssignment>
                        <OutputEvent port="spikeOutput" />
                    </OnCondition>
                </Regime>
                <Constant name="unitV" units="V">1</Constant>
                <Constant name="unitR" units="Ohm">1</Constant>
                <Constant name="unitT" units="s">1</Constant>
            </Dynamics>
        </ComponentClass>
        <Dimension name="voltage" m="1" l="2" t="-3" i="-1"/>
        <Dimension name="time" t="1"/>
        <Dimension name="current" i="1"/>
        <Dimension name="dimensionless"/>
        <Dimension name="resistance" m="1" l="2" t="-3" i="-2"/>
        <Dimension name="per_voltage" m="-1" l="-2" t="3" i="1"/>   
        <Dimension name="per_time" t="-1"/> 
        <Unit symbol="V" dimension="voltage"/>
        <Unit symbol="Ohm" dimension="resistance"/>
        <Unit symbol="s" dimension="time" power="1"/>
    </NineML>

  User Layer description for the above example:

.. code-block:: xml

    <?xml version='1.0' encoding='UTF-8'?>
    <NineML xmlns="http://nineml.net/9ML/1.0"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://nineml.net/9ML/1.0/NineML_v1.0.xsd">
      <Component name="IzhikevichNeuron">
        <Definition url="http://nineml.net/9ML/1.0/catalog/neurons/izhikevichCell.9ml"
          >IzhikevichCell</Definition>
        <Property name="theta" units="mV">
          <SingleValue>50</SingleValue>
        </Property>
        <Property name="a" units="per_s">
          <SingleValue>0.02</SingleValue>
        </Property>
        <Property name="b" units="per_V">
          <SingleValue>0.2</SingleValue>
        </Property>
        <Property name="c" units="mV">
          <SingleValue>-65</SingleValue>
        </Property>
        <Property name="d" units="none">
          <SingleValue>8</SingleValue>
        </Property>
      </Component>
      <Dimension name="voltage" m="1" l="2" t="-3" i="-1"/>
      <Dimension name="dimensionless"/>
      <Dimension name="per_time" t="-1"/>
      <Dimension name="per_voltage" m="-1" l="-2" t="3" i="1"/>  
      <Unit symbol="mV" dimension="voltage" power="-3"/>
      <Unit symbol="per_V" dimension="per_voltage"/>
      <Unit symbol="per_s" dimension="per_time"/>
      <Unit symbol="none" dimension="dimensionless"/>  
    </NineML>

Here, we show the simulation results of this XML representation with an
initial V=-60mV and U=0.

.. figure:: figures/example_IzVoltageWave.pdf
   :alt: Result of simulating of the XML model in this section
   :width: 8.00000cm

   Result of simulating of the XML model in this section

Leaky Integrate and Fire model
------------------------------

In this example, we build a representation of a integrate-and-fire
neuron, with an attached input synapse [Abbott1999]_.
We have a single StateVariable, *iaf\_V*. This time, the neuron has an
absolute refractory period; which is implemented by using 2 regimes.
*RegularRegime* & *RefractoryRegime* In *RegularRegime*, the neuron
voltage evolves as:

.. math::

   \begin{aligned}
   \frac{d(iaf\_V)}{dt} = \frac{ iaf\_gl*( iaf\_vrest - iaf\_V ) + iaf\_ISyn+cobaExcit\_I} {iaf\_cm}\end{aligned}

 In *RefractoryRegime*, the neuron voltage does not change in response
to any input:

.. math::

   \begin{aligned}
   \frac{d(iaf\_V)}{dt} = 0\end{aligned}

In both Regimes, the synapses dynamics evolve as:

.. math::

   \begin{aligned}
   \frac{d(cobaExcit\_g)}{dt} = - \frac{cobaExcit\_g}{cobaExcit\_tau}\end{aligned}

 The neuron has 2 EventPorts, *iaf\_spikeoutput* is a send port, which
sends events when the neuron fires, and *cobaExcit\_spikeinput* is a
recv port, which tells the attached synapse that it should ‘fire’. The
neuron has 4 transitions, 2 OnEvent transitions and 2 OnCondition
transitions. Two of the Transitions are triggered by
*cobaExcit\_spikeinput* events, which cause the conductance of the
synapse to increase by an amount :math:`q`, These happen in both
Regimes. The other OnConditions:

-  One is triggered the voltage being above threshold, which moves the
   component from *RegularRegime* to *RefractoryRegime*, sets V to the
   reset-voltage also emits a spike

-  The other is triggered by enough time having passed for the component
   to come out of the *RefractoryRegime* and move back to the
   *RegularRegime*

The corresponding Regime Graph is shown in Figure 5.

.. figure:: figures/demo2_Coba1_trnasition.pdf
   :alt: RegimeGraph for the XML model in this section
   :width: 14.00000cm

   RegimeGraph for the XML model in this section

The resulting XML description for the Abstraction Layer is :

.. code-block:: xml

    <?xml version='1.0' encoding='UTF-8'?>
    <NineML xmlns="http://nineml.net/9ML/1.0"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://nineml.net/9ML/1.0/NineML_v1.0.xsd">
      <ComponentClass name="IafCoba">
        <AnalogSendPort dimension="voltage" name="iaf_V" />
        <AnalogReducePort dimension="current" operator="+" name="iaf_ISyn" />
        <AnalogSendPort dimension="current" name="cobaExcit_I" />
        <EventSendPort name="iaf_spikeoutput"/>
        <EventReceivePort name="cobaExcit_spikeinput"/>
        <Parameter dimension="area" name="iaf_cm"/>
        <Parameter dimension="time" name="iaf_taurefrac"/>
        <Parameter dimension="conductanceDensity" name="iaf_gl"/>
        <Parameter dimension="voltage" name="iaf_vreset"/>
        <Parameter dimension="voltage" name="iaf_vrest"/>
        <Parameter dimension="voltage" name="iaf_vthresh"/>
        <Parameter dimension="time" name="cobaExcit_tau"/>
        <Parameter dimension="conductanceDensity" name="cobaExcit_q"/>
        <Parameter dimension="voltage" name="cobaExcit_vrev"/>
        <Dynamics>
          <StateVariable dimension="voltage" name="iaf_V"/>
          <StateVariable dimension="time" name="iaf_tspike"/>
          <StateVariable dimension="conductanceDensity" name="cobaExcit_g"/>
          <Regime name="RefractoryRegime">
            <TimeDerivative variable="iaf_V">
              <MathInline>0</MathInline>
            </TimeDerivative>
            <TimeDerivative variable="cobaExcit_g">
              <MathInline>-cobaExcit_g/cobaExcit_tau</MathInline>
            </TimeDerivative>
            <OnEvent target_regime="RefractoryRegime" src_port="cobaExcit_spikeinput">
              <StateAssignment variable="cobaExcit_g">
                <MathInline>cobaExcit_g+cobaExcit_q</MathInline>
              </StateAssignment>
            </OnEvent>
            <OnCondition target_regime="RegularRegime">
              <Trigger>
                <MathInline>t &gt; iaf_tspike + iaf_taurefrac</MathInline>
              </Trigger>
            </OnCondition>
          </Regime>
          <Regime name="RegularRegime">
            <TimeDerivative variable="iaf_V">
              <MathInline>( iaf_gl*( iaf_vrest - iaf_V ) + iaf_ISyn+cobaExcit_I)/(iaf_cm)</MathInline>
            </TimeDerivative>
            <TimeDerivative variable="cobaExcit_g">
              <MathInline>-cobaExcit_g/cobaExcit_tau</MathInline>
            </TimeDerivative>
            <OnEvent target_regime="RegularRegime" src_port="cobaExcit_spikeinput">
              <StateAssignment variable="cobaExcit_g">
                <MathInline>cobaExcit_g+cobaExcit_q</MathInline>
              </StateAssignment>
            </OnEvent>
            <OnCondition target_regime="RefractoryRegime">
              <StateAssignment variable="iaf_tspike">
                <MathInline>t</MathInline>
              </StateAssignment>
              <StateAssignment variable="iaf_V">
                <MathInline>iaf_vreset</MathInline>
              </StateAssignment>
              <OutputEvent port="iaf_spikeoutput"/>
              <Trigger>
                <MathInline>iaf_V &gt; iaf_vthresh</MathInline>
              </Trigger>
            </OnCondition>
          </Regime>
          <Alias name="cobaExcit_I">
            <MathInline>cobaExcit_g*(cobaExcit_vrev-iaf_V)</MathInline>
          </Alias>
        </Dynamics>
      </ComponentClass>
      <Dimension name="time" t="1"/>
      <Dimension name="voltage" m="1" l="2" t="-3" i="-1"/>
      <Dimension name="conductanceDensity" m="-1" t="3" l="-2" i="2"/>
      <Dimension name="area" l="2"/>
    </NineML>

 

The User Layer description for the above example:

.. code-block:: xml

    <?xml version='1.0' encoding='UTF-8'?>
    <NineML xmlns="http://nineml.net/9ML/1.0"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://nineml.net/9ML/1.0/NineML_v1.0.xsd">
      <Component name="IaFNeuron">
        <Definition url="http://nineml.net/catalog/neurons/IafCoba.9ml"
          >IafCoba</Definition>
        <Property name="iaf_V" units="mV">
          <SingleValue>-60</SingleValue>
        </Property>
        <Property name="iaf_tspike" units="ms">
          <SingleValue>-1</SingleValue>
        </Property>
        <Property name="cobaExcit_g" units="mS">
          <SingleValue>0</SingleValue>
        </Property>
        <Property name="iaf_cm" units="cm_square">
          <SingleValue>0.02</SingleValue>
        </Property>
        <Property name="iaf_taurefrac" units="ms">
          <SingleValue>3</SingleValue>
        </Property>
        <Property name="iaf_gl" units="mS">
          <SingleValue>0.1</SingleValue>
        </Property>
        <Property name="iaf_vreset" units="mV">
          <SingleValue>-70</SingleValue>
        </Property>
        <Property name="iaf_vrest" units="mV">
          <SingleValue>-60</SingleValue>
        </Property>
        <Property name="iaf_vthresh" units="mV">
          <SingleValue>20</SingleValue>
        </Property>
        <Property name="cobaExcit_tau" units="ms">
          <SingleValue>2</SingleValue>
        </Property>
        <Property name="cobaExcit_q" units="ms">
          <SingleValue>1</SingleValue>
        </Property>
        <Property name="cobaExcit_vrev" units="mV">
          <SingleValue>0</SingleValue>
        </Property>
      </Component>
      <Dimension name="time" t="1"/>
      <Dimension name="voltage" m="1" l="2" t="-3" i="-1"/>
      <Dimension name="conductanceDensity" m="-1" t="3" l="-2" i="2"/>
      <Dimension name="area" l="2"/>
      <Unit symbol="mV" dimension="voltage" power="-3"/>
      <Unit symbol="ms" dimension="time" power="-3"/>
      <Unit symbol="cm_square" dimension="area" power="-4"/>
      <Unit symbol="mS" dimension="conductanceDensity" power="-3"/>
    </NineML>

 

The simulation results is presented in Figure 6.

.. figure:: figures/demo2_Coba1_out.pdf
   :alt: Result of simulating of the XML model in this section.
   *cobaExcit\_spikeinput* is fed events from an external Poisson
   generator in this simulation
   :width: 14.00000cm

   Result of simulating of the XML model in this section.
   *cobaExcit\_spikeinput* is fed events from an external Poisson
   generator in this simulation

COBA IAF Network example
------------------------

This example is an implementation of *Benchmark 1* from
[Brette2009]_, which consists of a network of an
excitatory and inhibitory IAF populations randomly connected with COBA
synapses [Vogels2005]_. The excitatory and inhibitory
Population elements are created with 3,200 and 800 cells respectively.
Both populations are then concatenated into a single Selection element,
“AllNeurons”, which is used to randomly connect both populations to
every other neuron in the network with a 2% probability.

The abstraction layer description of the IAF input neuron
ComponentClassis:

.. code-block:: xml

    <?xml version='1.0' encoding='UTF-8'?>
    <NineML xmlns="http://nineml.net/9ML/1.0"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://nineml.net/9ML/1.0/NineML_v1.0.xsd">
      <ComponentClass name="IaF">
        <AnalogSendPort dimension="voltage" name="iaf_V" />
        <AnalogReducePort dimension="current" operator="+" name="iaf_ISyn" />
        <EventSendPort name="iaf_spikeoutput"/>
        <Parameter dimension="area" name="iaf_cm"/>
        <Parameter dimension="time" name="iaf_taurefrac"/>
        <Parameter dimension="voltage" name="iaf_vreset"/>
        <Parameter dimension="voltage" name="iaf_vrest"/>
        <Parameter dimension="voltage" name="iaf_vthresh"/>
        <Parameter dimension="conductanceDensity" name="iaf_gl"/>
        <Dynamics>
          <StateVariable dimension="voltage" name="iaf_V"/>
          <StateVariable dimension="time" name="iaf_tspike"/>
          <Regime name="RefractoryRegime">
            <TimeDerivative variable="iaf_V">
              <MathInline>0</MathInline>
            </TimeDerivative>
            <OnCondition target_regime="RegularRegime">
              <Trigger>
                <MathInline>t &gt; iaf_tspike + iaf_taurefrac</MathInline>
              </Trigger>
            </OnCondition>
          </Regime>
          <Regime name="RegularRegime">
            <TimeDerivative variable="iaf_V">
              <MathInline>( iaf_gl*( iaf_vrest - iaf_V ) + iaf_ISyn)/(iaf_cm)</MathInline>
            </TimeDerivative>
            <OnCondition target_regime="RefractoryRegime">
              <StateAssignment variable="iaf_tspike">
                <MathInline>t</MathInline>
              </StateAssignment>
              <StateAssignment variable="iaf_V">
                <MathInline>iaf_vreset</MathInline>
              </StateAssignment>
              <OutputEvent port="iaf_spikeoutput"/>
              <Trigger>
                <MathInline>iaf_V &gt; iaf_vthresh</MathInline>
              </Trigger>
            </OnCondition>
          </Regime>
        </Dynamics>
      </ComponentClass>
      <Dimension name="time" t="1"/>
      <Dimension name="voltage" m="1" l="2" t="-3" i="-1"/>
      <Dimension name="conductanceDensity" m="-1" t="3" l="-2" i="2"/>
      <Dimension name="area" l="2"/>
    </NineML>

 

and the description of the COBA ComponentClassis:

.. code-block:: xml

    <?xml version='1.0' encoding='UTF-8'?>
    <NineML xmlns="http://nineml.net/9ML/1.0"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://nineml.net/9ML/1.0/NineML_v1.0.xsd">
        <ComponentClass name="CoBa">
            <EventReceivePort name="coba_spikeinput"/>
            <AnalogReceivePort name="coba_vrev"/>
            <AnalogSendPort dimension="current" name="coba_I" />
            <Parameter dimension="time" name="coba_tau"/>
            <Parameter dimension="conductanceDensity" name="coba_q"/>
            <Dynamics>
                <StateVariable dimension="conductanceDensity" name="coba_g"/>
                <Regime name="RegularRegime">
                    <OnEvent target_regime="RegularRegime" src_port="coba_spikeinput">
                        <StateAssignment variable="coba_g">
                            <MathInline>coba_g+coba_q</MathInline>
                        </StateAssignment>
                    </OnEvent>
                    <TimeDerivative variable="coba_g">
                        <MathInline>-coba_g/coba_tau</MathInline>
                    </TimeDerivative>
                </Regime>
                <Alias name="coba_I">
                    <MathInline>coba_g*(coba_vrev-iaf_V)</MathInline>
                </Alias>
            </Dynamics>
        </ComponentClass>
        <Dimension name="time" t="1"/>
        <Dimension name="voltage" m="1" l="2" t="-3" i="-1"/>
        <Dimension name="conductanceDensity" m="-1" t="3" l="-2" i="2"/>
        <Dimension name="area" l="2"/>
    </NineML>

 

The cell Componentare parameterized and connected together in the User
Layer via Population, Selection and Projection elements:

.. code-block:: xml

    <?xml version='1.0' encoding='UTF-8'?>
    <NineML xmlns="http://nineml.net/9ML/1.0"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://nineml.net/9ML/1.0/NineML_v1.0.xsd">
      <Component name="IaFNeuron">
        <Definition url="./iaf.9ml"
          >IaF</Definition>
        <Property name="iaf_V" units="mV">
          <SingleValue>-60</SingleValue>
        </Property>
        <Property name="iaf_tspike" units="ms">
          <SingleValue>-1</SingleValue>
        </Property>
        <Property name="iaf_cm" units="cm_square">
          <SingleValue>0.2</SingleValue>
        </Property>
        <Property name="iaf_taurefrac" units="ms">
          <SingleValue>5</SingleValue>
        </Property>
        <Property name="iaf_gl" units="mS">
          <SingleValue>0.05</SingleValue>
        </Property>
        <Property name="iaf_vreset" units="mV">
          <SingleValue>-60</SingleValue>
        </Property>
        <Property name="iaf_vrest" units="mV">
          <SingleValue>-60</SingleValue>
        </Property>
        <Property name="iaf_vthresh" units="mV">
          <SingleValue>-50</SingleValue>
        </Property>
      </Component>
      <Component name="IaFSynapseExcitatory">
        <Definition url="./coba.9ml">CoBa</Definition>
        <Property name="coba_g" units="mS">
          <SingleValue>0</SingleValue>
        </Property>
        <Property name="coba_tau" units="ms">
          <SingleValue>5</SingleValue>
        </Property>
        <Property name="coba_q" units="ms">
          <SingleValue>0.004</SingleValue>
        </Property>
        <Property name="coba_vrev" units="mV">
          <SingleValue>0</SingleValue>
        </Property>
      </Component>
      <Component name="IaFSynapseInhibitory">
        <Definition url="./coba.xml">CoBa</Definition>
        <Property name="coba_g" units="mS">
          <SingleValue>0</SingleValue>
        </Property>
        <Property name="coba_tau" units="ms">
          <SingleValue>5</SingleValue>
        </Property>
        <Property name="coba_q" units="ms">
          <SingleValue>0.051</SingleValue>
        </Property>
        <Property name="coba_vrev" units="mV">
          <SingleValue>-80</SingleValue>
        </Property>
      </Component>
      <Population name="Excitatory">
        <Size>3200</Size>
        <Cell>
            <Reference>IaFNeuron</Reference>
        </Cell>
      </Population>
      <Population name="Inhibitory">
        <Size>800</Size>
        <Cell>
            <Reference>IaFNeuron</Reference>
        </Cell>
      </Population>
      <Selection name="AllNeurons">
        <Concatonate>
            <Item index="0">Excitatory</Item>
            <Item index="1">Inhibitory</Item>
        </Concatonate>
      </Selection>
      <Projection>
        <Source>
            <Reference>Excitatory</Reference>
        </Source>
        <Destination>
            <Reference>AllNeurons</Reference>
        </Destination>
        <Response>
            <Reference>IaFSynapseExcitatory</Reference>
            <FromDestination sender="iaf_V" receiver="coba_vrev"/>
        </Response>
        <Connectivity>
            <Component>
                <Definition url="http://nineml.net/9ML/1.0/catalog/connectionrules/Probabilistic.9ml"
                    >Probabilistic</Definition>
                <Property name="probability" units="unitless">
                    <SingleValue>0.02</SingleValue>
                </Property>
            </Component>
        </Connectivity>
      </Projection>
      <Projection>
        <Source>
            <Reference>Inhibitory</Reference>
        </Source>
        <Destination>
            <Reference>AllNeurons</Reference>
        </Destination>
        <Response>
            <Reference>IaFSynapseInhibitory</Reference>
            <FromDestination sender="iaf_V" receiver="coba_vrev"/>
        </Response>
        <Connectivity>
            <Component>
                <Definition url="http://nineml.net/9ML/1.0/catalog/connectionrules/Probabilistic.9ml"
                    >Probabilistic</Definition>
                <Property name="probability" units="unitless">
                    <SingleValue>0.02</SingleValue>
                </Property>             
            </Component>
        </Connectivity>
      </Projection>
      <Unit symbol="mV" dimension="voltage" power="-3"/>
      <Unit symbol="ms" dimension="time" power="-3"/>
      <Unit symbol="cm_square" dimension="area" power="-4"/>
      <Unit symbol="mS" dimension="conductanceDensity" power="-3"/>
      <Unit name="unitless" dimension="dimensionless" power="0"/>
      <Dimension name="time" t="1"/>
      <Dimension name="voltage" m="1" l="2" t="-3" i="-1"/>
      <Dimension name="conductanceDensity" m="-1" t="3" l="-2" i="2"/>
      <Dimension name="area" l="2"/>
      <Dimension name="dimensionless"/>
    </NineML>

Acknowledgments
===============

Former NineML INCF Task Force members
-------------------------------------

-  Robert Cannon

-  Robert Clewley

-  Alex Cope

-  Hugo Cornelis

-  Andrew P. Davison

-  Erik De Schutter

-  Mikael Djurfeldt

-  Damien Drix

-  Hans Ekkehard Plesser

-  Padraig Gleeson

-  Anatoli Gorchetchnikov

-  Valentin Haenel

-  Sean Hill

-  Michael Hull

-  Birgit Kriener

-  Yann Le Franc

-  Chung-Chua Lo

-  Abigail Morrison

-  Eilif Muller

-  Dragan Nikolic

-  Ivan Raikov

-  Subhasis Ray

-  Raphael Ritz

-  Malin Sandström

-  Lars Schwabe

.. |image| image:: figures/incf_new.png
.. |image| image:: figures/by-nc-sa.png
   :width: 3.00000cm
