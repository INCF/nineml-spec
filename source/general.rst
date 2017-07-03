****************
General Elements
****************

Document Layout
===============

NineML documents must be enclosed within an NineML element, which should
be in the ’http://nineml.net/9ML/1.0’ XML namespace.

NineML
------


+-----------+-----------------------------+----------+
| Attribute | Type/Format                 | Required |
+===========+=============================+==========+
| xmlns     | ‘http://nineml.net/9ML/1.0’ | yes      |
+-----------+-----------------------------+----------+


+-----------------------+--------------+----------+
| Children              | Multiplicity | Required |
+=======================+==============+==========+
| :ref:`Component`      | set          | no       |
+-----------------------+--------------+----------+
| :ref:`ComponentClass` | set          | no       |
+-----------------------+--------------+----------+
| Unit_                 | set          | no       |
+-----------------------+--------------+----------+
| Dimension_            | set          | no       |
+-----------------------+--------------+----------+
| :ref:`Population`     | set          | no       |
+-----------------------+--------------+----------+
| :ref:`Projection`     | set          | no       |
+-----------------------+--------------+----------+
| :ref:`Selection`      | set          | no       |
+-----------------------+--------------+----------+

Seven *document-level* elements are allowed to reside directly within
NineML elements: :ref:`Component`, :ref:`ComponentClass`, Unit_, Dimension_, :ref:`Population`,
:ref:`Projection` and :ref:`Selection`. Each element should be uniquely identified by
its *name* attribute within the scope of the document (see ).

Unit_ and Dimension_ elements must be defined within the document they are
referenced, whereas the remaining element types can also be referenced
from other NineML documents (see :ref:`Reference` and :ref:`Definition`).

Xmlns attribute
^^^^^^^^^^^^^^^

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

.. note::
    The format for units and dimensions is the same as is used for LEMS/NeuroML
    v2.0 (http://www.neuroml.org) [Cannon2014]_.

Dimension
---------

+-----------+-------------+----------+
| Attribute | Type/Format | Required |
+===========+=============+==========+
| name      | identifier  | yes      |
+-----------+-------------+----------+
| m         | ``integer`` | no       |
+-----------+-------------+----------+
| l         | ``integer`` | no       |
+-----------+-------------+----------+
| t         | ``integer`` | no       |
+-----------+-------------+----------+
| i         | ``integer`` | no       |
+-----------+-------------+----------+
| n         | ``integer`` | no       |
+-----------+-------------+----------+
| k         | ``integer`` | no       |
+-----------+-------------+----------+
| j         | ``integer`` | no       |
+-----------+-------------+----------+

Dimension_ objects are constructed values from the powers for each of the
seven SI base units: length (*l*), mass (*m*), time (*t*), electric
current (*i*), temperature (*k*), luminous intensity (*l*) and amount of
substance (*n*). For example, acceleration has dimension :math:`lt^{-2}`
and voltage is :math:`ml^2t^3i^{-1}`. Dimension_ objects must be declared
in the top-level scope of the NineML document where they are referenced.

Name attribute
^^^^^^^^^^^^^^

Each Dimension_ requires a *name* attribute, which should be a valid and
uniquely identify the Dimension_ in current the scope.

M attribute
^^^^^^^^^^^

The *m* attribute specifies the power of the mass dimension in the
Dimension_. If omitted the power is zero.

L attribute
^^^^^^^^^^^

The *l* attribute specifies the power of the length dimension in the
Dimension_. If omitted the power is zero.

T attribute
^^^^^^^^^^^

The *t* attribute specifies the power of the time dimension in the
Dimension_. If omitted the power is zero.

I attribute
^^^^^^^^^^^

The *i* attribute specifies the power of the current dimension in the
Dimension_. If omitted the power is zero.

N attribute
^^^^^^^^^^^

The *n* attribute specifies the power of the amount-of-substance
dimension in the Dimension_. If omitted the power is zero.

K attribute
^^^^^^^^^^^

The *k* attribute specifies the power of the temperature dimension in
the Dimension_. If omitted the power is zero.

J attribute
^^^^^^^^^^^

The *j* attribute specifies the power of the luminous-intensity
dimension in the Dimension_. If omitted the power is zero.

Unit
----

+-----------+-----------------+----------+
| Attribute | Type/Format     | Required |
+===========+=================+==========+
| symbol    | ``string``      | yes      |
+-----------+-----------------+----------+
| dimension | Dimension_.name | yes      |
+-----------+-----------------+----------+
| power     | ``integer``     | no       |
+-----------+-----------------+----------+
| offset    | ``integer``     | no       |
+-----------+-----------------+----------+

Unit_ objects specify the dimension multiplier and the offset of a unit
with respect to a defined Dimension_ object. Unit_ objects must be
declared in the top-level scope of the NineML documents where they are
referenced.

Symbol attribute
^^^^^^^^^^^^^^^^

Each Unit_ requires a *symbol* attribute, which should be a valid and
uniquely identify the Unit_ in current the scope.

Dimension attribute
^^^^^^^^^^^^^^^^^^^

Each Unit_ requires a *dimension* attribute. This attribute specifies the
dimension of the units and should refer to the name of a Dimension_
element in the document scope.

Power attribute
^^^^^^^^^^^^^^^

Each Unit_ requires a *power* attribute. This attribute specifies the
relative scale of the units compared to the equivalent SI units in
powers of ten. If omitted the power is zero.

Offset attribute
^^^^^^^^^^^^^^^^

A Unit_ can optionally have an *offset* attribute. This attribute
specifies the zero offset of the unit scale. For example,

.. code-block:: xml

    <Unit name="degC" dimension="temperature" power="0" offset="273.15"/>

If omitted, the offset is zero.


Annotating Elements
===================

Annotations are provided to add semantic information about the model,
preserving structure that is lost during conversion from an extended
format to core NineML, and provide suggestions for the simulation of the
model. It is highly recommended to add references to all publications on
which the model or property values are based in the annotations. For
adding semantic structure to the model it is recommended to use the
`Resource Description Framework (RDF) <http://www.w3.org/RDF/>`__
although it is not a strict requirement.

In order to be compliant with the NineML specification any tool handling
NineML descriptions must preserve all existing annotations, except where
a user explicitly edits/deletes them. In future versions of this section
will be expanded to include suggested formats for commonly used
annotations.

Annotations
-----------

+----------+--------------+----------+
| Children | Multiplicity | Required |
+==========+==============+==========+
| \*       | set          | no       |
+----------+--------------+----------+

The Annotations_ element is the top-level of the annotations attached to
a NineML element. They can be included within any NineML element (User
Layer and Abstraction Layer) and any valid XML is allowed within them.


.. [Cannon2014] Cannon, R.~C., Gleeson, P., Crook, S., Ganapathy, G.,
   Marin, B., Piasini, E., and Silver, R.~A. (2014).
   LEMS: a language for expressing complex biological models in concise
   and hierarchical form and its use in underpinning NeuroML 2.
   *Frontiers in neuroinformatics*, 8(September):79.
