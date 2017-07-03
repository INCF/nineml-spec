

**********
User Layer
**********

Components and Properties
=========================

Component
---------

+-----------+-------------+----------+
| Attribute | Type/Format | Required |
+===========+=============+==========+
| name      | identifier  | yes      |
+-----------+-------------+----------+

+------------------------+--------------+----------+
| Children               | Multiplicity | Required |
+========================+==============+==========+
| [Definition,Prototype] | singleton    | yes      |
+------------------------+--------------+----------+
| Property               | set          | no       |
+------------------------+--------------+----------+

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
^^^^^^^^^^^^^^

Each Component requires a *name* attribute, which should be a valid and
uniquely identify the Component from all other elements in the document
scope.

Definition
----------

+-----------+-----------------------------------------------------------------+----------+
| Attribute | Type/Format                                                     | Required |
+===========+=================================================================+==========+
| url       | `URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__ | no       |
+-----------+-----------------------------------------------------------------+----------+

+---------------------+----------+
| Body format         | Required |
+=====================+==========+
| ComponentClass.name | yes      |
+---------------------+----------+

The Definition element establishes a link between a User Layer component
and Abstraction Layer ComponentClass. This ComponentClass can be located
either in the current document or in another file if a *url* attribute
is provided.

Url attribute
^^^^^^^^^^^^^

If the ComponentClass referenced by the definition element is defined
outside the current document, the *url* attribute specifies a
`URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__\ for the
file which contains the ComponentClass definition. If the *url*
attribute is omitted the ComponentClass is referenced from the current
document.

Body
^^^^

The name of the ComponentClass to be referenced ComponentClass needs to
be provided in the body of the Definition element.

Prototype
---------

+-----------+-----------------------------------------------------------------+----------+
| Attribute | Type/Format                                                     | Required |
+===========+=================================================================+==========+
| url       | `URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__ | no       |
+-----------+-----------------------------------------------------------------+----------+

+----------------+----------+
| Body format    | Required |
+================+==========+
| Component.name | yes      |
+----------------+----------+

The Prototype element establishes a link to an existing User Layer
Component, which defines the ComponentClass and default properties of
the Component. The reference Component can be located either in the
current document or in another file if a *url* attribute is provided.

Url attribute
^^^^^^^^^^^^^

If the prototype Component is defined outside the current file, the
*URL* attribute specifies a
`URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__\ for the
file which contains the prototype Component. If the *url* attribute is
omitted the Component is referenced from the current document.

Body
^^^^

The name of the Component to be referenced Component needs to be
provided in the body of the Prototype element.

Property
--------

+-----------+----------------+----------+
| Attribute | Type/Format    | Required |
+===========+================+==========+
| name      | Parameter.name | yes      |
+-----------+----------------+----------+
| units     | Unit.symbol    | yes      |
+-----------+----------------+----------+

+---------------------------------------------------------+--------------+----------+
| Children                                                | Multiplicity | Required |
+=========================================================+==============+==========+
| [SingleValue,ArrayValue,ExternalArrayValue,RandomValue] | singleton    | yes      |
+---------------------------------------------------------+--------------+----------+

Property elements provide values for the parameters defined in the
ComponentClass of the Component. Their *name* attribute should match the
name of the corresponding Parameter element in the ComponentClass. The
Property should be provided units that match the dimensionality of the
corresponding Parameter definition.

Name attribute
^^^^^^^^^^^^^^

Each Property requires a *name* attribute. This should refer to the name
of a Parameter in the corresponding ComponentClass of the Component.

Units attribute
^^^^^^^^^^^^^^^

Each Property element requires a *units* attribute. The *units*
attribute specifies the units of the quantity and should refer to the
name of a Unit element in the document scope. For a dimensionless units
a Unitwith no SI dimensions can be used. The SI dimensions of the
Unitshould match the SI dimensions of the corresponding Parameter.

.. note::
    "Dimensionless" parameters can be defined by referring to an empty
    Dimension object, i.e. one without any power or offset attributes

Reference
---------

+-----------+-----------------------------------------------------------------+----------+
| Attribute | Type/Format                                                     | Required |
+===========+=================================================================+==========+
| url       | `URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__ | no       |
+-----------+-----------------------------------------------------------------+----------+

+-------------+----------+
| Body format | Required |
+=============+==========+
| \*.name     | yes      |
+-------------+----------+

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
^^^^^^^^^^^^^

The *url* attribute specifies a
`URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__\ for the
file which contains the User Layer element to be referenced. If the
*url* attribute is omitted the element is referenced from the current
document.

Body
^^^^

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
| Body format | Required |
+=============+==========+
| ``integer`` | yes      |
+-------------+----------+

A SingleValue element represents an array filled with a single value.

Body
^^^^

Any valid numeric value in `ANSI
C89 <http://en.wikipedia.org/wiki/ANSI_C>`__, including shorthand
scientific notation e.g. 1e-5 (:math:`1\times10^{-5}`).

ArrayValue
----------


+---------------+--------------+----------+
| Children      | Multiplicity | Required |
+===============+==============+==========+
| ArrayValueRow | set          | no       |
+---------------+--------------+----------+

ArrayValue elements are used to represent an explicit array of values in
XML. ArrayValue elements contain a set of ArrayValueRow elements (i.e.
unordered, since they are explicitly ordered by their *index*
attribute). Since XML is significantly slower to parse than plain text
and binary formats it is not recommended to use ArrayValue for large
arrays, preferring ExternalArrayValue instead.

ArrayValueRow
-------------

+-----------+-------------+----------+
| Attribute | Type/Format | Required |
+-----------+-------------+----------+
| index     | ``integer`` | yes      |
+-----------+-------------+----------+

+-------------+----------+
| Body format | Required |
+=============+==========+
| ``integer`` | yes      |
+-------------+----------+

ArrayValueRow elements represent the numerical values of the explicit
ArrayValue element.

Index attribute
^^^^^^^^^^^^^^^

The *index* attribute specifies the index of the ArrayValueRow in the
ArrayValue. It must be non-negative, unique amongst the set of
ArrayValueRow.index in the list, and the set of indices must be
contiguous for a single ArrayValue.

Body
^^^^

Any valid numeric value in `ANSI
C89 <http://en.wikipedia.org/wiki/ANSI_C>`__, including shorthand
scientific notation e.g. 1e-5 (:math:`1\times10^{-5}`).

.. note::
    The order of ArrayValueRow elements within an ArrayValue element does not
    effect the interpreted order of the values in the array in keeping with the
    order non-specific design philosophy of NineML (see Section 1.2).

ExternalArrayValue
------------------

+------------+-------------------------------------------------------------------+----------+
| Attribute  | Type/Format                                                       | Required |
+============+===================================================================+==========+
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
^^^^^^^^^^^^^

The *url* attribute specifies the
`URL <http://en.wikipedia.org/wiki/Uniform_resource_locator>`__\ of the
external data file.

MimeType attribute
^^^^^^^^^^^^^^^^^^

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
^^^^^^^^^^^^^^^^^^^^

Each ExternalArrayValue must have a *columnName* attribute, which refers
to a column header in the external data file.

RandomValue
-----------


+-----------------------+--------------+----------+
| Children              | Multiplicity | Required |
+=======================+==============+==========+
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

+-----------+-------------+----------+
| Attribute | Type/Format | Required |
+===========+=============+==========+
| name      | identifier  | yes      |
+-----------+-------------+----------+

+----------+--------------+----------+
| Children | Multiplicity | Required |
+==========+==============+==========+
| Size     | singleton    | yes      |
+----------+--------------+----------+
| Cell     | singleton    | yes      |
+----------+--------------+----------+

A Population defines a set of dynamic components of the same class. The
size of the set is specified by the Size element. The properties of the
dynamic components are generated from value types, which can be constant
across the population, randomly distributed or individually specified
(see [sec:Values]).

Name attribute
^^^^^^^^^^^^^^

Each Population requires a *name* attribute, which should be a valid and
uniquely identify the Population from all other elements in the document
scope.

Cell
----


+-----------------------+--------------+----------+
| Children              | Multiplicity | Required |
+=======================+==============+==========+
| [Component,Reference] | singleton    | yes      |
+-----------------------+--------------+----------+

The Cell element specifies the dynamic components that will make up the
population. The Component can be defined inline or via a Reference
element.

Size
----

+-------------+----------+
| Body format | Required |
+=============+==========+
| int         | yes      |
+-------------+----------+

The number of cells in the population is specified by the integer
provided in the body of the Size element. In future versions this may be
extended to allow the size of a population to be derived from other
features of the Population.

Body
^^^^

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

+-----------+-------------+----------+
| Attribute | Type/Format | Required |
+===========+=============+==========+
| name      | identifier  | yes      |
+-----------+-------------+----------+

+--------------+--------------+----------+
| Children     | Multiplicity | Required |
+==============+==============+==========+
| Source       | singleton    | yes      |
+--------------+--------------+----------+
| Destination  | singleton    | yes      |
+--------------+--------------+----------+
| Connectivity | singleton    | yes      |
+--------------+--------------+----------+
| Response     | singleton    | yes      |
+--------------+--------------+----------+
| Plasticity   | singleton    | no       |
+--------------+--------------+----------+
| Delay        | singleton    | yes      |
+--------------+--------------+----------+

The Projection element contains all the elements that define a
projection between two populations and should be uniquely identified in
the scope of the document.

Name attribute
^^^^^^^^^^^^^^

Each Projection requires a *name* attribute, which should be a valid and
uniquely identify the Projection from all other elements in the document
scope.

Connectivity
------------


+-----------+--------------+----------+
| Children  | Multiplicity | Required |
+===========+==============+==========+
| Component | singleton    | yes      |
+-----------+--------------+----------+

Each Connectivity element contains a Component, which defines the
connection pattern of the cells in the source population to cells in the
destination population (i.e. binary ‘connected’ or ‘not connected’
decisions). For each connection that is specified, a synapse, consisting
of a post-synaptic response and plasticity dynamic components, is
created to model the synaptic interaction between the cells.

Source
------


+-----------------------+--------------+----------+
| Children              | Multiplicity | Required |
+=======================+==============+==========+
| [Component,Reference] | singleton    | yes      |
+-----------------------+--------------+----------+
| FromDestination       | set          | no       |
+-----------------------+--------------+----------+
| FromPlasticity        | set          | no       |
+-----------------------+--------------+----------+
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
| Children              | Multiplicity | Required |
+=======================+==============+==========+
| [Component,Reference] | singleton    | yes      |
+-----------------------+--------------+----------+
| FromSource            | set          | no       |
+-----------------------+--------------+----------+
| FromPlasticity        | set          | no       |
+-----------------------+--------------+----------+
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
| Children              | Multiplicity | Required |
+=======================+==============+==========+
| [Component,Reference] | singleton    | yes      |
+-----------------------+--------------+----------+
| FromSource            | set          | no       |
+-----------------------+--------------+----------+
| FromDestination       | set          | no       |
+-----------------------+--------------+----------+
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
| Children              | Multiplicity | Required |
+=======================+==============+==========+
| [Component,Reference] | singleton    | yes      |
+-----------------------+--------------+----------+
| FromSource            | set          | no       |
+-----------------------+--------------+----------+
| FromDestination       | set          | no       |
+-----------------------+--------------+----------+
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
+===========+============================================================+==========+
| sender    | [AnalogSendPort,EventSendPort].name                        | yes      |
+-----------+------------------------------------------------------------+----------+
| receiver  | [AnalogReceivePort,EventReceivePort,AnalogReducePort].name | yes      |
+-----------+------------------------------------------------------------+----------+

The FromSource element specifies a port connection to the projection
component (either the destination cell, post-synaptic response or
plasticity dynamics) inside which it is inserted from the source cell
dynamics.

Sender attribute
^^^^^^^^^^^^^^^^

Each FromSource element requires a *sender* attribute. This should refer
to the name of a AnalogSendPort or EventSendPort in the Cellof the
source population. The transmission mode of the port (i.e. analog or
event) should match that of the port referenced by the *receiver*
attribute.

Receiver attribute
^^^^^^^^^^^^^^^^^^

Each FromSource element requires a *receiver* attribute. This should
refer to the name of a AnalogReceivePort, EventReceivePort or
AnalogReducePort in the Componentin the enclosing
Source/Destination/Plasticity/Response element. The transmission mode
of the port (i.e. analog or event) should match that of the port
referenced by the *sender* attribute.

FromDestination
---------------

+-----------+------------------------------------------------------------+----------+
| Attribute | Type/Format                                                | Required |
+===========+============================================================+==========+
| sender    | [AnalogSendPort,EventSendPort].name                        | yes      |
+-----------+------------------------------------------------------------+----------+
| receiver  | [AnalogReceivePort,EventReceivePort,AnalogReducePort].name | yes      |
+-----------+------------------------------------------------------------+----------+

The FromDestination element specifies a port connection to the
projection component (either the source cell, post-synaptic response or
plasticity dynamics) inside which it is inserted from the destination
cell dynamics.

Sender attribute
^^^^^^^^^^^^^^^^

Each FromDestination element requires a *sender* attribute. This should
refer to the name of a AnalogSendPort or EventSendPort in the Cellof the
source population. The transmission mode of the port (i.e. analog or
event) should match that of the port referenced by the *receiver*
attribute.

Receiver attribute
^^^^^^^^^^^^^^^^^^

Each FromDestination element requires a *receiver* attribute. This
should refer to the name of a AnalogReceivePort, EventReceivePort or
AnalogReducePort in the Componentin the enclosing
Source/Destination/Plasticity/Response element. The transmission mode
of the port (i.e. analog or event) should match that of the port
referenced by the *sender* attribute.

FromPlasticity
--------------

+-----------+------------------------------------------------------------+----------+
| Attribute | Type/Format                                                | Required |
+===========+============================================================+==========+
| sender    | [AnalogSendPort,EventSendPort].name                        | yes      |
+-----------+------------------------------------------------------------+----------+
| receiver  | [AnalogReceivePort,EventReceivePort,AnalogReducePort].name | yes      |
+-----------+------------------------------------------------------------+----------+

The FromPlasticity element specifies a port connection to the projection
component (either the source cell, destination cell or post-synaptic
response dynamics) inside which it is inserted from the plasticity
dynamics.

Sender attribute
^^^^^^^^^^^^^^^^

Each FromPlasticity element requires a *sender* attribute. This should
refer to the name of a AnalogSendPort or EventSendPort in the
Cell->Componentof the source population. The transmission mode of the
port (i.e. analog or event) should match that of the port referenced by
the *receiver* attribute.

Receiver attribute
^^^^^^^^^^^^^^^^^^

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
+===========+============================================================+==========+
| sender    | [AnalogSendPort,EventSendPort].name                        | yes      |
+-----------+------------------------------------------------------------+----------+
| receiver  | [AnalogReceivePort,EventReceivePort,AnalogReducePort].name | yes      |
+-----------+------------------------------------------------------------+----------+

The FromResponse element specifies a port connection to the projection
component (either the source cell, destination cell or plasticity
dynamics) inside which it is inserted from the post-synaptic response
dynamics.

Sender attribute
^^^^^^^^^^^^^^^^

Each FromResponse element requires a *sender* attribute. This should
refer to the name of a AnalogSendPort or EventSendPort in the
Cell->Componentof the source population. The transmission mode of the
port (i.e. analog or event) should match that of the port referenced by
the *receiver* attribute.

Receiver attribute
^^^^^^^^^^^^^^^^^^

Each FromResponse element requires a *receiver* attribute. This should
refer to the name of a AnalogReceivePort, EventReceivePort or
AnalogReducePort in the Componentin the enclosing Source/Destination/
Plasticity/Response element. The transmission mode of the port (i.e.
analog or event) should match that of the port referenced by the
*sender* attribute.

Delay
-----

+-----------+-------------+----------+
| Attribute | Type/Format | Required |
+===========+=============+==========+
| units     | Unit@symbol | yes      |
+-----------+-------------+----------+

+---------------------------------------------------------+--------------+----------+
| Children                                                | Multiplicity | Required |
+=========================================================+==============+==========+
| [SingleValue,ArrayValue,ExternalArrayValue,RandomValue] | singleton    | yes      |
+---------------------------------------------------------+--------------+----------+

In version 1.0, the Delay element specifies the delay between the
pre-synaptic cell port and both the Plasticityand Response. In future
versions, it is planned to include the delay directly into the
port-connection objects (i.e. FromSource, FromDestination, etc...) to
allow finer control of the delay between the different components.

Units attribute
^^^^^^^^^^^^^^^

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

+-----------+-------------+----------+
| Attribute | Type/Format | Required |
+===========+=============+==========+
| name      | identifier  | yes      |
+-----------+-------------+----------+

+-------------+--------------+----------+
| Children    | Multiplicity | Required |
+=============+==============+==========+
| Concatenate | singleton    | yes      |
+-------------+--------------+----------+

The Selection element contains the operations that are used to select
the cells to add to the selection.

Name attribute
^^^^^^^^^^^^^^

Each Selection requires a *name* attribute, which should be a valid and
uniquely identify the Selection from all other elements in the document
scope.

Concatenate
-----------


+----------+--------------+----------+
| Children | Multiplicity | Required |
+==========+==============+==========+
| Item     | set          | yes      |
+----------+--------------+----------+

The Concatenate element is used to add populations to a selection. It
contains a set of Item elements which reference the Population elements
to be concatenated. The order of the Item elements does not effect the
order of the concatenation, which is determined by the *index* attribute
of the Item elements. The set of Item@\ *index* attributes must be
non-negative, contiguous, not contain any duplicates and contain the
index 0 (i.e. :math:`i=0,\ldots,N-1`).

Item
----

+-----------+-------------+----------+
| Attribute | Type/Format | Required |
+===========+=============+==========+
| index     | ``integer`` | yes      |
+-----------+-------------+----------+

+-----------------------------------+--------------+----------+
| Children                          | Multiplicity | Required |
+===================================+==============+==========+
| Reference([Population,Selection]) | singleton    | yes      |
+-----------------------------------+--------------+----------+

Each Item element references as a Population or Selection element and
specifies their order in the concatenation.

Index attribute
^^^^^^^^^^^^^^^

Each Item requires a *index* attribute. This attribute specifies the
order in which the Populations in the Selection are concatenated and
thereby the indices of the cells within the combined Selection.

.. note::
    This preserves the order non-specific nature of elements in NineML

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


+----------+--------------+----------+
| Children | Multiplicity | Required |
+==========+==============+==========+
| \*       | set          | no       |
+----------+--------------+----------+

The Annotations element is the top-level of the annotations attached to
a NineML element. They can be included within any NineML element (User
Layer and Abstraction Layer) and any valid XML is allowed within them.