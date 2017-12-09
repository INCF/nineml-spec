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

+------------------------------+--------------+----------+
| Children                     | Multiplicity | Required |
+==============================+==============+==========+
| [Definition_,\ Prototype_\ ] | singleton    | yes      |
+------------------------------+--------------+----------+
| Property_                    | set          | no       |
+------------------------------+--------------+----------+

Component_ elements instantiate Abstraction Layer component classes by
providing properties for each of the parameters defined the class. Each
Component_ is linked to a :ref:`ComponentClass` class by a Definition_ element,
which locates the component class. A Component_ that instantiates a
:ref:`ComponentClass` directly must supply matching Property_ elements for each
:ref:`Parameter` in the :ref:`ComponentClass`. Alternatively, a Component_ can inherit
a :ref:`ComponentClass` and set of Property_ elements from an existing component
by substituting the Definition_ for a Prototype_ element, which locates
the reference Component_. In this case, only the properties that differ
from the reference component need to be specified.

Name attribute
^^^^^^^^^^^^^^

Each Component_ requires a *name* attribute, which should be a valid and
uniquely identify the Component_ from all other elements in the document
scope.

Definition
----------

+-----------+-------------+----------+
| Attribute | Type/Format | Required |
+===========+=============+==========+
| url       | URL_        | no       |
+-----------+-------------+----------+

+------------------------------+----------+
| Body format                  | Required |
+==============================+==========+
| :ref:`ComponentClass`.\ name | yes      |
+------------------------------+----------+

The Definition_ element establishes a link between a User Layer component
and Abstraction Layer :ref:`ComponentClass`. This :ref:`ComponentClass` can be located
either in the current document or in another file if a *url* attribute
is provided.

Url attribute
^^^^^^^^^^^^^

If the :ref:`ComponentClass` referenced by the definition element is defined
outside the current document, the *url* attribute specifies a
URL_\ for the
file which contains the :ref:`ComponentClass` definition. If the *url*
attribute is omitted the :ref:`ComponentClass` is referenced from the current
document.

Body
^^^^

The name of the :ref:`ComponentClass` to be referenced :ref:`ComponentClass` needs to
be provided in the body of the Definition_ element.

Prototype
---------

+-----------+-------------+----------+
| Attribute | Type/Format | Required |
+===========+=============+==========+
| url       | URL_        | no       |
+-----------+-------------+----------+

+-------------------+----------+
| Body format       | Required |
+===================+==========+
| Component_.\ name | yes      |
+-------------------+----------+

The Prototype_ element establishes a link to an existing User Layer
Component_, which defines the :ref:`ComponentClass` and default properties of
the Component_. The reference Component_ can be located either in the
current document or in another file if a *url* attribute is provided.

Url attribute
^^^^^^^^^^^^^

If the prototype Component_ is defined outside the current file, the
*URL* attribute specifies a
URL_\ for the
file which contains the prototype Component_. If the *url* attribute is
omitted the Component_ is referenced from the current document.

Body
^^^^

The name of the Component_ to be referenced Component_ needs to be
provided in the body of the Prototype_ element.

Property
--------

+-----------+-------------------------+----------+
| Attribute | Type/Format             | Required |
+===========+=========================+==========+
| name      | :ref:`Parameter`.\ name | yes      |
+-----------+-------------------------+----------+
| units     | :ref:`Unit`.\ symbol    | yes      |
+-----------+-------------------------+----------+

+-------------------------------------------------------------------------------------+--------------+----------+
| Children                                                                            | Multiplicity | Required |
+=====================================================================================+==============+==========+
| [SingleValue_,\ ArrayValue_\ ,\ ExternalArrayValue_\ ,\ RandomDistributionValue_\ ] | singleton    | yes      |
+-------------------------------------------------------------------------------------+--------------+----------+

Property_ elements provide values for the parameters defined in the
:ref:`ComponentClass` of the Component_. Their *name* attribute should match the
name of the corresponding :ref:`Parameter` element in the :ref:`ComponentClass`. The
Property_ should be provided units that match the dimensionality of the
corresponding :ref:`Parameter` definition.

Name attribute
^^^^^^^^^^^^^^

Each Property_ requires a *name* attribute. This should refer to the name
of a :ref:`Parameter` in the corresponding :ref:`ComponentClass` of the Component_.

Units attribute
^^^^^^^^^^^^^^^

Each Property_ element requires a *units* attribute. The *units*
attribute specifies the units of the quantity and should refer to the
name of a :ref:`Unit` element in the document scope. For a dimensionless units
a :ref:`Unit` with no SI dimensions can be used. The SI dimensions of the
:ref:`Unit` should match the SI dimensions of the corresponding :ref:`Parameter`.

.. note::
    "Dimensionless" parameters can be defined by referring to an empty
    Dimension object, i.e. one without any power or offset attributes

Reference
---------

+-----------+-------------+----------+
| Attribute | Type/Format | Required |
+===========+=============+==========+
| url       | URL_        | no       |
+-----------+-------------+----------+

+-------------+----------+
| Body format | Required |
+=============+==========+
| \*.name     | yes      |
+-------------+----------+

Reference_ elements are used to locate User Layer elements in the
document scope of the current separate documents. In most cases, User
Layer elements (with the exception of Population_ elements supplied to
Projection_) can be specified inline, i.e. within the element they are
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
URL_\ for the
file which contains the User Layer element to be referenced. If the
*url* attribute is omitted the element is referenced from the current
document.

Body
^^^^

The name of the User Layer element to be referenced should be included
in the body of the Reference_ element.

Values
======

In NineML, “values” are arrays that implicitly grow to fill the size of
the container (i.e. Population_ or Projection_) they are located within.
Values can be one of four types

-  SingleValue_, a consistent value across the container

-  ArrayValue_, an explicit array defined in NineML

-  ExternalArrayValue_, an explicit array defined in text (space
   delimited) or HDF5 format.

-  RandomDistributionValue_, an array of values derived from a random distribution.

SingleValue
-----------

+-------------+----------+
| Body format | Required |
+=============+==========+
| ``integer`` | yes      |
+-------------+----------+

A SingleValue_ element represents an array filled with a single value.

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

ArrayValue_ elements are used to represent an explicit array of values in
XML. ArrayValue_ elements contain a set of ArrayValue_Row elements (i.e.
unordered, since they are explicitly ordered by their *index*
attribute). Since XML is significantly slower to parse than plain text
and binary formats it is not recommended to use ArrayValue_ for large
arrays, preferring ExternalArrayValue_ instead.

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

ArrayValue_Row elements represent the numerical values of the explicit
ArrayValue_ element.

Index attribute
^^^^^^^^^^^^^^^

The *index* attribute specifies the index of the ArrayValue_Row in the
ArrayValue_. It must be non-negative, unique amongst the set of
ArrayValue_Row.index in the list, and the set of indices must be
contiguous for a single ArrayValue_.

Body
^^^^

Any valid numeric value in `ANSI
C89 <http://en.wikipedia.org/wiki/ANSI_C>`__, including shorthand
scientific notation e.g. 1e-5 (:math:`1\times10^{-5}`).

.. note::
    The order of ArrayValue_Row elements within an ArrayValue_ element does not
    effect the interpreted order of the values in the array in keeping with the
    order non-specific design philosophy of NineML.

ExternalArrayValue
------------------

+------------+-----------------------------------+----------+
| Attribute  | Type/Format                       | Required |
+============+===================================+==========+
| url        | URL_                              | yes      |
+------------+-----------------------------------+----------+
| mimeType   | `MIME  type`_                     | yes      |
+------------+-----------------------------------+----------+
| columnName | Data column name in external file | yes      |
+------------+-----------------------------------+----------+

ExternalArrayValue_ elements are used to explicitly define large arrays
of values. The array data are not stored in XML (which is slow to parse)
but more efficient text or binary `HDF5
(http://www.hdfgroup.org/HDF5/) <http://www.hdfgroup.org/HDF5/>`__
formats. As of version 1.0, the data in the external files are stored as
dense or arrays. However, sparse-array formats are planned for future
versions.

The *columnName* attribute of the ExternalArrayValue_ elements allows
multiple arrays of equal length (and therefore typically relating to the
same container) to be stored in the same external file.

Url attribute
^^^^^^^^^^^^^

The *url* attribute specifies the
URL_\ of the
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

Each ExternalArrayValue_ must have a *columnName* attribute, which refers
to a column header in the external data file.

RandomDistributionValue
-----------------------


+-----------------------------+--------------+----------+
| Children                    | Multiplicity | Required |
+=============================+==============+==========+
| [Component_,\ Reference_\ ] | singleton    | yes      |
+-----------------------------+--------------+----------+

:ref:`RandomDistributionValue` elements represent arrays of values drawn from random
distributions, which are defined by a Component_ elements. The size of the
generated array is determined by the size of the container (i.e.
Population_ or Projection_) the :ref:`RandomDistributionValue` is nested within.

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
| Size_    | singleton    | yes      |
+----------+--------------+----------+
| Cell_    | singleton    | yes      |
+----------+--------------+----------+

A Population_ defines a set of dynamic components of the same class. The
size of the set is specified by the Size_ element. The properties of the
dynamic components are generated from value types, which can be constant
across the population, randomly distributed or individually specified
(see Values_).

Name attribute
^^^^^^^^^^^^^^

Each Population_ requires a *name* attribute, which should be a valid and
uniquely identify the Population_ from all other elements in the document
scope.

Cell
----


+-----------------------------+--------------+----------+
| Children                    | Multiplicity | Required |
+=============================+==============+==========+
| [Component_,\ Reference_\ ] | singleton    | yes      |
+-----------------------------+--------------+----------+

The Cell_ element specifies the dynamic components that will make up the
population. The Component_ can be defined inline or via a Reference_
element.

Size
----

+-------------+----------+
| Body format | Required |
+=============+==========+
| int         | yes      |
+-------------+----------+

The number of cells in the population is specified by the integer
provided in the body of the Size_ element. In future versions this may be
extended to allow the size of a population to be derived from other
features of the Population_.

Body
^^^^

The text of the Size_ element contains an representing the size of the
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

SingleValue_ and :ref:`RandomDistributionValue` elements used in properties of a projection
(in the Connectivity_, Response_, Plasticity_ and Delay_ elements) take the
size of the number of connections made. Explicitly array values,
ArrayValue_ and ExternalArrayValue_, are only permitted with connection
rules (as defined by the Connectivity_ element) where the number of
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

+---------------+--------------+----------+
| Children      | Multiplicity | Required |
+===============+==============+==========+
| Source_       | singleton    | yes      |
+---------------+--------------+----------+
| Destination_  | singleton    | yes      |
+---------------+--------------+----------+
| Connectivity_ | singleton    | yes      |
+---------------+--------------+----------+
| Response_     | singleton    | yes      |
+---------------+--------------+----------+
| Plasticity_   | singleton    | no       |
+---------------+--------------+----------+
| Delay_        | singleton    | yes      |
+---------------+--------------+----------+

The Projection_ element contains all the elements that define a
projection between two populations and should be uniquely identified in
the scope of the document.

Name attribute
^^^^^^^^^^^^^^

Each Projection_ requires a *name* attribute, which should be a valid and
uniquely identify the Projection_ from all other elements in the document
scope.

Connectivity
------------


+------------+--------------+----------+
| Children   | Multiplicity | Required |
+============+==============+==========+
| Component_ | singleton    | yes      |
+------------+--------------+----------+

Each Connectivity_ element contains a Component_, which defines the
connection pattern of the cells in the source population to cells in the
destination population (i.e. binary ‘connected’ or ‘not connected’
decisions). For each connection that is specified, a synapse, consisting
of a post-synaptic response and plasticity dynamic components, is
created to model the synaptic interaction between the cells.

Source
------


+-----------------------------+--------------+----------+
| Children                    | Multiplicity | Required |
+=============================+==============+==========+
| [Component_,\ Reference_\ ] | singleton    | yes      |
+-----------------------------+--------------+----------+
| FromDestination_            | set          | no       |
+-----------------------------+--------------+----------+
| FromPlasticity_             | set          | no       |
+-----------------------------+--------------+----------+
| FromResponse_               | set          | no       |
+-----------------------------+--------------+----------+

The Source_ element specifies the pre-synaptic population or selection
(see Selection_) of the projection and all the port connections it
receives. The source population is specified via a Reference_ element
since it should not be defined within the Projection_. The source
population can receive incoming port connections from the post-synaptic
response (see FromResponse_), the plasticity rule (see FromPlasticity_) or
the post-synaptic population directly (see FromDestination_). Connections
with these ports are only made if the Connectivity_determines that the
source and destination cells should be connected.

Destination
-----------


+-----------------------------+--------------+----------+
| Children                    | Multiplicity | Required |
+=============================+==============+==========+
| [Component_,\ Reference_\ ] | singleton    | yes      |
+-----------------------------+--------------+----------+
| FromSource_                 | set          | no       |
+-----------------------------+--------------+----------+
| FromPlasticity_             | set          | no       |
+-----------------------------+--------------+----------+
| FromResponse_               | set          | no       |
+-----------------------------+--------------+----------+

The Destination_ element specifies the post-synaptic or selection (see
Selection_) population of the projection and all the port connections it
receives. The destination population is specified via a Reference_
element since it should not be defined within the Projection_. The source
population can receive incoming port connections from the post-synaptic
response (see FromResponse_), the plasticity rule (see FromPlasticity_) or
the pre-synaptic population directly (see FromSource_). Connections with
these ports are only made if the Connectivity_determines that the source
and destination cells should be connected.

Response
--------


+-----------------------------+--------------+----------+
| Children                    | Multiplicity | Required |
+=============================+==============+==========+
| [Component_,\ Reference_\ ] | singleton    | yes      |
+-----------------------------+--------------+----------+
| FromSource_                 | set          | no       |
+-----------------------------+--------------+----------+
| FromDestination_            | set          | no       |
+-----------------------------+--------------+----------+
| FromPlasticity_             | set          | no       |
+-----------------------------+--------------+----------+

The Response_ defines the effect on the post-synaptic cell dynamics of an
incoming synaptic input. The additional dynamics are defined by a
Component_ element, which can be defined inline or referenced. For static
connections (i.e. those without a Plasticity_ element), the magnitude of
the response (i.e. synaptic weight) is typically passed as a property of
the Response_ element.

The post-synaptic response dynamics can receive incoming port
connections from the plasticity rule (see FromPlasticity_) or the pre or
post synaptic populations (see FromSource_ and FromDestination_). The
post-synaptic response object is implicitly created and connected to
these ports if the Connectivity_determines that the source and
destination cells should be connected.

Plasticity
----------


+-----------------------------+--------------+----------+
| Children                    | Multiplicity | Required |
+=============================+==============+==========+
| [Component_,\ Reference_\ ] | singleton    | yes      |
+-----------------------------+--------------+----------+
| FromSource_                 | set          | no       |
+-----------------------------+--------------+----------+
| FromDestination_            | set          | no       |
+-----------------------------+--------------+----------+
| FromResponse_               | set          | no       |
+-----------------------------+--------------+----------+

The Plasticity_ element describes the dynamic processes that modulate the
dynamics of the post-synaptic response, typically the magnitude of the
response (see Response_). If the synapse is not plastic the
Plasticity_ element can be omitted.

The plasticity dynamics can receive incoming port connections from the
post-synaptic response rule (see FromResponse_) or the pre or post
synaptic populations (see FromSource_ and FromDestination_). The
plasticity object is implicitly created and connected to these ports if
the Connectivity_determines that the source and destination cells should
be connected.

FromSource
----------

+-----------+---------------------------------------------------------------------------------------+----------+
| Attribute | Type/Format                                                                           | Required |
+===========+=======================================================================================+==========+
| sender    | [:ref:`AnalogSendPort`,\ :ref:`EventSendPort`\ ].name                                 | yes      |
+-----------+---------------------------------------------------------------------------------------+----------+
| receiver  | [:ref:`AnalogReceivePort`,\ :ref:`EventReceivePort`,\ :ref:`AnalogReducePort`\ ].name | yes      |
+-----------+---------------------------------------------------------------------------------------+----------+

The FromSource_ element specifies a port connection to the projection
component (either the destination cell, post-synaptic response or
plasticity dynamics) inside which it is inserted from the source cell
dynamics.

Sender attribute
^^^^^^^^^^^^^^^^

Each FromSource_ element requires a *sender* attribute. This should refer
to the name of a :ref:`AnalogSendPort` or :ref:`EventSendPort` in the Cell_of the
source population. The transmission mode of the port (i.e. analog or
event) should match that of the port referenced by the *receiver*
attribute.

Receiver attribute
^^^^^^^^^^^^^^^^^^

Each FromSource_ element requires a *receiver* attribute. This should
refer to the name of a :ref:`AnalogReceivePort`, :ref:`EventReceivePort` or
:ref:`AnalogReducePort` in the Component_ in the enclosing
Source_/Destination_/Plasticity_/Response_ element. The transmission mode
of the port (i.e. analog or event) should match that of the port
referenced by the *sender* attribute.

FromDestination
---------------

+-----------+---------------------------------------------------------------------------------------+----------+
| Attribute | Type/Format                                                                           | Required |
+===========+=======================================================================================+==========+
| sender    | [:ref:`AnalogSendPort`,\ :ref:`EventSendPort`\ ].name                                 | yes      |
+-----------+---------------------------------------------------------------------------------------+----------+
| receiver  | [:ref:`AnalogReceivePort`,\ :ref:`EventReceivePort`,\ :ref:`AnalogReducePort`\ ].name | yes      |
+-----------+---------------------------------------------------------------------------------------+----------+

The FromDestination_ element specifies a port connection to the
projection component (either the source cell, post-synaptic response or
plasticity dynamics) inside which it is inserted from the destination
cell dynamics.

Sender attribute
^^^^^^^^^^^^^^^^

Each FromDestination_ element requires a *sender* attribute. This should
refer to the name of a :ref:`AnalogSendPort` or :ref:`EventSendPort` in the Cell_of the
source population. The transmission mode of the port (i.e. analog or
event) should match that of the port referenced by the *receiver*
attribute.

Receiver attribute
^^^^^^^^^^^^^^^^^^

Each FromDestination_ element requires a *receiver* attribute. This
should refer to the name of a :ref:`AnalogReceivePort`, :ref:`EventReceivePort` or
:ref:`AnalogReducePort` in the Component_ in the enclosing
Source_/Destination_/Plasticity_/Response_ element. The transmission mode
of the port (i.e. analog or event) should match that of the port
referenced by the *sender* attribute.

FromPlasticity
--------------

+-----------+---------------------------------------------------------------------------------------+----------+
| Attribute | Type/Format                                                                           | Required |
+===========+=======================================================================================+==========+
| sender    | [:ref:`AnalogSendPort`,\ :ref:`EventSendPort`\ ].name                                 | yes      |
+-----------+---------------------------------------------------------------------------------------+----------+
| receiver  | [:ref:`AnalogReceivePort`,\ :ref:`EventReceivePort`,\ :ref:`AnalogReducePort`\ ].name | yes      |
+-----------+---------------------------------------------------------------------------------------+----------+

The FromPlasticity_ element specifies a port connection to the projection
component (either the source cell, destination cell or post-synaptic
response dynamics) inside which it is inserted from the plasticity
dynamics.

Sender attribute
^^^^^^^^^^^^^^^^

Each FromPlasticity_ element requires a *sender* attribute. This should
refer to the name of a :ref:`AnalogSendPort` or :ref:`EventSendPort` in the
Cell_->Component_ of the source population. The transmission mode of the
port (i.e. analog or event) should match that of the port referenced by
the *receiver* attribute.

Receiver attribute
^^^^^^^^^^^^^^^^^^

Each FromPlasticity_ element requires a *receiver* attribute. This should
refer to the name of a :ref:`AnalogReceivePort`, :ref:`EventReceivePort` or
:ref:`AnalogReducePort` in the Component_ in the enclosing Source_/Destination_/
Plasticity_/Response_ element. The transmission mode of the port (i.e.
analog or event) should match that of the port referenced by the
*sender* attribute.

FromResponse
------------

+-----------+---------------------------------------------------------------------------------------+----------+
| Attribute | Type/Format                                                                           | Required |
+===========+=======================================================================================+==========+
| sender    | [:ref:`AnalogSendPort`,\ :ref:`EventSendPort`\ ].name                                 | yes      |
+-----------+---------------------------------------------------------------------------------------+----------+
| receiver  | [:ref:`AnalogReceivePort`,\ :ref:`EventReceivePort`,\ :ref:`AnalogReducePort`\ ].name | yes      |
+-----------+---------------------------------------------------------------------------------------+----------+

The FromResponse_ element specifies a port connection to the projection
component (either the source cell, destination cell or plasticity
dynamics) inside which it is inserted from the post-synaptic response
dynamics.

Sender attribute
^^^^^^^^^^^^^^^^

Each FromResponse_ element requires a *sender* attribute. This should
refer to the name of a :ref:`AnalogSendPort` or :ref:`EventSendPort` in the
Cell_->Component_ of the source population. The transmission mode of the
port (i.e. analog or event) should match that of the port referenced by
the *receiver* attribute.

Receiver attribute
^^^^^^^^^^^^^^^^^^

Each FromResponse_ element requires a *receiver* attribute. This should
refer to the name of a :ref:`AnalogReceivePort`, :ref:`EventReceivePort` or
:ref:`AnalogReducePort` in the Component_ in the enclosing Source_/Destination_/
Plasticity_/Response_ element. The transmission mode of the port (i.e.
analog or event) should match that of the port referenced by the
*sender* attribute.

Delay
-----

+-----------+--------------------+----------+
| Attribute | Type/Format        | Required |
+===========+====================+==========+
| units     | :ref:`Unit`.symbol | yes      |
+-----------+--------------------+----------+

+---------------------------------------------------------------------------------+--------------+----------+
| Children                                                                        | Multiplicity | Required |
+=================================================================================+==============+==========+
| [SingleValue_,\ ArrayValue_,\ ExternalArrayValue_,\ RandomDistributionValue_\ ] | singleton    | yes      |
+---------------------------------------------------------------------------------+--------------+----------+

In version 1.0, the Delay_ element specifies the delay between the
pre-synaptic cell port and both the Plasticity_and Response_. In future
versions, it is planned to include the delay directly into the
port-connection objects (i.e. FromSource_, FromDestination_, etc...) to
allow finer control of the delay between the different components.

Units attribute
^^^^^^^^^^^^^^^

The *units* attribute specifies the units of the delay and should refer
to the name of a :ref:`Unit` element in the document scope. The units should be
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

+--------------+--------------+----------+
| Children     | Multiplicity | Required |
+==============+==============+==========+
| Concatenate_ | singleton    | yes      |
+--------------+--------------+----------+

The Selection_ element contains the operations that are used to select
the cells to add to the selection.

Name attribute
^^^^^^^^^^^^^^

Each Selection_ requires a *name* attribute, which should be a valid and
uniquely identify the Selection_ from all other elements in the document
scope.

Concatenate
-----------


+----------+--------------+----------+
| Children | Multiplicity | Required |
+==========+==============+==========+
| Item_    | set          | yes      |
+----------+--------------+----------+

The Concatenate_ element is used to add populations to a selection. It
contains a set of Item_ elements which reference the Population_ elements
to be concatenated. The order of the Item_ elements does not effect the
order of the concatenation, which is determined by the *index* attribute
of the Item_ elements. The set of Item_@\ *index* attributes must be
non-negative, contiguous, not contain any duplicates and contain the
index 0 (i.e. :math:`i=0,\ldots,N-1`).

Item
----

+-----------+-------------+----------+
| Attribute | Type/Format | Required |
+===========+=============+==========+
| index     | ``integer`` | yes      |
+-----------+-------------+----------+

+--------------------------------------------+--------------+----------+
| Children                                   | Multiplicity | Required |
+============================================+==============+==========+
| Reference_\ ([Population_,\ Selection_\ ]) | singleton    | yes      |
+--------------------------------------------+--------------+----------+

Each Item_ element references as a Population_ or Selection_ element and
specifies their order in the concatenation.

Index attribute
^^^^^^^^^^^^^^^

Each Item_ requires a *index* attribute. This attribute specifies the
order in which the Population_\ s in the Selection_ are concatenated and
thereby the indices of the cells within the combined Selection_.

.. note::
    This preserves the order non-specific nature of elements in NineML
    
.. _URL: http://en.wikipedia.org/wiki/Uniform_resource_locator
.. _MIME type: http://en.wikipedia.org/wiki/Internet_media_type
