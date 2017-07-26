*************
Serialisation
*************

There are four officially supported data formats for serialising NineML:
XML_, JSON_, YAML_, and HDF5_ (although it is possible to use other data
formats). When referenced from another NineML document, the format of a NineML
file is recognised by the extension of its filename, i.e:

+--------+-----------+
| Format | Extension |
+========+===========+
| XML_   | .xml      |
+--------+-----------+
| JSON_  | .json     |
+--------+-----------+
| YAML_  | .yml      |
+--------+-----------+
| HDF5_  | .h5       |
+--------+-----------+

.. note:: Tools that plan to support NineML only need to support one data
          format since the officially supported `NineML Python Library`_ can
          be used to convert between the data formats listed above.

NineML is intended to be an abstract object model that is independent of the
choice of hierarchical data format used to serialise it. However, some aspects
of NineML were designed with XML in mind and there are some subtle differences
between hierarchical data formats that prevent general mappings from XML.
Therefore, in order to map the NineML object model onto non-XML data formats
some additional conventions are required.
 
Several features of XML that are used in the NineML specification and are not
present in JSON_/YAML_ (JSON_ and YAML_ are equivalent representations),
and/or HDF5_ are:

Namespaces (xmlns):
    There is no concept of namespaces in JSON_/YAML_ or HDF5_, which are used
    in NineML to distinguish the document version and annotations.
Attributes:
    In JSON_/YAML_ there is no concept of attributes. This does not pose a
    problem if a given NineML type does not have body text as attributes can
    be treated as separate children. However, for NineML types that do, such as
    :ref:`Constant` and :ref:`Definition`, both the body text and attributes
    can't be represented without additional conventions.
Sets of child elements:
    While there are list structures in JSON_/YAML_, which can be used to
    represent arbitrarily sized sets of child elements (e.g. parameters,
    properties, regimes), HDF5_ does not have an equivalent structure for
    storing sets of objects of the same type.

Fortunately, JSON_, YAML_ and HDF5_ all permit arbitrary strings as
field names, whereas element/attribute names in XML must start with
an alphabetic character. Therefore we can use non alphanumeric characters, in
this case the '@' symbol, to escape the following special fields.

@namespace:
    Holds the namespace of the element as the special attribute xmlns does in
    XML_.
@body:
    Used to differentiate body text from other attributes in JSON_/YAML_
    iff there are other attributes. Note that if the serial form of an element
    only contains body text (e.g. :ref:`MathInline`) then this is "flattened"
    to be the sole value of the element.
@multiple:
    A HDF_ group that has a @multiple attribute equal to 'true', 
    contains multiple child elements of the given NineML type, which are stored
    as sub-groups named by arbitrary integer indices. Note that this is not
    strictly required for elements in the NineML specification (although it
    simplifies code to read them), where the multiplicity of children of a
    given type is defined, but is for parsing arbitrary object hierarchies in
    annotations.

.. note:: Future versions of NineML will be designed to minimise the need for
          these special fields within the NineML object model. However,
          annotations and language extensions will still allow any object
          hierarchies that map to valid XML, so special fields will
          still be required.

Example YAML_ code of a Izhikevich neuron model demonstrating the use of
``@namespace`` and ``@body`` attributes.

.. code-block:: yaml

   NineML:
      '@namespace': http://nineml.net/9ML/1.0
      ComponentClass:
      - name: Izhikevich
        Parameter:
        - {name: C_m, dimension: capacitance}
        - {name: a, dimension: per_time}
        - {name: alpha, dimension: per_time_voltage}
        - {name: b, dimension: per_time}
        - {name: beta, dimension: per_time}
        - {name: c, dimension: voltage}
        - {name: d, dimension: voltage_per_time}
        - {name: theta, dimension: voltage}
        - {name: zeta, dimension: voltage_per_time}
        AnalogReducePort:
        - {name: Isyn, dimension: current, operator: +}
        EventSendPort:
        - {name: spike}
        AnalogSendPort:
        - {name: V, dimension: voltage}
        Dynamics:
          StateVariable:
          - {name: U, dimension: voltage_per_time}
          - {name: V, dimension: voltage}
          Regime:
          - name: subthreshold_regime
            TimeDerivative:
            - {MathInline: a*(-U + V*b), variable: U}
            - {MathInline: -U + V*beta + alpha*(V*V) + zeta + Isyn/C_m, variable: V}
            OnCondition:
            - Trigger: {MathInline: V > theta}
              target_regime: subthreshold_regime
              StateAssignment:
              - {MathInline: U + d, variable: U}
              - {MathInline: c, variable: V}
              OutputEvent:
              - {port: spike}
        Annotations:
          Validation:
          - {'@namespace': 'http://github.com/INCF/nineml-python', dimensionality: 'True'}
      Component:
      - Definition: {'@body': Izhikevich, url="./izhikevich.yml"}
        name: SampleIzhikevich
        Property:
        - {name: C_m, SingleValue: 1.0, units: pF}
        - {name: a, SingleValue: 0.2, units: per_ms}
        - {name: alpha, SingleValue: 0.04, units: per_mV_ms}
        - {name: b, SingleValue: 0.025, units: per_ms}
        - {name: beta, SingleValue: 5.0, units: per_ms}
        - {name: c, SingleValue: -75.0, units: mV}
        - {name: d, SingleValue: 0.2, units: mV_per_ms}
        - {name: theta, SingleValue: -50.0, units: mV}
        - {name: zeta, SingleValue: 140.0, units: mV_per_ms}
        Initial:
        - {name: U, SingleValue: -1.625, units: mV_per_ms}
        - {name: V, SingleValue: -70.0, units: mV}
      Dimension:
      - {name: capacitance, m: -1, l: -2, t: 4, i: 2}
      - {name: current, i: 1}
      - {name: per_time, t: -1}
      - {name: per_time_voltage, m: -1, l: -2, t: 2, i: 1}
      - {name: voltage, m: 1, l: 2, t: -3, i: -1}
      - {name: voltage_per_time, m: 1, l: 2, t: -4, i: -1}
      Unit:
      - {symbol: mV, dimension: voltage, power: -3}
      - {symbol: mV_per_ms, dimension: voltage_per_time, power: 0}
      - {symbol: pF, dimension: capacitance, power: -12}
      - {symbol: per_mV_ms, dimension: per_time_voltage, power: 6}
      - {symbol: per_ms, dimension: per_time, power: 3}

Example representation of sets of :ref:`Parameter` elements in HDF5 format::

    /NineML/ComponentClass/Parameter/@multiple = true
    /NineML/ComponentClass/Parameter/0/name = 'C_m'
    /NineML/ComponentClass/Parameter/0/dimension = 'capacitance'
    /NineML/ComponentClass/Parameter/1/name = 'a'
    /NineML/ComponentClass/Parameter/1/dimension = 'per_time'
    ...


.. _XML: http://www.w3.org/XML/
.. _YAML: http://yaml.org
.. _HDF5: http://www.hdfgroup.org/HDF5/
.. _JSON: http://www.json.org/
.. _`NineML Python Library`: http://github.com/INCF/nineml-python
