*************
Serialization
*************


While mapping 9ML to the different hierarchical formats I ran into some
structural issues with the way we have designed the language. Namely, not all
formats support namespaces (all except XML), body text (all except XML), and/or
list/set of members (HDF5). To get around these issues I added some special
elements.

- @namespace
- @body
- @multiple (open to suggestions on the best name for this)

'@namespace' is a straightforward replacement for xmlns

'@body' is a replacement for body text iff there are other attributes in the
element. If the element only contains a body (e.g. MathInline, SingleValue,
Delay...) then the body is flattened to be an "attribute" of the parent
element, e.g. the ``MathInline`` elements in the YAML below

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
      Dimension:
      - {name: capacitance, m: -1, l: -2, t: 4, i: 2}
      - {name: current, i: 1}
      - {name: per_time, t: -1}
      - {name: per_time_voltage, m: -1, l: -2, t: 2, i: 1}
      - {name: voltage, m: 1, l: 2, t: -3, i: -1}
      - {name: voltage_per_time, m: 1, l: 2, t: -4, i: -1}

'@multiple' is used in HDF5 to signify that the children of a given HDF5 group
are a set of child elements of the same type, which are assigned arbitrary
indices. So for the set of Parameters the example above the hierarchy would be

    /NineML/ComponentClass/Parameter/@multiple = true
    /NineML/ComponentClass/Parameter/0/name = 'C_m'
    /NineML/ComponentClass/Parameter/0/dimension = 'capacitance'
    /NineML/ComponentClass/Parameter/1/name = 'a'
    /NineML/ComponentClass/Parameter/1/dimension = 'per_time'
    ...
