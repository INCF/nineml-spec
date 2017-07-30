********
Examples
********

Single Cell Models
==================

Izhikevich Model
----------------

In this first example, we are describing how to represent the Izhikevich
model in NineML [Izhikevich2003]_. The model is
composed of single :ref:`ComponentClass`, containing a single :ref:`Regime`,
*subthresholdRegime*, and two state variables, :math:`U` & :math:`V`.

The ODEs defined for the :ref:`Regime` are:

.. math::

   \begin{aligned}
   \frac{dV}{dt} &= 0.04*V*V + 5*V + 140.0 - U + i_{\mathrm{synapse}} + i_{\mathrm{injected}}  \\
   \frac{dU}{dt} &= a * ( b* V -U )\end{aligned}

The :ref:`ComponentClass` has a single :ref:`OnCondition` transition, is triggered
when :math:`V>theta`. When triggered, It causes an Event called
*spikeOutput* to be emitted, and two :ref:`StateAssignment`\ s to be made:

.. math::

   \begin{aligned}
   U &\leftarrow U + d \\
   V &\leftarrow c\end{aligned}

The target-regime of the :ref:`OnCondition` transition is not declared
explicitly in the XML, implying that the target-regime is the same as
the source-regime, i.e. *subthresholdRegime*.

The RegimeGraph is shown in Figure [fig:EX1\_RegimeGraph]

.. figure:: figures/example_IzRegimeTransGraph.pdf
   :alt: RegimeGraph for the XML model in this section.
   :width: 16.00000cm

   RegimeGraph for the XML model in this section.

 

Using this Abstraction Layer definition, as well as suitable parameters
from the user layer;
:math:`a=0.02, b=0.2, c=-65, d= 8, i_{\mathrm{injected}}= 5.0`, we can
simulate this, giving output as shown in Figure [fig:Ex1\_Output].

In Figure [fig:Ex1\_Output], we can see the value of the :ref:`StateVariable`
:math:`V` over time. We can also see that when the value of
:math:`V>theta` triggers the condition, we emit a spike, and the
:ref:`StateAssignment` of :math:`V \leftarrow c` resets the value of :math:`V`.
The corresponding Abstraction Layer description for this model is:


.. code-block:: yaml

    NineML:
      '@namespace': http://nineml.net/9ML/1.0
      ComponentClass:
      - name: Izhikevich
        Parameter:
        - {name: a, dimension: per_time}
        - {name: b, dimension: per_voltage}
        - {name: c, dimension: voltage}
        - {name: d, dimension: dimensionless}
        - {name: iInj, dimension: current}
        - {name: theta, dimension: voltage}
        AnalogReducePort:
        - {name: iSyn, dimension: current, operator: +}
        EventSendPort:
        - {name: spikeOutput}
        AnalogSendPort:
        - {name: V, dimension: voltage}
        Dynamics:
          StateVariable:
          - {name: U, dimension: dimensionless}
          - {name: V, dimension: voltage}
          Regime:
          - name: subthresholdRegime
            TimeDerivative:
            - {variable: U, MathInline: a*(-U + V*b)}
            - {variable: V, MathInline: (5*V + 0.04*(V*V)/unitV + unitR*(iInj + iSyn)
                + unitV*(-U + 140.0))/unitT}
            OnCondition:
            - Trigger: {MathInline: V > theta}
              target_regime: subthresholdRegime
              StateAssignment:
              - {variable: U, MathInline: U + d}
              - {variable: V, MathInline: c}
              OutputEvent:
              - {port: spikeOutput}
          Constant:
          - {name: unitR, units: Ohm, '@body': 1.0}
          - {name: unitT, units: s, '@body': 1.0}
          - {name: unitV, units: V, '@body': 1.0}
      Dimension:
      - {name: dimensionless}
      - {name: per_time, t: -1}
      - {name: per_voltage, m: -1, l: -2, t: 3, i: 1}
      - {name: voltage, m: 1, l: 2, t: -3, i: -1}
      - {symbol: Ohm, dimension: resistance, power: 0}
      - {symbol: V, dimension: voltage, power: 0}
      - {symbol: s, dimension: time, power: 1}   

User Layer description for the above example is:


.. code-block:: yaml

    NineML:
      '@namespace': http://nineml.net/9ML/1.0
      ComponentClass:
      - name: IzhikevichProperties
        Definition: {'@body': Izhikevich}
        Property:
        - {name: a, SingleValue: 0.02, units: per_s}
        - {name: b, SingleValue: 0.2, units: per_V}
        - {name: c, SingleValue: -65.0, units: mV}
        - {name: d, SingleValue: 8.0, units: unitless}
        - {name: iInj, SingleValue: 10.0, units: nA}
        - {name: theta, SingleValue: 50.0, units: mV}
      Unit:
      - {symbol: mV, dimension: voltage, power: -3}
      - {symbol: per_V, dimension: per_voltage, power: 0}
      - {symbol: per_s, dimension: per_time, power: 0}
      - {symbol: unitless, dimension: dimensionless, power: 0}
      Dimension:
      - {name: dimensionless}
      - {name: per_time, t: -1}
      - {name: per_voltage, m: -1, l: -2, t: 3, i: 1}
      - {name: voltage, m: 1, l: 2, t: -3, i: -1}      

Here, we show the simulation results of this XML representation with an
initial V=-60mV and U=0.

.. figure:: figures/example_IzVoltageWave.pdf
   :alt: Result of simulating of the XML model in this section
   :width: 15.00000cm

   Result of simulating of the XML model in this section

 
Leaky Integrate and Fire model
------------------------------

In this example, we build a representation of a integrate-and-fire
neuron, with an attached input synapse [Abbott1999]_.
We have a single :ref:`StateVariable`, *iaf\_V*. This time, the neuron has an
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

In both :ref:`Regime`\ s, the synapses dynamics evolve as:

.. math::

   \begin{aligned}
   \frac{d(cobaExcit\_g)}{dt} = - \frac{cobaExcit\_g}{cobaExcit\_tau}\end{aligned}

The neuron has two EventPorts, *iaf\_spikeoutput* is a send port, which
sends events when the neuron fires, and *cobaExcit\_spikeinput* is a
recv port, which tells the attached synapse that it should ‘fire’. The
neuron has 4 transitions, 2 :ref:`OnEvent` transitions and 2 :ref:`OnCondition`
transitions. Two of the Transitions are triggered by
*cobaExcit\_spikeinput* events, which cause the conductance of the
synapse to increase by an amount :math:`q`, These happen in both
:ref:`Regime`\ s. The other :ref:`OnCondition`\ s:

-  One is triggered the voltage being above threshold, which moves the
   component from *RegularRegime* to *RefractoryRegime*, sets V to the
   reset-voltage also emits a spike

-  The other is triggered by enough time having passed for the component
   to come out of the *RefractoryRegime* and move back to the
   *RegularRegime*

The corresponding :ref:`Regime` Graph is shown in Figure 5.

.. figure:: figures/demo2_Coba1_trnasition.pdf
   :alt: RegimeGraph for the XML model in this section
   :width: 15.00000cm

   RegimeGraph for the XML model in this section


The resulting description for the Abstraction Layer is:


.. code-block:: yaml

    NineML:
      '@namespace': http://nineml.net/9ML/1.0
      ComponentClass:
      - name: IafCoba
          Parameter:
          - {name: cobaExcit_q, dimension: conductanceDensity}
          - {name: cobaExcit_tau, dimension: time}
          - {name: cobaExcit_vrev, dimension: voltage}
          - {name: iaf_cm, dimension: capacitance}
          - {name: iaf_gl, dimension: conductanceDensity}
          - {name: iaf_taurefrac, dimension: time}
          - {name: iaf_vreset, dimension: voltage}
          - {name: iaf_vrest, dimension: voltage}
          - {name: iaf_vthresh, dimension: voltage}
          EventReceivePort:
          - {name: cobaExcit_spikeinput}
          AnalogReducePort:
          - {name: iaf_ISyn, dimension: current, operator: +}
          EventSendPort:
          - {name: iaf_spikeoutput}
          AnalogSendPort:
          - {name: cobaExcit_I, dimension: current}
          - {name: iaf_V, dimension: voltage}
          Dynamics:
            StateVariable:
            - {name: cobaExcit_g, dimension: conductanceDensity}
            - {name: iaf_V, dimension: voltage}
            - {name: iaf_tspike, dimension: time}
            Regime:
            - name: RefractoryRegime
              TimeDerivative:
              - {variable: cobaExcit_g, MathInline: -cobaExcit_g/cobaExcit_tau}
              OnEvent:
              - port: cobaExcit_spikeinput
                target_regime: RefractoryRegime
                StateAssignment:
                - {variable: cobaExcit_g, MathInline: cobaExcit_g + cobaExcit_q}
              OnCondition:
              - Trigger: {MathInline: t > iaf_taurefrac + iaf_tspike}
                target_regime: RegularRegime
            - name: RegularRegime
              TimeDerivative:
              - {variable: cobaExcit_g, MathInline: -cobaExcit_g/cobaExcit_tau}
              - {variable: iaf_V, MathInline: (cobaExcit_I + iaf_ISyn + iaf_gl*(-iaf_V +
                  iaf_vrest))/iaf_cm}
              OnEvent:
              - port: cobaExcit_spikeinput
                target_regime: RegularRegime
                StateAssignment:
                - {variable: cobaExcit_g, MathInline: cobaExcit_g + cobaExcit_q}
              OnCondition:
              - Trigger: {MathInline: iaf_V > iaf_vthresh}
                target_regime: RefractoryRegime
                StateAssignment:
                - {variable: iaf_V, MathInline: iaf_vreset}
                - {variable: iaf_tspike, MathInline: t}
                OutputEvent:
                - {port: iaf_spikeoutput}
            Alias:
            - {MathInline: cobaExcit_g*(cobaExcit_vrev - iaf_V), name: cobaExcit_I}
      Dimension:
      - {name: capacitance, m: -1, l: -2, t: 4, i: 2}
      - {name: conductanceDensity, m: -1, l: -2, t: 3, i: 2}
      - {name: time, t: 1}
      - {name: voltage, m: 1, l: 2, t: -3, i: -1}
  
The User Layer description for the above example is:

.. code-block:: yaml

    NineML:
      '@namespace': http://nineml.net/9ML/1.0
      Component:
        name: IaFCobaProperties
        Definition: {'@body': IafCoba}
        Property:
        - {name: cobaExcit_q, SingleValue: 1.0, units: uF_per_cm2}
        - {name: cobaExcit_tau, SingleValue: 2.0, units: ms}
        - {name: cobaExcit_vrev, SingleValue: 0.0, units: mV}
        - {name: iaf_cm, SingleValue: 0.02, units: nF}
        - {name: iaf_gl, SingleValue: 0.1, units: mS}
        - {name: iaf_taurefrac, SingleValue: 3.0, units: ms}
        - {name: iaf_vreset, SingleValue: -70.0, units: mV}
        - {name: iaf_vrest, SingleValue: -60.0, units: mV}
        - {name: iaf_vthresh, SingleValue: 20.0, units: mV}
      Unit:
      - {symbol: mS, dimension: conductanceDensity, power: -3}
      - {symbol: mV, dimension: voltage, power: -3}
      - {symbol: ms, dimension: time, power: -3}
      - {symbol: nF, dimension: capacitance, power: -9}
      Dimension:
      - {name: capacitance, m: -1, l: -2, t: 4, i: 2}
      - {name: conductanceDensity, m: -1, l: -2, t: 3, i: 2}
      - {name: time, t: 1}
      - {name: voltage, m: 1, l: 2, t: -3, i: -1}

The simulation results is presented in Figure 6.

.. figure:: figures/demo2_Coba1_out.pdf
   :width: 15.00000cm   

   Result of simulating of the XML model in this section.
   *cobaExcit\_spikeinput* is fed events from an external Poisson
   generator in this simulation

 
Network Models
==============
 

COBA IAF Network example
------------------------

This example is an implementation of *Benchmark 1* from
[Brette2009]_, which consists of a network of an
excitatory and inhibitory IAF populations randomly connected with COBA
synapses [Vogels2005]_. The excitatory and inhibitory
:ref:`Population` elements are created with 3,200 and 800 cells respectively.
Both populations are then concatenated into a single :ref:`Selection` element,
“AllNeurons”, which is used to randomly connect both populations to
every other neuron in the network with a 2% probability.

The abstraction layer description of the IAF input neuron ComponentClass is:

.. code-block:: yaml

    NineML:
      '@namespace': http://nineml.net/9ML/1.0
      ComponentClass:
      - name: IaF
      Parameter:
      - {name: iaf_cm, dimension: capacitance}
      - {name: iaf_gl, dimension: conductanceDensity}
      - {name: iaf_taurefrac, dimension: time}
      - {name: iaf_vreset, dimension: voltage}
      - {name: iaf_vrest, dimension: voltage}
      - {name: iaf_vthresh, dimension: voltage}
      AnalogReducePort:
      - {name: iaf_ISyn, dimension: current, operator: +}
      EventSendPort:
      - {name: iaf_spikeoutput}
      AnalogSendPort:
      - {name: iaf_V, dimension: voltage}
      Dynamics:
        StateVariable:
        - {name: iaf_V, dimension: voltage}
        - {name: iaf_tspike, dimension: time}
        Regime:
        - name: RefractoryRegime
          OnCondition:
          - Trigger: {MathInline: t > iaf_taurefrac + iaf_tspike}
            target_regime: RegularRegime
        - name: RegularRegime
          TimeDerivative:
          - {variable: iaf_V, MathInline: (iaf_ISyn + iaf_gl*(-iaf_V + iaf_vrest))/iaf_cm}
          OnCondition:
          - Trigger: {MathInline: iaf_V > iaf_vthresh}
            target_regime: RefractoryRegime
            StateAssignment:
            - {variable: iaf_V, MathInline: iaf_vreset}
            - {variable: iaf_tspike, MathInline: t}
            OutputEvent:
            - {port: iaf_spikeoutput}


The description of the COBA ComponentClass is:

.. code-block:: yaml

    NineML:
      '@namespace': http://nineml.net/9ML/1.0
      ComponentClass:
      - name: CoBa
        Parameter:
        - {name: coba_q, dimension: conductanceDensity}
        - {name: coba_tau, dimension: time}
        - {name: coba_vrev, dimension: voltage}
        EventReceivePort:
        - {name: coba_spikeinput}
        AnalogReceivePort:
        - {name: iaf_V, dimension: voltage}
        AnalogSendPort:
        - {name: coba_I, dimension: current}
        Dynamics:
          StateVariable:
          - {name: coba_g, dimension: conductanceDensity}
          Regime:
          - name: RegularRegime
            TimeDerivative:
            - {variable: coba_g, MathInline: -coba_g/coba_tau}
            OnEvent:
            - port: coba_spikeinput
              target_regime: RegularRegime
              StateAssignment:
              - {variable: coba_g, MathInline: coba_g + coba_q}
          Alias:
          - {MathInline: coba_g*(coba_vrev - iaf_V), name: coba_I}

The connection probability component class:

.. code-block:: yaml

    NineML:
      '@namespace': http://nineml.net/9ML/1.0
      - name: Probabilistic
        Parameter:
        - {name: probability, dimension: dimensionless}
        ConnectionRule: {standard_library: 'http://nineml.net/9ML/1.0/connectionrules/Probabilistic'}

.. note::

    More complex connection rules are planned for NineML v2.0

The cell :ref:`Component` are parameterized and connected together in the User
Layer via :ref:`Population`, :ref:`Selection` and :ref:`Projection` elements:

.. code-block:: yaml

    NineML:
      '@namespace': http://nineml.net/9ML/1.0
      Component:
      - name: ExcConnectProb
        Definition: {'@body': Probabilistic}
        Property:
        - {name: probability, SingleValue: 0.02, units: unitless}
      - name: IaFProperties
        Definition: {'@body': IaF}
        Property:
        - {name: iaf_cm, SingleValue: 0.2, units: nF}
        - {name: iaf_gl, SingleValue: 0.05, units: mS}
        - {name: iaf_taurefrac, SingleValue: 5.0, units: ms}
        - {name: iaf_vreset, SingleValue: -60.0, units: mV}
        - {name: iaf_vrest, SingleValue: -60.0, units: mV}
        - {name: iaf_vthresh, SingleValue: -50.0, units: mV}
      - name: IaFSynapseExcitatory
        Definition: {'@body': CoBa}
        Property:
        - {name: coba_q, SingleValue: 0.004, units: uF_per_cm2}
        - {name: coba_tau, SingleValue: 5.0, units: ms}
        - {name: coba_vrev, SingleValue: 0.0, units: mV}
      - name: IaFSynapseInhibitory
        Definition: {'@body': CoBa}
        Property:
        - {name: coba_q, SingleValue: 0.051, units: uF_per_cm2}
        - {name: coba_tau, SingleValue: 5.0, units: ms}
        - {name: coba_vrev, SingleValue: -80.0, units: mV}
      - name: InhConnectProb
        Definition: {'@body': Probabilistic}
        Property:
        - {name: probability, SingleValue: 0.02, units: unitless}
      Population:
      - name: Excitatory
        Cell:
          Reference: {'@body': IaFProperties}
        Size: 3200
      - name: Inhibitory
        Cell:
          Reference: {'@body': IaFProperties}
        Size: 800
      Selection:
      - name: AllNeurons
        Concatenate:
          Item:
          - index: 0
            Reference: {'@body': Excitatory}
          - index: 1
            Reference: {'@body': Inhibitory}
      Projection:
      - name: Excitation
        Source:
          Reference: {'@body': Excitatory}
        Destination:
          Reference: {'@body': AllNeurons}
          FromResponse:
          - {send_port: coba_I, receive_port: iaf_ISyn}
        Connectivity:
          Reference: {'@body': ExcConnectProb}
        Response:
          Reference: {'@body': IaFSynapseExcitatory}
          FromSource:
          - {send_port: iaf_spikeoutput, receive_port: coba_spikeinput}
        Delay: {SingleValue: 1.5, units: ms}
      - name: Inhibition
        Source:
          Reference: {'@body': Inhibitory}
        Destination:
          Reference: {'@body': AllNeurons}
          FromResponse:
          - {send_port: coba_I, receive_port: iaf_ISyn}
        Connectivity:
          Reference: {'@body': InhConnectProb}
        Response:
          Reference: {'@body': IaFSynapseInhibitory}
          FromSource:
          - {send_port: iaf_spikeoutput, receive_port: coba_spikeinput}
        Delay: {SingleValue: 1.5, units: ms}
      Unit:
      - {symbol: mS, dimension: conductanceDensity, power: -3}
      - {symbol: mV, dimension: voltage, power: -3}
      - {symbol: nF, dimension: capacitance, power: -9}
      - {symbol: unitless, dimension: dimensionless, power: 0}
      Dimension:
      - {name: capacitance, m: -1, l: -2, t: 4, i: 2}
      - {name: conductanceDensity, m: -1, l: -2, t: 3, i: 2}
      - {name: dimensionless}
      - {name: time, t: 1}
      - {name: voltage, m: 1, l: 2, t: -3, i: -1}        
      

.. [Abbott1999] Abbott, L.~F. (1999).
   Lapicque's introduction of the integrate-and-fire model neuron (1907)}.
   *Brain Research Bulletin*, 50(99):303--304.

.. [Brette2009] Brette, R., Rudolph, M., Carnevale, T., Hines, M., Beeman,
   D., James, M., Diesmann, M., Morrison, A., Goodman, P.~H., Jr, F. C.~H.,
   Zirpe, M., Natschl\"{a}ger, T., Pecevski, D., Ermentrout, B., Djurfeldt,
   M., Lansner, A., Rochel, O., Vieville, T., Muller, E., Davison, A.~P.,
   El, S., and Destexhe, A. (2009).
   Simulation of networks of spiking neurons: A review of tools and strategies.
   *Journal of computational neuroscience*, 23(3):349--398.    

.. [Izhikevich2003] Izhikevich, E.~M. and Izhikevich, E.~M. (2003).
   Simple model of spiking neurons.
   *IEEE Transactions on Neural Networks*, 14(6):1569--72.

.. [Vogels2005] Vogels, T.~P. and Abbott, L.~F. (2005).
   Signal Propagation and Logic Gating in Networks of Integrate-and-Fire
   Neurons. *The Journal of Neuroscience*, 25(46):10786 --10795.
