

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
   :width: 16.00000cm

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
   :width: 15.00000cm

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

The neuron has two EventPorts, *iaf\_spikeoutput* is a send port, which
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
   :width: 15.00000cm

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
   :width: 15.00000cm   

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
