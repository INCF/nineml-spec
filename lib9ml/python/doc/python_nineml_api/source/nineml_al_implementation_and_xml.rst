

The Building Blocks of the NineML Abstraction Layer
-----------------------------------------------------

A NineML component can be considered in 2 parts: the internal *dynamics* and the 
external *interface*. 






Dynamics
--------

The dynamics are the *internal* mechanisms governing the behaviour of the component.
The dynamics of a component are specified in terms of the following:

* State Variables
* Regimes
* Transitions


* Aliases
* Events


Conceptually
~~~~~~~~~~~~


StateVariables & Regime Graphs
###############################

The dynamics of a component is defined by a set of *state-variables*; variables that can
change either continuously or discontinuously as a function of time. The changes
to state variables happen in two ways:
    * through ``TimeDerivatives``  , which define the state variables
      evolution over time, for example :math:`dX/dt=1-X`

    * through ``StateAssignments``, which make discrete changes to a
      StateVariable's value, for example :math:`X = X + 1`


A component contains a `Regime-graph`; a graph consisting of `Regimes` at the
vertices, and `Transitions` at the edges. At any given time, a component will be
in a single `Regime`. A regime contains a set of TimeDerivatives, one for each
StateVariable of the component, which define how the StateVariables evolve over
time. 


This diagram shows the dynamics block for an imaginary component. 

.. image::
        _static/images/build/SimpleRegimeGraph.png


This dynamics has 3 state variables, *X,Y & Z*, and a state graph with 3
regimes, *regime1*, *regime2* & *regime3*. At any time, a component will be in one of these regimes, and the state variables will evolve accordingly. 



Components can move between Regimes via transitions. There are 2 ways of
triggering a transation:

* By a condition of the state variables, for example :math:`X>Y`.
* By an `InputEvent` on a port. (Discussed futher in XX).

When a transition is triggered; three things can happen:

* The component can change regime. For example, in the example above, if the
  component is in *regime3*, and the trigger for *t3* is satisfied, then the
  component will move into *regime1*.

* `StateAssignments` can take place, for example, :math:`X=0`
* The component can send `OutputEvents` (Dicussed further in XX)

During a transition, multiple StateAssignments and `OutputEvents` can occur.
(For more on the resolution of Transition's, see XX)




Notes:

* The Regime-graph for a component must not contain an islands - regimes which
  can not be reached from each other.
* If a TimeDerivative for a state variable is not defined in a regime, it is 
  assumed to be zero.
* A Transition does not need to lead to a change of regime. It can cause
  StateAssignments and OutputEvents, and leave the component in the original
  regime. (For example *t5* in the diagram)




Aliases
########


Aliases are motivated from two problems;

* Rather than writing long expressions for functions of state-variables, we can
  define an alias once. 
  For example, we can define chains of aliases::
    
    m_alpha = (alphaA + alphaB*V) / ( alphaC + exp((alphaD+V/alphaE)) )
    m_beta =  (betaA + betaB*V) / ( betaC + exp((betaD+V/betaE)) )
    minf = m_alpha / (m_alpha + m_beta)
    mtau = 1./(m_alpha+m_beta)
    dm/dt = (1/C) * (minf-m)/mtau

  In this case, *m_alpha*, *m_beta*, *minf* and *mtau* are all alias
  definitions. There is no reason we couldn't expand our :math:`dm/dt`
  description out to eliminate the these intermediate aliases, but the expression
  would be very long and difficult to read.

* If we would like to communicate a value other than a simple state variable to
  to another component. For example, if we have a component representing a
  neuron, which has an internal statevariable, 'V', we may be interested in
  transmitting a current, for example :math:`i=g*(E-V)`

.. note:: 
    
    Aliases are defined in the Dynamics, *not* in the Regime. This means that
    aliases are the same accross all regimes.




Events
#######

As well as being able to comminicate continuous values, components are also able
to emit and recieve `Event` s. Events are discrete notifications that are transmitted 
over EventPorts (Discussed in section XX). Since EventPorts have names, saying
that we transmit a 'spikeevent' for example would mean transmitting an event on
the EventPort called 'spikeevent'. Events can be used to signal action
potentials firing for example. 











The Interface
--------------
The interface is the *external* view of the component; what inputs and outputs the component exposes
to other components and the parameters that can be set for the component.

The interface consists of *Ports* and *Parameters*.


Parameters
============
Parameters allow us to define the dynamics of a component once, then adjust the
behaviours by using different parameters. For example, if we are building an
integrate-and-fire neuron, we can specify that the Reset-Voltage and the
Firing-Threshold are parameters, write our dynamics in terms of these
parameters, then use the *User Layer* to provide parameters to create different
neurons. Parameters are set at the start of the simulation, and remain constant
throughout.


Ports
======

Ports allow components to communicate between each other during a simulation. 
There are 2 types of ports *Analog* and *Event* ports, and each can have
different modes.

Analog Ports:
    Analog ports transmit and recieve continuous values, `Alias` es and
    `StateVariables`. Analog ports can have 3 modes:

        * ``SendPort`` - transmit data originating in this component which can be read by
          other components
        * ``RecvPort`` - recieve data from another components ``SendPort`` port.
            Each recv port can be connected to *one* ``SendPort``.

        * ``ReducePort`` - recieve data from multiple ``SendPort`` . These
          differ from ``RecvPorts`` in that they can be connected to multiple
          ``SendPort`` . ``ReducePorts`` take an additional operator,
          ``reduce_op``, which specifies how the data from multiple ``Send``
          ports should be combined to produce a single value. Currently, the
          only supported operations is `+`, which sums the inputs. The
          motivation for ``ReducePorts`` is that it allows us to make our
          component defintions more general. For example, if we are defining a
          neuron, would define a ``ReducePort`` called, ``InjectedCurrents``.
          This allows us to write the membrane equation for that neuron as:
            
          .. math::
            
            dV/dt = (1/C) * InjectedCurrents
          
          Then, when we connect this neuron to synapses, current-clamps, etc, we
          simply need to connect the SendPorts containing the currents of these components onto
          the ``InjectedCurrents`` reduce-port, within having to change our
          original component definitions.
        

Event Ports:
    Event ports transmit discrete events. They are useful for example in
    simulation of integrate-and-fire neurons to notify components about neuron's
    spiking. Event ports only have 2 modes:

        * ``SendPort`` - transmit events originating in this component which can be read by
          other components
        * ``RecvPort`` - recieve events from another components ``SendPort`` port.
            Each recv port can be connected to *multiple* ``SendPort``.

    For example, a synapse component may have a ``RecvPort`` connected to the
    pre-synaptics neurons ``SendPort`` port. When the presynaptic neuron fires;
    it delivers an event to the synapse, which could cause it to produce current
    flow in a post-synaptic neuron. 




In XML
~~~~~~~~~~~~

When writing models in XML, they are 


An example model of an Izhikevich model is given:

.. code-block:: xml

    <?xml version='1.0' encoding='UTF-8'?>
    <NineML xmlns="http://nineml.org/9ML/0.1"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://nineml.org/9ML/0.1 NineML_v0.2.xsd">

      <ComponentClass name="izhikevichCellNew">

        <Parameter name="a" dimension='none'/>
        <Parameter name="c" dimension='none'/>
        <Parameter name="b" dimension='none'/>
        <Parameter name="d" dimension='none'/>
        <Parameter name="theta" dimension='voltage'/>

        <AnalogPort name="iSyn" mode='reduce' reduce_op='+' dimension='current'/>
        <AnalogPort name="U" mode="send" dimension='none'/>
        <AnalogPort name="V" mode="send" dimension='voltage'/>
        <EventPort name="spikeOutput" mode="send"/>
        

        <Dynamics>
            
            <StateVariable name="V" dimension="voltage"/>
            <StateVariable name="U" dimension="none"/> 
              
            <Alias name='rv' dimension='none'>
                <MathInline>V*U</MathInline>
            </Alias>

            <Regime name="subthresholdRegime">
                  
              <TimeDerivative variable="U">
                <MathInline>a*(b*V - U)</MathInline>
              </TimeDerivative>

              <TimeDerivative variable="V">
                <MathInline>0.04*V*V + 5*V + 140.0 - U + iSyn</MathInline>
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
                
                <EventOut port="spikeOutput" />
                
              </OnCondition>

            </Regime>
        </Dynamics>

      </ComponentClass>
    </NineML>



Tag Details:
=============

.. highlight:: xml










::

    <NineML>

This is the root namespace tag for a NineML file. It can contain
`<ComponentClass>` elements.

.. todo::
    
    XML namespaces.






::

<ComponentClass name="">

This tag starts an abstraction layer component definition. 

Attributes:

* name [Required]

Child Elements:

* <Parameter> [0+]
* <AnalogPort>[0+]
* <EventPort> [0+]
* <Dynamics>  [1]
  







::
    
    <Parameter name="" dimension="">

This tag specifies a parameter in the interface of the component

Attributes:

* name [Required]
* dimension [Required]

Child Elements: ``None``






    
::
    
    <AnalogPort name="" mode="" reduce_op="" dimension="" >

This tag specifies an AnalogPort in the interface of the component

Attributes:

* name [Required]
* mode [Required: 'send','recv' or 'reduce']
* reduce_op [Required if mode=='reduce']
* dimension [Required]

Child Elements: ``None``







::
    
    <EventPort name="" mode="">

This tag specifies an EventPort in the interface of the component

Attributes:

* name [Required]
* mode [Required: 'send','recv']
* dimension [Required]

Child Elements: ``None``





::
    
    <Dynamics>

This tag specifies the dynamics of the component

Attributes: ``None``

Child Elements: 

* <StateVariable> [0+]
* <Alias> [0+]
* <Regime> [1+]





::
    
    <StateVariable name='' dimension=''>

This tag declares a state-variable in the component

Attributes: 

* name [Required] (The variable name)
* dimension [Required] 

Child Elements: ``None``




::
    
    <Alias name=''>

This tag declares an alias in the component

Attributes: 

* name [Required] (The alias name)
* dimension [Required] 

Child Elements: 

* <MathInline> [Required] (The equation on the right-hand-side of the alias)





::
    
    <Regime>

This tag declares an regime in the component. There must be exactly on
TimeDerivative block for each StateVariable block declared in the enclosing
<Dynamics> block, even if it has a rhs of zero.

Attributes: 

* name [Required] (The regime name)

Child Elements: 

* <TimeDerivative> [0+] 
* <OnCondition> [0+] (The transitions from this regime, triggered by conditions)
* <OnEvent> [0+] (The transitions from this regime, triggered by events)



::
    
    <TimeDerivative>

This tag defines the differential equation controlling the evolution of a StateVariable while
in this regime.

Attributes: 

* variable [Required] (The name of the state variable)

Child Elements: 

* <MathInline> [1] (The right-hand-side of the differential equation)


::    

    <OnCondition>

Blah


::    

    <OnEvent>

Blah



::    

    <Trigger>

Blah



::    

    <StateAssignment>

Blah

::    

    <EventOut>

Blah


::    

    <MathInline>

Blah







.. highlight:: python

Some tips and tricks:
---------------------
Conductance Based synapses


Example Transition Graphs of simple components:
-----------------------------------------------


