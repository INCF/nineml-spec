
XML Format
===========


Tag Descriptions
----------------




.. highlight:: xml

<NineML>
#########

::

    <NineML>

This is the root namespace tag for a NineML file. It can contain
`<ComponentClass>` elements.

.. todo::
    
    XML namespaces.





<ComponentClass>
################

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
  






<Parameter>
###########


::
    
    <Parameter name="" dimension="">

This tag specifies a parameter in the interface of the component

Attributes:

* name [Required]
* dimension [Required]

Child Elements: ``None``






<AnalogPort>
#############
    
::
    
    <AnalogPort name="" mode="" reduce_op="" dimension="" >

This tag specifies an AnalogPort in the interface of the component

Attributes:

* name [Required]
* mode [Required: 'send','recv' or 'reduce']
* reduce_op [Required if mode=='reduce']
* dimension [Required]

Child Elements: ``None``






<EventPort>
#############

::
    
    <EventPort name="" mode="">

This tag specifies an EventPort in the interface of the component

Attributes:

* name [Required]
* mode [Required: 'send','recv']
* dimension [Required]

Child Elements: ``None``




<Dynamics>
#############

::
    
    <Dynamics>

This tag specifies the dynamics of the component

Attributes: ``None``

Child Elements: 

* <StateVariable> [0+]
* <Alias> [0+]
* <Regime> [1+]





<StateVariable>
###############

::
    
    <StateVariable name='' dimension=''>

This tag declares a state-variable in the component

Attributes: 

* name [Required] (The variable name)
* dimension [Required] 

Child Elements: ``None``



<Alias>
#######

::
    
    <Alias name=''>

This tag declares an alias in the component

Attributes: 

* name [Required] (The alias name)
* dimension [Required] 

Child Elements: 

* <MathInline> [Required] (The equation on the right-hand-side of the alias)





<Regime>
########

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



<TimeDerivative>
################

::
    
    <TimeDerivative>

This tag defines the differential equation controlling the evolution of a StateVariable while
in this regime.

Attributes: 

* variable [Required] (The name of the state variable)

Child Elements: 

* <MathInline> [1] (The right-hand-side of the differential equation)



<OnCondition>
##############

::    

    <OnCondition>

This block specifies a transition from the enclosing Regime, which is triggered
by a mathematical function of the Component's Aliases, StateVariables, Ports and
Parameters.

Attributes: ``None``


Child Elements: 

* <Trigger> [1] (A <Trigger> block defining the condition that causes this
      transition to occur)
* <StateAssignment> [0+] (The state assignments that should occur when this
  transition is triggered)
* <EventOut> [0+] (The events that should be sent when this transition is triggered)

<OnEvent>
#########

::    

    <OnEvent>

This block specifies a transition from the enclosing Regime, which is triggered
by an input event.

Attributes: 

* port [Required] The name of the input event port which triggers this
  transition


Child Elements: 

* <StateAssignment> [0+] (The state assignments that should occur when this
  transition is triggered)
* <EventOut> [0+] (The events that should be sent when this transition is triggered)



<Trigger>
#########
::    

    <Trigger>

This block is used by <OnCondition> blocks to define the condition needed for
them to be triggered.


Attributes: ``None``


Child Elements: 

* <MathInline> [1] (A mathematical expression. This should evaluate to a
  boolean, for example by invoking a comparison operator :math: `('>', '<')` )


<StateAssignment>
#################
::    

    <StateAssignment>

Used in transitions to assign a value to a statevariable during a transition. 

.. note::

    'Inplace' operations arenot supported and should be written out as in full: :math:`x+=z \rightarrow x=x+z`


Attributes: 

* variable [Required] (The name of the variable to be assigned to)


Child Elements: 

* <MathInline> [1] (The right-hand-side of the assignment expression)


<EventOut>
##########
::    

    <EventOut>

Used in transitions to emit an event.

Attributes: 

* port_name [Required] (The name of the eventport to send an event over)


Child Elements: ``None``



<MathInline>
############
::    

    <MathInline>

A block used to specify mathematical expressions. The expression is expected to
be in ``C`` style and given as text. In future versions of NineML, we will
support <MathML> blocks too.



Attributes:  ``None``

Child Elements: ``None``






Example XML
-----------


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


.. highlight:: python



