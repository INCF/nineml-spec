
Basic Structure of NineML Abstraction Layer
===========================================

In this tutorial, we build the Izekevich model neuron. It is defined by the
following dynamics:

.. math::

    \frac{dV}{dt} = 0.04V^2 + 5V + 140 -u + I

    \frac{du}{dt} = a(bV -u)

where if :math:`v> 30mV` then we have a spike

.. math::

    v \leftarrow c

    u \leftarrow u + d

where :math:`a,b,c,d` are parameters of our neuron and :math:`I` is the injected current. *V* and *U* are state variables, which need to be solved over time.


Interfaces: Parameters and Ports
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We begin by defining the *interface* to our neuron. The interface is composed of 

 * **Parameters**: values used to instantiate a component of a particular type. In this case, these would be
     *a*,*b*,*c* and *d*. Parameters are set once at the beginning of a simulation.

 * **Ports**: which allow the component to communicate with other components
     during the simulation. Ports are divided into two categories:

    - **Event** ports, which transmit or recieve single, discrete *events* at
            points in time. For example, an event could represent a neuron spiking.
    - **Analog** ports, which transmit or recieve continuous signals, for
            example the membrane voltage of the neuron. 

    Furthermore, ports have a ``direction``, specifying whether they represent
    information coming from the component ``send``, or information flowing into
    the component, ``recv`` (And ``reduce``, which will be discussed later.) 

In this case, the neuron recieves an injected current *I*, which will be a
``recv`` Analog-port. Other components (such as synapses) maybe interested in
the neurons voltage, *V*, so we should transmit this as a ``send`` Analog-port.
When the neuron reaches the condition for firing (:math:`v> 30mV`), we may also
want to notify other components about this event, so we also have a ``send``
Event-port. 

We can build a |COMPONENTCLASS|  with this interface with the following code:


.. literalinclude:: /tutorial_example_code/example1_izikevich_interface.py

If you try running this code, you will recieve the following error::

    nineml.exceptions.exceptions.NineMLRuntimeError: Unable to find an Alias or State variable for analog-port: V

This is because we have defined a component and promised that we will transmit
a value over the port *V*, but we have not defined *V* anywhere. We will fix
this next.



Dynamics: Regimes & StateVariables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Now that we have defined the *interface* of the |COMPONENTCLASS|, we now need
to define the internal *dynamics* of the system, to give it some behaviour. A
ComponentClass can contain ``StateVariables``, which are variables that
describe the internal state of the neuron. Typically, these are specified by
first-order-differential equations with-respect-to time. In our example, the
Izekivich model has 2 state-variables, *U* and *V*. 

The state-variables can have different behaviours in when operating in
different *Regimes*. A regime can be considered the 'mode' of the component;
for example, an integrate-and-fire neuron with an explicit refactory period
could be modelled as a component with 2 regimes, a default regime, where
injected current affects membrane voltage, where and a second *refactory*
regime where the voltage is fixed to a certain value. This will be further
discussed in XX.

For this model, the differential-equations governing the state variables never
change, so we only need a single regime.


.. literalinclude:: /tutorial_example_code/example1b_izikevich_statevariables.py

In this case, we have specified the state-variables for this component by
explicitly providing a list of the state-variables to the ``Dynamics`` blocks.
This is not essential, if it is not provided, it will automatically be inferred
from the state-variable definitions in the Regimes, but if it is given, it must
match exactly. 

This code should now run; but we are missing the condition, :math:`v> 30mV`.




Transitions: Events, Conditions & Assignments
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We have discussed that component can contain multiple ``Regimes``. In order to
move between regimes; we introduce the idea of ``Transition`` s. A transition
can be:

  * **OnEvent** - A transition triggered by an event arriving on a ``recv``
    EventPort.  
  * **OnCondition** - A transition triggered by a condition.

When a ``transition`` occurs, three things can optionally occur:
 
  * An event can be emitted on a ``send`` EventPort, for example, when a
    membrane voltage reaches a threshold values, we may want to send an event
    to signal a spike occuring.
  * StateVariables can be changes through ``StateAssignment``. For example, a
    transition in a synapse component may cause the post-synaptic conductance
    to increase by a fixed amount.

  * The component can switch to another regime; i.e. respond to another set of
    differential equations.



For the Izekivich model, we will use an 'OnCondition' transition, which should
update the state-variables, *U* and *V* according to the equations:


.. math::

    v \leftarrow c

    u \leftarrow u + d

We will also emit a spike on the EventPort ``spikeoutput``, as this might be
useful if we want to use this component as part of a larger system. Since we
only have a single regime, we will not change regime. 


.. literalinclude:: /tutorial_example_code/example1c_izikevich_transition.py



Multiple Regimes & Transitions
-------------------------------



Further Classes
===============

Aliases
~~~~~~~~


Reduce Ports
~~~~~~~~~~~~


Specifying Mathematical Strings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Shorthands for Construction
---------------------------
* Port mode -> aliases curry classes
* Inference of parameters

Pseudo Code for Simulation:
---------------------------
