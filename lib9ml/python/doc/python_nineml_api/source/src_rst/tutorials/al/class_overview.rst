
Basic Structure of NineML Abstraction Layer
===========================================

In this tutorial, we build the Izhikevich model neuron. It is defined by the
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
     *a*, *b*, *c* and *d*. Parameters are set once at the beginning of a simulation.

 * **Ports**: which allow the component to communicate with other components
     during the simulation. Ports are divided into two categories:

    - **Event** ports, which transmit or receive single, discrete *events* at
            points in time. For example, an event could represent a neuron spiking.
    - **Analog** ports, which transmit or receive continuous signals, for
            example the membrane voltage of the neuron. 

    Furthermore, ports have a ``direction``, specifying whether they represent
    information coming from the component ``send``, or information flowing into
    the component, ``recv`` (And ``reduce``, which will be discussed later.) 

In this case, the neuron receives an injected current *I*, which will be a
``recv`` Analog-port. Other components (such as synapses) may be interested in
the neuron's voltage, *V*, so we should transmit this as a ``send`` Analog-port.
When the neuron reaches the condition for firing (:math:`v> 30mV`), we may also
want to notify other components about this event, so we also have a ``send``
Event-port. 

We can build a |COMPONENTCLASS|  with this interface with the following code:


.. literalinclude:: /tutorial_example_code/example1_izikevich_interface.py

If you try running this code, you will receive the following error::

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
Izhikevich model has 2 state-variables, *U* and *V*. 

The state-variables can have different behaviours when operating in
different *Regimes*. A regime can be considered the 'mode' of the component; at
any time, the component will be in a single 'regime', and it is possible to
move between regimes.  for example, an integrate-and-fire neuron with an
explicit refractory period could be modelled as a component with two regimes, a
default regime, where injected current affects membrane voltage, where and a
second *refractory* regime where the voltage is fixed to a certain value. 

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
    to signal a spike occurring.
  * StateVariables can be changes through ``StateAssignment``. For example, a
    transition in a synapse component may cause the post-synaptic conductance
    to increase by a fixed amount.

  * The component can switch to another regime; i.e. respond to another set of
    differential equations.



For the Izhikevich model, we will use an 'OnCondition' transition, which should
update the state-variables, *U* and *V* according to the equations:


.. math::

    v \leftarrow c

    u \leftarrow u + d

We will also emit a spike on the EventPort ``spikeoutput``, as this might be
useful if we want to use this component as part of a larger system. Since we
only have a single regime, we will not change regime. 


.. literalinclude:: /tutorial_example_code/example1c_izikevich_transition.py



Multiple Regimes & Transitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We have only discussed the case of a single regime.  A leaky integrate-and-fire
model with refractory period has two dynamical regimes - the sub-threshold
regime and the refractory regime. Just for fun, we'll define the component in a
single step:


.. literalinclude:: /tutorial_example_code/example2_leaky_iaf_2regime.py

Note that here we used the *name* of the regime in the ``to`` argument to the
``On`` transition constructor, rather than a ``Regime`` object. These references
are resolved automatically when the component is built.

If the differential equations for a ``StateVariable`` are not defined within a
regime, then it is assumed that the state-variable does not change in that
Regime, i.e. d/dt = 0.




Further Classes
===============

Aliases
~~~~~~~~

Aliases are motivated by 2 cases; firstly that we would like to be able to
``send`` something other than pure ``StateVariables``,  and that often we end
up re-using calculations. For example, if we want to define a conductance-based
synapse in NineML, then we would like to specify the current in the
post-synaptic neuron.

.. literalinclude:: /tutorial_example_code/example3_cobasynapse.py

.. note::

    When specifying Aliases, we use the syntax ``:=`` instead of ``=``

In this case, we define an ``Alias``, *I*, which can used in a ``send`` port.
Aliases can also be used on the right-hand-side of other aliases, ``Condition``
s, ``StateAssignment`` s and ``TimeDerivative`` s.



Reduce Ports
~~~~~~~~~~~~

We have discussed ``send`` and ``recv`` ports, but there is another
*port-mode*, which is ``reduce``. A ``reduce`` port is also a port that takes
in data; but it can take information from multiple ``send`` ports. A typical
example might be the injected current into a neuron. Current can come into a
neuron from current injection, synapses or membrane channels. A ``recv`` port
is not sufficient in this case, because a ``recv`` port can only take
information from one other ``send`` port. Instead, we use a ``reduce`` port,
which takes an additional parameter ``reduce_op``. This specifies how the
incoming data should be defined. For example, to calculate the total current
flowing into a cell, we would ``add`` all the current sources together, so we
would create the port as::

    p = AnalogPort(name="I", mode='reduce', reduce_op='+')

See the docs for `AnalogPort` for more information.





Specifying Mathematical Strings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When specifying mathematics, we use a notation similar to C/C++. That is::

    (3B + 1)V^2

is not valid, it should be written as::

    V * V * (3*B + 1) 



Depending on what is being specified, we specify the mathematics slightly differently:

 * **Aliases** should be of the form::
    
     alias := some * equation

 * **TimeDerivatives** for a state-variable, *S*, should be of the form::
    
     dS/dt = some * equation

 * **StateAssignments** must be written out in full; there is no in-place operators::

     g += q     # Invalid
     g = g + q  # Valid



