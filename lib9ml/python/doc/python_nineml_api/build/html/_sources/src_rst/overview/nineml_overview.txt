




Some common problems for modellers are:

 * How do I share my model with others?
 * How do I change parameters in my model, without copying-&-pasting lots of code?
 * How do I reuse parts of my model, without copying-&-pasting lots of code?





At first glance it looks like we
are making our simulation much more complex - 




Reducing Duplication: Changing Parameters
    We would like to know the effects of adjusting the 'c' parameter on the
    dynamics of a neuron for example. By defining separating the parameters from the
    component dynamics definition, we do not need to respecify the component
    dynamics for each simulation, just the values of parameters on the
    user layer. 


More Complex Neuron Definitions: Regimes and Transitions
    The dynamics of the neurons in this simulation are relatively simple.
    Imagining that we now have a neuron which can be in two ``regimes``; 
    a *regular* regime and a *refractory* regime, which it enters for a time
    after a spike. 

Composing Components: 
    Imagining our network model contains 

    
    


Extensibility and Combinmetrics.


* What is NineML

* What do the python bindings allow you to do









..
    Workflow Example
    ----------------
    
    
    To make this a bit more concrete, we use an example from, Eugene Izhikevich, in
    which 1000 point neurons are connected together and simulated. 
    The simulation is given in Matlab:
    
    http://www.izhikevich.org/publications/net.m
    
    
    .. code-block:: matlab
    
    
        % Created by Eugene M. Izhikevich, February 25, 2003
        % Excitatory neurons    Inhibitory neurons
        Ne=800;                 Ni=200;
        re=rand(Ne,1);          ri=rand(Ni,1);
        a=[0.02*ones(Ne,1);     0.02+0.08*ri];
        b=[0.2*ones(Ne,1);      0.25-0.05*ri];
        c=[-65+15*re.^2;        -65*ones(Ni,1)];
        d=[8-6*re.^2;           2*ones(Ni,1)];
        S=[0.5*rand(Ne+Ni,Ne),  -rand(Ne+Ni,Ni)];
    
        v=-65*ones(Ne+Ni,1);    % Initial values of v
        u=b.*v;                 % Initial values of u
        firings=[];             % spike timings
    
        for t=1:1000            % simulation of 1000 ms
          I=[5*randn(Ne,1);2*randn(Ni,1)]; % thalamic input
          fired=find(v>=30);    % indices of spikes
          firings=[firings; t+0*fired,fired];
          v(fired)=c(fired);
          u(fired)=u(fired)+d(fired);
          I=I+sum(S(:,fired),2);
          v=v+0.5*(0.04*v.^2+5*v+140-u+I); % step 0.5 ms
          v=v+0.5*(0.04*v.^2+5*v+140-u+I); % for numerical
          u=u+a.*(b.*v-u);                 % stability
        end;
        plot(firings(:,1),firings(:,2),'.');
    
    
    
    Using NineML, we would first define a parameterised Izhikevich neuron component in the
    `Abstraction Layer` as:
    
    .. code-block:: python 
    
    
        comp= ComponentClass(   name = "Izhikevich", 
                                regimes = [
                                    Regime(
                                        "dV/dt = 0.04*V*V + 5*V + 140.0 - U + Isyn",
                                        "dU/dt = a*(b*V - U)",
    
                                        transitions = On("V > theta",
                                                         do =["V = c", 
                                                              "U = U + d", 
                                                              OutputEvent('spike') ] )       
                                        )],
                                analog_ports = [ 
                                    SendPort("V"),
                                    ReducePort("Isyn",reduce_op="+") ],
    
                                parameters = ['a','b','c','d','theta']
                                    )
    
    Now; we are able to 
    
    [In this case we have used the NineML-Python API, but we could have equivalent
    used the Scheme or XML specifications]










NineML aims to  

What sort of things can I model with NineML
--------------------------------------------



What Other Tools/Languages Should I consider?
----------------------------------------------
