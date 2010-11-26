
import brian.stateupdater
import nineml.abstraction_layer as nineml


def regime_equations(regime, level=1, **kwargs):
    """Return Brian Equations object from a Regime"""

    eqns = []
    for e in regime.equations:
        # TODO: add units to nineml to remove the
        # __time_factor__ hack
        # insert __time_factor__ for ODEs on rhs
        if isinstance(e, nineml.ODE):
            eqn = '%s = (%s)/__time_factor__' % (e.lhs,e.rhs,)
        # TODO: handling units
        eqns.append(eqn+' : 1.')
    e = brian.equations.Equations(eqns,level=level,**kwargs)
    e.prepare()
    return e


def forward_euler_subset(eqns,S,subset,dt):
    '''
    Updates the value of the state variables in dictionary S
    with the forward Euler algorithm over step dt.

    Subset is a slice on the neuron population
    '''
    # Calculate all static variables (or do that after?)
    #for var in self._eq_names:
    #    S[var]=call_with_dict(self._function[var],S)
    # Calculate derivatives
    buffer={}
    for varname in eqns._diffeq_names_nonzero:
        f=eqns._function[varname]
        buffer[varname]=f(*[S[var,subset] for var in f.func_code.co_varnames])
    # Update variables
    for var in eqns._diffeq_names_nonzero:
        S[var,subset]+=dt*buffer[var]



class RegimeStateUpdater(object):
    '''
    This is a StateUpdater for a regime
    '''
    def __init__(self,regime, regime_id, regime_sv_id, clock=None):
        '''
        Default model: dv/dt=-v
        '''
        self.clock = clock
        if clock==None:
            raise TypeError,"a clock must be passed."

        # level = 3 should get us up to userspace
        self.eqns = regime_equations(r,level=3)
        self.regime_sv_id = regime_sv_id

    def rest(self,P):
        '''
        Sets the variables at rest.
        P is the neuron group.
        '''
        warnings.warn('Rest a non-differential equation 9ML model')

    def __call__(self,P):
        '''
        Updates the state variables.
        Careful here: always use the slice operation for affectations.
        P is the neuron group.
        Euler integration
        '''

        # update regime subset - all neurons that are in this regime
        subset = numpy.nonzero(P.state_(self.regime_sv_id)==regime_id)[0]

        # update DEs
        states={}
        for var in self.eqs._diffeq_names:
            states[var]=P.state_(var)
        if states:
            states['t']=P.clock.t #time
            forward_euler_subset(states,subset,P.clock._dt)

        

        
    def __repr__(self):
        return 'Leaky integrate-and-fire StateUpdater'
    
    def __len__(self):
        '''
        Number of state variables
        '''
        return 1



class NonlinearStateUpdater(StateUpdater):
    '''
    A nonlinear model with dynamics dX/dt = f(X).
    Uses an Equations object.
    By default, uses Euler integration.
    '''
    def __init__(self,regime,clock=None):
        '''
        Initialize a nonlinear model with dynamics dX/dt = f(X).
        f is given as an Equations object (see examples).
        If compile is True, a Python code object is compiled.
        '''
        # TODO: global pref?
        self.eqs=eqs
        self.optimized=compile
        self._first_time=True
        if freeze:
            self.eqs.compile_functions(freeze=freeze)
        if compile:
            self._code=self.eqs.forward_euler_code()
        
    def rest(self,P):
        '''
        Sets the variables at rest.
        '''
        for name,value in self.eqs.fixed_point().iteritems():
            P.state(name)[:]=value
        
    def __call__(self,P):
        '''
        Updates the state variables.
        Careful here: always use the slice operation for affectations.
        P is the neuron group.
        Euler integration.
        '''
        #if self.optimized==False:
        #    self.eqs.optimize(len(P))
        #    self.optimized=True
        # TODO: do these operations once
        #states=dict.fromkeys(self.eqs.dynamicvars)
        # store that in the neurongroup?
        if self.optimized:
            if self._first_time:
                self._first_time=False
                P._dS=0*P._S
            dt=P.clock._dt
            t=P.clock.t
            exec(self._code)
        else:
            states=dict.fromkeys(self.eqs._diffeq_names) # ={}?
            #for var in self.eqs.dynamicvars:
            for var in self.eqs._diffeq_names:
                states[var]=P.state_(var)
            states['t']=P.clock.t #time
            self.eqs.forward_euler(states,P.clock._dt)

    def __repr__(self):
        return 'Nonlinear StateUpdater with '+str(len(self))+' state variables'
    
    def __len__(self):
        '''
        Number of state variables
        '''
        return len(self.eqs)





class ComponentStateUpdater(brian.stateupdater.StateUpdater):
    """
    Creates a StateUpdater from a NineML component.

    Each regime of the component is given a 

    """

    def __init__(self, nineml_component,
                 solver=brian.stateupdater.NonlinearStateUpdater, **kwargs):
        '''
        Creates a StateUpdater from a nineml Component

        Each regime is given an updater of type regime_updater_cls
        '''
        if not isinstance(nineml_component,nineml.Component)
        self.component = nineml_component
        self.clock = kwargs.get("clock")
        if self.clock==None:
            self.clock = brian.guess_clock()

        self.optimized = kwargs.get("compile")
        if self.optimized!=False:
            raise ValueError, "'compile' kwarg not yet supported for NineML StateUpdater"
        
        self.base_regime = kwargs.get("base_regime")
        if not isinstance(self.base_regime, nineml.Regime):
            raise ValueError, "Please provide kwarg 'base_regime' for NineML StateUpdater"

        # need to assign regimes an index, so convert to a list
        self.regimes = list(self.component.regimes)


        # TODO: state vars for spike output, spike input only if ports
        # spike_output must be first as it is the "Brian threshold+reset" variable.
        self.special_state_vars = ["_9ml_spike_output","_9ml_regime","_9ml_spike_input"]
        self.special_regime_sv_id = 1

        self.state_vars = self.special_state_vars+list(self.component.integrated_variables)

        #list(self.component.state_variables)+\
        #list(self.component.bound_variables)




        # create a sub-updater for equations for
        # each regime
        self.sub_up_map = {}
        self.regime2id_map = {}
        for i,r in enumerate(self.regimes):
            # check if the regime has ODEs for brian to solve
            if list(r.odes):
                regime_updater_cls = DERegimeStateUpdater
            else:
                regime_updater_cls = NonDERegimeStateUpdater
            # level=2 should get us to the namespace calling this constructer
            # TODO:for compile=True ... sub-regimes don't get correct _S, _dS for __call__ ...
            self.sub_SU_map[r]=regime_updater_cls(r, i, self.special_regime_sv_id, clock)
            self.regime2id_map[r]=i
            


        # assign place in state vector for each state var
        self.sv_map = {}
        i=0
        for sv in self.state_vars:
            self.sv_map[i]=sv
            i+=1

            
    def rest(self, P):
        '''
        Sets the variables at rest.
        '''
        self._S[self.special_regime_id,:]=float(self.regime2id_map[self.base_regime])
        self.sub_SU_map[self.base_regime].rest(P)
        # set regime state

    def __call__(self, P):
        '''
        Updates the state variables.
        Careful here: always use the slice operation for affectations.
        P is the neuron group.
        '''
        
        # 1) Do regime state updates


        # 2) Evaluate Events

        # -> SpikeOutput port -> _9ml_spike_output

        # 3) Evaluate Transitions
        

        



    def __repr__(self):
        return 'Leaky integrate-and-fire StateUpdater'

    def __len__(self):
        '''
        Number of state variables
        '''
        return len(self.state_vars)

