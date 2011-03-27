import nineml.abstraction_layer as nineml


# 9ML description of iaf_cond_exp
regimes = [
    nineml.Regime(
        "dV/dt = (-gL*(V-vL) + g_e*(E_e-V) + g_i*(E_i-V))/C",
        "dg_e/dt = -g_e/tau_e",
        "dg_i/dt = -g_i/tau_i",
        transitions = [
            nineml.On("V>Vth",do=["tspike = t","V = V_reset", nineml.SpikeOutputEvent],to="refractory-regime"),
            nineml.On(nineml.EventPort('excitatory', mode='recv'),do="g_e=g_e+q_e"),
            nineml.On(nineml.EventPort('inhibitory', mode='recv'),do="g_i=g_i+q_i")
            ],
        name = "sub-threshold-regime"
    ),
    nineml.Regime(
        "dg_e/dt = -g_e/tau_e",
        "dg_i/dt = -g_i/tau_i",
        transitions = [
            nineml.On("t >= tspike + trefractory",to="sub-threshold-regime"),
            nineml.On(nineml.EventPort('excitatory', mode='recv'),do="g_e=g_e+q_e"),
            nineml.On(nineml.EventPort('inhibitory', mode='recv'),do="g_i=g_i+q_i")
            ],
        name = "refractory-regime"
    )]


ports = [nineml.SendPort("V"),
         nineml.ReducePort("Isyn",op="+")]

iaf_cond_exp_9ML = nineml.Component("iaf_cond_exp", regimes = regimes, ports = ports)
