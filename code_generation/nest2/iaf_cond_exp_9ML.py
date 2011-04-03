import nineml.abstraction_layer as nineml


# 9ML description of iaf_cond_exp
regimes = [
    nineml.Regime(
        "dV_m/dt = (-g_L*(V_m-E_L) + g_ex*(E_ex-V_m) + g_in*(E_in-V_m))/C_m",
        "dg_ex/dt = -g_ex/tau_syn_ex",
        "dg_in/dt = -g_in/tau_syn_in",
        transitions = [
            nineml.On("V_m>V_th",do=["t_spike = t","V_m = V_reset", nineml.SpikeOutputEvent],to="refractory-regime"),
            nineml.On(nineml.EventPort('excitatory', mode='recv'),do="g_ex=g_ex+q_ex"),
            nineml.On(nineml.EventPort('inhibitory', mode='recv'),do="g_in=g_in+q_in")
            ],
        name = "sub-threshold-regime"
    ),
    nineml.Regime(
        "dg_ex/dt = -g_ex/tau_syn_ex",
        "dg_in/dt = -g_in/tau_syn_in",
        transitions = [
            nineml.On("t >= t_spike + t_ref",to="sub-threshold-regime"),
            nineml.On(nineml.EventPort('excitatory', mode='recv'),do="g_ex=g_ex+q_ex"),
            nineml.On(nineml.EventPort('inhibitory', mode='recv'),do="g_in=g_in+q_in")
            ],
        name = "refractory-regime"
    )]


ports = [nineml.SendPort("V_m"),
         nineml.ReducePort("Isyn",op="+")]

iaf_cond_exp_9ML = nineml.Component("iaf_cond_exp", regimes = regimes, ports = ports)
