
leaky_iaf = Component("LeakyIAF",
                      regimes=[
                          Regime("dV/dt = (-gL*(V-vL) + Isyn)/C",
                                 transitions=On("V>Vth",
                                                do=["tspike = t",
                                                    "V = V_reset",
                                                    SpikeOutputEvent],
                                                to="refractory-regime"),
                                 name="sub-threshold-regime"),
                          Regime(transitions=On("t >= tspike + trefractory",
                                                to="sub-threshold-regime"),
                                 name="refractory-regime")
                      ],
                      ports=[ReducePort("Isyn", op="+")])


leaky_iaf.write("leaky_iaf.xml")

