from nineml.abstraction_layer import *

# An nmda synapse

# providing parameters allows a sanity check on component construction

def get_component():
    parameters = ['E', 'eta', 'gamma', 'gmax', 'mg_conc', 'tau_d', 'tau_r', 'weight']

    inter_event_regime = Regime(
        name="inter_event_regime",
        time_derivatives = [
            "dA/dt = -A/tau_r",
            "dB/dt = -B/tau_d",],

        transition=On('spikeoutput',
                        do=["A = A + weight*factor",
                            "B = B + weight*factor"])
        )

    analog_ports = [RecvPort("V"),
             SendPort("Isyn"), # this notation takes the assignment of Isyn out of the Regime
             SendPort("gsyn")]

    c1 = ComponentClass("NMDA_PSR", 
            regimes=[inter_event_regime], 
            aliases = [
                "Isyn := g*(E - V)", # this notation takes the assignment of Isyn out of the Regime
                "gsyn := g",
                "tau_peak := tau_r*tau_d/(tau_d - tau_r)*log(tau_d/tau_r)",
                "factor := 1/(exp(-tau_peak/tau_d) - exp(-tau_peak/tau_r))",
                "gB := 1/(1 + mg_conc*eta*exp(-1*gamma*V))",
                "g := gB*gmax*(B-A)", ],
            analog_ports = analog_ports, 
            parameters = parameters)
    return c1

