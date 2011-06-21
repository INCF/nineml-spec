# A leaky_iaf which defines ports to play with an ampa synapse
import nineml.abstraction_layer as nineml

# Leaky iaf
regimes = [
    nineml.Regime(
        "dV/dt = (-gL*(V-vL) + Isyn)/C",
        transitions = nineml.On("V>Vth",do=["tspike = t","V = V_reset", nineml.SpikeOutputEvent],to="refractory-regime"),
        name = "sub-threshold-regime"
    ),
    nineml.Regime(
        transitions = nineml.On("t >= tspike + trefractory",to="sub-threshold-regime"),
        name = "refractory-regime"
    )]


ports = [nineml.SendPort("V"),
         nineml.ReducePort("Isyn",op="+")]

leaky_iaf = nineml.Component("LeakyIAF", regimes = regimes, ports = ports)

# ampa


regimes = [
    nineml.Regime(
        "dg/dt = -g/tau",
        transitions = nineml.On(nineml.SpikeInputEvent,do="g+=q")
        )]
        
ports = [nineml.RecvPort("V"),
         nineml.SendPort("Isyn = g(E-V)")]

coba_syn = nineml.Component("CoBaSynapse", regimes = regimes, ports = ports)

# User layer connects
# leaky_iaf.ports['V'] -> coba_syn.ports['V']
# coba_syn.ports['Isyn'] -> leaky_iaf.ports['Isyn'] for multiple synapses (reduce port)
# Simulator attaches to SpikeInput and SpikeOutput ports for input and output.


# write to file object f if defined

c1 = coba_syn

try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:
    import os

    base = "leaky_iaf_ampa_ports_events"
    c1.write(base+".xml")
    c2 = nineml.parse(base+".xml")
    assert c1==c2

    c1.to_dot(base+".dot")
    os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))


