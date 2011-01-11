
"""

Dynamic synaptic weight implementing phenomenological short term
depression and facilitation.

  Description:
  
   Implemented is the ODE form of the short-term depression and
   facilitation model as described Eq (2) and Eq (3) in [1] or Eq (6)
   in [2], whereby Eq (2) in [1] seems to have an error in the
   subscript of u_{n+1}.  It should be u_{n}.

   The model corresponds to the markram_synapse in NEST, which is a
   simplification of the NEST tsodyks_synapse (synaptic time course is
   neglected).

  References:
  
   [1] Markram, Wang, Tsodyks (1998) Differential Signaling via the same axon 
       of neocortical pyramidal neurons.  PNAS, vol 95, pp. 5323-5328.

   [2] D. Sussillo, T. Toyoizumi, and W. Maass. Self-tuning of neural circuits through
   short-term synaptic plasticity. Journal of Neurophysiology, 97:4079-4095, 2007.

Author: Eilif Muller, 2010.

"""

import nineml.abstraction_layer as nineml


regimes = [
    nineml.Regime(
        "dR/dt = (1-R)/tau_r",  # tau_r is the recovery time constant for depression
        "du/dt = -(u-U)/tau_f", # tau_f is the time constant of facilitation
        transitions = nineml.On(nineml.SpikeInputEvent,
                           do=["Wout = u*R*Win",
                               "R -= u*R",
                               "u += U*(1-u)",
                               nineml.SpikeInputEventRelay])  # Should I put a SpikeOutputEvent here?
    )]

ports = [nineml.RecvPort("Win")
         nineml.SendPort("Wout")]

c1 = nineml.Component("MarkramSynapseDynamics", regimes=regimes, ports = ports)

# write to file object f if defined
try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:
    import os

    base = "markram_synapse_dynamics"
    c1.write(base+".xml")
    c2 = nineml.parse(base+".xml")
    assert c1==c2

    c1.to_dot(base+".dot")
    os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))


