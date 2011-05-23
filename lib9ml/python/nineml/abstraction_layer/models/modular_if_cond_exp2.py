"""
A composite leaky integrate-and-fire with conductance-based, exponential
synapses, like the IF_cond_exp standard cell model in PyNN

"""
import random, os
import nineml.abstraction_layer as nineml

#from nineml.abstraction_layer import NewComponent, NewModel
import nineml.abstraction_layer.models as models
comp = models.Component( "comp",
        regimes = [
            nineml.Regime(
                "dV/dt = ( (gl+gSynapicInput)*(v_rest - V) + i + i_offset)/(cm)",
                transitions = [nineml.On("V > v_thresh", do=["t_spike = t", "V = v_reset", nineml.SpikeOutputEvent], to="r2",name="t1"), ],
                name = "r1"
                ),

            nineml.Regime(
                transitions = [ nineml.On("t >= t_spike + tau_refrac", to="r1",name="t2") ],
                name = "r2"
                )
                ],
        ports = [   nineml.SendPort("V"), 
                    nineml.ReducePort("gSynapticInput", op="+"), 
                    nineml.RecvPort("q") ]
            )
    





# Create a model, composed of an iaf neuron, and 
compoundcomp = models.Model( name="", subnodes = {"c1" : comp, "c2" : comp} )





reduced = models.reduce_to_single_component(compoundcomp)
models.dump_reduced(reduced,"testOutput.txt")

base = "modular_if_cond_expr2"
reduced.to_dot(base+".dot")
os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))

print "writing xml"
reduced.write(base+".xml")


print "Writing CVODE.cpp"
models.writeCSolverSimple(reduced, filename=base+".cpp")
