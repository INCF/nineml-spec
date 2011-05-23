"""
A composite leaky integrate-and-fire with conductance-based, exponential
synapses, like the IF_cond_exp standard cell model in PyNN

"""
import random, os
import nineml.abstraction_layer as nineml

#from nineml.abstraction_layer import NewComponent, NewModel
import nineml.abstraction_layer.models as models
iaf = models.Component( "iaf",
        regimes = [
            nineml.Regime(
                "dV/dt = ( (gl+gSynapticInput)*(v_rest - V) + i + i_offset)/(cm)",
                transitions = [nineml.On("V > v_thresh", do=["t_spike = t", "V = v_reset", nineml.SpikeOutputEvent], to="refractoryregime"), ],
                name = "subthresholdregime"
                ),

            nineml.Regime(
                "dV/dt = 0",
                transitions = [ nineml.On("t >= t_spike + tau_refrac", to="subthresholdregime") ],
                name = "refractoryregime"
                )
                ],
        ports = [   nineml.SendPort("V"), 
                    nineml.ReducePort("gSynapticInput", op="+"), 
                    nineml.RecvPort("q") ]
            )
    

coba = models.Component( "CobaSyn",
        regimes = [
            nineml.Regime(
                "dg/dt = -g/tau",
                transitions = [
                       nineml.On(nineml.EventPort('spike-input', mode="recv"), do="g=g+q"),
                       ],
                name = "default-regime"
                )
                ],

            ports = [ nineml.RecvPort("V"), nineml.SendPort("g"), nineml.RecvPort("q") ]
            )




# Create a model, composed of an iaf neuron, and 
iaf_2coba_model = models.Model( name="iaf_2coba", subnodes = {"iaf" : iaf, "coba1" : coba, "coba2" : coba} )

# Connections have to be setup as strings, because we are deep-copying objects.
iaf_2coba_model.connect_ports( "iaf.V", "coba1.V" )
iaf_2coba_model.connect_ports( "iaf.V", "coba2.V" )



networkModel = models.Model(name="NetworkModel")
nNeurons = 2
namespaces = dict( [ (i, "GID%d"%i) for i in range(nNeurons) ] ) 


# Create the submodels:
for i in range(nNeurons):
    networkModel.insert_subnode( subnode = iaf_2coba_model, namespace = namespaces[i] )

#Randomly connect them together:
for i in range(nNeurons):
    sink1 = random.randint(0,nNeurons-1)
    sink2 = random.randint(0,nNeurons-1)
    networkModel.connect_ports( src = namespaces[i]+".coba1.g", sink = namespaces[sink1] + ".iaf.gSynapticInput")
    networkModel.connect_ports( src = namespaces[i]+".coba1.g", sink = namespaces[sink2] + ".iaf.gSynapticInput")



reduced = models.reduce_to_single_component(networkModel)

base = "modular_if_cond_exp"
reduced.to_dot(base+".dot",relabel_nodes=True)
print "building dot"
#os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))


print "writing xml"
reduced.write(base+".xml")



models.dump_reduced(reduced,base+"testOutput.txt")
#Alias the name:
c1 = iaf_2coba_model


print "Writing CVODE.cpp"
models.writeCSolverSimple (reduced, filename=base+".cpp")
#
#
#try:
#    # This case is used in the test suite for examples.
#    c1.write(f)
#except NameError:
#    import os
#
#    base = "if_cond_exp"
#    c1.write(base+".xml")
#    c2 = nineml.parse(base+".xml")
#    assert c1==c2
#
#    c1.to_dot(base+".dot")
#    os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))
#

