#



from testmodels import get_iaf_2coba, get_iaf_2coba_network
import nineml.abstraction_layer.models as models
import os

def doOutputTests(component, base_name, location='out/'):
    if not os.path.exists(location): 
        os.makedirs(location)
    
    base = location + base_name
    
    # dot:
    component.to_dot(base+".dot",relabel_nodes=True)
    #os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))
    
    # xml:
    component.write(base+".xml")
    
    # human readable text format:
    models.dump_reduced(component, base+".txt")
    
    print "Writing CVODE.cpp"
    models.writeCSolverSimple (component, filename=base+".cpp")
    

def testSingleNeuron():
    base = "modular_iaf_2coba_network"
    neuron_model = get_iaf_2coba()
    reduced = models.reduce_to_single_component(neuron_model)
    doOutputTests(component=reduced, base_name=base)
    
    from nineml.nmodl import nineml2nmodl  
    nineml2nmodl.write_nmodldirect(reduced, output_filename='out/'+ base+'.mod', weight_variables={'cobaInhib':'q','cobaExcit':'q'} )
    
    
    
def testNetworkModel():
    base = "modular_iaf_2coba_network"
    network_model = get_iaf_2coba_network()
    reduced = models.reduce_to_single_component(network_model)
    doOutputTests(component=reduced, base_name=base)


testSingleNeuron()
testNetworkModel()

