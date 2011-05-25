




#SynapseDetails = namedtuple( 'SynapseDetails', ['label', 'conductance_connector', 'weight_connector', 'mode'] )




celltype = create_celltypeclass_from_component(
                                        name = "iaf_2coba",
                                        nineml_model = testModel,
                                        
                                        synapse_mode='conductance',
                                        membrane_voltage = "iaf.V",
                                        membrane_synaptic_conductance = 'iaf.gSynapticInput',
                                        
                                        synapse_components = [
                                            CoBaSyn( label='excitatory', namespace='cobaExcit', conductance_connector='g', weight_connector='q', tau='' ),
                                            CoBaSyn( label='inhibitory', namespace='cobaInhib', conductance_connector='g', weight_connector='q',  ),
                                                   ]
                                               
                                        )



parameters = {
    'iaf.C': 1.0,
    'iaf.gL': 50.0,
    'iaf.t_ref': 5.0,
    'iaf.theta': -50.0,
    'iaf.vL': -65.0,
    'iaf.V_reset': -65.0,
    'cobaExcit.tau': 2.0,
    'cobaInhib.tau': 5.0,
    'cobaExcit.E': 0.0,
    'cobaInhib.E': -70.0,

}




