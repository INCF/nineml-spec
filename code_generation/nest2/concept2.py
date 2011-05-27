"""
Python lib9ml code generation for NEST is licensed under the "BSD 3 License":

Author: Eilif Muller, 2011

Copyright (C) 2011 Eilif Muller. All Rights Reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above
    copyright notice, this list of conditions and the following
    disclaimer in the documentation and/or other materials provided
    with the distribution.

    3. The name of the authors or the INCF may not be used to endorse or promote
    products derived from this software without specific prior written
    permission. 

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
    CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
    INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
    INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
    CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
    OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
    EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


"""


import nineml.abstraction_layer as nineml
import os
#import nineml.models
from nineml.abstraction_layer.example_models import get_hierachical_iaf_2coba
from nineml.abstraction_layer.models import reduce_to_single_component, ModelToSingleComponentReducer
from nineml.abstraction_layer.models import dump_reduced



nest_classname = "iaf_cond_exp_9ml"



iaf_cond_exp_9ML_reduced = reduce_to_single_component( get_hierachical_iaf_2coba(), componentname=nest_classname )
dump_reduced(iaf_cond_exp_9ML_reduced,'reduced.txt')



iaf_cond_exp_9ML_reduced.long_description = """
Long description of the iaf_cond_exp ...
Author: Eilif Muller, Ecublens, 2011
"""


iaf_cond_exp_9ML_reduced.short_description = "Standard integrate and fire with exponential conductance based synapses"


# Things we need to know which should come from the user layer
synapse_ports = ['cobaExcit_spikeinput', 'cobaInhib_spikeinput']
AP_port = 'spike_output'
V_port = 'V_m'
initial_regime = "Regime3"

initial_values = {
    'iaf_V': -70.0,
    'cobaExcit_g': 0.0,
    'cobaInhib_g': 0.0,
    'iaf_tspike':0.0
    }


parameters = {
    'iaf.cm': 1.0,
    'iaf.gl': 50.0,
    'iaf.taurefrac': 5.0,
    'iaf.vrest': -65.0,
    'iaf.vreset': -65.0,
    'iaf.vthresh': -50.0,
    'cobaExcit.tau': 2.0,
    'cobaInhib.tau': 5.0,
    'cobaExcit.vrev': 0.0,
    'cobaInhib.vrev': -70.0,
    'cobaExcit.q': 2.0,
    'cobaInhib.q': 2.0,
    'iaf.ISyn':0.0,
}


default_values = ModelToSingleComponentReducer.flatten_namespace_dict( parameters )



from nestbuilder import NestFileBuilder
nfb = NestFileBuilder(  nest_classname = nest_classname, 
                        iaf_cond_exp_9ML = iaf_cond_exp_9ML_reduced, 
                        synapse_ports = synapse_ports,
                        initial_regime =  initial_regime,
                        initial_values = initial_values,
                        default_values = default_values,
                        
                        hack_fixed_values = {"iaf_ISyn":0.0} #"cobaInhib_q": 2.0, "cobaExcit_q":2.0, " },
                        
                        )
nfb.compile_files()


