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


from iaf_cond_exp_9ML import iaf_cond_exp_9ML
import nineml.abstraction_layer as nineml
import os

iaf_cond_exp_9ML.long_description = """
Long description of the iaf_cond_exp ...
Author: Eilif Muller, Ecublens, 2011
"""

iaf_cond_exp_9ML.short_description = "Standard integrate and fire with exponential conductance based synapses"


# Things we need to know which should come from the user layer
synapse_ports = ['excitatory', 'inhibitory']
AP_port = 'spike_output'
V_port = 'V_m'
initial_regime = "sub-threshold-regime"

initial_values = {
    'V_m': -70.0,
    'g_ex': 0.0,
    'g_in': 0.0,
    't_spike':0.0
    }

default_values = {
    'tau_syn_ex': 1.5,
    'tau_syn_in': 10.0,
    'E_ex': 0.0,
    'E_in': -80.0,
    'Isyn': 0.0,
    'C_m': 1.0,
    'g_L': 10.0,
    'q_ex': 2.0,
    'q_in': 2.0,
    't_ref': 5.0,
    'V_th': -55.0,
    'E_L': -70.0,
    'V_reset': -70.0
    }


# CG specific stuff which needs to be determined somehow
nest_classname = "iaf_cond_exp_9ml"


from nestbuilder import NestFileBuilder
nfb = NestFileBuilder(  nest_classname = nest_classname, 
                        component = iaf_cond_exp_9ML, 
                        synapse_ports = synapse_ports,
                        initial_regime =  initial_regime,
                        initial_values = initial_values,
                        default_values = default_values)
nfb.compile_files()


