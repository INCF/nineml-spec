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
#import os
from nineml.abstraction_layer.testing_utils import TestableComponent
from nineml.abstraction_layer.flattening import flatten, ComponentFlattener
from nineml.abstraction_layer.component_modifiers import ComponentModifier
from nineml.utility import LocationMgr
from nineml.abstraction_layer.visitors import RenameSymbol

LocationMgr.StdAppendToPath()


comp_data = TestableComponent('nestequivalent_iaf_cond_exp')

# Build the component:
component = comp_data()
component = flatten( component )
component.backsub_all()
ComponentModifier.close_all_reduce_ports(component=component)


# Copy the descriptive strings:
component.short_description = comp_data.metadata.short_description 
component.long_description = comp_data.metadata.long_description 



# Get the initial regime. If this component comes from an flattened component, 
# then we should look up the new regime from the locations in the old
# components, hence the nedd for this code:
initial_regime = comp_data.metadata.initial_regime
if component.was_flattened():
    new_regime = component.flattener.get_new_regime(initial_regime)
    initial_regime = new_regime.name



from nestbuilder import NestFileBuilder
nfb = NestFileBuilder(  nest_classname = comp_data.metadata.nest_classname, 
                        component = component, 
                        synapse_ports =  comp_data.metadata.synapse_ports,
                        initial_regime = initial_regime, # new_regime.name,
                        initial_values = comp_data.metadata.initial_values,
                        default_values = comp_data.metadata.parameters,
                        )
nfb.compile_files()


