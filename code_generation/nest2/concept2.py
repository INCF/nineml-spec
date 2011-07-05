"""
Python lib9ml code generation for NEST is licensed under the "BSD 3 License":

Author: Abigail Morrison & Susanne Kunkel, 2011

Copyright (C) 2011 Morrison & Kunkel. All Rights Reserved.

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

from STDPid8 import STDPid8
import nineml.abstraction_layer as nineml
import os

STDPid8.long_description = """
Long description of the STDPid8 ...
Author: Abigail Morrison & Susanne Kunkel, 2011
"""

STDPid8.short_description = "Guetig-STDP synapse"

# Things we need to know which should come from the user layer
W_port = 'W'
initial_regime = "basic regime"

initial_values = {
    'W': 0.5,
    'r': 0.0,
    'o': 0.0,
    't_last_pre':0.0,
    't_last_post':0.0
    }

default_values = {
    'delay': 1.0,
    'tau_plus': 15.0,
    'tau_minus': 30.0,
    'learning_rate': 0.01,
    'alpha': 1.2,
    'mu': 0.4
    }


# CG specific stuff which needs to be determined somehow
nest_classname = "stdp_guetig_9ml"


# This is a scafolding object model ... ontop of 9ML
# Once we have 9ML component namespaceing worked out
# we should devise a system so that we can inject
# Nest specific derived classes for each of the
# 9ML AL base classes which implement the necessary
# Nest specific attributes.

class NestParameter(object):

    # TODO: units
    def __init__(self, symbol, default_value, parent_component, Ctype = "double", unit="[unspecified unit]", notes = ""):
        self.symbol = symbol
        self.Ctype = Ctype
        self.unit = unit
        self.notes = notes
        self.default_value = default_value
        self.parent_component = parent_component
        parent_component.name_map[symbol] = "P_.%s" % symbol

class NestStateVar(object):
    
    # TODO: units
    def __init__(self, symbol, initial_value, parent_component, Ctype = "nest::double_t", unit="[unspecified unit]", notes = ""):
        self.symbol = symbol
        self.Ctype = Ctype
        self.unit = unit
        self.notes = notes
        self.initial_value = initial_value
        self.parent_component = parent_component
        parent_component.name_map[symbol] = "S_.%s" % symbol

class NestODE(object):

    def __init__(self, ode, parent_regime):
        self.parent_regime = parent_regime
        parent_component = parent_regime.parent_component
        name_map = parent_component.name_map
        self.dependent_variable = ode.dependent_variable
        self.CODE = name_map[ode.dependent_variable] + " += (" + ode.rhs_name_transform(name_map) + ")*h;"

class NestCondition(object):

    def __init__(self, condition, parent_trans):
        self.parent_trans = parent_trans
        self.condition = condition
        parent_component = parent_trans.parent_component
        name_map = parent_component.name_map
        # code to evaluate the condition
        self.CODE = "transPendingTmp[%d] = (%s)" % (parent_trans.index, condition.rhs_name_transform(name_map))
        # where the state of the evaluated condition may be stored
        self.PENDING = "transPendingTmp[%d]" % parent_trans.index
        # conditions, unlike eventports, can happen only once
        self.PENDING_FINALIZE = "break;"

class NestInputEventPort(object):

    def __init__(self, eventport, parent_trans):
        self.parent_trans = parent_trans
        self.eventport = eventport
        parent_component = parent_trans.parent_component
        name_map = parent_component.name_map
        # Code to check if a spike arrived at this port
        # should check if the list ring buffer at this lag
        # is empty or not
        self.CODE = '//!B_.spike_inputs_[%s-INF_SPIKE_RECEPTOR-1].get_list(lag).empty()' % eventport.name
        
        self.PENDING = self.CODE
        # TODO:
        # For now we are dropping the weight for this event until
        # we can fix 9ML so that we can do something sensible with it.
        self.PENDING_FINALIZE = '//B_.spike_inputs_[%s-INF_SPIKE_RECEPTOR-1].get_list(lag).pop_back();' % eventport.name


class NestOutputEventPort(object):

    def __init__(self, event_port, parent_trans):
        self.parent_trans = parent_trans
        self.event_port = event_port
        self.CODE = "e.set_weight(S_.W); // TODO"


class NestAssignment(object):

    def __init__(self, assignment, parent_trans):
        self.parent_trans = parent_trans
        self.assginment = assignment
        parent_component = parent_trans.parent_component
        name_map = parent_component.name_map
        self.CODE = name_map[assignment.lhs]+" = "+assignment.rhs_name_transform(name_map)+";"


class NestRegime(object):

    def __init__(self, regime, parent_component):
        self.parent_component = parent_component
        self.regime = regime
        self._compute_symbol()
        self.odes = [NestODE(ode, self) for ode in regime.odes]

    def _compute_symbol(self):
        """ Assign the C symbol to be used in enum for this regime in nest C code """
        self.symbol = self.regime.name.upper().replace('-','_')

class NestTransition(object):

    def __init__(self, transition, parent_component, index):
        self.parent_component = parent_component
        self.transition = transition
        self.index = index
        self.to = parent_component.regime_map[transition.to.name]
        self.nodes = [NestAssignment(a, self) for a in transition.equations]
        event_ports = list(transition.event_ports)
        # we only allow a SpikeOutputPort here so assume it to be so
        if event_ports:
            self.nodes.append(NestOutputEventPort(event_ports[0],self))
            
        if isinstance(transition.condition, nineml.Condition):
            self.condition = NestCondition(transition.condition, self)
        elif isinstance(transition.condition, nineml.EventPort):
            self.condition = NestInputEventPort(transition.condition, self)
        else:
            raise ValueError, "Condition was neither a nineml.Condition, nor a nineml.EventPort"


class NestSynapse(object):
    def __init__(self, symbol, notes = ""):
        self.symbol = symbol


class NestModel(object):

    def __init__(self, nest_classname, component, initial_value_dict, default_value_dict):
        import itertools

        self.nest_classname = nest_classname
        self.component_9ml = component
        self.name_map = {}
        self.header_name = "nest_9ml_stdp_connection.h"

        attr_copy_list = ['short_description', 'long_description']
        for attr in attr_copy_list:
            self.__setattr__(attr, component.__getattribute__(attr))

        # back sub binds
        self.binds = []

        # make NestParameters
        self.parameters = [NestParameter(symbol, default_value_dict[symbol], self) for symbol in component.parameters]

        # make NestStateVars
        self.state_vars = [NestStateVar(symbol, initial_value_dict[symbol], self) for symbol in component.state_variables]

        # make regimes
        self.regimes = [NestRegime(r, self) for r in component.regimes]

        self.regime_map = {}
        for r in self.regimes:
            self.regime_map[r.regime.name] = r

        self.initial_regime = self.regime_map[initial_regime].symbol
        # NB: iteration order accross iterations is assumed in the template
        # so this must be true for the type of self.transitions
        # moreover, self.transitions must have a length, i.e. len(self.transitions)
        self.transitions = [NestTransition(t, self, i) for i,t in zip(itertools.count(), component.transitions)]

        # build transition map
        self.transition_map = {}
        for t in self.transitions:
            self.transition_map[t.transition.name] = t

        # setup transitions for the regimes
        for r in self.regimes:
            r.transitions = [self.transition_map[t.name] for t in r.regime.transitions]

        # set model to debug mode
        self.debug=True
                 

nm = NestModel(nest_classname, STDPid8, initial_values, default_values)


from Cheetah.Template import Template

nineml_namespace = {
    'SpikeOutputEvent': nineml.SpikeOutputEvent,
    'EventPort': nineml.EventPort
    }

# make the destination subdir
if not os.path.exists("nest_model"):
    os.mkdir("nest_model")

f_h = file("nest_model/nest_9ml_stdp_connection.h",'w')
t_h = Template(file="nest_9ml_stdp_connection_h_cheetah.tmpl", searchList={'model':nm})
print >> f_h, str(t_h)
f_h.close()
