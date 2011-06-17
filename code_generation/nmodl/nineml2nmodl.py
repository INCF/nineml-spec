#! /usr/bin/python
"""
Converts 9ML abstraction layer neuron files to NMODL.
  
Copyright Andrew P. Davison, 2010-2011 # if you edit this file, add your name here
"""

from __future__ import with_statement
import os.path
from textwrap import dedent
from jinja2 import Template 
import nineml.abstraction_layer as al

import itertools

from nineml.utility import filter_expect_single, expect_single


template_file = os.path.join(os.path.dirname(__file__), "nmodl_template.jinja")
with open(template_file) as f:
    nmodl_template = Template(f.read())

FIRST_REGIME_FLAG = 1001


def as_expr(node):
    if isinstance(node, al.Assignment):
        return node.as_expr()
    elif isinstance(node, al.EventPort):
        assert False
        return ""
    elif isinstance(node, al.OutputEvent):
        return ""
        return "OUTPUTEVENT"
    else:
        raise Exception("Don't know how to handle nodes of type %s" % type(node))
    

def deriv_func_args(component, variable):
    """ """
    args = set([variable])
    for r in component.regimes:
        for ode in (eq for eq in r.time_derivatives if eq.dependent_variable == variable):
            for name in (name for name in ode.names if name in [ sv.name for sv in component.state_variables ] ):
                args.add(name)
    return args

#def threshold_crossing(transition):
#    assert False, 'Deprecated' 
#    condition = transition.trigger.as_expr()
#    return ">" in condition or "<" in condition

def ode_for(regime, variable):
    """
    Yields the ODE for the given variable in the regime
    """
    print 'Ode for %s in Regime %s '%(variable, regime)
    odes = [eq for eq in regime.time_derivatives if eq.dependent_variable == variable.name]
    if len(odes) == 0:
        odes.append(al.ODE(variable, "t", "0.0"))
    return expect_single( odes )

def get_on_event_channel(on_event, component):
    port = filter_expect_single( component.event_ports, lambda ep:ep.name==on_event.src_port_name)
    return port.channel_


def build_channels(component):
    channels = set()
    for event_port in [ep for ep in component.event_ports if ep.mode=='recv']:
        channel = event_port.name.upper()
        if channel not in channels:
             channels.add(channel)
        event_port.channel_ = channel
    return channels

def guess_weight_variable(component):
    receive_port_variables = set(p.name for p in component.analog_ports if p.mode == "recv")
    weight_variables = receive_port_variables.difference(component.state_variables)
    if len(weight_variables) == 0:
        # if we have spike input ports, should raise Exception here
        return "w"
    elif len(weight_variables) == 1:
        return list(weight_variables)[0]
    else:
        raise Exception("Can't yet handle multiple weight variables \n(%s)"%weight_variables)

def get_weight_variable(channel, weight_variables):
   
    for k in weight_variables.keys():
        if k.upper() in channel:
            return weight_variables[k]
    if len(weight_variables) == 1:
        return weight_variables.values()[0]
    assert False


    

def build_context(component, weight_variables, input_filename="[Unknown-Filename]", hierarchical_mode=False):
    """
    Return a dictionary that will be used to render the NMODL template.
    """
    for i, regime in enumerate(component.regimes):
        regime.flag = FIRST_REGIME_FLAG + i
        regime.label = regime.name.replace(' ', '').replace('-', '_').upper()
    if not weight_variables:
        weight_variables = {'': guess_weight_variable(component)}
        
    assert component.is_flat()
    weights_as_states = False
    if hierarchical_mode:
        weights_as_states = True
   

    component.backsub_aliases()
    component.backsub_equations()
        
    
    context = {
        "input_filename": input_filename,
        "version": al.__version__,
        "component": component,
        "channels": build_channels(component),
        "weight_variables": weight_variables,
        "get_weight_variable": get_weight_variable,
        "initial_regime": list(component.regimes)[0].label,
        "as_expr": as_expr,
        "deriv_func_args": deriv_func_args,
        "ode_for": ode_for,
        
        # Added by Mike:
        "weights_as_states": weights_as_states,
        'get_on_event_channel':get_on_event_channel,
        
    }
    return context


def write_nmodl(nineml_file, weight_variables={},hierarchical_mode=False): 
    component = nineml.parse(nineml_file)
    output_filename = nineml_file.replace(".xml", ".mod").replace("-", "_")
    print "Converting %s to %s" % (nineml_file, output_filename)
    write_nmodldirect(component=component, mod_filename=output_filename, weight_variables=weight_variables, hierarchical_mode=hierarchical_mode)
    



def write_nmodldirect(component, mod_filename, weight_variables={},hierarchical_mode=False):
    
    print "Writing Mod-File %s" % mod_filename
    with open(mod_filename, "w") as f:
        context = build_context(component, weight_variables,hierarchical_mode=hierarchical_mode) 
        f.write(nmodl_template.render(context))
     


if __name__ == "__main__":
    import sys
    write_nmodl(sys.argv[1])
    
