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

from nineml.utility import FilterExpectSingle


template_file = os.path.join(os.path.dirname(__file__), "nmodl_template.jinja")
with open(template_file) as f:
    nmodl_template = Template(f.read())

FIRST_REGIME_FLAG = 1001


def as_expr(node):
    if isinstance(node, al.Assignment):
        return node.as_expr()
    #elif isinstance(node, nineml.Inplace):
    #    return node.as_assignment().as_expr()
    elif isinstance(node, al.EventPort):
        return ""
    else:
        raise Exception("Don't know how to handle nodes of type %s" % type(node))



     
    

def deriv_func_args(component, variable):
    """ """
    #args = set([variable])
    #for ode in (eq for eq in component.odes if eq.to == variable):
    #    for name in (name for name in ode.names if name in component.integrated_variables):
    #        args.add(name)
    #return args
    print "finding args of state:", variable
    args = set([variable])
    for r in component.regimes:
        #for eq in r.time_derivatives:
        #    print eq, eq.dependent_variable
        for ode in (eq for eq in r.time_derivatives if eq.dependent_variable == variable):
            print ode
            for name in (name for name in ode.names if name in [ sv.name for sv in component.state_variables ] ):
                print 'VAR:',name
                args.add(name)
    return args
    #assert False 

        
    for ode in (eq for eq in component.odes if eq.to == variable):
        for name in (name for name in ode.names if name in component.integrated_variables):
            args.add(name)
    return args

def threshold_crossing(transition):
    
    condition = transition.trigger.as_expr()
    return ">" in condition or "<" in condition

def ode_for(regime, variable):
    """
    Yields the ODE for the given variable in the regime
    """
    print 'Ode for %s in Regime %s '%(variable, regime)
    odes = [eq for eq in regime.odes if eq.to == variable.name]
    if len(odes) == 0:
        odes.append(al.ODE(variable, "t", "0.0"))
    assert len(odes) == 1
    return odes[0]

def on_input_event(transition):
    assert False
    return isinstance(transition.condition, al.EventPort) and transition.condition.mode in ('recv', 'reduce')


def get_on_event_channel(on_event, component):
    print 'Looking for port:', on_event
    for ep in component.event_ports:
        print ep.name
    port = FilterExpectSingle( component.event_ports, lambda ep:ep.name==on_event.src_port)
    return port.channel_


def build_channels(component):
    channels = set()
    for event_port in component.event_ports:
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


def unconnected_analog_receive_ports(component, weight_variables):
    receive_ports = [p.symbol for p in
                     component.filter_ports(cls=al.AnalogPort,
                                            mode=('recv', 'reduce')
                                            )
                     if hasattr(p, 'connected') and not p.connected]
    for weight_var in weight_variables:
        if weight_var in receive_ports:
            receive_ports.remove(weight_var)
    return receive_ports
    

def build_context(component, weight_variables, input_filename="[Unknown-Filename]", hierarchical_mode=False):
    """
    Return a dictionary that will be used to render the NMODL template.
    """
    for i, regime in enumerate(component.regimes):
        regime.flag = FIRST_REGIME_FLAG + i
        regime.label = regime.name.replace(' ', '').replace('-', '_').upper()
    if not weight_variables:
        weight_variables = {'': guess_weight_variable(component)}
        
        
    
    reduce_port_terminations = []
    weights_as_states = False
    if hierarchical_mode:
        reduce_port_terminations = ['%s = 0'%p.name for p in component.analog_ports if p.mode=='reduce']
        weights_as_states = True
   

    from nineml.abstraction_layer.models import dump_reduced
    dump_reduced( component, '/tmp/reducedcomponent.txt')
    component.backsub_aliases()
    component.backsub_equations()
    dump_reduced( component, '/tmp/reducedcomponent.txt')
        
    
    context = {
        "input_filename": input_filename,
        "version": al.__version__,
        "component": component,
        "channels": build_channels(component),
        "weight_variables": weight_variables,
        "get_weight_variable": get_weight_variable,
        "unconnected_receive_ports": unconnected_analog_receive_ports(component, weight_variables),
        "on_input_event": on_input_event,
        "initial_regime": list(component.regimes)[0].label,
        "emit_spike": lambda node: isinstance(node, al.EventPort) and node.name == 'spike_output',
        "as_expr": as_expr,
        "deriv_func_args": deriv_func_args,
        "threshold_crossing": threshold_crossing,
        "ode_for": ode_for,
        
        # Added by Mike:
        "reduce_port_terminations": reduce_port_terminations,
        "weights_as_states": weights_as_states,
        'alias_names': [b.name for b in component.aliases],
        'list':list, #This is a hack so we can build lists from inside jinja
        'sorted':sorted, #This is a hack so we can build lists from inside jinja

        'get_on_event_channel':get_on_event_channel,
    }
    return context


def write_nmodl(nineml_file, weight_variables={},hierarchical_mode=False): # weight_variables should probably be within component described in nineml_file
    component = nineml.parse(nineml_file)
    output_filename = nineml_file.replace(".xml", ".mod").replace("-", "_")
    print "Converting %s to %s" % (nineml_file, output_filename)

    write_nmodldirect(component=component, mod_filename=output_filename, weight_variables=weight_variables, hierarchical_mode=hierarchical_mode)
    
    #with open(output_filename, "w") as f:
    #    weight_variables should probably be within component
    #    context = build_context(component, weight_variables, nineml_file,hierarchical_mode=hierarchical_mode) 
    #    f.write(nmodl_template.render(context))



def write_nmodldirect(component, mod_filename, weight_variables={},hierarchical_mode=False):
    
    print "Writing Mod-File %s" % mod_filename
    with open(mod_filename, "w") as f:
        context = build_context(component, weight_variables,hierarchical_mode=hierarchical_mode) # weight_variables should probably be within component
        f.write(nmodl_template.render(context))
     


if __name__ == "__main__":
    import sys
    write_nmodl(sys.argv[1])
    
