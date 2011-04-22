"""
Converts 9ML abstraction layer neuron files to NMODL.
  
Copyright Andrew P. Davison, 2010-2011 # if you edit this file, add your name here
"""

from __future__ import with_statement
import os.path
from textwrap import dedent
from jinja2 import Template 
import nineml.abstraction_layer as nineml

template_file = os.path.join(os.path.dirname(__file__), "nmodl_template.jinja")
with open(template_file) as f:
    nmodl_template = Template(f.read())

FIRST_REGIME_FLAG = 1001


def as_expr(node):
    if isinstance(node, nineml.Assignment):
        return node.as_expr()
    elif isinstance(node, nineml.Inplace):
        return node.as_assignment().as_expr()
    else:
        raise Exception("Don't know how to handle nodes of type %s" % type(node))

def deriv_func_args(component, variable):
    """ """
    args = set([variable])
    for ode in (eq for eq in component.odes if eq.to == variable):
        for name in (name for name in ode.names if name in component.integrated_variables):
            args.add(name)
    return args

def threshold_crossing(transition):
    condition = transition.condition.as_expr()
    return ">" in condition or "<" in condition

def ode_for(regime, variable):
    """
    Yields the ODE for the given variable in the regime
    """
    odes = [eq for eq in regime.odes if eq.to == variable]
    if len(odes) == 0:
        odes.append(nineml.ODE(variable, "t", "0.0"))
    return odes[0]

def on_input_event(transition):
    return isinstance(transition.condition, nineml.EventPort) and transition.condition.mode in ('recv', 'reduce')

def build_channels(component):
    channels = set()
    for transition in component.transitions:
        if on_input_event(transition):
            channel = transition.condition.name.upper()
            if channel not in channels:
                channels.add(channel)
            transition.condition.channel = channel
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
        raise Exception("Can't yet handle multiple weight variables")

def get_weight_variable(channel, weight_variables):
    # ugly hack
    #import pdb; pdb.set_trace()
    for k in weight_variables.keys():
        if k.upper() in channel:
            return weight_variables[k]
    if len(weight_variables) == 1:
        return weight_variables.values()[0]


def unconnected_analog_receive_ports(component, weight_variables):
    receive_ports = [p.symbol for p in
                     component.filter_ports(cls=nineml.AnalogPort,
                                            mode=('recv', 'reduce')
                                            )
                     if hasattr(p, 'connected') and not p.connected]
    for weight_var in weight_variables:
        if weight_var in receive_ports:
            receive_ports.remove(weight_var)
    return receive_ports
    

def build_context(component, weight_variables, input_filename):
    """
    Return a dictionary that will be used to render the NMODL template.
    """
    for i, regime in enumerate(component.regimes):
        regime.flag = FIRST_REGIME_FLAG + i
        regime.label = regime.name.replace(' ', '').replace('-', '_').upper()
    if not weight_variables:
        weight_variables = {'': guess_weight_variable(component)}
    context = {
        "input_filename": input_filename,
        "version": nineml.__version__,
        "component": component,
        "channels": build_channels(component),
        "weight_variables": weight_variables,
        "get_weight_variable": get_weight_variable,
        "unconnected_receive_ports": unconnected_analog_receive_ports(component, weight_variables),
        "on_input_event": on_input_event,
        "initial_regime": list(component.regimes)[0].label,
        "emit_spike": lambda node: isinstance(node, nineml.EventPort) and node.name == 'spike_output',
        "as_expr": as_expr,
        "deriv_func_args": deriv_func_args,
        "threshold_crossing": threshold_crossing,
        "ode_for": ode_for,
    }
    return context


def write_nmodl(nineml_file, weight_variables={}): # weight_variables should probably be within component described in nineml_file
    component = nineml.parse(nineml_file)
    output_filename = nineml_file.replace(".xml", ".mod").replace("-", "_")
    print "Converting %s to %s" % (nineml_file, output_filename)
    with open(output_filename, "w") as f:
        context = build_context(component, weight_variables, nineml_file) # weight_variables should probably be within component
        f.write(nmodl_template.render(context))

if __name__ == "__main__":
    import sys
    write_nmodl(sys.argv[1])
    