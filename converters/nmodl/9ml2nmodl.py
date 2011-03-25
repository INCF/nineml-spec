"""
Converts 9ML abstraction layer neuron files to NMODL.

For neuron models:
  - there must be no more than two top-level regimes (i.e. regimes connected by
    transitions, and not contained inside another regime);
  - only one of the top-level regimes may contain ODEs.
  
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

#def check_component(component):
#    transitions = list(component.transitions)
#    nt = len(transitions)
#    if nt == 1:
#        assert transitions[0].from_ is transitions[0].to
#        transition_to_subthreshold = spike_transition = transitions[0]
#    elif nt == 2:
#        for i in range(1):
#            assert transitions[i%2].from_ is transitions[(i+1)%2].to
#        if len(list(transitions[0].from_.odes)) > 0:
#            spike_transition = transitions[0]
#            transition_to_subthreshold = transitions[1]
#        else:
#            spike_transition = transitions[1]
#            transition_to_subthreshold = transitions[0]
#        assert len(list(spike_transition.to.odes)) == 0
#    else:
#        raise Exception("Must have either one or two transitions.")
#    return transition_to_subthreshold, spike_transition


#def build_derivative_block(transition_to_subthreshold, spike_transition):
#def build_derivative_block(component):
#    functions = []
#    derivative_block = []
#    #subthreshold_regime = transition_to_subthreshold.to
#    for variable in component.integrated_variables:
#        odes = [r.equations_for(variable) for r in ]
#        
#    for eqn in subthreshold_regime.equations:
#        if isinstance(eqn, nineml.ODE):
#            dv = eqn.dependent_variable
#            if transition_to_subthreshold is spike_transition: # only a single regime
#                derivative_block.append("%s' = %s" %(dv, eqn.rhs))
#            else:
#                # ODEs are only evaluated when the system is within the regime
#                # that defines them. Therefore we have to define a function
#                # that sets the derivative to zero when outside the appropriate
#                # regime.
#                derivative_block.append("%s' = deriv_%s(%s)" % (dv, dv, dv))
#                condition = transition_to_subthreshold.condition.as_expr()
#                functions.append(dedent("""
#                    FUNCTION deriv_%s(%s) {
#                      if (%s) {
#                        deriv_%s = %s
#                      } else {
#                        deriv_%s = 0.0
#                      }
#                    }""" % (dv, dv, condition, dv, eqn.rhs, dv)))
#        else:
#            assert isinstance(eqn, nineml.Assignment)
#            derivative_block.append("%s = %s" % (eqn.to, eqn.expr))
#    return derivative_block, functions
#
#
#def build_watch_statements(transition_to_subthreshold, spike_transition):
#    # For each non-automatic transition, we create a WATCH statement, with an
#    # associated integer flag.
#    flag = INIT_FLAG
#    watch_statements = []
#    transitions = set((transition_to_subthreshold, spike_transition))
#    for transition in transitions:
#        condition = transition.condition.as_expr()
#        if ">" in condition or "<" in condition:
#            flag += 1
#            stmt = "  WATCH (%s) %d" % (condition, flag)
#            stmt = stmt.replace("=", "") # WATCH does not like >=
#            watch_statements.append(stmt)
#            transition.to.flag = flag
#        else:
#            raise Exception("Don't know what to do with this transition condition: %s" % condition)
#    return watch_statements
#

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

#def transform_bindings(bindings):
#    constants = []
#    functions = []
#    for binding in bindings:
#        if binding.args:
#            functions.append(binding)
#        else:
#            constants.append(binding)
#    return constants, functions

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

def get_weight_variable(component):
    receive_port_variables = set(p.name for p in component.analog_ports if p.mode == "recv")
    weight_variables = receive_port_variables.difference(component.state_variables)
    if len(weight_variables) == 0:
        # if we have spike input ports, should raise Exception here
        return "w"
    elif len(weight_variables) == 1:
        return list(weight_variables)[0]
    else:
        raise Exception("Can't yet handle multiple weight variables")

def build_context(component, input_filename):
    """
    Return a dictionary that will be used to render the NMODL template.
    """
    for i, regime in enumerate(component.regimes):
        regime.flag = FIRST_REGIME_FLAG + i
        regime.label = regime.name.replace(' ', '').replace('-', '_').upper()
    context = {
        "input_filename": input_filename,
        "version": nineml.__version__,
        "component": component,
        "channels": build_channels(component),
        "weight_var": get_weight_variable(component),
        "on_input_event": on_input_event,
        "initial_regime": list(component.regimes)[0].label,
        "emit_spike": lambda node: isinstance(node, nineml.EventPort) and node.name == 'spike_output',
        "as_expr": as_expr,
        "deriv_func_args": deriv_func_args,
        "threshold_crossing": threshold_crossing,
        "ode_for": ode_for,
    }
    return context


def write_nmodl(nineml_file):
    component = nineml.parse(nineml_file)
    output_filename = nineml_file.replace(".xml", ".mod").replace("-", "_")
    print "Converting %s to %s" % (nineml_file, output_filename)
    with open(output_filename, "w") as f:
        context = build_context(component, nineml_file)
        f.write(nmodl_template.render(context))

if __name__ == "__main__":
    import sys
    write_nmodl(sys.argv[1])
    