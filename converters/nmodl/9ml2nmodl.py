"""
Converts 9ML abstraction layer neuron files to NMODL.

For neuron models:
  - there must be no more than two top-level regimes (i.e. regimes connected by
    transitions, and not contained inside another regime);
  - only one of the top-level regimes may contain ODEs.
  
Copyright Andrew P. Davison, 2010 # if you edit this file, add your name here
"""

from __future__ import with_statement
import os.path
from textwrap import dedent
import nineml.abstraction_layer as nineml

template_file = os.path.join(os.path.dirname(__file__), "nmodl_template.txt")
with open(template_file) as f:
    nmodl_template = f.read()
        
INIT_FLAG = 1

def check_component(component):
    transitions = list(component.transitions)
    nt = len(transitions)
    if nt == 1:
        assert transitions[0].from_ is transitions[0].to
        transition_to_subthreshold = spike_transition = transitions[0]
    elif nt == 2:
        for i in range(1):
            assert transitions[i%2].from_ is transitions[(i+1)%2].to
        if len(list(transitions[0].from_.odes)) > 0:
            spike_transition = transitions[0]
            transition_to_subthreshold = transitions[1]
        else:
            spike_transition = transitions[1]
            transition_to_subthreshold = transitions[0]
        assert len(list(spike_transition.to.odes)) == 0
    else:
        raise Exception("Must have either one or two transitions.")
    return transition_to_subthreshold, spike_transition

def build_derivative_block(transition_to_subthreshold, spike_transition):
    functions = []
    derivative_block = []
    subthreshold_regime = transition_to_subthreshold.to
    for eqn in subthreshold_regime.equations:
        if isinstance(eqn, nineml.ODE):
            dv = eqn.dependent_variable
            if transition_to_subthreshold is spike_transition: # only a single regime
                derivative_block.append("%s' = %s" %(dv, eqn.rhs))
            else:
                # ODEs are only evaluated when the system is within the regime
                # that defines them. Therefore we have to define a function
                # that sets the derivative to zero when outside the appropriate
                # regime.
                derivative_block.append("%s' = deriv_%s(%s)" % (dv, dv, dv))
                condition = transition_to_subthreshold.condition.as_expr()
                functions.append(dedent("""
                    FUNCTION deriv_%s(%s) {
                      if (%s) {
                        deriv_%s = %s
                      } else {
                        deriv_%s = 0.0
                      }
                    }""" % (dv, dv, condition, dv, eqn.rhs, dv)))
        else:
            assert isinstance(eqn, nineml.Assignment)
            derivative_block.append("%s = %s" % (eqn.to, eqn.expr))
    return derivative_block, functions

def build_net_receive_block(transition_to_subthreshold, spike_transition):
    # For each non-automatic transition, we create a WATCH statement, with an
    # associated integer flag.
    flag = INIT_FLAG
    watch_statements = []
    transitions = set((transition_to_subthreshold, spike_transition))
    for transition in transitions:
        condition = transition.condition.as_expr()
        if ">" in condition or "<" in condition:
            flag += 1
            stmt = "  WATCH (%s) %d" % (condition, flag)
            stmt = stmt.replace("=", "") # WATCH does not like >=
            watch_statements.append(stmt)
            transition.to.flag = flag
        else:
            raise Exception("Don't know what to do with this transition condition: %s" % condition)
    net_receive_block = ["if (flag == %d) {" % INIT_FLAG] + watch_statements
    for transition in transitions:
        if hasattr(transition.to, "flag"):
            net_receive_block.append("} else if (flag == %d) {" % transition.to.flag)
            # if there is an assignment to t within the transition, we emit an event.
            for node in transition.nodes:
                if isinstance(node, nineml.EventPort) and node.name == 'spike_output':
                    net_receive_block.append("  net_event(t)")
                else:
                    net_receive_block.append("  %s = %s" % (node.to, node.expr))
    net_receive_block.append("}")
    return net_receive_block


def build_context(component, input_filename):
    """
    Return a dictionary that will be used to render the NMODL template.
    """
    transition_to_subthreshold, spike_transition = check_component(component)
    derivative_block, functions = build_derivative_block(transition_to_subthreshold, spike_transition)
    net_receive_block = build_net_receive_block(transition_to_subthreshold, spike_transition)
    assigned_variables = component.assigned_variables.difference(component.integrated_variables)
    context = {
        "title": "Spiking node generated from the 9ML file %s using 9ml2nmodl.py version %s" % (input_filename, nineml.__version__),
        "model_name": component.name.replace("-", "_"),
        "range_variables": ", ".join(component.user_parameters.union(assigned_variables)),
        "initial_values": "\n  ".join("%s = 0" % p for p in component.assigned_variables) + "\n  " + \
                          "\n  ".join("%s = %s" % (b.name, b.value) for b in component.bindings),
        "init_flag": INIT_FLAG,
        "parameter_values": "\n  ".join("%s = 1" % p for p in component.user_parameters),
        "state_variables": " ".join(component.integrated_variables),
        "assigned_variables": "\n  ".join(assigned_variables),
        "function_blocks": "\n\n".join(functions),
        "derivative_block": "\n  ".join(derivative_block),
        "net_receive_block": "\n  ".join(net_receive_block),
    }
    return context


def write_nmodl(nineml_file):
    component = nineml.parse(nineml_file)
    output_filename = nineml_file.replace(".xml", ".mod")
    print "Converting %s to %s" % (nineml_file, output_filename)
    with open(output_filename, "w") as f:
        f.write(nmodl_template % build_context(component, nineml_file))


if __name__ == "__main__":
    import sys
    write_nmodl(sys.argv[1])
    