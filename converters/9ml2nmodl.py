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
            condition = transition_to_subthreshold.resolve_condition()
            if isinstance(condition, basestring):
                if transition_to_subthreshold is spike_transition: # only a single regime
                    derivative_block.append("%s' = %s" %(dv, eqn.rhs))
                else:
                    # ODEs are only evaluated when the system is within the regime
                    # that defines them. Therefore we have to define a function
                    # that sets the derivative to zero when outside the appropriate
                    # regime.
                    derivative_block.append("%s' = deriv_%s(%s)" % (dv, dv, dv))
                    functions.append(dedent("""
                        FUNCTION deriv_%s(%s) {
                          if (%s) {
                            deriv_%s = %s
                          } else {
                            deriv_%s = 0.0
                          }
                        }""" % (dv, dv, condition, dv, eqn.rhs, dv)))
            elif condition is True: # automatic transition
                derivative_block.append("%s' = %s" %(dv, eqn.rhs))
            else:
                raise Exception("Don't know what to do with this transition condition: %s" % condition)
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
        condition = transition.resolve_condition()
        if isinstance(condition, basestring):
            if ">" in condition or "<" in condition:
                flag += 1
                stmt = "  WATCH (%s) %d" % (condition, flag)
                stmt = stmt.replace("=", "") # WATCH does not like >=
                watch_statements.append(stmt)
                transition.to.flag = flag
            else:
                raise Exception("Don't know what to do with this transition condition: %s" % condition)
        elif condition is True:
            pass
        else:
            raise Exception("Don't know what to do with this transition condition: %s" % condition)
    net_receive_block = ["if (flag == %d) {" % INIT_FLAG] + watch_statements
    for transition in transitions:
        if hasattr(transition.to, "flag"):
            net_receive_block.append("} else if (flag == %d) {" % transition.to.flag)
            # if there is an assignment to t within the transition, we emit an event.
            if transition.assignment:
                assert transition.assignment.expr == "t"
                net_receive_block.append("  net_event(%s)" % transition.assignment.expr) # this is a bit dodgy. Should perhaps be just net_event(t)?
                net_receive_block.append("  %s = %s" % (transition.assignment.to, transition.assignment.expr))
            # assignments are executed at the start of the regime. This does
            # not take sequences into account. Need to find an example with a
            # sequence that breaks this.
            if transition.from_ is not transition.to:
                for equation in transition.to.equations:
                  if isinstance(equation, nineml.Assignment):
                      net_receive_block.append("  %s = %s" % (equation.to, equation.expr))
    net_receive_block.append("}")
    return net_receive_block

def build_context(component):
    """
    Return a dictionary that will be used to render the NMODL template.
    """
    transition_to_subthreshold, spike_transition = check_component(component)
    derivative_block, functions = build_derivative_block(transition_to_subthreshold, spike_transition)
    net_receive_block = build_net_receive_block(transition_to_subthreshold, spike_transition)
    context = {
        "title": "Spiking node generated from the 9ML file %s using 9ml2nmodl.py version %s" % (nineml_file, nineml.__version__),
        "model_name": component.name.replace("-", "_"),
        "range_variables": ", ".join(component.parameters),
        "initial_values": "\n  ".join("%s = 0" % p for p in component.assigned_variables) + "\n  " + \
                          "\n  ".join("%s = %s" % (b.name, b.value) for b in component.bindings),
        "init_flag": INIT_FLAG,
        "parameter_values": "\n  ".join("%s = 1" % p for p in component.parameters),
        "state_variables": " ".join(component.integrated_variables),
        "assigned_variables": "\n  ".join(component.assigned_variables.difference(component.integrated_variables)),
        "function_blocks": "\n\n".join(functions),
        "derivative_block": "\n  ".join(derivative_block),
        "net_receive_block": "\n  ".join(net_receive_block),
    }
    return context

if __name__ == "__main__":
    import sys
    nineml_file = sys.argv[1]
    component = nineml.parse(nineml_file)
    output_filename = nineml_file.replace(".xml", ".mod")
    print "Converting %s to %s" % (nineml_file, output_filename)
    with open(output_filename, "w") as f:
        f.write(nmodl_template % build_context(component))