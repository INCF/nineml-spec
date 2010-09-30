from nineml.abstraction_layer import *

 

a_eqn = ODE("A", "t", "-A/tau_r")

b_eqn = ODE("B", "t", "-B/tau_d")

voltage_block = Assignment("gB", "1/(1 + mg_conc*eta*exp(-1*gamma*V))")

g_eqn = Assignment("g", "gB*gmax*(B-A)")

stage1 = Union(a_eqn, b_eqn, voltage_block)

event_check = Assignment("event_arrived", "testEvent(inputEvent))")

inter_event_regime = Union(event_check, Sequence(stage1, g_eqn), name="inter_event_regime")

 

tau_peak = Binding("tau_peak", "tau_r*tau_d/(tau_d - tau_r)*log(tau_d/tau_r)")

factor = Binding("factor", "1/(exp(-tau_peak/tau_d) - exp(-tau_peak/tau_r))")

a_update = Assignment("A", "A + weight*factor")

b_update = Assignment("B", "B + weight*factor")

on_event_regime = Union(a_update, b_update, name="on_event_regime")

 

transition1 = Transition(inter_event_regime, on_event_regime, condition="event_arrived")

transition2 = Transition(on_event_regime, inter_event_regime, condition=None)

 

external_parameters =  ["tau_r", "tau_d", "mg_conc", "eta", "gamma", "gmax"]

variables_from_elsewhere = ["V", "weight", "inputEvent"]

state_variables = ["g", "A", "B"]

internal_variables = ["tau_peak", "factor", "gB"]

parameters = external_parameters + variables_from_elsewhere + state_variables + internal_variables

 

component = Component("NMDA_PSR", parameters, (transition1, transition2), (tau_peak, factor))

component.write("nmda.xml")