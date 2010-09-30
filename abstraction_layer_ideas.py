from collections import namedtuple

ode = namedtuple("ode", "dependent_variable, independent_variable, expression")
edge = namedtuple("edge", "first_value, second_value")
transition = namedtuple("transition", "from_, to, condition, assignment")
expression = namedtuple("expression", "function, first_value, second_value")
assignment = namedtuple("assignment", "name, value")

def EXPRESSION(func, val1, val2):
    return expression(function=func, first_value=val1, second_value=val2)

def ODE(dep, indep, expr):
    return ode(dependent_variable=dep, independent_variable=indep, expression=expr)

def EDGE(val1, val2):
    return edge(first_value=val1, second_value=val2)

def ASSIGNMENT(name, value):
    return assignment(name=name, value=value)

def TRANSITION(from_, to, condition, assignment):
    return transition(from_=from_, to=to, condition=condition, assignment=assignment)
    
    
def add(a,b):
    return a + b

def divide(a,b):
    return a/b
    
def multiply(a,b):
    return a*b
    
def subtract(a,b):
    return a-b
    
def geq(a,b):
    return a >= b

def gt(a,b):
    return a > b


class REF(object):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return "Ref(%s)" % repr(self.value)


class NOOP(object):
    pass


class Component(object):
    pass

class LeakIAF(Component):
    parameters = ["v", "t", "spike", "Isyn", "subthreshold", "gL", "vL", "theta", "vreset", "C", "trefactory", "tspike"]
    @staticmethod
    def make_diagram(v, t, spike, Isyn, subthreshold, gL, vL, theta, vreset, C, trefractory, tspike):
        subthreshold_eqn = ODE(v, t,  EXPRESSION(divide, EXPRESSION(subtract, Isyn, EXPRESSION(multiply, gL, EXPRESSION(subtract, v, vL))), C))
        threshold_detect = ASSIGNMENT(spike, EXPRESSION(geq, v, theta))
        refractory_eqn = ASSIGNMENT(v, vreset)
        subthreshold_node = REF(subthreshold_eqn)
        threshold_node = REF(threshold_detect) # threshold_eqn?
        subthreshold_regime = EDGE(subthreshold_node, threshold_node)
        refractory_regime = ASSIGNMENT(subthreshold, EXPRESSION(gt, t, EXPRESSION(add, tspike, trefractory)))
        tspike_assignment = ASSIGNMENT(tspike, t)
        spike_transition = TRANSITION(subthreshold_regime, refractory_regime, spike, tspike_assignment)
        subthreshold_transition = TRANSITION(refractory_regime, subthreshold_regime, subthreshold, NOOP)
        return [spike_transition, subthreshold_transition]

def testEvent(event):
    if event is None:
        return False
    else:
        return True

class ExpPSGNode(Component):
    parameters = ["tau", "t", "Isyn", "V", "e", "deltag", "g", "spike", "inputEvent"]
    @staticmethod
    def make_diagram(tau, t, Isyn, V, e, deltag, g, spike, inputEvent):
        check_event = ASSIGNMENT(spike, testEvent(inputEvent))
        input_summation = ASSIGNMENT(g, EXPRESSION(add, g, deltag))
        decay_equation = ODE(g, t , EXPRESSION(multiply, -1, EXPRESSION(multiply, tau, g)))
        summation_node = REF(input_summation)
        decay_node = REF(decay_equation)
        check_regime = [REF(check_event), decay_node] # list means they happen in parallel?
        decay_regime = EDGE(summation_node, decay_node) 
        spike_transition = TRANSITION(decay_regime, check_regime, spike, NOOP) 
        return_transition = TRANSITION(check_regime, decay_regime, True, NOOP)
        return [spike_transition, return_transition]

if __name__ == "__main__":
    #for transition in LeakIAF.make_diagram(-65.0, 0.0, False, 0.0, True, 0.0, -65.0, -50.0, -65.0, 1.0, 2.0, 0.0):
    #    print transition
    print LeakIAF.make_diagram("v", "t", "spike", "Isyn", "subthreshold", "gL", "vL", "theta", "vreset", "C", "trefactory", "tspike")
    print ExpPSGNode.make_diagram("tau", "t", "Isyn", "V", "e", "deltag", "g", "spike", "inputEvent")