

class ComponentVisitor(object):
    def VisitComponent(self, component):
        raise NotImplementedError()







from nineml.abstraction_layer.xmlns import *
from nineml.abstraction_layer.ports import EventPort

class XMLWriterOld(object):


    def VisitComponent(self,component):
        elements = [E.parameter(name=p) for p in component.parameters] + \
                   [p.AcceptVisitor(self) for p in component.analog_ports] +\
                   [r.AcceptVisitor(self) for r in component.regimes] + \
                   [b.AcceptVisitor(self) for b in component.aliases] +\
                   [t.AcceptVisitor(self) for t in component.transitions]
        print elements
        attrs = {"name": component.name}
        return E(component.element_name, *elements, **attrs)

    def VisitRegime(self,regime):
        kwargs = {}
        #nodes = [node.to_xml() for node in regime.nodes]
        nodes = [node.AcceptVisitor(self) for node in regime.nodes]
        return E(regime.element_name,
                 name=regime.name,
                 *nodes,
                 **kwargs)

    def VisitTransition(self, transition):
        attrs = {"name": transition.name}
        args = []

        # TODO this duality of EventPorts and Conditions
        # should be cleaned up
        if isinstance(transition.condition,EventPort):
            args+=[E("condition-on-event-port", transition.condition.to_xml())]
        else:
            attrs["condition"] = transition.condition.rhs

            
        if transition.to:
            attrs['to'] = transition.to.name
        if transition.from_:
            attrs['from'] = transition.from_.name
        
        nodeElements = [ node.AcceptVisitor(self) for node in transition.nodes]
        return E(transition.element_name, *args+nodeElements, **attrs)
        #return E(transition.element_name, *args+[node.to_xml() for node in transition.nodes], **attrs)


    def VisitPort(self, port, **kwargs):
        if port.reduce_op:
            kwargs['op']=port.reduce_op
        if port.expr:
            kwargs['expression']=port.expr.rhs
        return E(port.element_name, symbol=port.symbol,
                 mode=port.mode, **kwargs)
        

    def VisitAssignment(self, assignment, **kwargs):
        return E(assignment.element_name,
                 E("math-inline", assignment.rhs),
                 name=assignment.name,
                 to=assignment.to)

    def VisitAlias(self, alias, **kwargs):
        return E(alias.element_name,
                 E("math-inline", alias.rhs),
                 name=alias.lhs)

    def VisitODE(self,ode,**kwargs):
        return E(ode.element_name,
                 E("math-inline", ode.rhs),
                 name=ode.name,
                 dependent_variable=ode.dependent_variable,
                  independent_variable = ode.indep_variable)



    def VisitOnEvent(self, on_event, **kwargs):
        assert False

    def VisitOnCondition(self, condition):
        return E( "CONDITION-TEMP") 
        assert False
