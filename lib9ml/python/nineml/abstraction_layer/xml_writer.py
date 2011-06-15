
from itertools import chain

from nineml.abstraction_layer.xmlns import *
from nineml.abstraction_layer.ports import EventPort




class ComponentVisitor(object):
    def VisitComponent(self, component):
        raise NotImplementedError()



class XMLWriter(object):

    @classmethod 
    def write( cls, component, file, flatten=True):
        assert flatten

        xml = XMLWriter().VisitComponent(component)
        doc = E.nineml(xml, xmlns=nineml_namespace)
        etree.ElementTree(doc).write(file, encoding="UTF-8", pretty_print=True, xml_declaration=True)


    def VisitComponent(self,component):
        elements =  [p.AcceptVisitor(self) for p in component.analog_ports] +\
                    [p.AcceptVisitor(self) for p in component.event_ports] +\
                    [p.AcceptVisitor(self) for p in component.parameters] +\
                    [component.dynamics.AcceptVisitor(self) ]
        return E(component.element_name, *elements, name=component.name)

    def VisitDynamics(self, dynamics):
        elements = [r.AcceptVisitor(self) for r in dynamics.regimes] + \
                   [b.AcceptVisitor(self) for b in dynamics.aliases] + \
                   [b.AcceptVisitor(self) for b in dynamics.state_variables] 
        return E(dynamics.element_name, *elements)
        

    def VisitRegime(self,regime):
        nodes = [node.AcceptVisitor(self) for node in regime.time_derivatives] +\
                [node.AcceptVisitor(self) for node in regime.on_events] +\
                [node.AcceptVisitor(self) for node in regime.on_conditions] 
        return E(regime.element_name, name=regime.name, *nodes )


    def VisitStateVariable(self, state_variable):
        return E(state_variable.element_name, name=state_variable.name,dimension='??')

    def VisitOutputEvent(self, output_event, **kwargs):
        return E('EventOut', port = output_event.port ) 

    def VisitParameter(self, parameter):
        return E(parameter.element_name, name=parameter.name, dimension='??')

    def VisitPort(self, port, **kwargs):
        if port.reduce_op:
            kwargs['reduce_op']=port.reduce_op
        return E(port.element_name, name=port.name, mode=port.mode, **kwargs)

    def VisitAssignment(self, assignment, **kwargs):
        return E(assignment.element_name,
                 E("MathInline", assignment.rhs),
                 name=assignment.name,
                 to=assignment.to)

    def VisitAlias(self, alias, **kwargs):
        return E(alias.element_name,
                 E("MathInline", alias.rhs),
                 name=alias.lhs)

    def VisitODE(self,ode,**kwargs):
        return E(ode.element_name,
                 E("MathInline", ode.rhs),
                 variable=ode.dependent_variable,
                 )

    def VisitOnCondition(self, on_condition):
        nodes = chain( on_condition.state_assignments, on_condition.event_outputs, [on_condition.condition] )
        newNodes = [ n.AcceptVisitor(self) for n in nodes ] 
        return E(on_condition.element_name,*newNodes) 

    def VisitCondition(self, condition):
        return E('Trigger',
                 E("MathInline", condition.rhs),
                 )

    # TODO:
    def VisitOnEvent(self, on_event, **kwargs):
        assert False
