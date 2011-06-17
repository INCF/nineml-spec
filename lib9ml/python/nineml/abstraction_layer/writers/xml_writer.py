
from itertools import chain

#from nineml.abstraction_layer.xmlns import *
from nineml.abstraction_layer.visitors import ComponentVisitor

import nineml.abstraction_layer as al
import nineml.abstraction_layer.flattening as flattening




from nineml.abstraction_layer.xmlns import nineml_namespace
from nineml.abstraction_layer.xmlns import E, etree



class XMLWriter(ComponentVisitor):

    @classmethod 
    def write( cls, component, file, flatten=True):
        assert isinstance( component,al.ComponentClass )
        if not component.is_flat():
            if not flatten: 
                assert False, 'Trying to save nested models not yet supported'
            else:
                component = flattening.ComponentFlattener(component)

        xml = XMLWriter().Visit(component)
        doc = E.nineml(xml, xmlns=nineml_namespace)
        etree.ElementTree(doc).write(file, encoding="UTF-8", pretty_print=True, xml_declaration=True)


    def VisitComponentClass(self,component):
        elements =  [p.AcceptVisitor(self) for p in component.analog_ports] +\
                    [p.AcceptVisitor(self) for p in component.event_ports] +\
                    [p.AcceptVisitor(self) for p in component.parameters] +\
                    [component.dynamics.AcceptVisitor(self) ]
        return E('ComponentClass', *elements, name=component.name)

    def VisitDynamics(self, dynamics):
        elements = [r.AcceptVisitor(self) for r in dynamics.regimes] + \
                   [b.AcceptVisitor(self) for b in dynamics.aliases] + \
                   [b.AcceptVisitor(self) for b in dynamics.state_variables] 
        return E('Dynamics', *elements)
        
    def VisitRegime(self,regime):
        nodes = [node.AcceptVisitor(self) for node in regime.time_derivatives] +\
                [node.AcceptVisitor(self) for node in regime.on_events] +\
                [node.AcceptVisitor(self) for node in regime.on_conditions] 
        return E('Regime', name=regime.name, *nodes )

    def VisitStateVariable(self, state_variable):
        return E('StateVariable', name=state_variable.name,dimension='??')

    def VisitOutputEvent(self, output_event, **kwargs):
        return E('EventOut', port = output_event.port ) 

    def VisitParameter(self, parameter):
        return E('Parameter', name=parameter.name, dimension='??')

    def VisitAnalogPort(self, port, **kwargs):
        if port.reduce_op:
            kwargs['reduce_op']=port.reduce_op
        return E(port.element_name, name=port.name, mode=port.mode, **kwargs)

    def VisitEventPort(self, port, **kwargs):
        return E(port.element_name, name=port.name, mode=port.mode, **kwargs)

    def VisitAssignment(self, assignment, **kwargs):
        return E('StateAssignment',
                 E("MathInline", assignment.rhs),
                 variable=assignment.lhs)

    def VisitAlias(self, alias, **kwargs):
        return E('Alias',
                 E("MathInline", alias.rhs),
                 name=alias.lhs)

    def VisitODE(self,ode,**kwargs):
        return E('TimeDeriative',
                 E("MathInline", ode.rhs),
                 variable=ode.dependent_variable,
                 )

    def VisitOnCondition(self, on_condition):
        nodes = chain( on_condition.state_assignments, on_condition.event_outputs, [on_condition.trigger] )
        newNodes = [ n.AcceptVisitor(self) for n in nodes ] 
        kwargs = {}
        if on_condition.target_regime:
            kwargs['target_regime'] = on_condition._target_regime.name
        return E('OnCondition',*newNodes,**kwargs) 

    def VisitCondition(self, condition):
        return E('Trigger', 
                 E("MathInline" , condition.rhs),
                 )

    # TODO:
    def VisitOnEvent(self, on_event, **kwargs):
        elements =  [p.AcceptVisitor(self) for p in on_event.state_assignments] +\
                    [p.AcceptVisitor(self) for p in on_event.event_outputs] 
        kwargs ={'src_port':on_event.src_port_name}
        if on_event.target_regime:
            kwargs['target_regime'] = on_event.target_regime.name
        return E('OnEvent', *elements,  **kwargs )
        assert False
