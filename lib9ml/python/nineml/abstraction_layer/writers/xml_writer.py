
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

        xml = XMLWriter().visit(component)
        doc = E.nineml(xml, xmlns=nineml_namespace)
        etree.ElementTree(doc).write(file, encoding="UTF-8", pretty_print=True, xml_declaration=True)


    def visit_componentclass(self,component):
        elements =  [p.accept_visitor(self) for p in component.analog_ports] +\
                    [p.accept_visitor(self) for p in component.event_ports] +\
                    [p.accept_visitor(self) for p in component.parameters] +\
                    [component.dynamics.accept_visitor(self) ]
        return E('ComponentClass', *elements, name=component.name)

    def visit_dynamics(self, dynamics):
        elements = [r.accept_visitor(self) for r in dynamics.regimes] + \
                   [b.accept_visitor(self) for b in dynamics.aliases] + \
                   [b.accept_visitor(self) for b in dynamics.state_variables] 
        return E('Dynamics', *elements)
        
    def visit_regime(self,regime):
        nodes = [node.accept_visitor(self) for node in regime.time_derivatives] +\
                [node.accept_visitor(self) for node in regime.on_events] +\
                [node.accept_visitor(self) for node in regime.on_conditions] 
        return E('Regime', name=regime.name, *nodes )

    def visit_statevariable(self, state_variable):
        return E('StateVariable', name=state_variable.name,dimension='??')

    def visit_outputevent(self, output_event, **kwargs):
        return E('EventOut', port = output_event.port_name ) 

    def visit_parameter(self, parameter):
        return E('Parameter', name=parameter.name, dimension='??')

    def visit_analogport(self, port, **kwargs):
        if port.reduce_op:
            kwargs['reduce_op']=port.reduce_op
        return E('AnalogPort', name=port.name, mode=port.mode, **kwargs)

    def visit_eventport(self, port, **kwargs):
        return E('EventPort', name=port.name, mode=port.mode, **kwargs)

    def visit_assignment(self, assignment, **kwargs):
        return E('StateAssignment',
                 E("MathInline", assignment.rhs),
                 variable=assignment.lhs)

    def visit_alias(self, alias, **kwargs):
        return E('Alias',
                 E("MathInline", alias.rhs),
                 name=alias.lhs)

    def visit_timederivative(self,ode,**kwargs):
        return E('TimeDeriative',
                 E("MathInline", ode.rhs),
                 variable=ode.dependent_variable,
                 )

    def visit_oncondition(self, on_condition):
        nodes = chain( on_condition.state_assignments, on_condition.event_outputs, [on_condition.trigger] )
        newNodes = [ n.accept_visitor(self) for n in nodes ] 
        kwargs = {}
        if on_condition.target_regime:
            kwargs['target_regime'] = on_condition._target_regime.name
        return E('OnCondition',*newNodes,**kwargs) 

    def visit_condition(self, condition):
        return E('Trigger', 
                 E("MathInline" , condition.rhs),
                 )

    # TODO:
    def visit_onevent(self, on_event, **kwargs):
        elements =  [p.accept_visitor(self) for p in on_event.state_assignments] +\
                    [p.accept_visitor(self) for p in on_event.event_outputs] 
        kwargs ={'src_port':on_event.src_port_name}
        if on_event.target_regime:
            kwargs['target_regime'] = on_event.target_regime.name
        return E('OnEvent', *elements,  **kwargs )
        assert False
