

from base import ActionVisitor, ComponentVisitor
from itertools import chain




import nineml.abstraction_layer 
from nineml.abstraction_layer.component import math_namespace
from nineml.abstraction_layer.component.namespaceaddress import NamespaceAddress





class ExpandPortDefinition(ActionVisitor):
    def __init__(self, originalname, targetname):
        
        ActionVisitor.__init__(self, explicitly_require_action_overrides=False)
        
        self.originalname = originalname
        self.targetname = targetname


    def action_assignment(self, assignment, **kwargs):
        assignment.name_transform_inplace( {self.originalname:self.targetname} )
    def action_alias(self, alias, **kwargs):
        alias.name_transform_inplace( {self.originalname:self.targetname} )
    def action_timederivative(self,time_derivative, **kwargs):
        time_derivative.name_transform_inplace( {self.originalname:self.targetname} )
    def action_condition(self, condition, **kwargs):
        condition.rhs_name_transform_inplace( {self.originalname:self.targetname} )



class ExpandAliasDefinition(ActionVisitor):
    """ An action-class that walks over a component, and expands an alias in 
    Assignments, Aliases, TimeDerivatives and Conditions
    """

    def __init__(self, originalname, targetname):
        
        ActionVisitor.__init__(self, explicitly_require_action_overrides=False)
        
        self.originalname = originalname
        self.targetname = targetname


    def action_assignment(self, assignment, **kwargs):
        assignment.name_transform_inplace( {self.originalname:self.targetname} )
    def action_alias(self, alias, **kwargs):
        alias.rhs_name_transform_inplace( {self.originalname:self.targetname} )
    def action_timederivative(self,time_derivative, **kwargs):
        time_derivative.name_transform_inplace( {self.originalname:self.targetname} )
    def action_condition(self, condition, **kwargs):
        condition.rhs_name_transform_inplace( {self.originalname:self.targetname} )

















class ClonerVisitor(ComponentVisitor):


    def prefix_variable(self, variable, **kwargs):
        prefix = kwargs.get('prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[])
        if variable in prefix_excludes:
            return variable
        if math_namespace.is_in_math_namespace(variable): 
            return variable

        else:
            return prefix + variable


    def visit_componentclass(self, component, **kwargs ):
        ccn =  nineml.abstraction_layer.ComponentClass( name = component.name,
                               parameters  = [ p.accept_visitor(self,**kwargs) for p in component.parameters  ],
                               analog_ports= [ p.accept_visitor(self,**kwargs) for p in component.analog_ports],
                               event_ports = [ p.accept_visitor(self,**kwargs) for p in component.event_ports ],
                               dynamics    = component.dynamics.accept_visitor(self,**kwargs) if component.dynamics else None,
                               subnodes = dict( [ (k, v.accept_visitor(self,**kwargs)) for (k,v) in component.subnodes.iteritems() ] ),
                               portconnections = component.portconnections[:] )
        return ccn


                               

    def visit_dynamics(self, dynamics, **kwargs):
        return nineml.abstraction_layer.Dynamics( regimes =       [ r.accept_visitor(self,**kwargs) for r in dynamics.regimes ],
                         aliases =          [ a.accept_visitor(self,**kwargs) for a in dynamics.aliases ],
                         state_variables =  [ s.accept_visitor(self,**kwargs) for s in dynamics.state_variables ] )
        
    def visit_regime(self,regime,**kwargs):
        return nineml.abstraction_layer.Regime(  name = regime.name,
                        time_derivatives =  [t.accept_visitor(self,**kwargs) for t in regime.time_derivatives],
                        on_events =         [t.accept_visitor(self,**kwargs) for t in regime.on_events],
                        on_conditions =     [t.accept_visitor(self,**kwargs) for t in regime.on_conditions],
                        )

        

    def visit_statevariable(self, state_variable,**kwargs):
        return nineml.abstraction_layer.StateVariable(name =
                self.prefix_variable( state_variable.name,**kwargs) )

        
    def visit_parameter(self, parameter, **kwargs):
        return nineml.abstraction_layer.Parameter(name = self.prefix_variable( parameter.name,**kwargs) )

    def visit_analogport(self, port, **kwargs):
        p =nineml.abstraction_layer.AnalogPort( name=
                self.prefix_variable(port.name,**kwargs) , mode=port.mode,
                reduce_op=port.reduce_op )
        return p

    def visit_eventport(self, port, **kwargs):
        return nineml.abstraction_layer.EventPort( name = self.prefix_variable(port.name,**kwargs), mode=port.mode, reduce_op=port.reduce_op )


    def visit_outputevent(self, output_event, **kwargs):
        return nineml.abstraction_layer.OutputEvent( port_name =
                self.prefix_variable( output_event.port_name, **kwargs) )


    def visit_assignment(self, assignment, **kwargs):
        from nineml.abstraction_layer.component import MathUtil
        prefix = kwargs.get( 'prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[] )

        lhs = self.prefix_variable( assignment.lhs, **kwargs )
        rhs = MathUtil.get_prefixed_rhs_string( expr_obj=assignment, prefix=prefix, exclude=prefix_excludes )

        return nineml.abstraction_layer.StateAssignment( lhs = lhs, rhs = rhs )


    def visit_alias(self, alias, **kwargs):
        from nineml.abstraction_layer.component import MathUtil
        prefix = kwargs.get( 'prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[] )


        new_alias = nineml.abstraction_layer.Alias( lhs = alias.lhs, rhs = alias.rhs )
        name_map = dict( [ (a, self.prefix_variable(a,**kwargs) ) for a in new_alias.atoms ])
        new_alias.name_transform_inplace( name_map = name_map )
        return new_alias


    def visit_timederivative(self,time_derivative,**kwargs):
        from nineml.abstraction_layer.component import MathUtil
        prefix = kwargs.get( 'prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[] )

        dep = self.prefix_variable(time_derivative.dependent_variable, **kwargs)

        rhs = MathUtil.get_prefixed_rhs_string( expr_obj=time_derivative, prefix=prefix, exclude=prefix_excludes )
        return nineml.abstraction_layer.TimeDerivative( dependent_variable = dep, rhs =rhs)


    def visit_condition(self, condition,**kwargs):
        from nineml.abstraction_layer.component import MathUtil
        prefix = kwargs.get( 'prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[] )
        rhs = MathUtil.get_prefixed_rhs_string( expr_obj=condition, prefix=prefix, exclude=prefix_excludes )
        return nineml.abstraction_layer.Condition( rhs =rhs)


    def visit_oncondition(self, on_condition,**kwargs):
        return nineml.abstraction_layer.OnCondition(
                trigger = on_condition.trigger.accept_visitor(self,**kwargs),
                event_outputs = [ e.accept_visitor(self,**kwargs) for e in on_condition.event_outputs ],
                state_assignments = [ s.accept_visitor(self,**kwargs) for s in on_condition.state_assignments],
                target_regime_name = on_condition.target_regime_name 
                )

    def visit_onevent(self, on_event, **kwargs):
        return nineml.abstraction_layer.OnEvent(
                src_port_name = self.prefix_variable(on_event.src_port_name,**kwargs),
                event_outputs = [ e.accept_visitor(self,**kwargs) for e in on_event.event_outputs ],
                state_assignments = [ s.accept_visitor(self,**kwargs) for s in on_event.state_assignments],
                target_regime_name = on_event.target_regime_name
                )




class ClonerVisitorPrefixNamespace(ClonerVisitor):
    """ A visitor that walks over a hierarchical component, and prefixes every
    variable with the namespace that that variable is in. This is preparation
    for flattening
    """

    def visit_componentclass(self, component, **kwargs ):
        prefix = component.get_node_addr().get_str_prefix()
        if prefix == '_': prefix = ''
        prefix_excludes = ['t']
        kwargs = {'prefix':prefix, 'prefix_excludes':prefix_excludes }

        
        port_connections = []
        for src,sink in component.portconnections:
            # To calculate the new address of the ports, we take of the 'local' port
            # address, i.e. the parent address, then add the prefixed string:
            src_new = NamespaceAddress.concat( src.get_parent_addr(), src.getstr() )  
            sink_new = NamespaceAddress.concat( sink.get_parent_addr(), sink.getstr() )  
            port_connections.append ( (src_new,sink_new) )
            

        return nineml.abstraction_layer.ComponentClass( name = component.name,
                               parameters  = [ p.accept_visitor(self,**kwargs) for p in component.parameters  ],
                               analog_ports= [ p.accept_visitor(self,**kwargs) for p in component.analog_ports],
                               event_ports = [ p.accept_visitor(self,**kwargs) for p in component.event_ports ],
                               dynamics    = component.dynamics.accept_visitor(self,**kwargs) if component.dynamics else None,
                               subnodes = dict( [ (k, v.accept_visitor(self,**kwargs)) for (k,v) in component.subnodes.iteritems() ] ),
                               portconnections  = port_connections
                               )












