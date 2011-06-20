

from base import InplaceActionVisitorDF
from itertools import chain




from nineml.abstraction_layer.component import math_namespace
from nineml.abstraction_layer.component.expressions import MathUtil
from nineml.abstraction_layer.component.namespaceaddress import NamespaceAddress




class ExpandPortDefinition(InplaceActionVisitorDF):
    def __init__(self, originalname, targetname):
        
        InplaceActionVisitorDF.__init__(self, explicitly_require_action_overrides=False)
        
        self.originalname = originalname
        self.targetname = targetname


    def ActionAssignment(self, assignment, **kwargs):
        assignment.name_transform_inplace( {self.originalname:self.targetname} )
    def ActionAlias(self, alias, **kwargs):
        alias.name_transform_inplace( {self.originalname:self.targetname} )
    def ActionODE(self,ode, **kwargs):
        ode.name_transform_inplace( {self.originalname:self.targetname} )
    def ActionCondition(self, condition, **kwargs):
        condition.rhs_name_transform_inplace( {self.originalname:self.targetname} )



class ExpandAliasDefinition(InplaceActionVisitorDF):
    def __init__(self, originalname, targetname):
        
        InplaceActionVisitorDF.__init__(self, explicitly_require_action_overrides=False)
        
        self.originalname = originalname
        self.targetname = targetname


    def ActionAssignment(self, assignment, **kwargs):
        assignment.name_transform_inplace( {self.originalname:self.targetname} )
    def ActionAlias(self, alias, **kwargs):
        alias.rhs_name_transform_inplace( {self.originalname:self.targetname} )
    def ActionODE(self,ode, **kwargs):
        ode.name_transform_inplace( {self.originalname:self.targetname} )
    def ActionCondition(self, condition, **kwargs):
        condition.rhs_name_transform_inplace( {self.originalname:self.targetname} )
















#import nineml.abstraction_layer as al
#import .. as al
import nineml.abstraction_layer 

from base import ComponentVisitor

class ClonerVisitor(ComponentVisitor):


    def prefixVariable(self, variable, **kwargs):
        prefix = kwargs.get('prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[])
        if variable in prefix_excludes:
            return variable
        else:
            return prefix + variable


    def VisitComponentClass(self, component, **kwargs ):
        ccn =  nineml.abstraction_layer.ComponentClass( name = component.name,
                               parameters  = [ p.accept_visitor(self,**kwargs) for p in component.parameters  ],
                               analog_ports= [ p.accept_visitor(self,**kwargs) for p in component.analog_ports],
                               event_ports = [ p.accept_visitor(self,**kwargs) for p in component.event_ports ],
                               dynamics    = component.dynamics.accept_visitor(self,**kwargs) if component.dynamics else None,
                               subnodes = dict( [ (k, v.accept_visitor(self,**kwargs)) for (k,v) in component.subnodes.iteritems() ] ),
                               portconnections = component.portconnections[:] )
        return ccn


                               

    def VisitDynamics(self, dynamics, **kwargs):
        return nineml.abstraction_layer.Dynamics( regimes =       [ r.accept_visitor(self,**kwargs) for r in dynamics.regimes ],
                         aliases =          [ a.accept_visitor(self,**kwargs) for a in dynamics.aliases ],
                         state_variables =  [ s.accept_visitor(self,**kwargs) for s in dynamics.state_variables ] )
        
    def VisitRegime(self,regime,**kwargs):
        return nineml.abstraction_layer.Regime(  name = regime.name,
                        time_derivatives =  [t.accept_visitor(self,**kwargs) for t in regime.time_derivatives],
                        on_events =         [t.accept_visitor(self,**kwargs) for t in regime.on_events],
                        on_conditions =     [t.accept_visitor(self,**kwargs) for t in regime.on_conditions],
                        )

        

    def VisitStateVariable(self, state_variable,**kwargs):
        return nineml.abstraction_layer.StateVariable(name = self.prefixVariable( state_variable.name,**kwargs) )

        
    def VisitParameter(self, parameter, **kwargs):
        return nineml.abstraction_layer.Parameter(name = self.prefixVariable( parameter.name,**kwargs) )

    def VisitAnalogPort(self, port, **kwargs):
        p =nineml.abstraction_layer.AnalogPort( name=
                self.prefixVariable(port.name,**kwargs) , mode=port.mode,
                reduce_op=port.reduce_op )
        return p

    def VisitEventPort(self, port, **kwargs):
        return nineml.abstraction_layer.EventPort( name = self.prefixVariable(port.name,**kwargs), mode=port.mode, reduce_op=port.reduce_op )


    def VisitOutputEvent(self, output_event, **kwargs):
        return nineml.abstraction_layer.OutputEvent( port_name = self.prefixVariable( output_event.port_name, **kwargs) )

    def VisitInputEvent(self, input_event, **kwargs):
        return nineml.abstraction_layer.InputEvent( port_name = self.prefixVariable( input_event.port_name, **kwargs) )

    def VisitAssignment(self, assignment, **kwargs):
        prefix = kwargs.get( 'prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[] )
        lhs = assignment.lhs if assignment.lhs in prefix_excludes else prefix + assignment.lhs
        rhs = MathUtil.get_prefixed_rhs_string( expr_obj=assignment, prefix=prefix, exclude=prefix_excludes )

        return nineml.abstraction_layer.StateAssignment( lhs = lhs, rhs = rhs )


    def VisitAlias(self, alias, **kwargs):
        prefix = kwargs.get( 'prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[] )

        def doPrefix(atom):
            if a in prefix_excludes: return False
            if math_namespace.is_in_math_namespace(a): return False
            return True

        new_alias = nineml.abstraction_layer.Alias( lhs = alias.lhs, rhs = alias.rhs )
        name_map = dict( [ (a, prefix+a) for a in new_alias.atoms if doPrefix(a) ])
        new_alias.name_transform_inplace( name_map = name_map )
        return new_alias


    def VisitTimeDerivative(self,ode,**kwargs):
        prefix = kwargs.get( 'prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[] )

        dep = ode.dependent_variable if ode.dependent_variable in prefix_excludes else prefix + ode.dependent_variable
        rhs = MathUtil.get_prefixed_rhs_string( expr_obj=ode, prefix=prefix, exclude=prefix_excludes )
        return nineml.abstraction_layer.TimeDerivative( dependent_variable = dep, rhs =rhs)


    def VisitCondition(self, condition,**kwargs):
        prefix = kwargs.get( 'prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[] )
        rhs = MathUtil.get_prefixed_rhs_string( expr_obj=condition, prefix=prefix, exclude=prefix_excludes )
        return nineml.abstraction_layer.Condition( rhs =rhs)


    def VisitOnCondition(self, on_condition,**kwargs):
        return nineml.abstraction_layer.OnCondition(
                trigger = on_condition.trigger.accept_visitor(self,**kwargs),
                event_outputs = [ e.accept_visitor(self,**kwargs) for e in on_condition.event_outputs ],
                state_assignments = [ s.accept_visitor(self,**kwargs) for s in on_condition.state_assignments],
                target_regime_name = on_condition.target_regime_name 
                )

    def VisitOnEvent(self, on_event, **kwargs):
        return nineml.abstraction_layer.OnEvent(
                src_port_name = self.prefixVariable(on_event.src_port_name,**kwargs),
                event_outputs = [ e.accept_visitor(self,**kwargs) for e in on_event.event_outputs ],
                state_assignments = [ s.accept_visitor(self,**kwargs) for s in on_event.state_assignments],
                target_regime_name = on_event.target_regime_name
                )




class ClonerVisitorPrefixNamespace(ClonerVisitor):

    def VisitComponentClass(self, component, **kwargs ):
        prefix = component.get_node_addr().get_str_prefix()
        if prefix == '_': prefix = ''
        prefix_excludes = ['t']
        kwargs = {'prefix':prefix, 'prefix_excludes':prefix_excludes }

        
        port_connections = []
        for src,sink in component.portconnections:
            src_new = NamespaceAddress( tuple(list( src.loctuple[:-1] ) + [src.getstr( ) ] )  )
            sink_new = NamespaceAddress( tuple(list( sink.loctuple[:-1] ) + [sink.getstr( ) ] )  )
            port_connections.append ( (src_new,sink_new) )
            
        #assert False

        ccn =  nineml.abstraction_layer.ComponentClass( name = component.name,
                               parameters  = [ p.accept_visitor(self,**kwargs) for p in component.parameters  ],
                               analog_ports= [ p.accept_visitor(self,**kwargs) for p in component.analog_ports],
                               event_ports = [ p.accept_visitor(self,**kwargs) for p in component.event_ports ],
                               dynamics    = component.dynamics.accept_visitor(self,**kwargs) if component.dynamics else None,
                               subnodes = dict( [ (k, v.accept_visitor(self,**kwargs)) for (k,v) in component.subnodes.iteritems() ] ),
                               #portconnections = component.portconnections,
                               portconnections  = port_connections
                               )

        return ccn











