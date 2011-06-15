
import nineml.abstraction_layer as al


class ClonerVisitor(object):

    def __init__(self, prefix, prefix_excludes ):
        self.prefix = prefix
        self.prefix_excludes = prefix_excludes


    def prefixVariable(self, variable):
        if variable in self.prefix_excludes:
            return variable
        else:
            return self.prefix + variable


    def VisitComponent(self, component):
        return al.ComponentClass( name = component.name,
                               parameters  = [ p.AcceptVisitor(self) for p in component.parameters  ],
                               analog_ports= [ p.AcceptVisitor(self) for p in component.analog_ports],
                               event_ports = [ p.AcceptVisitor(self) for p in component.event_ports ],
                               dynamics    = component.dynamics.AcceptVisitor(self)  )
                               

    def VisitDynamics(self, dynamics):
        return al.Dynamics( regimes =          [ r.AcceptVisitor(self) for r in dynamics.regimes ],
                         aliases =          [ a.AcceptVisitor(self) for a in dynamics.aliases ],
                         state_variables =  [ s.AcceptVisitor(self) for s in dynamics.state_variables ] )
        
    def VisitRegime(self,regime):
        return al.Regime(  name = regime.name,
                        time_derivatives =  [t.AcceptVisitor(self) for t in regime.time_derivatives],
                        on_events =         [t.AcceptVisitor(self) for t in regime.on_events],
                        on_conditions =     [t.AcceptVisitor(self) for t in regime.on_conditions],
                        )

        

    def VisitStateVariable(self, state_variable):
        return al.StateVariable(name = self.prefixVariable( state_variable.name) )

        
    def VisitParameter(self, parameter):
        return al.Parameter(name = self.prefixVariable( parameter.name) )

    def VisitAnalogPort(self, port, **kwargs):
        return al.AnalogPort( internal_symbol= self.prefixVariable(port.name) , mode=port.mode, op=port.reduce_op )

    def VisitEventPort(self, port, **kwargs):
        return al.EventPort( internal_symbol= self.prefixVariable(port.name), mode=port.mode, op=port.reduce_op )


    def VisitOutputEvent(self, output_event, **kwargs):
        return al.OutputEvent( port= self.prefixVariable( output_event.port) )

    def VisitInputEvent(self, input_event, **kwargs):
        return al.InputEvent( port= self.prefixVariable( input_event.port) )

    def VisitAssignment(self, assignment, **kwargs):
        to = assignment.to if assignment.to in self.prefix_excludes else self.prefix + assignment.to
        return al.Assignment( 
                    to = to,
                    expr = al.Expression.prefix(assignment, prefix=self.prefix,exclude=self.prefix_excludes,expr=assignment.rhs),
                    name = assignment.name
                    )

    def VisitAlias(self, alias, **kwargs):
        return alias.clone( prefix=self.prefix, prefix_excludes=self.prefix_excludes )

    def VisitODE(self,ode,**kwargs):
        return ode.clone( prefix=self.prefix, prefix_excludes=self.prefix_excludes )

    def VisitCondition(self, condition):
        return condition.clone( prefix=self.prefix, prefix_excludes=self.prefix_excludes )


    def VisitOnCondition(self, on_condition):
        return al.OnCondition(
                trigger = on_condition.trigger.AcceptVisitor(self),
                event_outputs = [ e.AcceptVisitor(self) for e in on_condition.event_outputs ],
                state_assignments = [ s.AcceptVisitor(self) for s in on_condition.state_assignments])

    def VisitOnEvent(self, on_event, **kwargs):
        return al.OnEvent(
                src_port = self.prefixVariable(on_event.src_port),
                event_outputs = [ e.AcceptVisitor(self) for e in on_event.event_outputs ],
                state_assignments = [ s.AcceptVisitor(self) for s in on_event.state_assignments])


class ModelPrefixerVisitor( object ):
    
    def VisitModelClass( modelclass, **kwargs)
        
        newsubnodes = {}
        for ns,node in modelclass.subnodes.iteritems():
            newsubnodes[ns] = node.AcceptVisitor(self)

        from nineml.abstraction_layer import models
        newModel = models.Model(name=modelclass.name, subnodes=newsubnodes
        return newModel
             


    def VisitComponentClass( self, componentclass):
        prefix = component.getTreePosition(jointoken="_") + "_"
        prefix_excludes = ['t']
        return ClonerVisitor(prefix=prefix, prefix_excludes=prefix_excludes).VisitComponent(componentclass)


