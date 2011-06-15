
import nineml.abstraction_layer as al

from itertools import chain

class InplaceActionVisitorDF(object):

    def VisitComponentNode(self, component):
        nodes = chain(component.parameters, component.analog_ports, component.event_ports, [component.dynamics])
        for p in nodes:
            p.AcceptVisitor(self)
        self.ActionComponent(component)

    def VisitDynamics(self, dynamics):
        nodes = chain(dynamics.regimes, dynamics.aliases, dynamics.state_variables)
        for p in nodes:
            p.AcceptVisitor(self)
        self.ActionDynamics(dynamics)
        
    def VisitRegime(self,regime):
        nodes = chain(regime.time_derivatives, regime.on_events, regime.on_conditions)
        for p in nodes:
            p.AcceptVisitor(self)
        self.ActionRegime(regime)

    def VisitStateVariable(self, state_variable):
        self.ActionStateVariable( state_variable) 

    def VisitParameter(self, parameter):
        self.ActionParameter(parameter)

    def VisitAnalogPort(self, port, **kwargs):
        self.ActionAnalogPort(port)

    def VisitEventPort(self, port, **kwargs):
        self.ActionEventPort(port)

    def VisitOutputEvent(self, output_event, **kwargs):
        self.ActionOutputEvent(output_event)

    def VisitInputEvent(self, input_event, **kwargs):
        self.ActionInputEvent(input_event)

    def VisitAssignment(self, assignment, **kwargs):
        self.ActionAssignment(assignment)

    def VisitAlias(self, alias, **kwargs):
        self.ActionAlias(alias)

    def VisitODE(self,ode,**kwargs):
        self.ActionODE(ode)

    def VisitCondition(self, condition):
        self.ActionCondition(condition)

    def VisitOnCondition(self, on_condition):
        nodes = chain([on_condition.trigger], on_condition.event_outputs, on_condition.state_assignments)
        for p in nodes:
            p.AcceptVisitor(self)
        self.ActionOnCondition(on_condition)

    def VisitOnEvent(self, on_event, **kwargs):
        nodes = chain( on_event.event_outputs, on_event.state_assignments)
        for p in nodes:
            p.AcceptVisitor(self)
        self.ActionOnEvent(on_event)


    def ActionComponent(self, component):
        pass
    def ActionDynamics(self, dynamics):
        pass
    def ActionRegime(self,regime):
        pass
    def ActionStateVariable(self, state_variable):
        pass
    def ActionParameter(self, parameter):
        pass
    def ActionAnalogPort(self, port, **kwargs):
        pass
    def ActionEventPort(self, port, **kwargs):
        pass
    def ActionOutputEvent(self, output_event, **kwargs):
        pass
    def ActionInputEvent(self, input_event, **kwargs):
        pass
    def ActionAssignment(self, assignment, **kwargs):
        pass
    def ActionAlias(self, alias, **kwargs):
        pass
    def ActionODE(self,ode,**kwargs):
        pass
    def ActionCondition(self, condition):
        pass
    def ActionOnCondition(self, on_condition):
        pass
    def ActionOnEvent(self, on_event, **kwargs):
        pass





class InPlaceTransform(InplaceActionVisitorDF):
    def __init__(self, originalname, targetname):
        self.originalname = originalname
        self.targetname = targetname

    #def ActionComponent(self, component):
    #    pass
    #def ActionDynamics(self, dynamics):
    #    pass
    #def ActionRegime(self,regime):
    #    pass
    #def ActionStateVariable(self, state_variable):
    #    pass
    #def ActionParameter(self, parameter):
    #    pass
    #def ActionAnalogPort(self, port, **kwargs):
    #    pass
    #def ActionEventPort(self, port, **kwargs):
    #    pass
    #def ActionOutputEvent(self, output_event, **kwargs):
    #    pass
    #def ActionInputEvent(self, input_event, **kwargs):
    #    pass
    def ActionAssignment(self, assignment, **kwargs):
        assignment.name_transform_inplace( {self.originalname:self.targetname} )
    def ActionAlias(self, alias, **kwargs):
        alias.name_transform_inplace( {self.originalname:self.targetname} )
    def ActionODE(self,ode,**kwargs):
        print ode
        ode.name_transform_inplace( {self.originalname:self.targetname} )
    def ActionCondition(self, condition):
        condition.name_transform_inplace( {self.originalname:self.targetname} )













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
        return al.models.ComponentNode( name = component.name,
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
    
    def VisitModelClass(self, modelclass, **kwargs):
        print "Visit Model Class"
        
        newsubnodes = {}
        for ns,node in modelclass.subnodes.iteritems():
            newsubnodes[ns] = node.AcceptVisitor(self)
        from nineml.abstraction_layer import models
        newModel = models.Model(name=modelclass.name, subnodes=newsubnodes)

        for src,sink in modelclass.portconnections:
            newModel.connect_ports(src=src,sink=sink)
        
        return newModel
             
    

    def VisitComponentNode( self, componentclass):
        prefix = componentclass.getTreePosition(jointoken="_") + "_"
        prefix_excludes = ['t']
        return ClonerVisitor(prefix=prefix, prefix_excludes=prefix_excludes).VisitComponent(componentclass)


