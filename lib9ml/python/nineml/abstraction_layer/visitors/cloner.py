
import nineml.abstraction_layer as al

from itertools import chain

class InplaceActionVisitorDF(object):

    def VisitComponentNodeCombined(self, componentNodeCombined):
        self.ActionComponentNodeCombined(componentNodeCombined)

        comp = componentNodeCombined
        nodes = chain(comp.parameters, comp.analog_ports, comp.event_ports, )
        for p in nodes:
            p.AcceptVisitor(self)

        if comp.dynamics:
            comp.dynamics.AcceptVisitor(self)

        for subnode in componentNodeCombined.subnodes.keys():
            subnode.AcceptVisitor(self)

    def VisitComponent(self, component):
        self.ActionComponent(component)
        nodes = chain(component.parameters, component.analog_ports, component.event_ports, [component.dynamics])
        l = list( nodes )
        for p in l:
            p.AcceptVisitor(self)


    def VisitComponentNode(self, component):
        self.ActionComponent(component)
        nodes = chain(component.parameters, component.analog_ports, component.event_ports, [component.dynamics])
        for p in nodes:
            p.AcceptVisitor(self)

    def VisitDynamics(self, dynamics):
        self.ActionDynamics(dynamics)
        nodes = chain(dynamics.regimes, dynamics.aliases, dynamics.state_variables)
        for p in nodes:
            p.AcceptVisitor(self)
        
    def VisitRegime(self,regime):
        self.ActionRegime(regime)
        nodes = chain(regime.time_derivatives, regime.on_events, regime.on_conditions)
        for p in nodes:
            p.AcceptVisitor(self)

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
        self.ActionOnCondition(on_condition)
        nodes = chain([on_condition.trigger], on_condition.event_outputs, on_condition.state_assignments)
        for p in nodes:
            p.AcceptVisitor(self)

    def VisitOnEvent(self, on_event, **kwargs):
        self.ActionOnEvent(on_event)
        nodes = chain( on_event.event_outputs, on_event.state_assignments)
        for p in nodes:
            p.AcceptVisitor(self)


    # To be overridden:
    def ActionComponentNodeCombined(self, component_node_combined):
        pass
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


    def ActionComponentNodeCombined(self, component_node_combined):
        pass

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
        ode.name_transform_inplace( {self.originalname:self.targetname} )
    def ActionCondition(self, condition):
        condition.name_transform_inplace( {self.originalname:self.targetname} )





















class ClonerVisitor(object):

    def Visit(self, obj,**kwargs):
        return obj.AcceptVisitor(self,**kwargs)

    def __init__(self): #, prefix='', prefix_excludes=None ):
        pass


    def prefixVariable(self, variable, **kwargs):
        prefix = kwargs.get('prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[])
        if variable in prefix_excludes:
            return variable
        else:
            return prefix + variable


    def VisitComponentNodeCombined(self, component, **kwargs ):

        ccn =  al.models.ComponentNodeCombined( name = component.name,
                               parameters  = [ p.AcceptVisitor(self,**kwargs) for p in component.parameters  ],
                               analog_ports= [ p.AcceptVisitor(self,**kwargs) for p in component.analog_ports],
                               event_ports = [ p.AcceptVisitor(self,**kwargs) for p in component.event_ports ],
                               dynamics    = component.dynamics.AcceptVisitor(self,**kwargs) if component.dynamics else None,
                               subnodes = dict( [ (k, v.AcceptVisitor(self,**kwargs)) for (k,v) in component.subnodes.iteritems() ] )
                               )

        # Copy Port COnnections:
        assert 'portconnections' in ccn.__dict__
        ccn.portconnections = component.portconnections[:]

        return ccn

    def VisitComponentNode(self, component,**kwargs):
        return al.models.ComponentNode( name = component.name,
                               parameters  = [ p.AcceptVisitor(self,**kwargs) for p in component.parameters  ],
                               analog_ports= [ p.AcceptVisitor(self,**kwargs) for p in component.analog_ports],
                               event_ports = [ p.AcceptVisitor(self,**kwargs) for p in component.event_ports ],
                               dynamics    = component.dynamics.AcceptVisitor(self,**kwargs)  )

    def VisitComponent(self, component,**kwargs):
        return al.models.ComponentNode( name = component.name,
                               parameters  = [ p.AcceptVisitor(self,**kwargs) for p in component.parameters  ],
                               analog_ports= [ p.AcceptVisitor(self,**kwargs) for p in component.analog_ports],
                               event_ports = [ p.AcceptVisitor(self,**kwargs) for p in component.event_ports ],
                               dynamics    = component.dynamics.AcceptVisitor(self,**kwargs)  )
                               

    def VisitDynamics(self, dynamics, **kwargs):
        return al.Dynamics( regimes =       [ r.AcceptVisitor(self,**kwargs) for r in dynamics.regimes ],
                         aliases =          [ a.AcceptVisitor(self,**kwargs) for a in dynamics.aliases ],
                         state_variables =  [ s.AcceptVisitor(self,**kwargs) for s in dynamics.state_variables ] )
        
    def VisitRegime(self,regime,**kwargs):
        return al.Regime(  name = regime.name,
                        time_derivatives =  [t.AcceptVisitor(self,**kwargs) for t in regime.time_derivatives],
                        on_events =         [t.AcceptVisitor(self,**kwargs) for t in regime.on_events],
                        on_conditions =     [t.AcceptVisitor(self,**kwargs) for t in regime.on_conditions],
                        )

        

    def VisitStateVariable(self, state_variable,**kwargs):
        return al.StateVariable(name = self.prefixVariable( state_variable.name,**kwargs) )

        
    def VisitParameter(self, parameter, **kwargs):
        return al.Parameter(name = self.prefixVariable( parameter.name,**kwargs) )

    def VisitAnalogPort(self, port, **kwargs):
        p =al.AnalogPort( internal_symbol= self.prefixVariable(port.name,**kwargs) , mode=port.mode, op=port.reduce_op )
        print p
        return p

    def VisitEventPort(self, port, **kwargs):
        return al.EventPort( internal_symbol= self.prefixVariable(port.name,**kwargs), mode=port.mode, op=port.reduce_op )


    def VisitOutputEvent(self, output_event, **kwargs):
        return al.OutputEvent( port= self.prefixVariable( output_event.port,**kwargs) )

    def VisitInputEvent(self, input_event, **kwargs):
        return al.InputEvent( port= self.prefixVariable( input_event.port,**kwargs) )

    def VisitAssignment(self, assignment, **kwargs):
        prefix = kwargs.get( 'prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[] )
        to = assignment.to if assignment.to in prefix_excludes else prefix + assignment.to
        return al.Assignment( 
                    to = to,
                    expr = al.Expression.prefix(assignment, prefix=prefix,exclude=prefix_excludes,expr=assignment.rhs),
                    name = assignment.name
                    )

    def VisitAlias(self, alias, **kwargs):
        prefix = kwargs.get( 'prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[] )
        return alias.clone( prefix=prefix, prefix_excludes=prefix_excludes )

    def VisitODE(self,ode,**kwargs):
        prefix = kwargs.get( 'prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[] )
        return ode.clone( prefix=prefix, prefix_excludes=prefix_excludes )

    def VisitCondition(self, condition,**kwargs):
        prefix = kwargs.get( 'prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[] )
        return condition.clone( prefix=prefix, prefix_excludes=prefix_excludes )


    def VisitOnCondition(self, on_condition,**kwargs):
        return al.OnCondition(
                trigger = on_condition.trigger.AcceptVisitor(self,**kwargs),
                event_outputs = [ e.AcceptVisitor(self,**kwargs) for e in on_condition.event_outputs ],
                state_assignments = [ s.AcceptVisitor(self,**kwargs) for s in on_condition.state_assignments])

    def VisitOnEvent(self, on_event, **kwargs):
        return al.OnEvent(
                src_port = self.prefixVariable(on_event.src_port),
                event_outputs = [ e.AcceptVisitor(self,**kwargs) for e in on_event.event_outputs ],
                state_assignments = [ s.AcceptVisitor(self,**kwargs) for s in on_event.state_assignments])




class ClonerVisitorPrefixNamespace(ClonerVisitor):

    def Visit(self, obj):
        return obj.AcceptVisitor(self)





    def VisitComponentNodeCombined(self, component, **kwargs ):
        prefix = component.get_node_addr().get_str_prefix()
        if prefix == '_': prefix = ''
        prefix_excludes = ['t']
        kwargs = {'prefix':prefix, 'prefix_excludes':prefix_excludes }

        ccn =  al.models.ComponentNodeCombined( name = component.name,
                               parameters  = [ p.AcceptVisitor(self,**kwargs) for p in component.parameters  ],
                               analog_ports= [ p.AcceptVisitor(self,**kwargs) for p in component.analog_ports],
                               event_ports = [ p.AcceptVisitor(self,**kwargs) for p in component.event_ports ],
                               dynamics    = component.dynamics.AcceptVisitor(self,**kwargs) if component.dynamics else None,
                               subnodes = dict( [ (k, v.AcceptVisitor(self,**kwargs)) for (k,v) in component.subnodes.iteritems() ] )
                               )

        # Copy Port COnnections:
        assert 'portconnections' in ccn.__dict__
        for src,sink in component.portconnections:
            ccn.connect_ports(src=src,sink=sink)

        return ccn









class ModelPrefixerVisitor( object ):
    pass
    
#    def VisitModelClass(self, modelclass, **kwargs):
#        print "Visit Model Class"
#        
#        newsubnodes = {}
#        for ns,node in modelclass.subnodes.iteritems():
#            newsubnodes[ns] = node.AcceptVisitor(self)
#        from nineml.abstraction_layer import models
#        newModel = models.Model(name=modelclass.name, subnodes=newsubnodes)
#
#        for src,sink in modelclass.portconnections:
#            newModel.connect_ports(src=src,sink=sink)
#        
#        return newModel
#             
#    
#
#    def VisitComponentNode( self, componentclass):
#        prefix = componentclass.getTreePosition(jointoken="_") + "_"
#        prefix_excludes = ['t']
#        return ClonerVisitor(prefix=prefix, prefix_excludes=prefix_excludes).VisitComponent(componentclass)
#
#



