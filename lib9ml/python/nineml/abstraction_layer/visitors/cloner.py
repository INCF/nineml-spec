

from base import InplaceActionVisitorDF
from itertools import chain








class InPlaceTransform(InplaceActionVisitorDF):
    def __init__(self, originalname, targetname):
        self.originalname = originalname
        self.targetname = targetname


    def ActionComponentClass(self, component_node_combined):
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


















#import nineml.abstraction_layer as al
#import .. as al
import nineml.abstraction_layer 

from base import ComponentVisitor

class ClonerVisitor(ComponentVisitor):

    def Visit(self, obj,**kwargs):
        return obj.AcceptVisitor(self,**kwargs)


    def prefixVariable(self, variable, **kwargs):
        prefix = kwargs.get('prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[])
        if variable in prefix_excludes:
            return variable
        else:
            return prefix + variable


    def VisitComponentClass(self, component, **kwargs ):
        assert False

        ccn =  nineml.abstraction_layer.ComponentClass( name = component.name,
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


                               

    def VisitDynamics(self, dynamics, **kwargs):
        return nineml.abstraction_layer.Dynamics( regimes =       [ r.AcceptVisitor(self,**kwargs) for r in dynamics.regimes ],
                         aliases =          [ a.AcceptVisitor(self,**kwargs) for a in dynamics.aliases ],
                         state_variables =  [ s.AcceptVisitor(self,**kwargs) for s in dynamics.state_variables ] )
        
    def VisitRegime(self,regime,**kwargs):
        return nineml.abstraction_layer.Regime(  name = regime.name,
                        time_derivatives =  [t.AcceptVisitor(self,**kwargs) for t in regime.time_derivatives],
                        on_events =         [t.AcceptVisitor(self,**kwargs) for t in regime.on_events],
                        on_conditions =     [t.AcceptVisitor(self,**kwargs) for t in regime.on_conditions],
                        )

        

    def VisitStateVariable(self, state_variable,**kwargs):
        return nineml.abstraction_layer.StateVariable(name = self.prefixVariable( state_variable.name,**kwargs) )

        
    def VisitParameter(self, parameter, **kwargs):
        return nineml.abstraction_layer.Parameter(name = self.prefixVariable( parameter.name,**kwargs) )

    def VisitAnalogPort(self, port, **kwargs):
        p =nineml.abstraction_layer.AnalogPort( internal_symbol= self.prefixVariable(port.name,**kwargs) , mode=port.mode, op=port.reduce_op )
        print p
        return p

    def VisitEventPort(self, port, **kwargs):
        return nineml.abstraction_layer.EventPort( internal_symbol= self.prefixVariable(port.name,**kwargs), mode=port.mode, op=port.reduce_op )


    def VisitOutputEvent(self, output_event, **kwargs):
        return nineml.abstraction_layer.OutputEvent( port= self.prefixVariable( output_event.port,**kwargs) )

    def VisitInputEvent(self, input_event, **kwargs):
        return nineml.abstraction_layer.InputEvent( port= self.prefixVariable( input_event.port,**kwargs) )

    def VisitAssignment(self, assignment, **kwargs):
        prefix = kwargs.get( 'prefix','')
        prefix_excludes = kwargs.get('prefix_excludes',[] )
        to = assignment.to if assignment.to in prefix_excludes else prefix + assignment.to
        return nineml.abstraction_layer.Assignment( 
                    to = to,
                    expr = nineml.abstraction_layer.Expression.prefix(assignment, prefix=prefix,exclude=prefix_excludes,expr=assignment.rhs),
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
        return nineml.abstraction_layer.OnCondition(
                trigger = on_condition.trigger.AcceptVisitor(self,**kwargs),
                event_outputs = [ e.AcceptVisitor(self,**kwargs) for e in on_condition.event_outputs ],
                state_assignments = [ s.AcceptVisitor(self,**kwargs) for s in on_condition.state_assignments],
                target_regime_name = on_condition.target_regime_name 
                )

    def VisitOnEvent(self, on_event, **kwargs):
        return nineml.abstraction_layer.OnEvent(
                src_port = self.prefixVariable(on_event.src_port,**kwargs),
                event_outputs = [ e.AcceptVisitor(self,**kwargs) for e in on_event.event_outputs ],
                state_assignments = [ s.AcceptVisitor(self,**kwargs) for s in on_event.state_assignments],
                target_regime_name = on_event.target_regime_name
                )




class ClonerVisitorPrefixNamespace(ClonerVisitor):

    #def Visit(self, obj):
    #    clone = obj.AcceptVisitor(self)
    #    return clone


    def VisitComponentClass(self, component, **kwargs ):
        prefix = component.get_node_addr().get_str_prefix()
        if prefix == '_': prefix = ''
        prefix_excludes = ['t']
        kwargs = {'prefix':prefix, 'prefix_excludes':prefix_excludes }

        ccn =  nineml.abstraction_layer.ComponentClass( name = component.name,
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



