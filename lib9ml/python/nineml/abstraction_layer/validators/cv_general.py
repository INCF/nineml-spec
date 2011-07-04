
from collections import defaultdict


from base import ComponentValidatorPerNamespace
import nineml

#from nineml.exceptions import NineMLRuntimeError
#from nineml.abstraction_layer.component.namespaceaddress import NamespaceAddress
#from nineml.abstraction_layer import math_namespace
#from nineml.utility import assert_no_duplicates

class ComponentValidatorTimeDerivativesAreDeclared(ComponentValidatorPerNamespace):
    """ Check all variables used in TimeDerivative blocks are defined 
        as  StateVariables.
    """
     
    def __init__(self, component):
        ComponentValidatorPerNamespace.__init__(self, explicitly_require_action_overrides=False)
        self.sv_declared = defaultdict(list)
        self.time_derivatives_used = defaultdict(list)
            
        self.visit(component)
        
        for namespace, time_derivatives in self.time_derivatives_used.iteritems():
            for td in time_derivatives:
                if not td in self.sv_declared[namespace]:
                    err = 'StateVariable not declared: %s'%td
                    raise nineml.exceptions.NineMLRuntimeError(err)
        
        
    def action_statevariable(self, state_variable, namespace, **kwargs):
        self.sv_declared[namespace].append(state_variable.name)
        
    def action_timederivative(self, timederivative, namespace, **kwargs):
        self.time_derivatives_used[namespace].append(timederivative.dependent_variable)




class ComponentValidatorStateAssignmentsAreOnStateVariables(ComponentValidatorPerNamespace):
    """ Check that we only attempt to make StateAssignments to state-variables.
    """
     
    def __init__(self, component):
        ComponentValidatorPerNamespace.__init__(self, explicitly_require_action_overrides=False)
        self.sv_declared = defaultdict(list)
        self.state_assignments_lhses = defaultdict(list)
            
        self.visit(component)
        
        for namespace, state_assignments_lhs in self.state_assignments_lhses.iteritems():
            for td in state_assignments_lhs:
                if not td in self.sv_declared[namespace]:
                    err = 'Not Assigning to state-variable: %s'%state_assignment
                    raise nineml.exceptions.NineMLRuntimeError(err)
            
    def action_statevariable(self, state_variable, namespace, **kwargs):
        self.sv_declared[namespace].append(state_variable.name)
        
    def action_stateassignment(self, state_assignment, namespace, **kwargs):
        assert False
        self.state_assignments_lhses[namespace].append(state_assignment.lhs)









class ComponentValidatorAliasesAreNotRecursive(ComponentValidatorPerNamespace):
    """Check that aliases are not self-referential"""
    
    def __init__(self, component):
        ComponentValidatorPerNamespace.__init__(self, explicitly_require_action_overrides=False)
        self.visit(component)
        
    def action_componentclass(self, component, namespace):
        
        unresolved_aliases = dict( (a.lhs, a) for a in component.aliases ) 
        
        def alias_contains_unresolved_symbols(alias):
            unresolved = [ sym for sym in alias.rhs_atoms if sym in unresolved_aliases]
            return len(unresolved) != 0
        
        def get_resolved_aliases( ):
            return [alias for alias in unresolved_aliases.values() if not alias_contains_unresolved_symbols(alias) ]
        
                            
        while( unresolved_aliases ):
            
            resolved_aliases = get_resolved_aliases()
            if resolved_aliases:
                for r in resolved_aliases:
                    del unresolved_aliases[r.lhs]
                
            else:
                errmsg =  "Unable to resolve all aliases in %s. " % namespace
                errmsg += "You may have a recursion issue."
                errmsg += "Remaining Aliases: %s"%  ','.join(unresolved_aliases.keys()) 
                raise nineml.exceptions.NineMLRuntimeError(errmsg)
            
            






class ComponentValidatorAssignmentsAliasesAndStateVariablesHaveNoUnResolvedSymbols(ComponentValidatorPerNamespace):
    """Check that aliases and timederivatives are defined in terms of other parameters, aliases, statevariables and ports"""
    def __init__(self, component):
        ComponentValidatorPerNamespace.__init__(self, explicitly_require_action_overrides=False)

        self.available_symbols = defaultdict(list)
        self.aliases = defaultdict(list)
        self.time_derivatives = defaultdict(list)
        self.state_assignments = defaultdict(list)
        
        self.visit(component)

         
        excludes =  nineml.maths.get_reserved_and_builtin_symbols()

        # Check Aliases:
        for ns, aliases in self.aliases.iteritems():
            for alias in aliases:
                for rhs_atom in alias.rhs_atoms:
                    if rhs_atom in excludes:
                        continue
                    if not rhs_atom in self.available_symbols[ns]:
                        err = 'Unresolved Symbol in Alias: %s [%s]'%(rhs_atom, alias)                
                        raise nineml.exceptions.NineMLRuntimeError(err)                

        #Check TimeDerivatives:
        for ns, timederivatives in self.time_derivatives.iteritems():
            for timederivative in timederivatives:
                for rhs_atom in timederivative.rhs_atoms:
                    if not rhs_atom in self.available_symbols[ns] and not rhs_atom in excludes:
                        err = 'Unresolved Symbol in Time Derivative: %s [%s]'%(rhs_atom, timederivative)
                        raise nineml.exceptions.NineMLRuntimeError(err)

        # Check StateAssignments
        for ns, state_assignments in self.state_assignments.iteritems():
            for state_assignment in state_assignments:
                for rhs_atom in state_assignment.rhs_atoms:
                    if not rhs_atom in self.available_symbols[ns] and not rhs_atom in excludes:
                        err = 'Unresolved Symbol in Assignment: %s [%s]'%(rhs_atom, state_assignment)
                        raise nineml.exceptions.NineMLRuntimeError(err)
            
        
    def add_symbol(self, namespace, symbol):
        if symbol in self.available_symbols[namespace]:
            err = "Duplicate Symbol: [%s] found in namespace: %s"%(symbol, namespace) 
            raise NineMLRuntimeError(err)
        self.available_symbols[namespace].append(symbol)
    
    def action_analogport(self, port, namespace, **kwargs):
        if port.is_incoming():
            self.available_symbols[namespace].append(port.name)
            
    def action_statevariable(self, state_variable, namespace, **kwargs):
        self.add_symbol(namespace=namespace, symbol=state_variable.name)
        
    def action_timederivative(self, time_derivative, namespace, **kwargs):
        self.time_derivatives[namespace].append(time_derivative)
                        
    def action_alias(self, alias, namespace, **kwargs):
        self.add_symbol(namespace=namespace, symbol=alias.lhs )
        self.aliases[namespace].append(alias)

    def action_parameter(self, parameter, namespace, **kwargs):
        self.add_symbol(namespace=namespace, symbol=parameter.name )

    def action_assignment(self, state_assignment, namespace, **kwargs):
        self.state_assignments[namespace].append(state_assignment)










class ComponentValidatorPortConnections(ComponentValidatorPerNamespace):
    """Check that all the port connections point to a port, and that
    each send & recv port only has a single connection. 
    """
    def __init__(self, component):
        ComponentValidatorPerNamespace.__init__(self, explicitly_require_action_overrides=False)

        self.ports = defaultdict(list)
        self.portconnections = list()
        
        self.visit(component)
        
        connected_recv_ports = set()
        
        # Check each source and sink exist,
        # and that each recv port is connected at max once.
        for src, sink in self.portconnections:
            if not src in self.ports:
                raise nineml.exceptions.NineMLRuntimeError('Unable to find port specified in connection: %s'%(src) )
            if self.ports[src].is_incoming():
                raise nineml.exceptions.NineMLRuntimeError('Port was specified as a source, but is incoming: %s'%(src) )
            
            
            if not  sink in self.ports:
                raise nineml.exceptions.NineMLRuntimeError('Unable to find port specified in connection: %s'%(src) )
            
            if not self.ports[sink].is_incoming():
                raise nineml.exceptions.NineMLRuntimeError('Port was specified as a sink, but is not incoming: %s'%(src) )
                
                
            if self.ports[sink].mode == 'recv':
                if self.ports[sink] in connected_recv_ports:
                    raise nineml.exceptions.NineMLRuntimeError("Port was 'recv' and specified twice: %s"%(src) )
                connected_recv_ports.add( self.ports[sink] )
        
        
    def action_analogport(self, analogport, namespace):
        port_address = nineml.al.NamespaceAddress.concat( namespace, analogport.name) 
        if port_address in self.ports:
            raise nineml.exceptions.NineMLRuntimeError('Duplicated Name for port found: %s'%port_address)
        self.ports[port_address] = analogport
        
        
    def action_eventport(self, analogport, namespace):
        port_address = nineml.al.NamespaceAddress.concat( namespace, analogport.name )
        if port_address in self.ports:
            raise nineml.exceptions.NineMLRuntimeError('Duplicated Name for port found: %s'%port_address)
        self.ports[port_address] = analogport
        
        
    def action_componentclass(self, component, namespace):
        for src, sink in component.portconnections:
            full_src   = nineml.al.NamespaceAddress.concat( namespace, src )
            full_sink  = nineml.al.NamespaceAddress.concat( namespace, sink )
            self.portconnections.append( (full_src, full_sink) )
            
        
        
        

    
class ComponentValidatorRegimeGraph(ComponentValidatorPerNamespace):
    
    def __init__(self, component):
        ComponentValidatorPerNamespace.__init__(self, explicitly_require_action_overrides=False)
        
        self.connected_regimes_from_regime = defaultdict( set ) 
        self.regimes_in_namespace = defaultdict( set )
    
        self.visit(component)
    
    
        def add_connected_regimes_recursive( regime, connected ):
            connected.add(regime)
            for r in self.connected_regimes_from_regime[regime]:
                if not r in connected:
                    add_connected_regimes_recursive( r, connected )
                
        for namespace, regimes in self.regimes_in_namespace.iteritems():
            
            # Perhaps we have no transition graph; this is OK:
            if len( regimes ) == 0:continue
        
            connected = set()
            add_connected_regimes_recursive( regimes[0], connected)
            if len( connected ) != len( self.regimes_in_namespace[namespace] ):
                raise nineml.exceptions.NineMLRuntimeError('Transition graph is contains islands')
    
    
    def action_componentclass(self, component, namespace):
        self.regimes_in_namespace[namespace] = list( component.regimes )
        


    def action_regime(self, regime, namespace):
        for transition in regime.transitions:
            self.connected_regimes_from_regime[regime].add( transition.target_regime )
            self.connected_regimes_from_regime[transition.target_regime].add( regime )
        
    


class ComponentValidatorNoDuplicatedObjects(ComponentValidatorPerNamespace):
    def __init__(self, component):
        ComponentValidatorPerNamespace.__init__(self, explicitly_require_action_overrides=True)

        self.all_objects = list()
    
        self.visit(component)
    
        nineml.utility.assert_no_duplicates(self.all_objects)
        
    
    def action_componentclass(self, component,  **kwargs):
        self.all_objects.append(component)
        
    def action_dynamics(self, dynamics, **kwargs):
        self.all_objects.append(dynamics)
        
    def action_regime(self, regime,  **kwargs):
        self.all_objects.append(regime)
        
    def action_statevariable(self, state_variable, **kwargs):
        self.all_objects.append(state_variable)
        
    def action_parameter(self, parameter, **kwargs):
        self.all_objects.append(parameter)
        
    def action_analogport(self, port, **kwargs):
        self.all_objects.append(port)
        
    def action_eventport(self, port, **kwargs):
        self.all_objects.append(port)
        
    def action_outputevent(self, output_event, **kwargs):
        self.all_objects.append(output_event)
        
    def action_assignment(self, assignment, **kwargs):
        self.all_objects.append(assignment)
        
    def action_alias(self, alias, **kwargs):
        self.all_objects.append(alias)
        
    def action_timederivative(self, time_derivative, **kwargs):
        self.all_objects.append(time_derivative)
        
    def action_condition(self, condition, **kwargs):
        self.all_objects.append(condition)
        
    def action_oncondition(self, on_condition, **kwargs):
        self.all_objects.append(on_condition)
        
    def action_onevent(self, on_event, **kwargs):
        self.all_objects.append(on_event)




class ComponentValidatorRegimeOnlyHasOneHandlerPerEvent(ComponentValidatorPerNamespace):
    def __init__(self, component):
        ComponentValidatorPerNamespace.__init__(self, explicitly_require_action_overrides=False)

        self.visit(component)
        

    def action_regime(self, regime, namespace, **kwargs):
        event_triggers = [on_event.src_port_name for on_event in regime.on_events ] 
        nineml.utility.assert_no_duplicates( event_triggers )

        
#from nineml.abstraction_layer import math_namespace

class ComponentValidatorCheckNoLHSAssignmentsToMathsNamespace(ComponentValidatorPerNamespace):
    """
    This class checks that there is not a mathematical symbols, (e.g. pi, e)
    on the left-hand-side of an equation
    """

    def __init__(self, component):
        ComponentValidatorPerNamespace.__init__(self, explicitly_require_action_overrides=False)

        self.visit(component)

    def check_lhssymbol_is_valid(self, symbol):
        assert isinstance( symbol, basestring)

        if not nineml.maths.is_valid_lhs_target(symbol) :
            err = 'Symbol: %s found on left-hand-side of an equation'
            raise nineml.exceptions.NineMLRuntimeError(err)
            
    def action_statevariable(self, state_variable, **kwargs):
        self.check_lhssymbol_is_valid( state_variable.name )
        
    def action_parameter(self, parameter, **kwargs):
        self.check_lhssymbol_is_valid( parameter.name )
        
    def action_assignment(self, assignment, **kwargs):
        self.check_lhssymbol_is_valid( assignment.lhs )
        
    def action_alias(self, alias, **kwargs):
        self.check_lhssymbol_is_valid( alias.lhs)
        
    def action_timederivative(self, time_derivative, **kwargs):
        self.check_lhssymbol_is_valid( time_derivative.dependent_variable)
        
