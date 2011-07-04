
#from operator import and_


from nineml.exceptions import NineMLRuntimeError
from namespaceaddress import NamespaceAddress
import componentqueryer
import nineml.utility
import dynamics as dyn


#import copy
import itertools
from interface import Parameter
from dynamics import StateVariable
from ports import EventPort
#import math_namespace 
from nineml.utility import check_list_contain_same_items


class ComponentClassMixinFlatStructure(object):

    def __init__(self, name, parameters = None, analog_ports = None, 
                 event_ports = None, dynamics=None):

        self._name = name
        self._parameters = parameters or []
        self._analog_ports = analog_ports or []
        self._event_ports = event_ports or []
        self._dynamics = dynamics

        import nineml
        nineml.utility.ensure_valid_c_variable_name(name)

    # Basic properties:
    @property
    def name(self):
        return self._name

    @property
    def parameters(self):
        return iter(self._parameters)

    @property
    def analog_ports(self):
        return self._analog_ports

    @property
    def event_ports(self):
        return self._event_ports

    @property
    def dynamics(self):
        return self._dynamics
    # -------------------------- #

    #  Forwarding functions to the dynamics #
    @property
    def aliases_map(self):
        """Forwarding function to self.dynamics.alias_map"""
        return self._dynamics.aliases_map

    @property
    def aliases(self):
        """Forwarding function to self.dynamics.aliases"""
        return self._dynamics.aliases

    @property
    def regimes(self):
        """Forwarding function to self.dynamics.regimes"""
        return self._dynamics.regimes

    @property
    def regimes_map(self):
        """Forwarding function to self.dynamics.regimes_map"""
        return self._dynamics.regimes_map

    @property
    def transitions(self):
        """Forwarding function to self.dynamics.transitions"""
        return self._dynamics.transitions

    @property
    def state_variables(self):
        """Forwarding function to self.dynamics.state_variables"""
        return self._dynamics.state_variables

    @property
    def state_variables_map(self):
        """Forwarding function to self.dynamics.state_variables_map"""
        return self._dynamics.state_variables_map
    # -------------------------- #





    def backsub_all(self): 
        """Expand all alias definitions in local equations.

        This function finds ``Aliases``, ``TimeDerivatives``, ``SendPorts``, ``Assignments``
        and ``Conditions``  with which are defined in terms of other aliases,
        and expands them, such that each only has Parameters,
        StateVariables and recv/reduce AnalogPorts on the RHS.
        
        """

        from nineml.abstraction_layer.visitors import ExpandAliasDefinition
        for alias in self.aliases:
            alias_expander = ExpandAliasDefinition(originalname=alias.lhs, 
                                                   targetname="(%s)"%alias.rhs )
            alias_expander.visit(self)





    def write(self, file, flatten=True):
        """Export this model to an XML file.

        :params file: A filename or fileobject
        :params flatten: Boolean specifying whether the component should be
            flattened before saving

        """

        from nineml.abstraction_layer.writers import XMLWriter
        return XMLWriter.write(component=self, file=file, flatten=flatten)






class ComponentClassMixinNamespaceStructure(object):
    def __init__(self, subnodes = None, portconnections=None):  

        # Prevent dangers with default arguments.
        subnodes = subnodes or {}
        portconnections = portconnections or []

        # Initialise class variables:
        self._parentmodel = None
        self.subnodes = {}
        self.portconnections = []

        # Add the parameters using class methods:
        for namespace, subnode in subnodes.iteritems():
            self.insert_subnode(subnode=subnode, namespace=namespace)
        
        for src, sink in portconnections:
            self.connect_ports(src, sink)

    # Parenting:
    def _set_parent_model(self, parentmodel):
        assert not self._parentmodel
        self._parentmodel = parentmodel
    def _get_parent_model(self): 
        return self._parentmodel



    def _validate_self(self):
        """ Over-ridden in mix'ed class"""
        raise NotImplementedError()


    def get_node_addr(self):
        """Get the namespace address of this component"""
        from nineml.utility import invert_dictionary

        parent = self._get_parent_model() 
        if not parent:
            return NamespaceAddress.create_root()
        else:
            contained_namespace = invert_dictionary(parent.subnodes)[self]
            return parent.get_node_addr().get_subns_addr( contained_namespace ) 


    def get_subnode(self, addr):
        namespace_addr = NamespaceAddress(addr)

        # Look up the first name in the namespace
        if len( namespace_addr.loctuple ) == 0:
            return self

        local_namespace_ref = namespace_addr.loctuple[0]
        if not local_namespace_ref in self.subnodes:
            err = "Attempted to lookup node: %s, but doesn't exist in this namespace %s"% (local_namespace_ref, self.subnodes.keys() )
            raise NineMLRuntimeError(err)

        return self.subnodes[local_namespace_ref].get_subnode( addr = NamespaceAddress(namespace_addr.loctuple[1:] ) )

        

    def insert_subnode(self,  namespace, subnode ):
        """Insert a subnode into this component
        

        :param subnode: An object of type ``ComponentClass``.
        :param namespace: A `string` specifying the name of the component in
            this components namespace.

        :raises: ``NineMLRuntimeException`` if there is already a subcomponent at
            the same namespace location

        .. note::
            
            This method will clone the subnode.

        """
        if not isinstance( namespace, basestring):
            err = 'Invalid Namespace: %s'%type(subnode)
            raise NineMLRuntimeError(err)

        if not isinstance( subnode, ComponentClass):
            err = 'Attempting to insert invalid object as subcomponent: %s'%type(subnode)
            raise NineMLRuntimeError(err)

        if namespace in self.subnodes:
            err = 'Trying to insert duplicate key into namespace: %s'%namespace
            raise NineMLRuntimeError(err)
        from nineml.abstraction_layer.visitors.cloner import ClonerVisitor
        self.subnodes[namespace] = ClonerVisitor().visit( subnode )
        self.subnodes[namespace]._set_parent_model(self)
        
        self._validate_self()


    def connect_ports( self, src, sink ):
        """Connects the ports of 2 subcomponents.
        
        The ports can be specified as ``string`` s or ``NamespaceAddresses`` es.


        :param src: The source port of one sub-component; this should either an
            event port or analog port, but it *must* be a send port.

        :param sink: The sink port of one sub-component; this should either an
            event port or analog port, but it *must* be either a 'recv' or a
            'reduce' port.

        """

        connection = (NamespaceAddress(src), NamespaceAddress(sink) )
        self.portconnections.append( connection ) 

        self._validate_self()

   

from nineml.abstraction_layer.visitors import ActionVisitor

class InterfaceInferer(ActionVisitor):
    """ Used to infer output EventPorts, statevariables & parameters."""

    def __init__(self, dynamics, analog_ports):
        ActionVisitor.__init__(self, explicitly_require_action_overrides=True) 

        # State Variables:
        self.state_variable_names = set()
        for regime in dynamics.regimes:
            for time_deriv in regime.time_derivatives:
                self.state_variable_names.add(time_deriv.dependent_variable)
            for transition in regime.transitions:
                for state_assignment in transition.state_assignments:
                    self.state_variable_names.add( state_assignment.lhs)


        # Which symbols can we account for: 
        alias_symbols = set( dynamics.aliases_map.keys() )

        if analog_ports is not None:
            analog_ports_in = [ap.name for ap in analog_ports if ap.is_incoming()]
        else:
            analog_ports_in = []

        import nineml.maths as math_namespace    

        self.accounted_for_symbols = set( itertools.chain(
            self.state_variable_names, 
            alias_symbols, 
            analog_ports_in,
            nineml.maths.get_reserved_and_builtin_symbols()
            #math_namespace.functions,
            #math_namespace.symbols,
            #math_namespace.reserved_symbols,
            ) )

        #Parameters:
        # Use visitation to collect all atoms that are not aliases and not
        # state variables
        
        self.parameter_names = set()
        self.input_event_port_names = set()
        self.output_event_port_names = set()

        self.visit(dynamics)


    def action_dynamics(self, dynamics, **kwargs): 
        pass
    def action_regime(self, regime, **kwargs): 
        pass
    def action_statevariable(self, state_variable, **kwargs): 
        pass

    def notify_atom(self, atom):
        if not atom in self.accounted_for_symbols:
            self.parameter_names.add(atom)

    # Events:
    def action_outputevent(self, output_event, **kwargs):
        self.output_event_port_names.add( output_event.port_name)

    def action_onevent(self, on_event, **kwargs):
        self.input_event_port_names.add( on_event.src_port_name )


    # Atoms (possible parameters):
    def action_assignment(self, assignment, **kwargs):
        for atom in assignment.rhs_atoms:
            self.notify_atom(atom)

    def action_alias(self, alias, **kwargs):
        for atom in alias.rhs_atoms:
            self.notify_atom(atom)

    def action_timederivative(self, time_derivative, **kwargs):
        for atom in time_derivative.rhs_atoms:
            self.notify_atom(atom)

    def action_condition(self, condition, **kwargs):
        for atom in condition.rhs_atoms:
            self.notify_atom(atom)

    def action_oncondition(self, on_condition, **kwargs): 
        pass




    



class ComponentClass( ComponentClassMixinFlatStructure, 
                      ComponentClassMixinNamespaceStructure ):
    """A ComponentClass object represents a *component* in NineML. 

      .. todo::
    
         For more information, see

    """
    

    def __init__(self, name, parameters=None, analog_ports=None, 
                    event_ports=None, dynamics=None, subnodes=None, 
                    portconnections=None, interface=None,
                    regimes=None, aliases=None,state_variables=None
                    
                    ):
        """Constructs a ComponentClass
        
        :param name: The name of the component.
        :param parameters: A list containing either ``Parameter`` objects 
            or strings representing the parameter names. If ``None``, then the
            parameters are automatically infered from the dynamics block.
        :param analog_ports: A list of ``AnalogPort`` objects, which will be the
            local analog-ports for this object.
        :param event_ports: A list of ``EventPorts`` objects, which will be the
            local event-ports for this object. If this is ``None``, then they
            will be automatically inferred from the dyamics block. 
        :param dynamics: A ``Dynamics`` object, defining the local dynamics of the
            component.
        :param subnodes: A dictionary mapping namespace-names to sub-component.
            [Type: ``{string:ComponentClass, string:ComponentClass,
            string:ComponentClass}`` ] describing the namespace of subcomponents 
            for this component.
        :param portconnections: A list of pairs, specifying the connections
            between the ports of the subcomponents in this component. These can
            be `(NamespaceAddress, NamespaceAddress)' or ``(string, string)``.
        :param interface: A shorthand way of specifying the **interface** for
            this component; Parameters, AnalogPorts and EventPorts.
            ``interface`` takes a list of these objects, and automatically
            resolves them by type into the correct types.

        Examples:

        >>> a = ComponentClass(name='MyComponent1')
        
        .. todo::
            
            Point this towards and example of constructing ComponentClasses.
            This can't be here, because we also need to know about dynamics.
            For examples

        """


        # We can specify in the componentclass, and they will get forwarded to
        # the dynamics class. We check that we do not specify half-and-half:
        if dynamics is not None:
            if regimes or aliases or state_variables:
                err =  "Either specify a 'dynamics' parameter, or "
                err += "state_variables /regimes/aliases, but not both!"
                raise NineMLRuntimeError(err)
        
        else:
            ## We should always create a dynamics object, even is it is empty:
            dynamics = dyn.Dynamics(regimes=regimes,
                                    aliases=aliases,
                                    state_variables=state_variables)
                    
        
        # Turn any strings in the parameter list into Parameters:
        from nineml.utility import filter_discrete_types
        if parameters is not None:
            param_td = filter_discrete_types( parameters, (basestring, Parameter) )
            params_from_strings = [Parameter(s) for s in param_td[basestring]]
            parameters = param_td[Parameter] + params_from_strings


        self._query = componentqueryer.ComponentQueryer(self)


        
        # EventPort, StateVariable and Parameter Inference:
        inferred_struct = InterfaceInferer(dynamics, analog_ports=analog_ports)
        inf_check = lambda l1, l2, desc: check_list_contain_same_items( l1, l2,
                desc1='Declared', desc2='Inferred', ignore=['t'], desc=desc) 
        
        # Check any supplied parameters match:
        if parameters is not None:
            parameter_names = [p.name for p in parameters]
            inf_check( parameter_names, inferred_struct.parameter_names,'Parameters' )
        else:
            parameters = [Parameter(n) for n in inferred_struct.parameter_names]


        # Check any supplied state_variables match:
        if dynamics._state_variables:
            state_var_names = [p.name for p in dynamics.state_variables]
            inf_check( state_var_names, inferred_struct.state_variable_names, 'StateVariables' )
        else:
            state_vars = [StateVariable(n) for n in 
                            inferred_struct.state_variable_names ] 
            dynamics._state_variables = state_vars


        # Check Event Ports Match:

        if event_ports is not None:
            ip_evtport_names =  [ep.name for ep in event_ports if ep.is_incoming()]
            op_evtport_names = [ep.name for ep in event_ports if ep.is_outgoing()]
            #Check things Match:
            inf_check(ip_evtport_names, inferred_struct.input_event_port_names, 'Event Ports In')
            inf_check(op_evtport_names, inferred_struct.output_event_port_names,'Event Ports Out')
        else:
            event_ports = []
            #Event ports not supplied, so lets use the inferred ones.
            for evt_port_name in inferred_struct.input_event_port_names:
                event_ports.append( EventPort(name=evt_port_name, mode='recv')) 
            for evt_port_name in inferred_struct.output_event_port_names:
                event_ports.append( EventPort(name=evt_port_name, mode='send')) 


        # Construct super-classes:
        ComponentClassMixinFlatStructure.__init__(self, 
                                                   name=name, 
                                                   parameters = parameters, 
                                                   analog_ports=analog_ports, 
                                                   event_ports = event_ports, 
                                                   dynamics = dynamics )

        ComponentClassMixinNamespaceStructure.__init__(self,
                                                subnodes=subnodes, 
                                                portconnections=portconnections)

        #Finalise initiation:
        self._resolve_transition_regime_names()

        # Store flattening Information:
        self._flattener = None

        # Is the finished component valid?:
        self._validate_self()
        
    
    @property
    def flattener(self):
        return self._flattener
    
    def set_flattener(self, flattener):
        if not flattener:
            raise NineMLRuntimeError('Setting flattener to None??')
        if self.flattener:
            raise NineMLRuntimeError('Trying to change flattener')
        self._flattener = flattener

    def was_flattened(self):
        return self.flattener != None


    def _validate_self(self):
        from nineml.abstraction_layer.validators import ComponentValidator
        ComponentValidator.validate_component(self)
        


    @property
    def query(self):
        """ Returns the ``ComponentQuery`` object associated with this class"""
        return self._query


    def is_flat(self):
        """Is this component flat or does it have subcomponents?
        
        Returns a ``Boolean`` specifying whether this component is flat; i.e.
        has no subcomponent
        """
        
        return len(self.subnodes) == 0
        


    def accept_visitor(self, visitor, **kwargs):
        """ |VISITATION| """
        return visitor.visit_componentclass(self, **kwargs)

        


    def _resolve_transition_regime_names(self):
        # Check that the names of the regimes are unique:
        names = [ r.name for r in self.regimes ]
        nineml.utility.assert_no_duplicates(names)

        #Create a map of regime names to regimes:
        regime_map = dict( [ (r.name, r) for r in self.regimes] )

        # We only worry about 'target' regimes, since source regimes are taken 
        # care of for us by the Regime objects they are attached to.
        for trans in self.transitions:
            if not trans.target_regime_name in regime_map:
                errmsg = "Can't find regime: %s" % trans.target_regime_name
                raise NineMLRuntimeError(errmsg)
            trans.set_target_regime( regime_map[trans.target_regime_name] )

