
from operator import and_


from namespaceaddress import NamespaceAddress
import componentqueryer
import nineml.utility
import dynamics as dyn


import copy
import itertools


class ComponentClassMixinFlatStructure(object):

    def __init__(self, name, parameters = None, analog_ports = None, 
                 event_ports = None, dynamics=None):

        self._name = name
        self._parameters = parameters or []
        self._analog_ports = analog_ports or []
        self._event_ports = event_ports or []
        self._dynamics = dynamics


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
    def transitions(self):
        """Forwarding function to self.dynamics.transitions"""
        return self._dynamics.transitions

    @property
    def state_variables(self):
        """Forwarding function to self.dynamics.state_variables"""
        return self._dynamics.state_variables
    # -------------------------- #





    def backsub_all(self): 
        """Expand all alias definitions in local equations.

        This function finds ``Aliases``, ``TimeDerivatives``, ``SendPorts``, ``Assignments``
        and ``Conditions``  with which are defined in terms of other aliases,
        and expands them, such that each only has Parameters,
        StateVariables and recv/reduce AnalogPorts on the RHS.
        
        It is syntactic sugar for::
            
            self.backsub_aliases()
            self.backsub_equations()

        """

        self.backsub_aliases()
        self.backsub_equations()


    def backsub_aliases(self):
        """Expands all alias definitions within the local aliases.
        
        This function finds aliases with which are defined in terms of other
        aliases, and expands them, such that each aliases only has Parameters,
        StateVariables and recv/reduce AnalogPorts on the RHS.

        """
        #TODO: Check for recursion

        # Back-substitute aliases, by resolving them
        # them then substituting recursively:
        def build_and_resolve_alias(alias):
            for missing_alias_name in alias.rhs_missing_functions:
                if missing_alias_name in self.aliases_map:
                    missing_alias = self.aliases_map[missing_alias_name]
                    build_and_resolve_alias( missing_alias )
                    # resolve (lower level is already resolved now) 
                    alias.substitute_alias( missing_alias )
                else:
                    errmsg = "Unable to resolve alias %s" % alias.as_expr()
                    raise NineMLRuntimeException(errmsg)

        
        for alias in self.aliases:
            build_and_resolve_alias(alias)

    def backsub_equations(self):
        """Expands all equations definitions within the local aliases.
        
        This function finds ``TimeDerivatives``, ``SendPorts``, ``Assignments``
        and ``Conditions``  with which are defined in terms of other aliases,
        and expands them, such that each only has Parameters,
        StateVariables and recv/reduce AnalogPorts on the RHS.
        """

        from nineml.abstraction_layer.visitors import InPlaceTransform

        for alias in self.aliases:
            trans = InPlaceTransform( originalname = alias.lhs, 
                                      targetname = "(%s)"%alias.rhs )
            # Since we do not want to backsub in lhs of this alias, we can't
            # call self.accept_visitor() directly
            for r in self.regimes:
                r.accept_visitor(trans)


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
        for namespace,subnode in subnodes.iteritems():
            self.insert_subnode(subnode=subnode, namespace=namespace)
        
        for src,sink in portconnections:
            self.connect_ports(src,sink)

    # Parenting:
    def _set_parent_model(self,parentmodel):
        assert not self._parentmodel
        self._parentmodel = parentmodel
    def _get_parent_model(self): 
        return self._parentmodel




    def get_node_addr(self):
        """Get the namespace address of this component"""
        from nineml.utility import invert_dictionary

        parent = self._get_parent_model() 
        if not parent:
            return NamespaceAddress.create_root()
        else:
            contained_namespace = invert_dictionary(parent.subnodes)[self]
            return parent.get_node_addr().get_subns_addr( contained_namespace ) 


        

    def insert_subnode(self, subnode, namespace):
        """Insert a subnode into this component
        

        :param subnode: An object of type ``ComponentClass``.
        :param namespace: A `string` specifying the name of the component in
            this components namespace.

        :raises: ``NineMLRuntimeException`` if there is already a subcomponent at
            the same namespace location


        """

        assert not namespace in self.subnodes
        self.subnodes[namespace] = copy.deepcopy( subnode ) 
        self.subnodes[namespace]._set_parent_model(self)

    def connect_ports( self, src, sink ):
        """Connects the ports of 2 subcomponents.
        
        The ports can be specified as ``string`` s or ``NamespaceAddresses`` es.


        :param src: The source port of one sub-component; this should either an
            event port or analog port, but it *must* be a send port.

        :param sink: The sink port of one sub-component; this should either an
            event port or analog port, but it *must* be either a 'recv' or a
            'reduce' port.

        """

        #TODO: Check that the ports are connected to items in this model.
        connection = (NamespaceAddress(src),NamespaceAddress(sink) )
        self.portconnections.append( connection ) 



    



class ComponentClass( ComponentClassMixinFlatStructure, 
                      ComponentClassMixinNamespaceStructure ):
    """A ComponentClass object represents a *component* in NineML. 

      .. todo::
    
         For more information, see

    """
    

    def __init__(self, name, parameters=None, analog_ports=None, 
                    event_ports=None, dynamics=None, subnodes=None, 
                    portconnections=None, interface=None):
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
            be `(NamespaceAddress,NamespaceAddress)' or ``(string,string)``.
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

        self._query = componentqueryer.ComponentQueryer(self)

        # We should always create a dynamics object, even is it is empty:
        if dynamics == None:
            dynamics = dyn.Dynamics()

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
        self._ResolveTransitionRegimeNames()

        # Add some additional error checking:
        # TODO


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
        


    def accept_visitor(self, visitor,**kwargs):
        """ |VISITATION| """
        return visitor.VisitComponentClass(self)

        


    def _ResolveTransitionRegimeNames(self):
        # Check that the names of the regimes are unique:
        names = [ r.name for r in self.regimes ]
        nineml.utility.assert_no_duplicates(names)

        #Create a map of regime names to regimes:
        regimeMap = dict( [ (r.name,r) for r in self.regimes] )

        # We only worry about 'target' regimes, since source regimes are taken 
        # care of for us by the Regime objects they are attached to.
        for t in self.transitions:
            if not t.target_regime_name in regimeMap:
                errmsg = "Can't find target regime: %s"%t.target_regime_name
                raise NineMLRuntimeException(errmsg)
            t.set_target_regime( regimeMap[t.target_regime_name] )

