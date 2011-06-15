
from operator import and_
from nineml import __version__
import re
import copy
import itertools

from nineml.cache_decorator import im_cache_decorator as cache
import math_namespace
from nineml import helpers
from expressions import *
from conditions import *
from ports import *
from cond_parse import cond_parse
from expr_parse import expr_parse
from ..xmlns import *

from nineml.utility import *

from nineml.utility import ExpectSingle, FilterExpectSingle, Filter, FilterType

# System Imports:
import copy
import itertools




from nineml.utility import invertDictionary

class ComponentClass(object):
    element_name = "ComponentClass"

    #def AcceptVisitor(self,visitor,**kwargs):
    #    return visitor.VisitComponent(self,**kwargs)
    
    def __init__(self, name, parameters = [], analog_ports = [], event_ports = [], dynamics=None):
        parameters = parameters or []
        analog_ports = analog_ports or []
        event_ports = event_ports or []

        self._name = name
        self._parameters = parameters
        self._analog_ports = analog_ports
        self._event_ports = event_ports
        self._dynamics = dynamics

    @property
    def dynamics(self):
        return self._dynamics

    @property
    def name(self):
        return self._name

    @property
    def parameters(self):
        return iter(self._parameters)

    # <- Forwarding functions -->
    @property
    def ports_map(self):
        return dict( [ (p.name,p) for p in chain(self._analog_ports, self._event_ports) ] )
    
    @property
    def aliases_map(self):
        return self._dynamics.aliases_map

    @property
    def aliases(self):
        return self._dynamics.aliases

    @property
    def alias_symbols(self):
        return [ a.lhs for a in self.aliases ]

    @property
    def regimes(self):
        return self._dynamics.regimes


    @property
    def analog_ports(self):
        return self._analog_ports

    @property
    def event_ports(self):
        return self._event_ports

    @property
    def transitions(self):
        return self._dynamics.transitions

    @property
    def state_variables(self):
        return self._dynamics.state_variables


    @property
    def on_conditions(self):
        for r in self.regimes:
            for c in r.on_conditions:
                yield c

    def get_regimes_to(self,regime):
        """ Gets as a list all regimes that transition to regime"""
        
        return [t.from_ for t in self.transitions if t.to==regime]

    @property
    def ports(self):    
        """ yield all ports for component"""
        for p in self.analog_ports:
            yield p 
        for p in self.event_ports:
            yield p

    @property
    def reduce_ports(self):    
        """ yield all reduce ports for component"""
        for p in self.analog_ports:
            if p.mode=='reduce':
                yield p 
    

    def check_ports(self):
        for p in self.analog_ports:
            if not isinstance(p,AnalogPort):
                raise ValueError, "Component ports attribute can contain only AnalogPort objects."+\
                      "EventPorts go in Transition conditions(recv) and Transition nodes (send)"
            # may only write to user_parameters
            if p.mode=="recv" and p.symbol in self.non_parameter_symbols:
                raise ValueError, "'recv' AnalogPorts may not target existing alias symbols,"+\
                      "ODE lhs vars, or lhs of Assignments ops."

            alias_names = [b.lhs for b in self.aliases]
            #print alias_names
            if p.mode=="send" and p.expr==None and (p.symbol not in self.variables and not p.symbol in alias_names) :
                raise ValueError, "'send' AnalogPort with symbol='%s' source undefined in component." % (p.symbol,)
        

    def filter_ports(self,cls=None,mode=None,symb=None):
        """ yields all ports filtered by class, mode, symb

        mode,symb can be strings, or collections of strings
        on which "in" is defined.
        
        """
        if isinstance(mode,str):
            mode = (mode,)
        if isinstance(symb,str):
            symb = (symb,)

        for p in self.ports:
            if cls and not isinstance(p,cls): continue
            if mode and p.mode not in mode: continue
            if symb and p.symbol not in symb: continue
            yield p

    def backsub_aliases(self):
        """ This function finds aliases with undefined functions, and uses
        the alias_map to attempt to resolve them. """

        # build alias dependency tree
        # and perform substitution, recursively
        def build_and_resolve_bdtree(b):
            _bd_tree = {}
            for f in b.missing_functions:
                if f in self.aliases_map:
                    _bd_tree[f] = build_and_resolve_bdtree(self.aliases_map[f])
                    # resolve (lower level is already resolved now) 
                    b.substitute_alias(self.aliases_map[f])
                    # re-calc functions
                    b.parse()
                else:
                    raise ValueError, "alias '%s' calls unresolvable functions." % b.as_expr()
            return _bd_tree  
        
        bd_tree = {}
        for b in self.aliases_map.itervalues():
            bd_tree[b.name] = build_and_resolve_bdtree(b)

    def backsub_equations(self):
        """ this function finds all undefined functions in equations, and uses
        the alias_map to resolve them """
        from nineml.abstraction_layer.visitors import InPlaceTransform

        for alias in self.aliases:
            trans = InPlaceTransform( originalname = alias.lhs, targetname = "(%s)"%alias.rhs )
            # Since we do not want to backsub in lhs of alias, we can't call self.AcceptVisitor() directly
            for r in self.regimes:
                r.AcceptVisitor(trans)




    
    def resolve_references(self):
        """ Uses self.regimes_map and self.transitions_map to resolve references in self.regimes and self.transitions"""

        # resolve transition from_=None to=None to parent regime
        for r in self.regimes:
            for t in r.transitions:
                if t.from_==None:
                    t.from_=r
                if t.to==None:
                    t.to=r

        # resolve regime references in transitions:
        for t in self.transitions:
            for attr in ('to','from_'):
                ref = t.__getattribute__(attr)
                # transition defines no to,from
                if ref==None: continue
                if not isinstance(ref,Regime):
                    assert isinstance(ref,Reference) and issubclass(ref.cls,Regime), "Expected Regime reference or Regime"
                    t.__setattr__(attr,self.regime_map[ref.name])

        # resolve transition references in regimes

        for r in self.regimes:
            for t in r.transitions:
                if isinstance(t, Transition ): continue
                assert isinstance(t,Reference) and r.cls==Transition, "Expected Transition reference or Transition" 
                r.transitions.remove(t)
                r.transitions.add(self.transition_map[t.name])

        # add transitions to regimes:
        for t in self.transitions:
            t.from_.add_transition(t)
            


    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False

        sort_key = lambda node: node.name

        return reduce(and_, (self.name == other.name,
                             self.parameters == other.parameters,
                             sorted(self.transitions, key=sort_key) == sorted(other.transitions, key=sort_key),
                             sorted(self.regimes, key=sort_key) == sorted(other.regimes, key=sort_key),
                             sorted(self.aliases, key=sort_key) == sorted(other.aliases, key=sort_key)))

    @property
    def odes(self):
        for r in self.regimes:
            for ode in r.odes:
                yield ode

    @property
    def equations(self):

        for r in self.regimes:
            for t in r.transitions:
                for eq in t.equations:
                    yield eq
            for eq in r.equations:
                yield eq
        # 'send' AnalogPorts can have an expression
        for p in self.filter_ports(cls=AnalogPort,mode='send'):
            if p.expr: yield p.expr

    @property
    def conditions(self):
        """ Returns all conditions """
        # TODO transitions
        for t in self.transitions:
            yield t.condition

    #@property
    #def aliases(self):
    #    return self.aliases_map.itervalues()


    def check_alias_expressions(self):
        """ Bound symbols (which are static when running the model)
        can depend only on 'user parameters' (which are static when running the model)

        This parses the alias rhs expressions to verify this is so.
        """

        params = self.user_parameters
        
        for alias in self.aliases:
            # It is up to the user to call backsub at the appropriate time,
            # or implement other facilities for resolving user defined functions
            # aliases ...
            # There for the following check is removed:
            #for f in alias.missing_functions:
            #    raise ValueError, "Alias '%s' calls undefined function '%s' " % str(alias.as_expr(),f)
            
            non_param_names = self.non_parameter_symbols.intersection(alias.names)
            # may reference other aliases
            non_param_names = non_param_names.difference(self.aliases_map.iterkeys())
            if non_param_names:
                raise ValueError, "Alias symbols referencing variables is illegal: %s" % str(non_param_names)

    def check_non_parameter_symbols(self):
        """ Check that non-parameters symbols are not conflicting
        with math_namespace symbols """
        if self.non_parameter_symbols.intersection(math_namespace.symbols)!=set():
            raise ValueError, "Non-parameters symbols (variables and bound symbols) may "+\
                  "not redefine math symbols (such as 'e','pi')"
            
                

    @property
    @cache
    def user_parameters(self):
        """ User parameters for the component. """
        # TODO: cache once determined
        # compare to the parameters lists declared by the user 


        symbols = set([])
        for e in self.equations:
            symbols.update( e.names )
            
            
        # now same for conditions
        for c in self.conditions:
            symbols.update(c.names)

        # now same for aliases
        for b in self.aliases:
            symbols.update(b.names)

        symbols = symbols.difference(self.non_parameter_symbols)
        symbols = symbols.difference(math_namespace.symbols)
        # remove symbols of AnalogPorts with mode='recv'
        symbols = symbols.difference([p.symbol for p in self.filter_ports(cls=AnalogPort,mode=('recv'))])
        return symbols.difference(math_namespace.reserved_symbols)

                 
    @property
    @cache
    def non_parameter_symbols(self):
        """ All aliases, assignment and inplace left-hand-sides, plus X for ODE dX/dt = ...""" 
        # TODO: cache once determined
        symbols = set([])
        symbols.update(self.variables)
        symbols.update(self.bound_symbols)
        return symbols

    @property
    @cache
    def variables(self):
        symbols = set([])
        symbols.update(self.integrated_variables)
        symbols.update(self.assigned_variables)
        symbols.update(self.independent_variables)
        return symbols

#    @property
#    @cache
#    def state_variables(self):
#        symbols = set([])
#        symbols.update(self.integrated_variables)
#        symbols.update(self.assigned_variables)
#        return symbols
#

    @property
    @cache
    def bound_symbols(self):
        # TODO: cache once determined
        """ Return symbols which are subject to aliases (static assignments)"""
        # construct set of keys (bound symbols)
        statics = set(self.aliases_map)

        # check user is not writing to bound variables
        if statics.intersection(self.integrated_variables)!=set():
            raise ValueError, "Error: user bound symbols which appear on lhs of ODEs"
        if statics.intersection(self.assigned_variables)!=set():
            raise ValueError, "Error: user bound symbols which appear on lhs of Assignments OPs"
        
        return statics

    @property
    @cache
    def assigned_variables(self):
        """ All assignment and inplace lhs' (which may also be ODE integrated variables),
        but not aliases (which are not variables, but static) """

        # TODO: cache once determined
        variables = set([])
        for equation in self.equations:
            if isinstance(equation, (Assignment)):
                variables.add(equation.to)
        return variables

    @property
    @cache
    def independent_variables(self):
        """ All X for ODE dY/dX """
        # TODO: cache once determined
        variables = set([])
        for r in self.regimes:
            for eq in r.odes:
                variables.add(eq.indep_variable)
        return variables

    
    @property
    @cache
    def integrated_variables(self):
        """ All X for ODE dX/dt """
        # TODO: cache once determined
        variables = set([])
        for equation in self.equations:
            if isinstance(equation, ODE):
                variables.add(equation.dependent_variable)
        return variables

    def add_prefix(self, prefix, exclude=[]):
        """Add a prefix to all symbols except those explicitly excluded"""
        #Temporary solution for namespacing, until we get namespaces figured out.
        for eq in self.equations:
            if eq.to not in exclude:
                eq.to = prefix + eq.to
            eq.rhs = eq.prefix(prefix, exclude=exclude)
            eq.parse() # update names, funcs
        for port in self.ports:
            if port.symbol not in exclude and port.symbol != 't':
                self.ports_map.pop(port.symbol)
                port.symbol = prefix + port.symbol
                self.ports_map[port.symbol] = port

        # invalidate the instance method cache, triggering all the @cache decorated methods to be re-calculated
        self.__cache__={}

        # TODO: still this is not working. See test_component_symbols.py -> test_add_prefix
        # Presumably, for this to work inplace, we need to "re-run" certain parts of the
        # constructor ... maybe this is improved with Namespaces ...

        # See also: test_expressions.py -> test_prefixing
    
    def to_xml(self):
        from nineml.abstraction_layer.writers import XMLWriter
        return XMLWriter().Visit(self)

  
    @classmethod
    def from_xml(cls, element):
        """
        Parse an XML ElementTree structure and return a Component instance.
        
        `element` - should be an ElementTree Element instance.
        
        See:
            http://docs.python.org/library/xml.etree.elementtree.html
            http://codespeak.net/lxml/
        """
        assert False
        from nineml.abstraction_layer.readers import XMLReader
        return XMLReader.read_component(elem)

        assert element.tag == NINEML+cls.element_name
        parameters = [p.get("name") for p in element.findall(NINEML+"parameter")]
        aliases = [Alias.from_xml(b) for b in element.findall(NINEML+Alias.element_name)] 
        regimes = [Regime.from_xml(e) for e in element.findall(NINEML+Regime.element_name)]

        analog_ports = []
        for port_cls in (AnalogPort,):
            for e in element.findall(NINEML+port_cls.element_name):
                analog_ports.append(port_cls.from_xml(e))

        transitions = [Transition.from_xml(t) for t in element.findall(NINEML+Transition.element_name)]


        new_comp = cls(element.get("name"), parameters, regimes=regimes, transitions=transitions, aliases=aliases, ports=analog_ports)

        return new_comp


    def write(self, file):
        """
        Export this model to a file in 9ML XML format.

        file is filename or file object.
        """
        doc = E.nineml(self.to_xml(), xmlns=nineml_namespace)
        etree.ElementTree(doc).write(file, encoding="UTF-8",
                                     pretty_print=True, xml_declaration=True)

    def to_dot(self,out, show_contents=True, relabel_nodes=False):
        """ Write a DOT graph representation of component

        http://en.wikipedia.org/wiki/DOT_language

        Convert a dot file to an image using, i.e.:
          dot -Tsvg spike_generator.dot -o spike_generator.svg
          dot -Tpng spike_generator.png -o spike_generator.png

        """

        # if out is a str, make a file
        if isinstance(out,str):
            out = file(out,'w')

        out.write("""digraph "NineML Component '%s'" {\n""" % self.name)

        out.write('\toverlap = "scale";\n')

        regime_id = dict([(kv[0],i) for i,kv in enumerate(self.regime_map.iteritems())])

        #TransitionLabelDict:
        node_labels = {}
        for r in itertools.chain(self.regimes):
            node_labels[r] = "Regime:%d"%len(node_labels)

        for t in itertools.chain(self.transitions):
            node_labels[t] = "Trans:%d"%len(node_labels)


        if show_contents:

            out.write('\tgraph [fontsize=30 labelloc="t" label="" splines=true overlap=false rankdir = "LR"];\n\tratio = auto\n');
            props = 'style = "filled, bold" penwidth = 1 fillcolor = "white" fontname = "Courier New" shape = "Mrecord" '
            # regime template
            t_regime = '\t"%(node)s" [ style = "filled, bold" penwidth = 1 fillcolor = "white" fontname = "Courier New" '+\
                       'shape = "Mrecord" \\\n\t\tlabel =<<table border="0" cellborder="0" cellpadding="3" bgcolor="white">'+\
                       '<tr><td bgcolor="black" \\\n\t\talign="center" colspan="2"><font color="white">'+\
                       '%(regime_name)s</font></td></tr>\\\n\t\t%(contents)s</table>> ];\n ' 
            # to fill: node, regime_name, contents
            ns = {}

            for r in self.regimes:
                
                ns['node'] = "regime_%d" % regime_id[r.name]
                ns['regime_name'] = r.name if not relabel_nodes else node_labels[r]
                ns['contents'] = r.dot_content()
                out.write(t_regime % ns)



        # transition template
        t_transition = '\t"%(from)s" -> "%(to)s" [ style = "filled, bold" penwidth = 1 fillcolor = "white" fontname = "Courier New" \\\n'+\
                       '\t\tlabel =<<table border="0" cellborder="0" cellpadding="3" bgcolor="#C0C0C0"> \\\n'+\
                       '\t\t<tr><td bgcolor="blue" align="center" colspan="2"><font color="white"> \\\n'+\
                       '\t\t%(trans_name)s</font> \\\n'+\
                       '\t\t</td></tr> \\\n'+\
                       '\t\t<tr><td bgcolor="green" align="center" colspan="2"><font color="black"> \\\n'+\
                       '\t\t @ %(condition)s</font></td></tr> \\\n'+\
                       '\t\t%(contents)s</table>> ];\n ' 
        
        for t in self.transitions:
            if show_contents:

                #out.write('label="%s @ %s"' % (t.name.encode('utf-8'), t.condition.encode('utf-8'),))
                #out.write('\tregime_%d -> regime_%d [label="%s @ %s"];\n' % (regime_id[t.from_.name], regime_id[t.to.name],
                #                                                   t.name.encode('utf-8'), t.condition.encode('utf-8')))

                # to fill: node, regime_name, contents
                ns = {}

                ns['from'] = "regime_%d" % regime_id[t.from_.name]
                ns['to'] = "regime_%d" % regime_id[t.to.name]
                ns['trans_name'] = t.name if not relabel_nodes else node_labels[t]
                ns['condition'] = dot_escape(t.condition.as_expr())
                #ns['contents'] = t.dot_content()
                ns['contents'] = t.dot_content()
                out.write(t_transition % ns)
                
            else:
                out.write('\tregime_%d -> regime_%d [label="%s"];\n' % (regime_id[t.from_.name], regime_id[t.to.name],
                                                                   t.name.encode('utf-8')))

        out.write('}')


    def join(self, component, prefix=None, **port_connections):
        """ Creates a component which is the self component joined with component.

        For example, this is great for constructing HH components with additional channels in
        a modular way.

        The Regime spaces are 'crossed' such that if c1 as regimes=(a,b), and c2 has regimes=(X,Y)
        then the joined component, CJ, has regimes Union(a,X), Union(a,Y), Union(b,X), Union(b,Y),
        where Union here refers to the Regime "Union".  Transition 'to=' are set up to transition so as
        to leave the other component unaffected, i.e. if c2.X has an transition with to=Y, then
        Union(a,X) has an Transition with to=Union(a,Y) and Union(b,X) has an Transition with to=Union(b,Y).

        Symbol/Name collisions:

        Unioning two components brings with it the possibility for name collisions.

        An example would be the HH model and an additional channel model both using the symbol 'm'
        to denote a gating variable.

        As such, the component to join has all its variables prefixed
        by its component name by default.  If the component name is
        such that this is not possible, or the user simple chooses to
        do so, the user may override the preix with the prefix kwarg.

        hh_im = hh.join(im,prefix="Im",I=im.I)

        This will prefix the im component state vars, aliases, parameters with Im_ in the resulting Component hh_im.

        Notes:

         - RecvPorts which are connected are no longer visible in the ComponentUnion.

         - ReducePorts, SendPorts persist and are visible in the ComponentUnion.

         - If SendPorts are connected to a ReducePort, the local symbol (LS_reduce) of the reduce port is replaced
           by: (LS_reduce <reduce_op> SP0 <reduce_op> ... SPN)  where SP0..SPN are the local symbols of the SendPorts.
           In this way, the ReducePort remains available in the joined Component for additional connections.

         - EventPorts persist and are visible in the ComponentUnion.

         - Connected EventPorts:
           1) For the InputEvent, the "do" must be moved to where the OutputEvent
              the "to" should modify the "to" of the Transition which generates the OutputEvent
              to induce the appropriate transition in the Regime space of the InputEvent component.

           2) OutputEvents persist.  Connected InputEvents do not persist in the joined model.


        """

        # cross regime spaces
        # setup new transitions

        # component name prefixing:
        # aliases: lhs, rhs names & funcs if not in math_namespace
        # ode dep var, rhs names & funcs if not in math_namespace
        # assignment lhs, rhs names & funcs if not in math_namespace
        # inplace lhs, rhs names & funcs if not in math_namespace
        # conditions, cond names & funcs if not in math_namespace
        

        # contianer for cross of regime spaces of 2 components
        new_regimes = []
        for r1 in self.regimes:
            for r2 in component.regimes:
                name = "%s*%s" % (r1.name, r2.name)
                # we will lose all sub-Union-like nodes this way
                nodes = list(r2.nodes_filter(lambda x: True))
                # AnalogPorts: What needs to be done:
                # - RecvPorts -> substitute local name of sent variable to prefixed port symbol
                # - SendPorts -> prefix
                # - ReducePorts -> substitute: prefixed_port_symbol op sent_var

                nodes += list(r1.nodes_filter(lambda x: True))
                # AnalogPorts: What needs to be done:
                # - RecvPorts -> substituted local (prefixed name) of sent variable to port symbol
                # - SendPorts -> nothing.
                # - ReducePorts -> substitute: port_symbol op prefixed_sent_var

                # transitions:
                transitions = []
                for e in r1.transitions:
                    # prevent name collision
                    e_name = "%s.%s"  % (r1.name,e.name)
                    # to name does not change r2.regime
                    to_name = "%s*%s" % (e.to.name,r2.name)
                    transitions += [Transition(*e.nodes, to=to_name, name=e_name, condition=e.condition)]

                new_regimes += [Regime(nodes, name=name,transitions=transitions)]

        # ports


        # build list of outward facing AnalogPorts

        name = "*".join([c.name for c in comps])
        return Component(name, regimes = regimes, ports=ports)




class TreeNode(object):
    def __init__(self, subnodes = None):  
        subnodes = subnodes or {}

        self._parentmodel = None
        self.subnodes = {}
        self.portconnections = []

        for namespace,subnode in subnodes.iteritems():
                self.insert_subnode(subnode=subnode, namespace=namespace)


    # Naming Functions:
    # != TO GO -!
    def getContainedNamespaceName(self):
        if not self.getParentModel(): return ""
        return invertDictionary(self.getParentModel().subnodes)[self]

    def get_node_addr(self):
        if not self.getParentModel():
            return NamespaceAddress.create_root()
        else:
            return self.getParentModel().get_node_addr().get_subns_addr( self.getContainedNamespaceName() ) 


    # Parenting:
    def setParentModel(self,parentmodel):
        assert not self._parentmodel
        self._parentmodel = parentmodel
    def getParentModel(self): 
        return self._parentmodel
        

    def insert_subnode(self, subnode, namespace):
        assert not namespace in self.subnodes
        self.subnodes[namespace] = copy.deepcopy( subnode ) 
        self.subnodes[namespace].setParentModel(self)

    def connect_ports( self, src, sink ):
        #TODO: Check that the ports are connected to items in this model.
       self.portconnections.append( (NamespaceAddress(src),NamespaceAddress(sink) ) ) 

    def isLeaf(self):
        return len(self.subnodes) == 0


    




class ComponentNodeCombined( ComponentClass, TreeNode ):
    
    def isflat(self):
        return self.isLeaf()

    def __init__(self, name, parameters=None, analog_ports=None, event_ports=None, dynamics=None, subnodes=None, model=None):

        self.query = ComponentQueryer(self)

        if dynamics == None:
            from nineml.abstraction_layer import Dynamics
            dynamics = Dynamics()

        ComponentClass.__init__(self, name=name, parameters = parameters, analog_ports=analog_ports, event_ports = event_ports, dynamics = dynamics)
        TreeNode.__init__(self,subnodes=subnodes)


        
    # Connections and Subnodes:
    

    def get_fully_qualified_port_connections(self):
        self.namespace = self.get_node_addr()
        def make_fqname(target):
            return NamespaceAddress.concat( self.namespace, target)
        conns = [ (make_fqname(src),make_fqname(sink)) for (src,sink) in self.portconnections ]
        return conns

    def AcceptVisitor(self,visitor,**kwargs):
        return visitor.VisitComponentNodeCombined(self)






















class ComponentQueryer(object):
    def __init__(self, component):
        self.component = component

    # Find basic properties by name
    def regime(self, name=None,):
        assert isinstance(name,basestring)
        rFunc = lambda r:r.name==name 
        return FilterExpectSingle( self.component.regimes,rFunc ) 
        
    def event_send_ports(self):
        return [ p for p in self.component.event_ports if p.mode=='send']

    def event_recv_ports(self):
        return [ p for p in self.component.event_ports if p.mode=='recv']
    
    def get_fully_addressed_analogports_new(self):
        comp_addr = self.component.get_node_addr()
        return dict( [ (comp_addr.get_subns_addr(port.name), port) for port in self.component.analog_ports] )

    #More advanced searches on just this node:
    @property
    def analog_reduce_ports(self):
        reduce_ports = [ p for p in self.component.analog_ports if p.mode=='reduce' ]
        return reduce_ports





class NamespaceAddress(object):
    def __init__(self, loc):
        if isinstance(loc, basestring):
            self.loctuple = loc.split('.')
        elif isinstance(loc, tuple):
            self.loctuple = loc
        elif isinstance(loc, NamespaceAddress):
            self.loctuple = loc.loctuple
        else:
            print loc, type(loc)
            assert False

    
    def __hash__(self):
        return hash(self.loctuple)

    def __eq__(self,rhs):
        if not isinstance(rhs, self.__class__): 
            return False
        return self.loctuple == rhs.loctuple


    def get_subns_addr(self, component_name):
        return NamespaceAddress( loc = tuple( list(self.loctuple) + [component_name] ) )
    def get_parent_addr(self):
        assert len(self.loctuple) > 0
        return NamespaceAddress( loc = self.loctuple[:-1] )

    def __repr__(self):
        return "NSAddr: '" + "//" + "/".join( self.loctuple) + "/'"

    @classmethod
    def create_root(cls):
        return NamespaceAddress( loc=() )

    def getstr(self):
        return "_".join( self.loctuple )

    def get_str_prefix(self):
        return self.getstr() + "_" 

    @classmethod
    def concat(cls,*args):
        print 'Concatenating:', args
        from nineml.utility import flattenFirstLevel
        loctuple = tuple( flattenFirstLevel( [ list(a.loctuple) for a in args  ] ) )
        res = NamespaceAddress(loc=loctuple)
        print 'yields:', res
        return res
