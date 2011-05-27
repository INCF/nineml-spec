

# System Imports:
import copy
import itertools



# Relative Imports:
import util






# Base class for depth-first visitation:
class ModelVisitorDF(object):

    def Visit(self,model):
        model.AcceptVisitor(self)

    def AcceptComponentNode(self,node):
        self.VisitComponentNode(node)

    def AcceptModelNode(self,node):
        self.VisitModelNode(node)
        for sn in sorted(node.subnodes.values(), key=lambda sn:sn.getContainedNamespaceName() ):
            sn.AcceptVisitor(self)


    # To be overridden
    def VisitComponentNode(self, node):
        pass

    # To be overridden
    def VisitModelNode(self, node):
        pass





# Base class for visitation:
class ModelVisitorDF_NodeCollector(ModelVisitorDF):
    def __init__(self, model=None):
        self.nodes = []
        if model: self.Visit(model)

    def VisitComponentNode(self, node):
        self.nodes.append(node)
    def VisitModelNode(self, node):
        self.nodes.append(node)

    def __iter__(self):
        return iter(self.nodes)

class ModelVisitorDF_ComponentCollector(ModelVisitorDF):
    def __init__(self, model=None):
        self.components = []
        if model: self.Visit(model)

    def VisitComponentNode(self, node):
        self.components.append(node)

    def __iter__(self):
        return iter(self.components)


class ModelVisitorDF_ModelCollector(ModelVisitorDF):
    def __init__(self, model=None,include_root=True):
        assert include_root
        self.models = []
        if model: self.Visit(model)

    def VisitModelNode(self, node):
        self.models.append(node)

    def __iter__(self):
        return iter(self.components)





import nineml.abstraction_layer as nineml


class NamespaceAddress(object):
    def __init__(self, loc):
        if isinstance(loc, basestring):
            self.loctuple = loc.split('.')
        elif isinstance(loc, tuple):
            self.loctuple = loc
        else:
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

    def get_str_prefix(self):
        return "_".join( self.loctuple ) + "_"

    @classmethod
    def concat(cls,*args):
        loctuple = tuple( flattenFirstLevel( [ list(a.loctuple) for a in args ] ) )
        return NamespaceAddress(loc=loctuple)











#c = Component(...)
#c.query["./%TRANSITION{TO=...}"]
#c.query["./%REGIME{NAME=...}"]
#c.query["./subcomponent1/%REGIME{NAME=...}"]
#print c.query.ports


from Cheetah.Template import Template

def dump_reduced(component, filename):
    
    tmpl = """
    MODEL: 

    PORTS:
    ==========

    #for p in $component.analog_ports:
      AnalogPort: $p.name, $p.mode, $p.reduce_op
    #end for
    
    #for p in $component.event_ports:
      EventPort: $p.name, $p.mode, $p.reduce_op
    #end for


    PARAMETERS
    ===============
    #for up in $component.user_parameters:
      Parameter: $up
    #end for

    REGIMES:
    ========
    #for regime in $component.regimes:
    
    Regime: $regime
    ----------------
    
    #for eqn in $regime.equations:
       Eqn: $eqn
    #end for

        Transitions
        ~~~~~~~~~~~~~~
        #for $transi in $regime.transitions:
           Transition: $transi.name

             Condition: $transi.condition

           #for node in $transi.nodes:
             Node: $node
           #end for

        #end for


    #end for

    """


    data = { 'component':component }
    f = open(filename,"w")
    s = Template(tmpl, data).respond()
    f.write(s)
    f.close()















def writeCSolverSimple( component, filename):
    assert isinstance(component, nineml.Component)


    templ1 = """
    
    int current_regime;

    // State Variables:
    #for statevar in $component.odes:
    float $statevar.dependent_variable = 0;
    #end for





    """


    for i in range(5): 
        print 

    cppTxt = Template( templ1, {'component':component } ).respond()

    print cppTxt
