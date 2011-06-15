
from nineml.utility import LocationMgr, Join, ExpectSingle, FilterExpectSingle


from collections import defaultdict


import os




#import core as al_core
import nineml.abstraction_layer as al
import al.core as al_core
from xmlns import etree,E,MATHML,nineml_namespace,NINEML


NS = "{CoModL}"


def load_ComponentClass(element):
    from nineml.abstraction_layer.models import ComponentNode
    name = element.get("name")
     
    subnodes = loadBlocks( element,blocks=('Parameter','AnalogPort','EventPort','Dynamics' ) )
    return ComponentNode(   name=name,
                                   parameters = subnodes["Parameter" ] ,
                                   analog_ports = subnodes["AnalogPort"] ,
                                   event_ports = subnodes["EventPort"],
                                   dynamics = ExpectSingle( subnodes["Dynamics"] )
                                   )
   
def load_Parameter(element):
    name = element.get("name")
    return al_core.Parameter(name=name) 

def load_AnalogPort(element):
    name = element.get("name")
    mode = element.get("mode")
    reduce_op = element.get("reduce_op",None)
    return al_core.AnalogPort( internal_symbol = name, mode = mode, op = reduce_op )

def load_EventPort(element):
    name = element.get("name")
    mode = element.get("mode")
    return al_core.EventPort( internal_symbol = name, mode = mode )



def load_Dynamics(element):
    subnodes = loadBlocks( element, blocks=('Regime','Alias','StateVariable' )  )
    return al_core.Dynamics(  regimes = subnodes["Regime"] ,
                             aliases = subnodes["Alias"] ,
                             state_variables = subnodes["StateVariable"] ,
                             )


def load_RegimeClass(element):
    name = element.get("name")
    subnodes = loadBlocks( element, blocks=('TimeDerivative','OnCondition','OnEvent' )  )
    return al_core.Regime( name=name,
                          time_derivatives = subnodes["TimeDerivative"],
                          on_events = subnodes["OnEvent"],
                          on_conditions = subnodes["OnCondition"] )



def load_StateVariable(element):
    name = element.get("name")
    return al_core.StateVariable( name=name)


def load_TimeDerivative(element):
    variable = element.get("variable")
    expr = load_SingleInternalMathsBlock(element)
    return al_core.ODE( dependent_variable=variable, indep_variable='t', rhs=expr)
    
def load_Alias(element):
    name = element.get("name")
    rhs =  load_SingleInternalMathsBlock(element)
    return al_core.Alias( lhs=name,  rhs=rhs)


def load_OnCondition(element):
    subnodes = loadBlocks( element, blocks=('Trigger','StateAssignment','EventOut' )  )
    target_regime = element.get('target_regime',None)

    return al_core.OnCondition(  trigger = ExpectSingle( subnodes["Trigger"] ),
                                 state_assignments = subnodes[ "StateAssignment"],
                                 event_outputs = subnodes[ "EventOut" ],
                                 target_regime = target_regime)
                                

def load_OnEvent(element):
    subnodes = loadBlocks( element, blocks=('StateAssignment','EventOut' )  )
    target_regime = element.get('target_regime',None)
    src_port = element.get('port')
    return al_core.OnEvent(  src_port = src_port,
                             state_assignments = subnodes[ "StateAssignment"],
                             event_outputs = subnodes[ "EventOut" ],
                             target_regime = target_regime)
    assert False, 'Not implemented yet'

def load_Trigger(element):
    return load_SingleInternalMathsBlock ( element ) 


def load_StateAssignment(element):
    to = element.get('variable')
    expr = load_SingleInternalMathsBlock(element)
    return al_core.Assignment(to=to, expr=expr)

def load_EventOut(element):
    port = element.get('port')
    return al_core.OutputEvent(port=port)


def load_EventIn(element):
    return al_core.InputEvent( port = element.get('port') )


def load_SingleInternalMathsBlock(element, checkOnlyBlock=True):
    if checkOnlyBlock:
        elements = list(element.iterchildren(tag=etree.Element ) ) 
        if  len( elements ) != 1:
            print elements
            assert False, 'Unexpected tags found'

    assert len( element.findall(NS+"MathML") ) == 0
    assert len( element.findall(NS+"MathInline") ) == 1
    
    return ExpectSingle( element.findall(NS+'MathInline') ).text



tag_to_loader = {
    "ComponentClass" : load_ComponentClass,
    "Dynamics" : load_Dynamics,
    "Regime" : load_RegimeClass,
    "StateVariable" : load_StateVariable,
    "Parameter": load_Parameter,
    "EventPort": load_EventPort,
    "AnalogPort": load_AnalogPort,
    "Dynamics": load_Dynamics,
    "OnCondition": load_OnCondition,
    "OnEvent": load_OnEvent,
    "Alias": load_Alias,
    "TimeDerivative": load_TimeDerivative,
    "Trigger": load_Trigger,
    "StateAssignment": load_StateAssignment,
    "EventOut": load_EventOut,
    "EventIn": load_EventIn,
        }







# These blocks map directly in to classes:
def loadBlocks( element, blocks=None, checkForSpuriousBlocks=True ):
    """ 
    Creates a dictionary that maps class-types to instantiated objects
    """

    if blocks:
        for t in element.iterchildren(tag=etree.Element):
            assert t.tag[len(NS):] in blocks  or t.tag in blocks

    
    blocks = blocks if blocks is not None else tag_to_class_dict.keys()

    res = defaultdict( list )
    for blk in blocks:
        loader = tag_to_loader[blk]
        res[blk].extend( [ loader( e ) for e in element.findall(NS+blk) ] )

    return dict( res.iteritems() )



class XMLReader(object):


    @classmethod
    def _load_include(cls,include_element, basedir):
        filename = include_element.get('file')
        print "Loading 'Include:' %s" % filename

        # Load the new XML
        included_xml = cls._loadNestedXML( os.path.join(basedir,filename) )

        #Insert it into the parent node:
        include_element.getparent().extend( included_xml.getchildren() )
        include_element.getparent().remove( include_element )


    @classmethod
    def _loadNestedXML(cls,filename):
        """ Load the XML, including Include files """
        doc = etree.parse(filename)
        root = doc.getroot()
        assert root.nsmap[None] == nineml_namespace

        for include_element in root.getiterator(tag=NINEML+'Include'):
            cls._load_include(include_element=include_element, basedir=os.path.dirname(filename) )
        return root


    @classmethod
    def read_component(cls, filename, component_name=None):
        components = cls.read_components(filename)
        if component_name == None:
            return ExpectSingle(components)
        else:
            return FilterExpectSingle( components, lambda c:c.name==component_name )

    @classmethod
    def read_components(cls,filename):
        root = cls._loadNestedXML(filename)
        component_blocks = root.findall(NS + "ComponentClass")
        components = [ load_ComponentClass(comp) for comp in component_blocks ]
        return components
        


    @classmethod
    def read_model(cls, filename, model_name=None):
        models = cls.read_models(filename)
        if model_name == None:
            return ExpectSingle(models)
        else:
            return FilterExpectSingle( models, lambda m:m.name==model_name )





    @classmethod
    def read_models(cls, filename, ):
        root = cls._loadNestedXML(filename)

        # Read the Components
        component_blocks = root.findall(NS + "ComponentClass")
        components = [ load_ComponentClass(comp) for comp in component_blocks ]
        component_map = dict( [ (c.name,c) for c in components ] )

        # Read the Models:
        models = [cls._build_model(e,component_map) for e in root.findall(NS+'ModelClass') ]
        for sn in root.getchildren():
            print sn, sn.tag
        
    
        
        return models
        pass


    @classmethod
    def _build_model( cls, modelclass_element, component_map):

        # Create the model:
        from nineml.abstraction_layer import models
        model = models.Model( name= modelclass_element.get('name') )

        # Insert the subnodes:
        for sn in modelclass_element.iterchildren(tag=NS+'Subnode'):
            ns = sn.get('namespace')
            component = component_map[ sn.get('node')]
            model.insert_subnode( subnode = component, namespace=ns )

        # Insert the Connections:
        for sn in modelclass_element.iterchildren(tag=NS+'ConnectPorts'):
            model.connect_ports( src=sn.get('source'), sink=sn.get('sink') )
        
        return model
