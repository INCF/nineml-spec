
from nineml.utility import LocationMgr, Join, expect_single, filter_expect_single


from collections import defaultdict


import os




import nineml.abstraction_layer as al
#import nineml.abstraction_layer.component as core
from ..xmlns import etree,E,MATHML,nineml_namespace,NINEML


NS = "{CoModL}"








class XMLLoader1(object):

    def __init__(self, xmlroot, xmlNodeFilenameMap):
        
        self.components = []
        self.component_srcs = {}
        for comp_block in xmlroot.findall(NS + "ComponentClass"):
            component = self.load_ComponentClass( comp_block )
            src_filename = xmlNodeFilenameMap[comp_block]

            self.components.append(component)
            self.component_srcs[component] = xmlNodeFilenameMap[comp_block]



    def load_ConnectPorts(self, element ):
        return element.get('source'), element.get('sink')



    def load_Subnode(self, subnode):
        namespace = subnode.get('namespace')
        component = filter_expect_single( self.components, lambda c: c.name==subnode.get('node') )
        return namespace,component
    


    def load_ComponentClass(self,element):
        

        from nineml.abstraction_layer import ComponentClass
        name = element.get("name")
         

        subnodes = self.loadBlocks( element,blocks=('Parameter','AnalogPort','EventPort','Dynamics','Subnode','ConnectPorts' ) )

        component = ComponentClass(  name=name,
                                       parameters = subnodes["Parameter" ] ,
                                       analog_ports = subnodes["AnalogPort"] ,
                                       event_ports = subnodes["EventPort"],
                                       dynamics = expect_single( subnodes["Dynamics"] )
                                       )

        # Load namespaces:
        for namespace,componentclass in subnodes['Subnode']:
            component.insert_subnode(subnode = componentclass, namespace=namespace)
        
        for src,sink in subnodes['ConnectPorts']:
            component.connect_ports(src=src,sink=sink)

        return component

       
    def load_Parameter(self,element):
        name = element.get("name")
        return al.Parameter(name=name) 

    def load_AnalogPort(self,element):
        name = element.get("name")
        mode = element.get("mode")
        reduce_op = element.get("reduce_op",None)
        return al.AnalogPort( internal_symbol = name, mode = mode, op = reduce_op )

    def load_EventPort(self,element):
        name = element.get("name")
        mode = element.get("mode")
        return al.EventPort( internal_symbol = name, mode = mode )



    def load_Dynamics(self,element):
        subnodes = self.loadBlocks( element, blocks=('Regime','Alias','StateVariable' )  )
        return al.Dynamics(  regimes = subnodes["Regime"] ,
                                 aliases = subnodes["Alias"] ,
                                 state_variables = subnodes["StateVariable"] ,
                                 )


    def load_RegimeClass(self,element):
        name = element.get("name")
        subnodes = self.loadBlocks( element, blocks=('TimeDerivative','OnCondition','OnEvent' )  )
        return al.Regime( name=name,
                              time_derivatives = subnodes["TimeDerivative"],
                              on_events = subnodes["OnEvent"],
                              on_conditions = subnodes["OnCondition"] )



    def load_StateVariable(self,element):
        name = element.get("name")
        return al.StateVariable( name=name)


    def load_TimeDerivative(self,element):
        variable = element.get("variable")
        expr = self.load_SingleInternalMathsBlock(element)
        return al.ODE( dependent_variable=variable, indep_variable='t', rhs=expr)
        
    def load_Alias(self,element):
        name = element.get("name")
        rhs =  self.load_SingleInternalMathsBlock(element)
        return al.Alias( lhs=name,  rhs=rhs)


    def load_OnCondition(self,element):
        subnodes = self.loadBlocks( element, blocks=('Trigger','StateAssignment','EventOut' )  )
        target_regime = element.get('target_regime',None)

        return al.OnCondition(  trigger = expect_single( subnodes["Trigger"] ),
                                     state_assignments = subnodes[ "StateAssignment"],
                                     event_outputs = subnodes[ "EventOut" ],
                                     target_regime_name = target_regime)
                                    

    def load_OnEvent(self,element):
        subnodes = self.loadBlocks( element, blocks=('StateAssignment','EventOut' )  )
        target_regime = element.get('target_regime',None)
        src_port_name = element.get('port')
        return al.OnEvent(  src_port_name = src_port_name,
                                 state_assignments = subnodes[ "StateAssignment"],
                                 event_outputs = subnodes[ "EventOut" ],
                                 target_regime_name = target_regime)

    def load_Trigger(self,element):
        return self.load_SingleInternalMathsBlock ( element ) 


    def load_StateAssignment(self,element):
        to = element.get('variable')
        expr = self.load_SingleInternalMathsBlock(element)
        return al.Assignment(to=to, expr=expr)

    def load_EventOut(self,element):
        port = element.get('port')
        return al.OutputEvent(port=port)


    def load_EventIn(self,element):
        return al.InputEvent( port = element.get('port') )


    def load_SingleInternalMathsBlock(self,element, checkOnlyBlock=True):
        if checkOnlyBlock:
            elements = list(element.iterchildren(tag=etree.Element ) ) 
            if  len( elements ) != 1:
                print elements
                assert False, 'Unexpected tags found'

        assert len( element.findall(NS+"MathML") ) == 0
        assert len( element.findall(NS+"MathInline") ) == 1
        
        return expect_single( element.findall(NS+'MathInline') ).text










# These blocks map directly in to classes:
    def loadBlocks(self, element, blocks=None, checkForSpuriousBlocks=True ):
        """ 
        Creates a dictionary that maps class-types to instantiated objects
        """

        if blocks:
            for t in element.iterchildren(tag=etree.Element):
                assert t.tag[len(NS):] in blocks  or t.tag in blocks

        
        blocks = blocks if blocks is not None else tag_to_class_dict.keys()

        res = {}
        for blk in blocks:
            elements =list ( element.findall(NS+blk) ) 
            res[blk] = []
            if elements:
                loader = tag_to_loader[blk]
                res[blk].extend( [ loader(self, e ) for e in elements ] )

        return res







tag_to_loader = {
    "ComponentClass" : XMLLoader1.load_ComponentClass,
    "Dynamics" : XMLLoader1.load_Dynamics,
    "Regime" : XMLLoader1.load_RegimeClass,
    "StateVariable" : XMLLoader1.load_StateVariable,
    "Parameter": XMLLoader1.load_Parameter,
    "EventPort": XMLLoader1.load_EventPort,
    "AnalogPort": XMLLoader1.load_AnalogPort,
    "Dynamics": XMLLoader1.load_Dynamics,
    "OnCondition": XMLLoader1.load_OnCondition,
    "OnEvent": XMLLoader1.load_OnEvent,
    "Alias": XMLLoader1.load_Alias,
    "TimeDerivative": XMLLoader1.load_TimeDerivative,
    "Trigger": XMLLoader1.load_Trigger,
    "StateAssignment": XMLLoader1.load_StateAssignment,
    "EventOut": XMLLoader1.load_EventOut,
    "EventIn": XMLLoader1.load_EventIn,
    "Subnode": XMLLoader1.load_Subnode,
    "ConnectPorts": XMLLoader1.load_ConnectPorts,
        }











xmlNodeFilenameMap = {}



class XMLReader(object):


    @classmethod
    def _load_include(cls,include_element, basedir):
        filename = include_element.get('file')
        print "Loading 'Include:' %s" % filename

        # Load the new XML
        included_xml = cls._loadNestedXML( os.path.join(basedir,filename) )

        #Insert it into the parent node:
        indexOfNode = include_element.getparent().index(include_element)
        for i,newchild in enumerate(included_xml.getchildren()):
            include_element.getparent().insert(  i+indexOfNode,newchild)

        include_element.getparent().remove( include_element )


    @classmethod
    def _loadNestedXML(cls,filename):
        """ Load the XML, including Include files """
        doc = etree.parse(filename)

        # Store the source filenames of all the nodes:
        for node in doc.getroot().getiterator():
            xmlNodeFilenameMap[node] = filename

        
        root = doc.getroot()
        assert root.nsmap[None] == nineml_namespace
        
        # Recursively Load Include Nodes:
        for include_element in root.getiterator(tag=NINEML+'Include'):
            cls._load_include(include_element=include_element, basedir=os.path.dirname(filename) )

        return root


    @classmethod
    def read_component(cls, filename, component_name=None):
        root = cls._loadNestedXML(filename)
        loader1 = XMLLoader1( root, xmlNodeFilenameMap = xmlNodeFilenameMap  )
        if component_name == None:
            return filter_expect_single( loader1.components, lambda c: loader1.component_srcs[c] == filename)
        else:
            return filter_expect_single( loader1.components, lambda c:c.name==component_name )

    @classmethod
    def read_components(cls,filename):
        root = cls._loadNestedXML(filename)
        loader1 = XMLLoader1( root, xmlNodeFilenameMap = xmlNodeFilenameMap  )
        return loader1.components

        




