


from nineml.utility import LocationMgr, Join, ExpectSingle, FilterExpectSingle
from nineml.abstraction_layer.xmlns import *

import nineml.abstraction_layer as al
import pyNN.neuron.nineml as pyNNml

from collections import defaultdict


LocationMgr.StdAppendToPath()


 
sample_xml_dir = Join( LocationMgr.getCatalogDir(), "sample_xml_files")
tenml_dir = Join( LocationMgr.getCatalogDir(), "sample_xml_files/10ml/")

print 'Loading First XML File'
print '----------------------'
component = al.XMLReader.read_component(  Join( sample_xml_dir, 'PostTF_izhikevich.xml' ) )
al.XMLWriter.write(component, '/tmp/nineml_toxml1.xml' )

print 'Loading Second XML File (IAF-Component'
print '--------------------------------------'
component = al.XMLReader.read_component(  Join( tenml_dir, 'comp_iaf.9ml' ) )
al.XMLWriter.write(component, '/tmp/nineml_toxml2.xml' )

print 'Loading Third XML File (COBA-Component)'
print '---------------------------------------'
component = al.XMLReader.read_component(  Join( tenml_dir, 'comp_coba.9ml' ) )
al.XMLWriter.write(component, '/tmp/nineml_toxml3.xml' )

print 'Loading Forth XML File (iaf-2coba-Model)'
print '----------------------------------------'
component = al.XMLReader.read_component(  Join( tenml_dir, 'iaf_2coba.10ml' ), component_name='iaf' )
al.XMLWriter.write(component, '/tmp/nineml_toxml4.xml', )
model = al.XMLReader.read_model(  Join( tenml_dir, 'iaf_2coba.10ml' ) )


from nineml.abstraction_layer.models import reduce_to_single_component
flatcomponent = reduce_to_single_component(model, componentname='iaf_2coba')
model = al.XMLWriter.write(flatcomponent, '/tmp/nineml_out_iaf_2coba.9ml' ) 



#import sys
#sys.exit(0)


print 'Attempting to simulate From Model:'
print '----------------------------------'
celltype_cls = pyNNml.nineml_celltype_from_model(
                        name = "iaf_2coba",
                        nineml_model = model,
                        synapse_components = [
                            pyNNml.CoBaSyn( namespace='cobaExcit',  weight_connector='q' ),
                            pyNNml.CoBaSyn( namespace='cobaInhib',  weight_connector='q' ),
                                   ]
                        )


