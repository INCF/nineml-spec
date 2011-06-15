


from nineml.utility import LocationMgr, Join, ExpectSingle, FilterExpectSingle
from nineml.abstraction_layer.xmlns import *

import nineml.abstraction_layer as nineml
import pyNN.neuron.nineml as pyNNml

from collections import defaultdict


LocationMgr.StdAppendToPath()


 
from nineml.abstraction_layer.xml_reader import new_parse


sample_xml_dir = Join( LocationMgr.getCatalogDir(), "sample_xml_files")
component = new_parse(  Join( sample_xml_dir, 'PostTF_izhikevich.xml' ) )




from nineml.abstraction_layer import XMLWriter
XMLWriter.write(component, '/tmp/nineml_toxml1.xml' )
import sys
sys.exit(0)

celltype_cls = pyNNml.nineml_celltype_from_model(
                        name = "iaf_2coba",
                        nineml_model = component,
                        synapse_components = [
                            pyNNml.CoBaSyn( namespace='cobaExcit',  weight_connector='q' ),
                            pyNNml.CoBaSyn( namespace='cobaInhib',  weight_connector='q' ),
                                   ]
                        )


