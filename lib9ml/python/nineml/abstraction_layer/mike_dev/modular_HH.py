
"""
Script for generating a single-compartment Hodgkin-Huxley cell in NineML XML format.

Andrew Davison, 2010. 
Mike Hull May 2011
"""

from nineml.abstraction_layer import *
import nineml.abstraction_layer.models as models
import os












c1_hh_model = models.Model( name="HH-Model", 
                        subnodes = {
                            "hhBase":c1_base,
                            "hhNa":c1_na,
                            "hhK":c1_k,
                            "hhPas":c1_pas,
                                        } )
c1_hh_model.connect_ports( "hhBase.V","hhNa.V" )
c1_hh_model.connect_ports( "hhBase.V","hhK.V" )
c1_hh_model.connect_ports( "hhBase.V","hhPas.V" )

c1_hh_model.connect_ports( "hhPas.i","hhBase.i" )
c1_hh_model.connect_ports( "hhNa.i","hhBase.i" )
c1_hh_model.connect_ports( "hhK.i","hhBase.i" )


c1 = c1_hh_model


models.reduce_to_single_component(c1)


comps = models.ModelVisitorDF_ComponentCollector(c1)
for c in comps:
    print c

# write to file object f if defined
try:
    # This case is used in the test suite for examples.
    c1.write(f)
except NameError:

    base = "hh"
    c1.write(base+".xml")
    c2 = parse(base+".xml")
    assert c1==c2

    c1.to_dot(base+".dot")
    os.system("dot -Tpng %s -o %s" % (base+".dot",base+".png"))
