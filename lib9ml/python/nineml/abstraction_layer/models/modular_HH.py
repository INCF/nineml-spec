
"""
Script for generating a single-compartment Hodgkin-Huxley cell in NineML XML format.

Andrew Davison, 2010. 
Mike Hull May 2011
"""

from nineml.abstraction_layer import *
import nineml.abstraction_layer.models as models
import os







c1_na = models.Component(
		"Hodgkin-Huxley-Na", 
		parameters= [ 'ena', 'gnabar',  'celsius'],
       		regimes=[ 
			   Regime(
			     "dm/dt = (minf(V)-m)/mtau(V)",
			     "dh/dt = (hinf(V)-h)/htau(V)",
			     name="hh_regime_na") 
			],

                bindings=[
			    "q10 := 3.0**((celsius - 6.3)/10.0)",  
			    "alpha_m(V) := -0.1*(V+40.0)/(exp(-(V+40.0)/10.0) - 1.0)",  
			    "beta_m(V) := 4.0*exp(-(V+65.0)/18.0)",
			    "mtau(V) := 1/(q10*(alpha_m(V) + beta_m(V)))",
			    "minf(V) := alpha_m(V)/(alpha_m(V) + beta_m(V))",
			    "alpha_h(V) := 0.07*exp(-(V+65.0)/20.0)",               
			    "beta_h(V) := 1.0/(exp(-(V+35)/10.0) + 1.0)",
			    "htau(V) := 1.0/(q10*(alpha_h(V) + beta_h(V)))",
			    "hinf(V) := alpha_h(V)/(alpha_h(V) + beta_h(V))",
			    "gna(m,h) := gnabar*m*m*m*h",
                        ],

	        ports=[	
		   	  SendPort("i=gna(m,h)*(ena-V)"),
			  RecvPort("V") 
		      ],
		)


c1_k =  models.Component("Hodgkin-Huxley-K", 
		      parameters=['ek', 'gkbar', 'celsius'],
                      regimes=[
				Regime( 
				  "dn/dt = (ninf(V)-n)/ntau(V)",
    				  name="hh_regime_k",
				      )
			      ],

                      bindings=[
			    "q10 := 3.0**((celsius - 6.3)/10.0)",  
			    "alpha_n(V) := -0.01*(V+55.0)/(exp(-(V+55.0)/10.0) - 1.0)",
			    "beta_n(V) := 0.125*exp(-(V+65.0)/80.0)",
			    "ntau(V) := 1.0/(q10*(alpha_n(V) + beta_n(V)))",
			    "ninf(V) := alpha_n(V)/(alpha_n(V) + beta_n(V))",
			    "gk(n) := gkbar*n*n*n*n"
				],
		      ports=[ 	SendPort("i=gk(n)*(ek - V)"),
				RecvPort("V") ]
		)


c1_pas = models.Component("Hodgkin-Huxley-Pas", 
		      parameters=['el',  'gl'],
                      regimes=[
			   Regime(
    			   name="hh_regime_pas",
				)
			      ],

                      bindings=[],
		      ports=[ 	SendPort("i=gl*(el - V)"),
				RecvPort("V") ]
		)


c1_base = models.Component("Hodgkin-Huxley-Base", 
		      parameters=['C','theta'],
                      regimes=[
			  Regime(
			    "dV/dt = i/C",
			    name="hh_regime_base",
			    transitions=On("V > theta",do=[SpikeOutputEvent])
 				)
			      ],
                      bindings=[], 
		      ports=[ ReducePort("i", op="+"), 
                              SendPort("V") ]
		)





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
