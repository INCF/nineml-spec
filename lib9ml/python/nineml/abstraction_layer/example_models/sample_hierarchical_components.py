
import nineml.abstraction_layer.models as models
import os

import nineml.abstraction_layer as nineml
from nineml.abstraction_layer import Regime, Transition, SendPort, RecvPort, On, SpikeInputEvent, SpikeOutputEvent, ReducePort

#from nineml import Regime, Transition


hh_na = models.ComponentNode(
		"Hodgkin-Huxley-Na", 
		#parameters= [ 'ena', 'gnabar',  'celsius'],
       		regimes=[ 
			   nineml.Regime(
			     "dm/dt = (minf(V)-m)/mtau(V)",
			     "dh/dt = (hinf(V)-h)/htau(V)",
			     name="hh_regime_na") 
			],

                bindings=[
			    "q10 := 3.0**((celsius - 6.3)/10.0)",  
			    "alpha_m := -0.1*(V+40.0)/(exp(-(V+40.0)/10.0) - 1.0)",  
			    "beta_m := 4.0*exp(-(V+65.0)/18.0)",
			    "mtau := 1/(q10*(alpha_m(V) + beta_m(V)))",
			    "minf := alpha_m(V)/(alpha_m(V) + beta_m(V))",
			    "alpha_h := 0.07*exp(-(V+65.0)/20.0)",               
			    "beta_h := 1.0/(exp(-(V+35)/10.0) + 1.0)",
			    "htau := 1.0/(q10*(alpha_h(V) + beta_h(V)))",
			    "hinf := alpha_h(V)/(alpha_h(V) + beta_h(V))",
			    "gna := gnabar*m*m*m*h",
                "i := gna * (ena-V)",
                        ],

	        analog_ports=[	
		   	  #SendPort("i=gna(m,h)*(ena-V)"),
		   	  SendPort("i"),
			  RecvPort("V") 
		      ],
		)


hh_k =  models.ComponentNode("Hodgkin-Huxley-K", 
		      #parameters=['ek', 'gkbar', 'celsius'],
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
		      analog_ports=[ 	SendPort("i=gk(n)*(ek - V)"),
				RecvPort("V") ]
		)


hh_pas = models.ComponentNode("Hodgkin-Huxley-Pas", 
		      #parameters=['el',  'gl'],
                      regimes=[
			   Regime(
    			   name="hh_regime_pas",
				)
			      ],

                      bindings=[],
		      analog_ports=[ 	SendPort("i=gl*(el - V)"),
				RecvPort("V") ]
		)


hh_base = models.ComponentNode("Hodgkin-Huxley-Base", 
		      #parameters=['C','theta'],
                      regimes=[
			  Regime(
			    "dV/dt = i/C",
			    name="hh_regime_base",
			    transitions=On("V > theta",do=[SpikeOutputEvent])
 				)
			      ],
                      bindings=[], 
		      analog_ports=[ ReducePort("i", op="+"), 
                              SendPort("V") ]
		)


