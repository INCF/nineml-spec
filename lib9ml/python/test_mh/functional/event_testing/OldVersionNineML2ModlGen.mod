TITLE Spiking node generated from the 9ML file [Unknown-Filename] using 9ml2nmodl.py version 0.1preevision$)


NEURON {
  POINT_PROCESS Comp1
  RANGE regime 
  RANGE cc1_cc_I 
  RANGE cc1_cc_tchange 
  RANGE cc1_evs_tchange 
  RANGE nrn_V
  RANGE cc1_cc_dur
  RANGE cc1_cc_i
  RANGE cc1_evs_cyclelength
  RANGE nrn_Cm
  RANGE nrn_gL
  RANGE nrn_E 
  RANGE cc1_cc_I 
  RANGE cc1_cc_tchange 
  RANGE cc1_evs_tchange 
  RANGE nrn_V

}

CONSTANT {
  SPIKE = 0
  INIT = 1
  REGIME4 = 1001
  CC1_CC_INPUTEVENT = 0
}



INITIAL {
  cc1_cc_I = 0
  cc1_cc_tchange = 0
  cc1_evs_tchange = 0
  nrn_V = 0

  regime = REGIME4
  net_send(0, INIT)
}

PARAMETER {
  cc1_cc_dur = 1
  cc1_cc_i = 1
  cc1_evs_cyclelength = 1
  nrn_Cm = 1
  nrn_gL = 1
  nrn_E = 1
}

STATE { 
    cc1_cc_I 
    cc1_cc_tchange 
    cc1_evs_tchange 
    nrn_V


}

ASSIGNED {
  regime
    
}

BREAKPOINT {
  SOLVE states METHOD derivimplicit

}

DERIVATIVE states {
    cc1_cc_I' = deriv_cc1_cc_I(cc1_cc_I)
    cc1_cc_tchange' = deriv_cc1_cc_tchange(cc1_cc_tchange)
    cc1_evs_tchange' = deriv_cc1_evs_tchange(cc1_evs_tchange)
    nrn_V' = deriv_nrn_V(cc1_cc_I, nrn_V)
}


FUNCTION deriv_cc1_cc_I(cc1_cc_I) {
  if (regime==REGIME4) {
    deriv_cc1_cc_I = 0.0
  }
}
FUNCTION deriv_cc1_cc_tchange(cc1_cc_tchange) {
  if (regime==REGIME4) {
    deriv_cc1_cc_tchange = 0.0
  }
}
FUNCTION deriv_cc1_evs_tchange(cc1_evs_tchange) {
  if (regime==REGIME4) {
    deriv_cc1_evs_tchange = 0.0
  }
}
FUNCTION deriv_nrn_V(cc1_cc_I, nrn_V) {
  if (regime==REGIME4) {
    deriv_nrn_V = (0+cc1_cc_I + (nrn_E-nrn_V)*nrn_gL )/nrn_Cm
  }
}



NET_RECEIVE(w, channel) {

  


  INITIAL {
    : stop channel being set to 0 by default
  }
  if (flag == SPIKE) {
    printf("Received spike with weight %f on channel %f at %f\n", w, channel, t)
    if (regime == REGIME4) {
      if (channel == CC1_CC_INPUTEVENT) {
      		printf("  Resolved to channel  \n" )
        
        w = w
        
        cc1_cc_I = cc1_cc_i
        cc1_cc_tchange = t
      }
    }
  } else if (flag == INIT) { 
    WATCH (t>cc1_cc_tchange + cc1_cc_dur) 1001 
    WATCH (t > cc1_evs_tchange + cc1_evs_cyclelength) 1001
  } else if (regime == REGIME4 && flag == REGIME4 ) {
    
    printf("\nt=%fChanging Regime from Regime4 to Regime4",t )
    regime = flag
    cc1_cc_I = 0
    cc1_cc_tchange = t
  } else if (regime == REGIME4 && flag == REGIME4 ) {
    
    printf("\nt=%fChanging Regime from Regime4 to Regime4",t )
    regime = flag
    net_event(t)
    cc1_evs_tchange = t
    cc1_cc_I = cc1_cc_i
    cc1_cc_tchange = t
  }
}