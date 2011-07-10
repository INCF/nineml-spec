#! /usr/bin/python
"""
Converts 9ML abstraction layer neuron files to NMODL.
  
Copyright Andrew P. Davison, 2010-2011 # if you edit this file, add your name here
"""

from __future__ import with_statement
import os.path
from textwrap import dedent
#from jinja2 import Template 
import nineml.abstraction_layer as al

import itertools

from nineml.utility import filter_expect_single, expect_single

from Cheetah.Template import Template


#template_file = os.path.join(os.path.dirname(__file__), "nmodl_template.jinja")
#with open(template_file) as f:
#    nmodl_template = Template(f.read())

FIRST_REGIME_FLAG = 1001





tmpl_contents = """

TITLE Spiking node generated from the 9ML file $input_filename using 9ml2nmodl.py version $version


NEURON {
  POINT_PROCESS $component.name
  RANGE regime
  
  :StateVariables:
  #for sv in $component.state_variables 
  RANGE $sv.name 
  #end for

  :Parameters
  #for p in $component.parameters 
  RANGE $p.name
  #end for

  :Aliases
  #for alias in $component.aliases  
  RANGE $alias.lhs 
  #end for 


}

CONSTANT {
  SPIKE = 0
  INIT = 1

  #for regime in $component.regimes 
  $regime.label = $regime.flag
  #end for

  #for i,channel in enumerate($channels):
  $channel = $i
  #end for


  #for transition in $component.transitions
  $transition.label = $transition.flag
  #end for
}



INITIAL {

  : Initialise State Variables:
  #for var in $component.state_variables:
  $var.name = 0
  #end for

  : Initialise Regime:
  regime = $initial_regime 

  : Initialise the NET_RECEIVE block:
  net_send(0, INIT)
}

PARAMETER {
  #for p in $component.parameters
  $p.name = 0
  #end for 
}

STATE { 
    #for var in $component.state_variables
    $var.name 
    #end for 
}

ASSIGNED {
  regime

  #for alias in $component.aliases:
  $alias.lhs
  #end for

  
}



BREAKPOINT {
  SOLVE states METHOD derivimplicit

  #for alias in $component.aliases
  $alias.lhs = $alias.rhs 
  #end for

}

DERIVATIVE states {
  #for var in $component.state_variables 
    $var.name' = deriv_${var.name}($deriv_func_args($component, $var.name))
  #end for
}

#for var in $component.state_variables 
FUNCTION deriv_${var.name}($deriv_func_args($component, $var.name)) {
  #for regime in $component.regimes
  if (regime== $regime.label ) {
    deriv_${var.name} = $ode_for($regime, $var).rhs 
  }
  #end for
}
#end for









NET_RECEIVE(w, channel) {

  #* printf("Received event with weight %f on channel %f at %f\\n", w, channel, t) *#


  INITIAL {
    : stop channel being set to 0 by default
  }



 if (flag == INIT) {

    #for regime in $component.regimes 
    #for transition in $regime.on_conditions 
    WATCH ( $transition.trigger.rhs.replace('=','') )  $transition.flag
    #end for 
    #end for


  } 
  
  
  else if (flag == SPIKE) {
    printf("Received spike with weight %f on channel %f at %f\\n", w, channel, t)

    #for regime in $component.regimes 
    if (regime == $regime.label) {
      #for on_event in $regime.on_events 

      #set channel = $get_on_event_channel($on_event,$component) 
      if (channel == $channel ) {
      		printf("  Resolved to channel $channel\\n" )
        
        #if $weight_variables 
        $get_weight_variable($channel, $weight_variables)  = w
        #end if

        #for sa in $on_event.state_assignments 
        $sa.lhs  =  $sa.rhs
        #end for 
        
        #for node in $on_event.event_outputs 
        $as_expr(node)
        #end for 
      }
    #end for 
    }
    #end for

  } 



  #for regime in $component.regimes:

  else if ( regime == $regime.label) {


  printf("\\nt=%f In Regime $regime.name Event With Flag: %d", t, flag )

  if(0){}

  #for transition in $regime.on_conditions:
  else if (flag == $transition.flag) {
    printf("\\nt=%f Changing Regime from $regime.name to $transition.target_regime.name via $transition.flag", t )
    regime = $transition.target_regime.flag
    
    #for node in $transition.event_outputs
    :net_event(t)
    #end for 

    #for sa in transition.state_assignments 
    $sa.lhs  = $sa.rhs
    #end for

    }
  #end for 
  
  }
  
  #end for 


}

"""

















def as_expr(node):
    if isinstance(node, al.StateAssignment):
        return node.as_expr()
    elif isinstance(node, al.EventPort):
        assert False
        return ""
    elif isinstance(node, al.OutputEvent):
        return ""
        return "OUTPUTEVENT"
    else:
        raise Exception("Don't know how to handle nodes of type %s" % type(node))
    

def deriv_func_args(component, variable):
    """ """
    args = set([variable])
    for r in component.regimes:
        for time_derivative in (eq for eq in r.time_derivatives if eq.dependent_variable == variable):
            for name in (name for name in time_derivative.rhs_names if name in [ sv.name for sv in component.state_variables ] ):
                args.add(name)
    return ','.join(args)
    return args


def ode_for(regime, variable):
    """
    Yields the TimeDerivative for the given variable in the regime
    """
    odes = [eq for eq in regime.time_derivatives if eq.dependent_variable == variable.name]
    if len(odes) == 0:
        odes.append(al.TimeDerivative(dependent_variable = variable, rhs = "0.0"))
    return expect_single( odes )

def get_on_event_channel(on_event, component):
    port = filter_expect_single( component.event_ports, lambda ep:ep.name==on_event.src_port_name)
    return port.channel_


def build_channels(component):
    channels = set()
    for event_port in [ep for ep in component.event_ports if ep.mode=='recv']:
        channel = event_port.name.upper()
        if channel not in channels:
             channels.add(channel)
        event_port.channel_ = channel
    return channels

def guess_weight_variable(component):
    receive_port_variables = set(p.name for p in component.analog_ports if p.mode == "recv")
    weight_variables = receive_port_variables.difference(component.state_variables)
    if len(weight_variables) == 0:
        # if we have spike input ports, should raise Exception here
        return "w"
    elif len(weight_variables) == 1:
        return list(weight_variables)[0]
    else:
        raise Exception("Can't yet handle multiple weight variables \n(%s)"%weight_variables)

def get_weight_variable(channel, weight_variables):
   
    for k in weight_variables.keys():
        if k.upper() in channel:
            return weight_variables[k]
    if len(weight_variables) == 1:
        return weight_variables.values()[0]
    assert False


    

def build_context(component, weight_variables, input_filename="[Unknown-Filename]", hierarchical_mode=False):
    """
    Return a dictionary that will be used to render the NMODL template.
    """
    for i, regime in enumerate(component.regimes):
        regime.flag = FIRST_REGIME_FLAG + i
        regime.label = regime.name.replace(' ', '').replace('-', '_').upper()
    if not weight_variables:
        weight_variables = {'': guess_weight_variable(component)}
        

    # THIS IS JUST NASTY. 
    # TODO - PROPERLY WITH A DICTIONARY.
    FIRST_TRANSITION_FLAG = 5000
    for i, transition in enumerate(component.transitions):
        n = FIRST_TRANSITION_FLAG + i
        transition.label = 'TRANSITION%d'%n 
        transition.flag = n


    assert component.is_flat()
    weights_as_states = False
    if hierarchical_mode:
        weights_as_states = True
   

    component.backsub_all()
        
    
    context = {
        "input_filename": input_filename,
        "version": al.__version__,
        "component": component,
        "channels": build_channels(component),
        "weight_variables": weight_variables,
        "get_weight_variable": get_weight_variable,
        "initial_regime": list(component.regimes)[0].label,
        "as_expr": as_expr,
        "deriv_func_args": deriv_func_args,
        "ode_for": ode_for,
        
        
        # Added by Mike:
        "weights_as_states": weights_as_states,
        'get_on_event_channel':get_on_event_channel,
        
    }
    return context


def write_nmodl(nineml_file, weight_variables={},hierarchical_mode=False): 

    from nineml.abstraction_layer.readers import XMLReader
    components = XMLReader.read_components(nineml_file)

    if len(components) == 0:
        print 'No components found in file!'

    elif len(components) == 1:
        output_filename = nineml_file.replace(".xml", ".mod").replace("-", "_")
        print "Converting %s to %s" % (nineml_file, output_filename)
        write_nmodldirect(component=component, mod_filename=output_filename, weight_variables=weight_variables, hierarchical_mode=hierarchical_mode)
    
    else:
        for c in components:
            output_filename = nineml_file.replace(".xml", "_%s.mod"%c.name).replace("-", "_")
            print "Converting %s to %s" % (nineml_file, output_filename)
            write_nmodldirect(component=component, mod_filename=output_filename, weight_variables=weight_variables, hierarchical_mode=hierarchical_mode)




def write_nmodldirect(component, mod_filename, weight_variables={},hierarchical_mode=False):
    
    print "Writing Mod-File %s" % mod_filename
    with open(mod_filename, "w") as f:
        context = build_context(component, weight_variables,hierarchical_mode=hierarchical_mode) 
        #f.write( nmodl_template.render(context))
        f.write( Template( tmpl_contents, context).respond() )
     
    
    #import sys
    #sys.exit(0)


if __name__ == "__main__":
    import sys
    write_nmodl(sys.argv[1])
    
