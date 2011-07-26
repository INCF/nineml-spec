#! /usr/bin/python
"""
Converts 9ML abstraction layer neuron files to NMODL.
  
Copyright Andrew P. Davison, 2010-2011; Michael Hull 2011 
# if you edit this file, add your name here
"""

from __future__ import with_statement
import os.path
from textwrap import dedent
#from jinja2 import Template 
import nineml.abstraction_layer as al
import os
from nineml.exceptions import NineMLRuntimeError

import itertools

from nineml.utility import filter_expect_single, expect_single

from Cheetah.Template import Template

FIRST_REGIME_FLAG = 1001




#\#include <nineml.h>

tmpl_contents = """

VERBATIM
extern double nineml_gsl_normal(double, double);
extern double nineml_gsl_uniform(double, double);
extern double nineml_gsl_binomial(double, int);
extern double nineml_gsl_exponential(double);
extern double nineml_gsl_poisson(double);
ENDVERBATIM

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

  first_round_fired = 0
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
    first_round_fired 
}

ASSIGNED {
  regime

  #for alias in $component.aliases:
  $alias.lhs
  #end for

  
}



BREAKPOINT {
  :SOLVE states METHOD derivimplicit
  SOLVE states METHOD cnexp

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
    WATCH ( $transition.trigger.rhs.replace('=','') )  4000
    :WATCH ( $transition.trigger.rhs.replace('=','') )  $transition.flag
    #end for 
    #end for


  } 
  

  : 
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
        $sa.lhs  =  $sa.neuron_rhs
        #end for 
        
        #for node in $on_event.event_outputs 
        $as_expr(node)
        #end for 
      }
    #end for 
    }
    #end for

  } 


  : First Round of event filtering:
  : Pick up events emitted by WATCH Block.
  : Re-validate them against Conditions and Regime:
  : Retransmit the correct Transition ID

  else if (flag == 4000) {
  if( first_round_fired == 1)
  {
  }

  #for $regime in $component.regimes:
  else if( regime == $regime.flag )
  {
    #for transition in $regime.on_conditions:
    if( $transition.trigger.rhs )
    {
      printf("\\nFirst Round Transition Filtering: Forwarding Event: $transition.flag @ t=%f",t)
      net_send(0, $transition.flag )
    }
    #end for
  }
  #end for

  : Make sure that we only enter this block once
  first_round_fired = 1

  }

  : Second Round


  #for regime in $component.regimes:
  #for transition in $regime.on_conditions:
  else if (flag == $transition.flag) {

    first_round_fired = 0
    printf("\\nt=%f In Regime $regime.name Event With Flag: %f", t, flag )
    printf("\\nt=%f Changing Regime from $regime.name to $transition.target_regime.name via $transition.flag", t )
    regime = $transition.target_regime.flag
    
    #for node in $transition.event_outputs
    :net_event(t)
    #end for 

    #for sa in transition.state_assignments 
    $sa.lhs  = $sa.neuron_rhs
    #end for

    }
  #end for 
  
  
  #end for 


}

$random_functions

"""





import nineml


neuron_random_func_defs = {
'normal': """FUNCTION nineml_random_normal(m,s) {
                VERBATIM
                _lnineml_random_normal = nineml_gsl_normal(_lm,_ls);
                ENDVERBATIM
            } """,

'uniform': """FUNCTION nineml_random_uniform(m,s) {
                VERBATIM
                _lnineml_random_uniform = nineml_gsl_uniform(_lm,_ls);
                ENDVERBATIM
            } """,

'binomial': """FUNCTION nineml_random_binomial(m,s) {
                VERBATIM
                _lnineml_random_binomial = nineml_gsl_binomial(_lm,_ls);
                ENDVERBATIM
            } """,

'poisson': """FUNCTION nineml_random_poisson(m) {
                VERBATIM
                _lnineml_random_poisson = nineml_gsl_poisson(_lm);
                ENDVERBATIM
            } """,
'exponential': """FUNCTION nineml_random_exponential(m) {
                VERBATIM
                _lnineml_random_exponential = nineml_gsl_exponential(_lm);
                ENDVERBATIM
            } """,
        }




class NeuronExprRandomBuilder(nineml.abstraction_layer.visitors.ActionVisitor):

    def __init__(self,):
        self.required_random_functions = set([])
    def get_modl_function_defs(self):
        return '\n'.join( neuron_random_func_defs[f] for f in self.required_random_functions )
    def has_random_functions(self):
        return len(self.required_random_functions) != 0


    def action_componentclass(self, component,  **kwargs):
        pass 
    def action_dynamics(self, dynamics, **kwargs):
        pass
    def action_regime(self,regime,  **kwargs):
        pass
    def action_statevariable(self, state_variable, **kwargs):
        pass
    def action_parameter(self, parameter, **kwargs):
        pass
    def action_analogport(self, port, **kwargs):
        pass
    def action_eventport(self, port, **kwargs):
        pass
    def action_outputevent(self, output_event, **kwargs):
        pass
    def action_oncondition(self, on_condition, **kwargs):
        pass
    def action_onevent(self, on_event, **kwargs):
        pass
        

    # Things that shouldn't have randomness:
    def action_alias(self, alias, **kwargs):
        if alias.rhs_atoms_in_namespace('random'):
            raise NineMLRuntimeError("Alias uses 'random' namespace")
        
    def action_timederivative(self,time_derivative, **kwargs):
        if time_derivative.rhs_atoms_in_namespace('random'):
            raise NineMLRuntimeError("Time Derivative uses 'random' namespace")
        
    def action_condition(self, condition, **kwargs):
        if condition.rhs_atoms_in_namespace('random'):
            raise NineMLRuntimeError("Condition uses 'random' namespace")
        

    def action_assignment(self, assignment, **kwargs):
        import nineml
        from nineml.abstraction_layer.component import MathUtil
        rand_map = {
                    'normal' : r'nineml_random_normal(\1,\2)',
                    'uniform' : r'nineml_random_uniform(\1,\2)',
                    'binomial' : r'nineml_random_binomial(\1,\2)',
                    'poisson' : r'nineml_random_poisson(\1)',
                    'exponential' : r'nineml_random_exponential(\1)',
                }


        expr = assignment.rhs
        for atom in assignment.rhs_atoms_in_namespace('random'):
            if not atom in rand_map:
                err = 'Neuron Simulator does not support: %s'%atom
                raise nineml.exceptions.NineMLRuntimeError(err)
            
            expr = MathUtil.rename_function(expr, '%s.%s'%('random',atom), rand_map[atom] )
            self.required_random_functions.add(atom)
        assignment.neuron_rhs = expr










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


    



ninemlnrn_libfile = 'libninemlnrn.so'

def get_ninemlnrnlibdir():
    file_loc = os.path.dirname(os.path.realpath( __file__ ))
    libdir = os.path.join( file_loc, '../lib/ninemlnrn' )
    libdir = os.path.normpath(libdir)
    return libdir


def get_ninemlnrnlibfile():
    libfilefull = '%s/%s'%(get_ninemlnrnlibdir(),ninemlnrn_libfile) 
    return libfilefull







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
   

    # Close

    component.backsub_all()
        
    # Resolve Randomness:
    rand = NeuronExprRandomBuilder()
    rand.visit(component)
    

    if rand.has_random_functions():
        libfilefull = get_ninemlnrnlibfile()
        libdir = get_ninemlnrnlibdir()
        print 'LibFileFull', libfilefull
        if not os.path.exists( libfilefull ):
            err = 'Random Library Proxy not built: %s.\nRun make in %s'%(libfilefull, libdir) 
            raise NineMLRuntimeError(err)
    
        LD_LIB_PATH = 'LD_LIBRARY_PATH'
        err = " *** WARNING CAN'T FIND %s in LD_LIBRARY_PATH\nTHERE MAY BE A PROBLEM USING RANDOM FUNCTIONS"%libdir
        if not LD_LIB_PATH in os.environ:
            raise NineMLRuntimeError(err)
        else:
            if not libdir in os.environ[LD_LIB_PATH]:
                raise NineMLRuntimeError(err)

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
        'random_functions': rand.get_modl_function_defs(),
        
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
        f.write( Template( tmpl_contents, context).respond() )
     
    

import subprocess
def call_nrnivmodl():

    import nineml
    if nineml.utility.Settings.enable_nmodl_gsl:
        flgs = ["-L%s"% get_ninemlnrnlibdir(), "-lninemlnrn -lgsl -lgslcblas"]
        subprocess.check_call(['nrnivmodl','-loadflags','"%s"'%(' '.join(flgs) ) ] )
    else:
        subprocess.check_call(['nrnivmodl',] )






if __name__ == "__main__":
    import sys
    write_nmodl(sys.argv[1])
    
