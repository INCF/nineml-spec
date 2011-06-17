
import nineml.abstraction_layer as nineml
import os, shutil
from Cheetah.Template import Template
from os.path import join as Join



# This is a scafolding object model ... ontop of 9ML
# Once we have 9ML component namespaceing worked out
# we should devise a system so that we can inject
# Nest specific derived classes for each of the
# 9ML AL base classes which implement the necessary
# Nest specific attributes.

class NestParameter(object):

    # TODO: units
    def __init__(self, symbol, default_value, parent_component, Ctype = "double", unit="[unspecified unit]", notes = ""):
        self.symbol = symbol
        self.Ctype = Ctype
        self.unit = unit
        self.notes = notes
        self.default_value = default_value
        self.parent_component = parent_component
        parent_component.name_map[symbol] = "P_.%s" % symbol

class NestStateVar(object):
    
    # TODO: units
    def __init__(self, symbol, initial_value, parent_component, Ctype = "nest::double_t", unit="[unspecified unit]", notes = ""):
        self.symbol = symbol
        self.Ctype = Ctype
        self.unit = unit
        self.notes = notes
        self.initial_value = initial_value
        self.parent_component = parent_component
        parent_component.name_map[symbol] = "S_.%s" % symbol

class NestODE(object):

    def __init__(self, ode, parent_regime):
        self.parent_regime = parent_regime
        parent_component = parent_regime.parent_component
        name_map = parent_component.name_map
        self.dependent_variable = ode.dependent_variable
        self.CODE = name_map[ode.dependent_variable] + " += (" + ode.rhs_name_transform(name_map) + ")*h;"

class NestCondition(object):

    def __init__(self, condition, parent_trans):
        self.parent_trans = parent_trans
        self.condition = condition
        parent_component = parent_trans.parent_component
        name_map = parent_component.name_map
        # code to evaluate the condition
        self.CODE = "transPendingTmp[%d] = (%s)" % (parent_trans.index, condition.rhs_name_transform(name_map))
        # where the state of the evaluated condition may be stored
        self.PENDING = "transPendingTmp[%d]" % parent_trans.index
        # conditions, unlike eventports, can happen only once
        self.PENDING_FINALIZE = "break;"

class NestInputEventPort(object):

    def __init__(self, eventport, parent_trans):
        self.parent_trans = parent_trans
        self.eventport = eventport
        parent_component = parent_trans.parent_component
        name_map = parent_component.name_map
        # Code to check if a spike arrived at this port
        # should check if the list ring buffer at this lag
        # is empty or not
        #self.CODE = '!B_.spike_inputs_[%s-INF_SPIKE_RECEPTOR-1].get_list(lag).empty()' % eventport.name MH
        self.CODE = '!B_.spike_inputs_[%s-INF_SPIKE_RECEPTOR-1].get_list(lag).empty()' % eventport
        
        self.PENDING = self.CODE
        # TODO:
        # For now we are dropping the weight for this event until
        # we can fix 9ML so that we can do something sensible with it.
        #self.PENDING_FINALIZE = 'B_.spike_inputs_[%s-INF_SPIKE_RECEPTOR-1].get_list(lag).pop_back();' % eventport.name MH
        self.PENDING_FINALIZE = 'B_.spike_inputs_[%s-INF_SPIKE_RECEPTOR-1].get_list(lag).pop_back();' % eventport


class NestOutputEventPort(object):

    def __init__(self, event_port, parent_trans):
        self.parent_trans = parent_trans
        self.event_port = event_port
        self.CODE = "set_spiketime(nest::Time::step(origin.get_steps()+lag+1));nest::SpikeEvent se;network()->send(*this, se, lag);"


class NestAssignment(object):

    def __init__(self, assignment, parent_trans):
        self.parent_trans = parent_trans
        self.assginment = assignment
        parent_component = parent_trans.parent_component
        name_map = parent_component.name_map
        self.CODE = name_map[assignment.lhs]+" = "+assignment.rhs_name_transform(name_map)+";"


class NestRegime(object):

    def __init__(self, regime, parent_component):
        self.parent_component = parent_component
        self.regime = regime
        self._compute_symbol()
        self.odes = [NestODE(ode, self) for ode in regime.time_derivatives]

    def _compute_symbol(self):
        """ Assign the C symbol to be used in enum for this regime in nest C code """
        self.symbol = self.regime.name.upper().replace('-','_')

class NestTransition(object):

    def __init__(self, transition, parent_component, index):
        self.parent_component = parent_component
        self.transition = transition
        self.index = index
        self.to = parent_component.regime_map[transition.to]
        #self.nodes = [NestAssignment(a, self) for a in transition.equations] MH
        self.nodes = [NestAssignment(a, self) for a in transition.state_assignments]
        #event_ports = list(transition.event_ports) MH
        event_ports = list(transition.event_outputs)
        # we only allow a SpikeOutputPort here so assume it to be so
        if event_ports:
            self.nodes.append(NestOutputEventPort(event_ports[0],self))
            
        #MH:
        #if isinstance(transition.condition, nineml.Condition):
        #    self.condition = NestCondition(transition.trigger, self)
        #elif isinstance(transition.condition, nineml.EventPort):
        #    self.condition = NestInputEventPort(transition.condition, self)
        #else:
        #    raise ValueError, "Condition was neither a nineml.Condition, nor a nineml.EventPort"

        if isinstance(transition, nineml.OnCondition):
            self.condition = NestCondition(transition.trigger, self)
        elif isinstance(transition, nineml.OnEvent):
            self.condition = NestInputEventPort(transition.src_port_name, self)
        else:
            raise ValueError, "Condition was neither a nineml.Condition, nor a nineml.EventPort"

class NestSynapse(object):
    def __init__(self, symbol, notes = ""):
        self.symbol = symbol

class NestModel(object):

    def __init__(self, nest_classname, component, synapse_ports, initial_regime, initial_value_dict, default_value_dict):
        import itertools

        self.nest_classname = nest_classname
        self.component_9ml = component
        self.name_map = {}
        self.header_name = "nest_9ml_neuron.h"

        attr_copy_list = ['short_description', 'long_description']
        for attr in attr_copy_list:
            self.__setattr__(attr, component.__getattribute__(attr))

        # TODO: we should find a 9ML standard way of checking
        # the things a model should satisfy for it to be valid.
        # i.e. A neuron model should satisfy a few things:

        # vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

        # transitions can define at most 1 EventPort node which should be a SpikeOutputPort

        # Commmented out by MH:
        if False:
            for t in component.transitions:
                event_ports = list(t.event_port_nodes)
                if event_ports:
                    if len( event_ports) != 1:
                        raise ValueError, "Only one nineml.SpikeOutputEvent allowed as a transition node EventPort for neuron models."
                    
                    #if event_ports != [nineml.SpikeOutputEvent]:
                    #    print event_ports
                    #    raise ValueError, "Only one nineml.SpikeOutputEvent allowed as a transition node EventPort for neuron models."

            # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        # back sub binds
        self.binds = []

        # make NestParameters
        self.parameters = [NestParameter(param.name, default_value_dict[param.name], self) for param in component.parameters]

        # make NestStateVars
        self.state_vars = [NestStateVar(sv.name, initial_value_dict[sv.name], self) for sv in component.state_variables]

        # make synapse types
        self.synapses = [NestSynapse(symbol) for symbol in synapse_ports] 

        # make regimes
        self.regimes = [NestRegime(r, self) for r in component.regimes]

        self.regime_map = {}
        for r in self.regimes:
            self.regime_map[r.regime.name] = r
            
        print self.regime_map.keys()
        self.initial_regime = self.regime_map[initial_regime].symbol

        # NB: iteration order accross iterations is assumed in the template
        # so this must be true for the type of self.transitions
        # moreover, self.transitions must have a length, i.e. len(self.transitions)
        self.transitions = [NestTransition(t, self, i) for i,t in zip(itertools.count(), component.transitions)]

        # build transition map
        self.transition_map = {}
        for t in self.transitions:
            self.transition_map[t.transition] = t

        # setup transitions for the regimes
        for r in self.regimes:
            r.transitions = [self.transition_map[t] for t in r.regime.transitions]

        # set model to debug mode
        self.debug=True
                 



def restore_working_directory( func ):
    def wrapped_func(*args, **kwargs):
        oldcwd = os.getcwd()
        res = func(*args,**kwargs)
        os.chdir(oldcwd)
        return res
    return wrapped_func

def ensure_directory_exists( location ):
    if not os.path.exists(location):
        os.mkdir(location)


class NestFileBuilder(object):
    
    def __init__(self, nest_classname, component, synapse_ports, initial_regime, initial_values, default_values, hack_fixed_values={}):
        
        # The template files are in the same directory as this file, 
        # but we could call it from anywhere, so lets set up the locations:
        self.src_dir = os.path.dirname(__file__)
        self.src_tmpl_h   = os.path.join(self.src_dir, "nest_9ml_neuron_h_cheetah.tmpl")
        self.src_tmpl_cpp = os.path.join(self.src_dir, "nest_9ml_neuron_cpp_cheetah.tmpl")

        self.mymodule_tmpl_cpp = os.path.join(self.src_dir, "mymodule_cpp_cheetah.tmpl")


        self.src_bootstrap = os.path.join(self.src_dir, "nest_model/bootstrap.sh")
        self.src_configure_ac = os.path.join(self.src_dir, "nest_model/configure.ac")
        self.src_makefile_am = os.path.join(self.src_dir, "nest_model/Makefile.am")


        # Output Files:
        self.build_dir = "nest_model"
        self.output_h_file   = Join(self.build_dir, "nest_9ml_neuron.h")
        self.output_cpp_file = Join(self.build_dir, "nest_9ml_neuron.cpp")
        self.output_mymodule_cpp_file = Join(self.build_dir, "mymodule.cpp")

        self.nm = NestModel(nest_classname, component, synapse_ports, initial_regime, initial_values, default_values)
        self.buildCPPFiles()

    
    @restore_working_directory
    def buildCPPFiles(self):
        print "Building NEST CPP and H files... "
        os.chdir(self.src_dir)
        print os.getcwd()

        # make the destination subdir
        ensure_directory_exists(self.build_dir)

        # Write the .h file:
        with open( self.output_h_file,'w') as f_h:
            f_h.write( Template(file=self.src_tmpl_h, searchList= {'model':self.nm}).respond() )

        #Write the .cpp file
        with open( self.output_cpp_file,'w') as f_h:
            f_h.write( Template(file=self.src_tmpl_cpp, searchList= {'model':self.nm}).respond() )
        
        #Write the mymodule.cpp file
        with open( self.output_mymodule_cpp_file,'w') as f_h:
            f_h.write( Template(file=self.mymodule_tmpl_cpp, searchList= {'model':self.nm}).respond() )



    @restore_working_directory
    def compile_files(self):
        print 'Compiling for NEST...'
        os.chdir(self.src_dir)
        os.chdir(self.build_dir)
        
        #Copy some files accross:
        #shutil.copy(self.src_bootstrap, 'bootstrap.sh')
        #shutil.copy(self.src_configure_ac, 'configure.ac')
        #shutil.copy(self.src_makefile_am, 'Makefile.am')


        os.system("./bootstrap.sh")
        ensure_directory_exists('build')
        os.chdir("build")

        nest_config = os.popen("which nest-config").read()
        if nest_config=='':
            raise RuntimeError("Please put 'nest-config' in your path.")
            
            
        e = os.system("../configure --with-nest=%s" % nest_config)
        if e!=0:
            raise RuntimeError("NEST MyModule configure failed.")
        os.system("make -j2")
        os.system("make -j2")
        #"export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/src/nineml-svn-trunk/code_generation/nest2/nest_model/build/.libs"
        #sys.environ['LD_LIBRARY_PATH']=sys.environ['LD_LIBRARY_PATH']+['$HOME/src/nineml-svn-trunk/code_generation/nest2/nest_model/build/.libs']
        os.chdir('../..')
        #"cd ../.."

        #os.system("")
        
