
import nineml.abstraction_layer as nineml
import os
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
        self.CODE = '!B_.spike_inputs_[%s-INF_SPIKE_RECEPTOR-1].get_list(lag).empty()' % eventport.name
        
        self.PENDING = self.CODE
        # TODO:
        # For now we are dropping the weight for this event until
        # we can fix 9ML so that we can do something sensible with it.
        self.PENDING_FINALIZE = 'B_.spike_inputs_[%s-INF_SPIKE_RECEPTOR-1].get_list(lag).pop_back();' % eventport.name


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
        self.odes = [NestODE(ode, self) for ode in regime.odes]

    def _compute_symbol(self):
        """ Assign the C symbol to be used in enum for this regime in nest C code """
        self.symbol = self.regime.name.upper().replace('-','_')

class NestTransition(object):

    def __init__(self, transition, parent_component, index):
        self.parent_component = parent_component
        self.transition = transition
        self.index = index
        self.to = parent_component.regime_map[transition.to.name]
        self.nodes = [NestAssignment(a, self) for a in transition.equations]
        event_ports = list(transition.event_ports)
        # we only allow a SpikeOutputPort here so assume it to be so
        if event_ports:
            self.nodes.append(NestOutputEventPort(event_ports[0],self))
            
        if isinstance(transition.condition, nineml.Condition):
            self.condition = NestCondition(transition.condition, self)
        elif isinstance(transition.condition, nineml.EventPort):
            self.condition = NestInputEventPort(transition.condition, self)
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
        for t in component.transitions:
            event_ports = list(t.event_port_nodes)
            if event_ports:
                if event_ports != [nineml.SpikeOutputEvent]:
                    print event_ports
                    raise ValueError, "Only one nineml.SpikeOutputEvent allowed as a transition node EventPort for neuron models."

        # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        # back sub binds
        self.binds = []

        # make NestParameters
        self.parameters = [NestParameter(symbol, default_value_dict[symbol], self) for symbol in component.parameters]

        # make NestStateVars
        self.state_vars = [NestStateVar(symbol, initial_value_dict[symbol], self) for symbol in component.state_variables]

        # make synapse types
        self.synapses = [NestSynapse(symbol) for symbol in synapse_ports] 

        # make regimes
        self.regimes = [NestRegime(r, self) for r in component.regimes]

        self.regime_map = {}
        for r in self.regimes:
            self.regime_map[r.regime.name] = r

        self.initial_regime = self.regime_map[initial_regime].symbol

        # NB: iteration order accross iterations is assumed in the template
        # so this must be true for the type of self.transitions
        # moreover, self.transitions must have a length, i.e. len(self.transitions)
        self.transitions = [NestTransition(t, self, i) for i,t in zip(itertools.count(), component.transitions)]

        # build transition map
        self.transition_map = {}
        for t in self.transitions:
            self.transition_map[t.transition.name] = t

        # setup transitions for the regimes
        for r in self.regimes:
            r.transitions = [self.transition_map[t.name] for t in r.regime.transitions]

        # set model to debug mode
        self.debug=True
                 



def restore_working_directory( func ):
    def wrapped_func(*args, **kwargs):
        oldcwd = os.getcwd()
        res = func(*args,**kwargs)
        os.chdir(oldcwd)
        return res
    return wrapped_func


class NestFileBuilder(object):

    def __init__(self, nest_classname, iaf_cond_exp_9ML, synapse_ports, initial_regime, initial_values, default_values):
        
        # Output Files:
        self.build_dir = "nest_model"
        self.output_h_file = Join(self.build_dir, "nest_9ml_neuron.h")
        self.output_cpp_file = Join(self.build_dir, "nest_9ml_neuron.cpp")

        self.nm = NestModel(nest_classname, iaf_cond_exp_9ML, synapse_ports, initial_regime, initial_values, default_values)
        self.buildCPPFiles()

    
    def buildCPPFiles(self):
        print "Building NEST CPP and H files... "

        # make the destination subdir
        if not os.path.exists(self.build_dir):
            os.mkdir(self.build_dir)

        f_h = file(self.output_h_file,'w')
        t_h = Template(file="nest_9ml_neuron_h_cheetah.tmpl", searchList={'model':self.nm})
        print >> f_h, str(t_h)
        f_h.close()

        f_cpp = file(self.output_cpp_file,'w')
        t_cpp = Template(file="nest_9ml_neuron_cpp_cheetah.tmpl", searchList=[{'model':self.nm}])
        print >> f_cpp, str(t_cpp)
        f_cpp.close()

    @restore_working_directory
    def compile_files(self):
        print 'Compiling for NEST...'

        os.chdir( self.build_dir)
        os.system("./bootstrap.sh")
        if not os.path.exists('build'):
            os.mkdir("build")
        os.chdir("build")

        os.system("../configure --with-nest=/opt/nest2/bin/nest-config")
        os.system("make -j2")
        os.system("make -j2")
        #"export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/src/nineml-svn-trunk/code_generation/nest2/nest_model/build/.libs"
        os.chdir('../..')
        #"cd ../.."

        #os.system("")
        
