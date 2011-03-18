

import neuron
h = neuron.h
import pylab
import numpy


def configure():
    cvode = h.CVode()
    cvode.active(1)
    cvode.condition_order(2)

def run(duration):
    h.finitialize()
    while h.t < duration:
        h.fadvance()

class _Initializer(object):
    """
    Manage initialization of NEURON cells. Rather than create an
    `FInializeHandler` instance for each cell that needs to initialize itself,
    we create a single instance, and use an instance of this class to maintain
    a list of cells that need to be initialized.
    
    Public methods:
        register()
    """
    
    def __init__(self):
        """
        Create an `FinitializeHandler` object in Hoc, which will call the
        `_initialize()` method when NEURON is initialized.
        """
        h('objref initializer')
        h.initializer = self
        self.fih = h.FInitializeHandler(1, "initializer._initialize()")
        self.cell_list = []
    
    def register(self, cell):
        """
        Add to the list of cells to be initialized. Cell objects must have an
        `initialize()` method.
        """
        assert hasattr(cell, "initialize")
        self.cell_list.append(cell)
    
    def _initialize(self):
        """Call `initialize()` for all registered cell objects."""
        for cell in self.cell_list:
            cell.initialize()
initializer = _Initializer()
del _Initializer # make sure only one instance exists


class TestCase(object):
    
    def __init__(self, name, parameters, initial_values, expected_spike_times):
        self.name = name
        self.section = h.Section()
        self.cell = getattr(h, name)(0.5, sec=self.section)
        for param, value in parameters.items():
            setattr(self.cell, param, value)
        self.initial_values = initial_values
        initializer.register(self)
        self.setup_recording()
        self.expected_spike_times = numpy.array(expected_spike_times)

    def initialize(self):
        #print "Initialising %s (time = %g)" % (self.name, h.t)
        for variable, value in self.initial_values.items():
            setattr(self.cell, variable, value)

    def setup_recording(self):
        self.Vm = h.Vector()
        self.times = h.Vector()
        self.Vm.record(self.cell._ref_V)
        self.times.record(h._ref_t)
        
        self.spike_times = h.Vector()
        self.source = h.NetCon(self.cell, None)
        self.source.record(self.spike_times)

    def plot(self, filename):
        pylab.plot(self.times, self.Vm, 'bo-')
        pylab.savefig(filename)
    
    def calculate_errors(self):
        spike_times = numpy.array(self.spike_times)
        if spike_times.shape != self.expected_spike_times.shape:
            errors = "Different spike counts: actual: %d, expected %d" % (spike_times.size, self.expected_spike_times.size)
        else:
            errors = (spike_times - self.expected_spike_times)/self.expected_spike_times
        return errors
    
    @property
    def success(self):
        errors = self.calculate_errors()
        if isinstance(errors, basestring): # error message
            success = False
        else:
            success = (errors < 0.001).all()
        return success
        
    


