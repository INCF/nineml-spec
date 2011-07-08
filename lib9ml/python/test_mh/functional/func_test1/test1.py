

from nineml.abstraction_layer import * 

import unittest

class FuncTest_Flat1(unittest.TestCase):
    """ Create a Neuron with leak, and a current clamp, and check that the 
        Output is what we would expect.
    """

    def test1(self):

        cc = ComponentClass(
                name = 'SimpleCurrentClamp',
                parameters = ['i'],
                analog_ports = [ SendPort('I') ],
                aliases = 'I:=i',
                )

        nrn = ComponentClass(
                name = 'LeakyNeuron',
                parameters = ['Cm','gL','E'],
                regimes = [ Regime( 'dV/dt = (iInj + (E-V)*gL )/Cm'),],
                analog_ports = [ SendPort('V'), 
                                 ReducePort('iInj', reduce_op='+') ],
                )


        combined_comp = ComponentClass( name='Comp1',
                            subnodes = { 'nrn':nrn, 'cc1':cc},
                            portconnections = [ ('cc1.I','nrn.iInj') ] )
                            

        from nineml.abstraction_layer.testing_utils import std_pynn_simulation
        from nineml.abstraction_layer.testing_utils import RecordValue

        records = [
            RecordValue(what='cc1_I', tag='Current', label='Current Clamp 1'), 
            RecordValue(what='nrn_V', tag='Voltage', label='Neuron Voltage'), 
                ]

        res = std_pynn_simulation( test_component = combined_comp,
                parameters = {'cc1_i': 13.8, 'nrn_gL': 2, 'nrn_E':-70},
                             initial_values = {},
                             synapse_components = [],
                             records = records,
                             plot = False
                             )
        
        t, records = res
        
        self.assertAlmostEqual( records['cc1_I'][ t>10 ].mean(), 13.8)
        self.assertAlmostEqual( records['cc1_I'][ t>10 ].std(),  0.0)
                
        self.assertAlmostEqual( records['nrn_V'][ t>10 ].mean(), -63.1)
        self.assertAlmostEqual( records['nrn_V'][ t>10 ].std(),  0.0)


