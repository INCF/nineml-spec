
import unittest
import nineml.abstraction_layer as nineml




import os, tempfile


class ExamplesTestCase(unittest.TestCase):

    def test_roundtrip_examples(self):

        
        examples = ['izhikevich2.py','izhikevich.py', 'nmda.py',
                    'leaky_iaf.py','hh.py','coba_synapse.py',
                    'step_current.py','spike_generator.py',
                    'leaky_iaf_w_synapses.py','destexhe_ampa.py',
                    'hill_tononi_Ih.py','pfister_triplet_stdp.py',
                    'markram_synapse_dynamics.py',
                    'logistic_map.py','morris-lecar.py',
                    'iaf_sfa_relref.py']

        print ""
                    
        for e in examples:
            print e
            f = tempfile.TemporaryFile()
            d = dict(f=f)
            execfile(os.path.join(os.path.dirname(__file__), '../examples/AL',e),d)
            # create component 'c1' and writes to f, if defined

            # re-read the xml output of the component
            f.seek(0)
            c2 = nineml.parse(f)
            f.close()

            assert d['c1'] == c2, "Example %s had problems." % e




def suite():

    suite = unittest.makeSuite(ExamplesTestCase,'test')
    return suite

if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
