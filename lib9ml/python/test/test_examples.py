
import unittest
import nineml.abstraction_layer as nineml




import os, tempfile


class ExamplesTestCase(unittest.TestCase):

    def test_roundtrip_examples(self):

        examples = ['izhikevich2.py','izhikevich.py', 'nmda.py',
                    'leaky_iaf2.py', 'leaky_iaf.py','hh2.py',
                    'step_current.py','spike_generator.py']
                    #'hh.py']

        for e in examples:
            f = tempfile.TemporaryFile()
            d = dict(f=f)
            execfile(os.path.join('../examples',e),d)
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
