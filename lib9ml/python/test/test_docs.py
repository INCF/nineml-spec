
import doctest
import unittest
import os

doc_root = "../doc"
files = ["abstraction_layer_tutorial.txt", "user_layer_tutorial.txt"] 

def suite():
    s = unittest.TestSuite()
    for f in files:
        s.addTests(doctest.DocFileSuite(os.path.join(doc_root,f)))
    return s


if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())


