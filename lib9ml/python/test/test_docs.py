from __future__ import with_statement
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

def write_scripts():
    for filename in files:
        with open(os.path.join(doc_root,filename)) as f:
            script = doctest.script_from_examples(f.read())
            with open(filename.replace(".txt", ".py"), 'w') as f_out:
                f_out.write(script)

if __name__ == "__main__":
    write_scripts()
    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())


