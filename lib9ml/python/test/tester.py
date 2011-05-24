"""
A test running script

Options:

-h, --help -> display this usage
--exclude -> comma seperated list of test names to exlude.  Useful with 'all' tests

Examples:

python tester.py all
-> runs all test_*.py tests defined

python tester.py basic io edit
-> runs test_basic.py, test_io.py, test_edit.py

python tester.py --exclude docs,parse,API all
-> runs all tests except test_docs.py,test_parse.py,test_API.py

"""

usage = __doc__

import unittest

# colors

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARN = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'

    def disable(self):
        self.HEADER = ''
        self.OKBLUE = ''
        self.OKGREEN = ''
        self.WARNING = ''
        self.FAIL = ''
        self.ENDC = ''


def import_tests(path, names, prefix='test_',ext='.py', exclude=[]):
    """ Get modules with given prefix
    and 'names' is a list of suffixes or 'all'
    path is where to look"""

    import glob
    from os.path import split, splitext, join, exists
    import os
    import new
    import sys

    if names == 'all':

        wild = join(path,'%s*%s' % (prefix,ext))
        print "Finding all tests matching wildcard: %s" % wild
        files = glob.glob(wild)
        
    else:
        files = [join(path,'%s%s%s' % (prefix,name,ext)) for name in names]

    # remove excluded files
    for e in exclude:
        f = join(path,'%s%s%s' % (prefix,e,ext))
        try:
            files.remove(f)
        except ValueError:
            pass

    # make sure path is in the sys.path
    if path not in sys.path:
        sys.path.append(path)

    # import the modules as found
    modules = []
    for f in files:
        mod_name = split(splitext(f)[0])[1]
        if exists(f):
            modules.append(__import__(mod_name))
        else:
            print (bcolors.WARN + "Skipping %s:"+bcolors.ENDC+"\n\ttest module file %s not found.") % (mod_name,f)
    
    return modules

def suite(modules):

    suite = unittest.TestSuite()

    for module in modules:
        suite.addTest(module.suite())

    # add additional test cases here
    return suite





if __name__ == "__main__":

    import sys, getopt

    try:
        opts, args = getopt.getopt(sys.argv[1:], "h", ["--help","exclude="])
    except getopt.GetoptError, err:
        # print help information and exit:
        print str(err) # will print something like "option -a not recognized"
        print usage
        print sys.exit(2)

        
    names = []
    exclude = []

    for o, a in opts:
        if o in ("-h", "--help"):
            usage()
            sys.exit()
        elif o in ("--exclude"):
            exclude = a.split(',')
        else:
            assert False, "unhandled option"

 
    if len(args)==0:
        print "Please names of tests to run, or 'all'"
        usage()
        sys.exit(1)

    names = args

    if 'all' in names:
        if len(names) !=1:
            print bcolors.FAIL+\
                "Error: 'all' keyword cannot be accompanied by other test names."+\
                bcolors.ENDC

            print usage
            print sys.exit(1)
        names = 'all'

    modules = import_tests(sys.path[0],names,prefix='test_',ext='.py',exclude=exclude)
    
    # We found some tests right?
    if modules == []:
        print bcolors.FAIL +"No tests found."+bcolors.ENDC
        print usage
        sys.exit(1)

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=3)
    runner.run(suite(modules))
