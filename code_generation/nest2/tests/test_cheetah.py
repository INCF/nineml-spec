""" This tests various features of Cheetah required to template nest2 model C++ & h files """


import unittest
import Cheetah
from Cheetah.Template import Template


class Model:
    pass

test_model = Model()

test_model.nest_classname = "test_model"
test_model.parameters = [{'symbol':'C_m',
                          'Ctype':'double',
                          'notes':'capacitance',
                          'unit':'pF',
                          'initial_value':250.0},
                         {'symbol':'I_e',
                          'Ctype':'double',
                          'notes':'current',
                          'unit':'nA',
                          'initial_value':0.0},
                         {'symbol':'tau_syn',
                          'Ctype':'double',
                          'notes':'time constant',
                          'unit':'ms',
                          'initial_value':2.0}]
                         
test_model.synapses = [{'symbol':'AMPA'},
                      {'symbol':'GABA_A'}]
        

loop_test_template = """#set $i=0
#for $x in $stuff
q[$i] = $i+$x
#set $i=$i+1
#end for $x"""

loop_test_result = """q[0] = 0+aa
q[1] = 1+bb
q[2] = 2+ab
q[3] = 3+ba
"""

class CheetahTestCase(unittest.TestCase):

    def test_preprocessor_directives(self):
        """ check rendering of C preprocesser directives """

        # include needs escaping with \\
        t = Template('\\#include "$header"', {'header':'mymodule.h'})
        assert str(t) == '#include "mymodule.h"'

        # the rest do not seem to
        t = Template('#ifdef $var', {'var':'MYVAR'})
        assert str(t) == '#ifdef MYVAR'

        t = Template('#ifndef $var', {'var':'MYVAR'})
        assert str(t) == '#ifndef MYVAR'

        t = Template('#define $var', {'var':'MYVAR'})
        assert str(t) == '#define MYVAR'

        t = Template('#endif $var', {'var':'MYVAR'})
        assert str(t) == '#endif MYVAR'


    def test_synapse_enum(self):
        """ check rendering a collections of variables provided in a Python list """
        t = Template("""#echo ', '.join([x['symbol'] for x in $model.synapses])""",{'model':test_model})
        assert str(t) == "AMPA, GABA_A"

    def test_len(self):
        """ check rendering len(list) """
        t = Template("""$len($model.synapses)""", {'model':test_model})
        assert str(t)==str(len(test_model.synapses))

    def test_len2(self):
        """ another check of rendering len(list) """
        t = Template("$len($x)", {'x':[1,2,3]})
        assert str(t)=="3"

    def test_raise(self):
        """ check raising errors """
        t = Template("#raise TypeError, 'expected ODE or Assignment class'")

        try:
            s = str(t)
        except TypeError as e:
            assert e.args[0] == 'expected ODE or Assignment class'

    def test_loop(self):
        """ check rending using loops """
        print ""
        t = Template(loop_test_template, {'stuff':['aa','bb','ab','ba']})
        #print str(t)
        #print loop_test_result
        assert str(t) == loop_test_result


    def test_doublecolon(self):
        """ check that double colon postfix doesn't mangle """
        t = Template("$x::", {'x':3})
        assert str(t)=="3::"


    def test_dynamic_contructs(self):
        """check conditional rending with newline slurping"""

        # test if, with slurping
        s = """#if $switch
foo#slurp
#else
bar#slurp
#end if"""
        
        t = Template(s)
        t.switch = True
        assert str(t) == "foo"
        t.switch = False
        assert str(t) == "bar"


    def test_forloop_whitespace(self):
        """check loop rendering with control of intermediate newline or whitespace"""

        l = ['foo','bar','baz']

        # no space between loop iterations
        s = """#for $x in $l
$x#slurp
#end for """

        t = Template(s)
        t.l = l
        assert str(t) == "foobarbaz"


        # one space between loop iterations
        s = """#for $x in $l
$x #slurp
#end for """

        t = Template(s)
        t.l = l
        assert str(t) == "foo bar baz "


        # one line between loop iterations
        s = """#for $x in $l
$x
#end for """

        t = Template(s)
        t.l = l
        assert str(t) == "foo\nbar\nbaz\n"


    def test_class_attr_lookup(self):
        """ check rendering of class attributes """
        class X:
            pass

        x = X()
        x.foo = 'FOO'
        x.bar = 'BAR'

        t = Template("$foo, $bar", x)
        assert str(t) == "FOO, BAR"

        t = Template("$Y.foo, $Y.bar", {'Y':x})
        assert str(t) == "FOO, BAR"


    def test_parameters_constructor(self):
        """ check strategy for rendering NEST parameters constructor (need commas for each parameter, but not after last) """

        ref_s="""
nest_nineml::test_model::Parameters_::Parameters_()
  : \\
    C_m(250.0), // pF
    I_e(0.0), // nA
    tau_syn(2.0) // ms
  {}
"""

        s = """
nest_nineml::$nest_classname::Parameters_::Parameters_()
  : \\
#set $com = ','
#for $p in $parameters
  #if $p==$parameters[-1]
    #set $com = ''
  #end if
    ${p.symbol}(${p.initial_value})$com // $p.unit
#end for
  {}
"""


        t = Template(s,test_model)

##         print ""
##         print "*****************"
##         print t
##         print "*****************"

##         print "*****************"
##         print ref_s
##         print "*****************"

        assert str(t) == ref_s

    def test_names_namespace(self):
        """check configuration of Cheetah shorthand for a long C++ namespace"""

        s = "#set $mynames='nest_nineml::%s_names' % ($class,)\n$mynames::V_m"
        t = Template(s,{'class':'MyClass'})
        assert str(t)=="nest_nineml::MyClass_names::V_m"

 



        


def suite():

    suite = unittest.makeSuite(CheetahTestCase,'test')
    return suite

if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
