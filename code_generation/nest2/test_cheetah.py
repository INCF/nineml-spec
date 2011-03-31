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
        



class CheetahTestCase(unittest.TestCase):

    def test_preprocessor_directives(self):
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
        t = Template("""#echo ', '.join([x['symbol'] for x in $model.synapses])""",{'model':test_model})
        assert str(t) == "AMPA, GABA_A"

    def test_len(self):
        t = Template("""$len($model.synapses)""", {'model':test_model})
        assert str(t)==str(len(test_model.synapses))

    def test_raise(self):

        t = Template("#raise TypeError, 'expected ODE or Assignment class'")

        try:
            s = str(t)
        except TypeError as e:
            assert e.args[0] == 'expected ODE or Assignment class'


    def test_dynamic_contructs(self):

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

        s = "#set $mynames='nest_nineml::%s_names' % ($class,)\n$mynames::V_m"
        t = Template(s,{'class':'MyClass'})
        assert str(t)=="nest_nineml::MyClass_names::V_m"

    def test_parameters_get(self):

        ref_s = """
void nest_nineml::$model.nest_classname::Parameters_::get(DictionaryDatum &d) const
{
  (*d)[names::C_m    ] = C_m;
  (*d)[names::I_e    ] = I_e;
  (*d)[names::tau_syn] = tau_syn;
  (*d)[names::V_th   ] = V_th;
  (*d)[names::V_reset] = V_reset;
  (*d)[names::t_ref  ] = t_ref;
}
"""
        s = """
void nest_nineml::$model.nest_classname::Parameters_::get(DictionaryDatum &d) const
{
#for $p in $model.parameters
  (*d)[names::C_m    ] = C_m;
#end for
  (*d)[names::I_e    ] = I_e;
  (*d)[names::tau_syn] = tau_syn;
  (*d)[names::V_th   ] = V_th;
  (*d)[names::V_reset] = V_reset;
  (*d)[names::t_ref  ] = t_ref;
}
"""

    



        


def suite():

    suite = unittest.makeSuite(CheetahTestCase,'test')
    return suite

if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
