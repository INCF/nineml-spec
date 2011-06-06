
from Cheetah.Template import Template

def dump_reduced(component, filename):
    
    tmpl = """
    MODEL: 

    PORTS:
    ==========

    #for p in $component.analog_ports:
      AnalogPort: $p.name, $p.mode, $p.reduce_op
    #end for
    
    #for p in $component.event_ports:
      EventPort: $p.name, $p.mode, $p.reduce_op
    #end for


    PARAMETERS
    ===============
    #for up in $component.user_parameters:
      Parameter: $up
    #end for


    BINDINGS:
    ==================
    #for b in $component.aliases:
        Alias: $b
    #end for


    REGIMES:
    ========
    #for regime in $component.regimes:
    
    Regime: $regime
    ----------------
    
    #for eqn in $regime.equations:
       Eqn: $eqn
    #end for

        Transitions
        ~~~~~~~~~~~~~~
        #for $transi in $regime.transitions:
           Transition: $transi.name

             Condition: $transi.condition

           #for node in $transi.nodes:
             Node: $node
           #end for

        #end for


    #end for

    """


    return
    data = { 'component':component }
    f = open(filename,"w")
    s = Template(tmpl, data).respond()
    f.write(s)
    f.close()















def writeCSolverSimple( component, filename):
    assert isinstance(component, nineml.Component)


    templ1 = """
    
    int current_regime;

    // State Variables:
    #for statevar in $component.odes:
    float $statevar.dependent_variable = 0;
    #end for





    """


    for i in range(5): 
        print 

    cppTxt = Template( templ1, {'component':component } ).respond()

    print cppTxt
