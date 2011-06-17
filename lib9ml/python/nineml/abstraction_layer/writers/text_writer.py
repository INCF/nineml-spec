
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


    PARAMETERS:
    ===========
    #for up in $component.parameters:
      Parameter: $up
    #end for


    Aliases:
    ========
    #for b in $component.aliases:
        Alias: $b
    #end for


    State Variables:
    =================
    #for s in $component.state_variables:
        State Variable: $s
    #end for

    REGIMES:
    ========
    #for regime in $component.regimes:
    
    Regime: $regime
    ----------------
    
    #for eqn in $regime.time_derivatives:
       TimeDeriv: $eqn
    #end for

        OnEvents:
        ~~~~~~~~~~~~~~
        #for $on_event in $regime.on_events:
           Event: $on_event.src_port [To -> $on_event.to ]
           #for node in $on_event.nodes:
             Node: $node
           #end for
        #end for

        OnConditions:
        ~~~~~~~~~~~~~~
        #for $on_condition in $regime.on_conditions:
           Event: $on_condition.trigger [To -> $on_event.to ]
           #for node in $on_condition.nodes:
             Node: $node
           #end for
        #end for



    #end for



    """


    assert component.isflat()

    data = { 'component':component }
    f = open(filename,"w")
    s = Template(tmpl, data).respond()
    f.write(s)
    f.close()


class TextWriter(object):
    """TextWriter DocString"""
    pass
