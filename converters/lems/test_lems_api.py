# -*- coding: utf-8 -*-
"""

Initial version of converter 9ML -> LEMS
Can be used to convert AL models in 9ml to LEMS to be executed with the LEMS interpreter

Based on scripts from Andrew Davison & Eilif Muller's libnineml

Author: Padraig Gleeson

"""

from lems import *



if __name__ == "__main__":

    print "Building simulation using lems.py"
    
    ref = "MyTestLEMSApi"

    comp = Component("adExIaFCell", "burster", C="281pF", gL="30nS", \
                     EL="-70.6mV", reset="-47.2mV", VT = "-50.4mV", \
                     thresh = "-40.4mV", delT="2mV", tauw="40ms",  \
                     a ="4nS",   b = "0.08nA", Iamp="0.8nA",  \
                     Idel="0ms",   Idur="2000ms")

    net = Network("Network1")

    pop = Population("pop1", comp.id, 1)

    net.add_population(pop)


    lems = LEMS()
    lems.add_component(comp)
    lems.add_network(net)

    lems.gen_sim_with_def_plots(net.id, 250, 0.01)

    lems_file = ref+".xml"
    lems.write(lems_file)
    