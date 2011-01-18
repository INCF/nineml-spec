# -*- coding: utf-8 -*-
"""

Initial version of converter 9ML -> LEMS
Can be used to convert AL models in 9ml to LEMS to be executed with the LEMS interpreter

Based on scripts from Andrew Davison & Eilif Muller's libnineml

Author: Padraig Gleeson

"""


from lems import *

import nineml.user_layer as UL




if __name__ == "__main__":

    al_ref = "gLIFid1"

    print "Running test on lems.py with AL created with %s.py"%al_ref

    params = UL.ParameterSet(Isyn=(15, ""))

    instance_name = "IaF1"

    test_9ml_al("TestIaF", al_ref, instance_name, params)