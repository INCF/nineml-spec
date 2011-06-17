"""
Python module for reading 9ML abstraction layer files in XML format.

Copyright Andrew P. Davison, Eilif B. Muller, 2010, Michael Hull, 2011  # if you edit this file, add your name here
"""

from dynamics import * 
from interface import *

from expressions import * 
from ports import * 
from cond_parse import * 
from expr_parse import * 
from namespaceaddress import * 
from ..xmlns import * 

from nineml import __version__
from core_component import *

import math_namespace 

from exceptions import * 

from componentqueryer import ComponentQueryer





# Wrapper for writing XML:
def parse(filename):
    from nineml.abstraction_layer.readers import XMLReader
    return XMLReader.read_component(filename)
