"""
Python module for reading and writing 9ML abstraction layer files in XML format.


Copyright Andrew P. Davison, Eilif B. Muller, 2010, Michael Hull, 2011  # if you edit this file, add your name here
"""

from xmlns import * 
from nineml import __version__

from component import *
import component

import visitors
import readers
import writers
import validators
import component_modifiers
import flattening

import testing_utils
