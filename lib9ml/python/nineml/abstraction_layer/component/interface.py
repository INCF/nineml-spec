

from operator import and_
from expressions import *
from conditions import *
from ports import *
from ..xmlns import *

import nineml.utility


from itertools import chain









class Parameter(object):
    element_name = 'Parameter'

    def __init__(self, name, ):
        self.name = name

    def __str__(self):
        return "<Parameter: %s>"%(self.name)

    def AcceptVisitor(self, visitor, **kwargs):
        return visitor.VisitParameter(self, **kwargs)
