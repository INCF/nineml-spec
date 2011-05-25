





from testmodels import get_iaf_2coba, get_iaf_2coba_network, get_iaf_1coba
from nineml.nmodl.nineml2nmodl import write_nmodldirect
from nineml.abstraction_layer import models

testComponent = get_iaf_1coba()

testComponent = models.reduce_to_single_component(testComponent)

write_nmodldirect( component=testComponent,
                   output_filename='test1.mod',
                   weight_variables= {'cobaExcit':'cobaExcit_q'} 
                   #weight_variables= {'cobaExcit':'q', 'cobaInhib':'q'}
                   )  

#import sys
#sys.exit(0)



class NMODLGenerator(object):
    def __init__(self,component):
        ...component
        
    def getWeightVariables(self):
        
        
        
    def getModFileString(self):
        zd
        
        
        
".".join(lst)

        