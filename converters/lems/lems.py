# -*- coding: utf-8 -*-
"""

Initial version of converter 9ML -> LEMS
Can be used to convert AL models in 9ml to LEMS to be executed with the LEMS interpreter

Based on scripts from Andrew Davison & Eilif Muller's libnineml

Author: Padraig Gleeson

"""

from lxml import etree
from lxml.builder import E

LEMS_EL = "Lems"
DEFAULT_RUN="DefaultRun"
SIMULATION="Simulation"


class LEMS():
  
  def __init__(self, sim_name, dur, dt):
    self.sim_name = sim_name
    self.dur = float(dur)
    self.dt = float(dt)
  
  def to_xml(self):
    def_run = E(DEFAULT_RUN, component=self.sim_name)
    simulation = E(SIMULATION, id=self.sim_name, length="%fms"%self.dur, step="%fms"%self.dt, target="net1")
    return E(LEMS_EL, def_run, simulation)
    

  def write(self, file):
    
    
    etree.ElementTree(self.to_xml()).write(file, encoding="UTF-8",
                                     pretty_print=True, xml_declaration=True)
    
  
  
  
if __name__ == "__main__":

    file_name = "TestLEMS.xml"
    
    print "Testing LEMS export..."
    f = open(file_name, 'w')
    
    lems = LEMS("TestSim", 100, 0.01)
    
    lems.write(f)
    
    
    f.close()
    
    print "Saved file to %s"%file_name
    
    
    