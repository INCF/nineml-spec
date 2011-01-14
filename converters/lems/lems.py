# -*- coding: utf-8 -*-
"""

Initial version of converter 9ML -> LEMS
Can be used to convert AL models in 9ml to LEMS to be executed with the LEMS interpreter

Based on scripts from Andrew Davison & Eilif Muller's libnineml

Author: Padraig Gleeson

"""

from lxml import etree
from lxml.builder import E
import os, sys


import nineml.user_layer as UL
import nineml.abstraction_layer as AL

LEMS_EL = "Lems"
DEFAULT_RUN="DefaultRun"
SIMULATION="Simulation"


class LEMS():

  componentTypes = {}
  networks = {}
  
  def __init__(self, sim_name, dur, dt):
    self.sim_name = sim_name
    self.dur = float(dur)
    self.dt = float(dt)

  def add_network(self, network):
    self.networks[network.id] = network

  
  def to_xml(self):
    def_run = E(DEFAULT_RUN, component=self.sim_name)
    simulation = E(SIMULATION, id=self.sim_name, length="%fms"%self.dur, step="%fms"%self.dt, target="net1")
    lems_xml = E(LEMS_EL, def_run)

    for comp_type in self.componentTypes.keys():
        lems_xml.append(self.componentTypes[comp_type].to_xml())

    for net in self.networks.keys():
        lems_xml.append(self.networks[net].to_xml())

    lems_xml.append(simulation)

    return lems_xml

  def read_9ml(self, components_9ml, model):
      print "Reading elements from 9ML..."

      for comp9 in components_9ml:
          print "  Adding component of type: %s"%comp9.name
          componentType = ComponentType(comp9.name)
          componentType.read_9ml(comp9)
          self.componentTypes[componentType.name] = componentType


      for group9 in model.groups.keys():
          print "  Adding group %s as a network in the LEMS object model"%group9
          network = Network(group9)
          self.networks[network.id] = network
          
          for pop9 in model.groups[group9].populations.values():
              print "  Adding population %s as a population in the LEMS object model"%pop9
              population = Population(pop9.name, pop9.prototype.name, pop9.number)
              network.add_population(population)
              
      print "Added all elements from 9ML..."
    

  def write(self, file): 
    
    
    etree.ElementTree(self.to_xml()).write(file, encoding="UTF-8",
                                     pretty_print=True, xml_declaration=True)


class BaseLEMS():

    type = "ComponentType"
    name = "???"

    def to_xml(self):
        element = E(self.type,
                    name=self.name)
        return element


class BaseNeuroML2():

    type = "component"
    id = "???"

    def to_xml(self):
        element = E(self.type,
                    id=self.id)
        return element


class Network(BaseNeuroML2):

    type = "network"
    populations = {}

    def __init__(self, id):
        self.id = id

    def add_population(self, pop):
        self.populations[pop.id] = pop

    def to_xml(self):
        element = E(self.type,
                    id=self.id)

        for p in self.populations.keys():
            pop = self.populations[p]
            print "Appending to XML of network: %s"%pop
            element.append(pop.to_xml())
            
        return element

class Population(BaseNeuroML2):

    type = "population"

    def __init__(self, id, component, size):
        self.id = id
        self.component = component
        self.size = size

    def __str__(self):
        return "LEMS/NeuroML 2 Population: %s"%(self.id)

    def to_xml(self):
        element = E(self.type,id=self.id, component = self.component, size=str(self.size))
                    
        return element



class ComponentType(BaseLEMS):

    type = "ComponentType"
    parameters = []

    def __init__(self, name):
        self.name = name
        self.behavior = Behavior()

    def to_xml(self):
        element = E(self.type,name=self.name)

        for param in self.parameters:
            element.append(param.to_xml())

        element.append(self.behavior.to_xml())
        return element
    
    def read_9ml(self, component9):
        for param9 in component9.parameters:
            #param9 = component9.parameters[param_name9]
            print "Adding param: "+param9
            param = Parameter(param9)
            self.parameters.append(param)

        self.behavior.state_variables.append(StateVariable("ff"))

class Parameter(BaseLEMS):
    type = "Parameter"

    def __init__(self, name):
        self.name = name

    def to_xml(self):
        element = E(self.type,name=self.name, dimension="none")
        return element


class Behavior(BaseLEMS):
    type = "Behavior"
    state_variables = []

    def to_xml(self):
        element = E(self.type)
        for sv in self.state_variables:
            element.append(sv.to_xml())
        return element
    


class StateVariable(BaseLEMS):
    type = "StateVariable"

    def __init__(self, name):
        self.name = name
        
    def to_xml(self):
        element = E(self.type,name=self.name, dimension="none", exposure=self.name)
        return element

if __name__ == "__main__":

    file_name = "TestLEMS.xml"
    file_al_9ml = "TestLEMS_AL.9ml"
    file_ul_9ml = "TestLEMS_UL.9ml"
    
    print "Testing LEMS export..."


    sys.path.append("../../lib9ml/python/examples/AL")
    import izhikevich

    print "Loaded abstraction layer definition: %s" % izhikevich.c1.name

    components_9ml = []
    components_9ml.append(izhikevich.c1)

    catalog = "../../catalog/"
    network = UL.Group("Network1")
    model = UL.Model("Simple 9ML example model to run on LEMS")
    model.add_group(network)

    izh_burst_cell = UL.SpikingNodeType(izhikevich.c1.name, catalog + "neurons/IaF_tau.xml", UL.ParameterSet())

    unstructured = UL.Structure("Unstructured", catalog + "networkstructures/Unstructured.xml")

    cellPop = UL.Population("CellsA", 2, izh_burst_cell, UL.PositionList(structure=unstructured))

    network.add(cellPop)

    model.write(file_ul_9ml)


    





    lems_file = open(file_name, 'w')
    
    lems = LEMS("TestSim", 100, 0.01)

    lems.read_9ml(components_9ml, model)
    
    lems.write(lems_file)
    
    
    lems_file.close()
    
    print "Saved file to %s"%file_name
    
    
    