#encoding: utf-8

import unittest
import os, tempfile
from nineml.user_layer import parse

import nineml.user_layer as nineml
from copy import deepcopy

def simple_example():

    catalog = "http://svn.incf.org/svn/nineml/catalog/"

    tau_distr = nineml.RandomDistribution(
        "normal(20.0,3.0)",
        catalog + "randomdistributions/normal_distribution.xml",
        {'standardDeviation': (3.0, "dimensionless"),
         'mean': (20.0, "dimensionless")})

    reset_distr = nineml.RandomDistribution(
        "uniform(-70.0,-60.0)",
        catalog + "randomdistributions/uniform_distribution.xml",
        {'lowerBound': (-70.0, "dimensionless"),
         'upperBound': (-60.0, "dimensionless")})

    exc_cell_parameters = nineml.ParameterSet(
        membraneCapacitance=(1.0, "nF"),
        membraneTimeConstant=(tau_distr, "ms"),
        refractoryTime=(5.0, "ms"),
        threshold=(-50.0, "mV"),
        restingPotential=(-65.0, "mV"),
        resetPotential=(reset_distr, "mV"))

    inh_cell_parameters = nineml.ParameterSet(
        membraneTimeConstant=(20.0, "ms"),
        resetPotential=(-60.0, "mV"))

    inh_cell_parameters.complete(exc_cell_parameters)

    exc_celltype = nineml.SpikingNodeType("Excitatory neuron type",
                                          catalog + "neurons/IaF_tau.xml",
                                          exc_cell_parameters)
    inh_celltype = nineml.SpikingNodeType("Inhibitory neuron type",
                                          catalog + "neurons/IaF_tau.xml",
                                          inh_cell_parameters)

    grid2D = nineml.Structure("2D grid",
                              catalog + "networkstructures/2Dgrid.xml",
                              {'fillOrder': ("sequential", None),
                               'aspectRatioXY': (1.0, "dimensionless"),
                               'dx': (1.0, u"µm"), 'dy': (1.0, u"µm"),
                               'x0': (0.0, u"µm"), 'y0': (0.0, u"µm")})

    exc_cells = nineml.Population("Excitatory cells", 100, exc_celltype,
                                  nineml.PositionList(structure=grid2D))
    inh_cells = nineml.Population("Inhibitory cells", 25, inh_celltype,
                                  nineml.PositionList(structure=grid2D))
    all_cells = nineml.Selection("All cells",
        nineml.Any(
            nineml.Eq("population[@name]", exc_cells.name),
            nineml.Eq("population[@name]", inh_cells.name))
    )

    connection_rule = nineml.ConnectionRule(
        "random connections",
        catalog + "connectionrules/fixed_probability.xml",
        {'p_connect': (0.1, "dimensionless")})

    exc_psr = nineml.SynapseType(
        "Excitatory post-synaptic response",
        catalog + "postsynapticresponses/exp_g.xml",
        dict(decayTimeConstant=(5.0, "ms"), reversalPotential=(0.0, "mV")))

    inh_psr = nineml.SynapseType(
        "Inhibitory post-synaptic response",
        catalog + "postsynapticresponses/exp_g.xml",
        dict(decayTimeConstant=(5.0, "ms"), reversalPotential=(-70.0, "mV")))

    exc_connection_type = nineml.ConnectionType(
        "Static excitatory connections",
        catalog + "connectiontypes/static_connection.xml",
        {'weight': (0.1, "nS"), 'delay': (0.3, "ms")})

    inh_connection_type = nineml.ConnectionType(
        "Static inhibitory connections",
        catalog + "connectiontypes/static_connection.xml",
        {'weight': (0.2, "nS"), 'delay': (0.3, "ms")})

    exc2exc = nineml.Projection("Excitatory cells-Excitatory cells",
                                exc_cells, exc_cells, connection_rule,
                                exc_psr, exc_connection_type)
    inh2all = nineml.Projection("Inhibitory connections",
                                inh_cells, all_cells, connection_rule,
                                inh_psr, inh_connection_type)

    network = nineml.Group("Network")
    network.add(exc_cells)
    network.add(inh_cells)
    network.add(all_cells)
    network.add(exc2exc)
    network.add(inh2all)

    model = nineml.Model("Simple 9ML example model")
    model.add_group(network)
    return model

simple_example = simple_example()


def nested_example():
    catalog = "http://svn.incf.org/svn/nineml/catalog/"

    cell_parameters = {
        "membraneCapacitance": (1.0, "nF"),
        "membraneTimeConstant": (20.0, "ms"),
        "refractoryTime": (5.0, "ms"),
        "threshold": (-50.0, "mV"),
        "restingPotential": (-65.0, "mV"),
        "resetPotential": (-70.0, "mV"),
    }

    exc_celltype = nineml.SpikingNodeType(
        name="Excitatory neuron type",
        definition=catalog + "neurons/IaF_tau.xml",
        parameters=cell_parameters)

    inh_celltype = nineml.SpikingNodeType(
        name="Inhibitory neuron type",
        definition=catalog + "neurons/IaF_tau.xml",
        parameters=cell_parameters)

    inner_grid = nineml.Structure(
        name="neuronal grid with 1 micron spacing",
        definition=catalog + "networkstructures/2Dgrid.xml",
        parameters={'fillOrder': ("sequential", None),
                    'aspectRatioXY': (1.0, "dimensionless"),
                    'dx': (1.0, u"µm"), 'dy': (1.0, u"µm"),
                    'x0': (0.0, u"µm"), 'y0': (0.0, u"µm")})

    outer_grid = nineml.Structure(
        name="column grid with 100 micron spacing",
        reference=inner_grid.name,
        parameters={'dx': (100.0, u"µm"), 'dy': (100.0, u"µm")})

    exc_cells = nineml.Population(
        name="Excitatory cells",
        number=100,
        prototype=exc_celltype,
        positions=nineml.PositionList(structure=inner_grid))

    inh_cells = nineml.Population(
        name="Inhibitory cells",
        number=25,
        prototype=inh_celltype,
        positions=nineml.PositionList(structure=inner_grid))

    column = nineml.Group("Column")
    column.add(exc_cells, inh_cells)

    exc_psr = nineml.SynapseType(
        name="Excitatory post-synaptic response",
        definition=catalog + "postsynapticresponses/exp_g.xml",
        parameters={'decayTimeConstant': (5.0, "ms"),
                    'reversalPotential': (0.0, "mV")})
    inh_psr = nineml.SynapseType(
        name="Inhibitory post-synaptic response",
        reference=exc_psr.name,
        parameters={'reversalPotential': (-70.0, "mV")})

    exc_connection_type = nineml.ConnectionType(
        name="Static excitatory connections",
        definition=catalog + "connectiontypes/static_connection.xml",
        parameters={'weight': (0.1, "nS"), 'delay': (0.3, "ms")})

    inh_connection_type = nineml.ConnectionType(
        name="Static inhibitory connections",
        reference=exc_connection_type.name,
        parameters={'weight': (0.2, "nS")})

    intra_column_connector = nineml.ConnectionRule(
        name="local random connections",
        definition=catalog + "connectionrules/fixed_probability.xml",
        parameters={'p_connect': (0.1, "dimensionless")})

    inner_exc2all = nineml.Projection(
        name="Intra-column excitatory connections",
        source=exc_cells,
        target=column,
        rule=intra_column_connector,
        synaptic_response=exc_psr,
        connection_type=exc_connection_type)

    inner_inh2all = nineml.Projection(
        name="Intra-column inhibitory connections",
        source=inh_cells,
        target=column,
        rule=intra_column_connector,
        synaptic_response=inh_psr,
        connection_type=inh_connection_type)

    column.add(inner_exc2all, inner_inh2all)

    inter_column_connector = nineml.ConnectionRule(
        name="lateral random connections",
        reference=intra_column_connector.name,
        parameters={'p_connect': (0.05, "dimensionless")})

    network = nineml.Group("Network")

    columns = nineml.Population(
        name="Columns",
        number=9,
        prototype=column,
        positions=nineml.PositionList(structure=outer_grid))

    outer_exc2all = nineml.Projection(
        name="Inter-column excitatory connections",
        source=columns,
        target=columns,
        rule=inter_column_connector,
        synaptic_response=exc_psr,
        connection_type=exc_connection_type)

    network.add(columns, outer_exc2all)

    model = nineml.Model("Nested 9ML example model")
    model.add_group(network)
    return model

nested_example = nested_example()


class RountripULTestCase(unittest.TestCase):

    def test_roundtrip_simple(self):
        global simple_example

        model = simple_example
        
        f = tempfile.TemporaryFile()
        model.write(f)
        f.seek(0)

        model1 = parse(f)
        f.close()

        assert model == model1

    def test_roundtrip_nested(self):
        global nested_example

        model = nested_example
        
        f = tempfile.TemporaryFile()
        model.write(f)
        f.seek(0)

        model1 = parse(f)
        f.close()

        assert model == model1



def suite():

    suite = unittest.makeSuite(RountripULTestCase,'test')
    return suite

if __name__ == "__main__":

    # unittest.main()
    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
