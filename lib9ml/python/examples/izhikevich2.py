import nineml.abstraction_layer as nineml


parameters = ["V", "U", "t", "spike", "Isyn",
              "a", "b", "c", "d", "theta"]

subthreshold_regime = nineml.Sequence(
    nineml.Union(
        "dV/dt = 0.04*V*V + 5*V + 140.0 - U + Isyn",
        "dU/dt = a*(b*V - U)",
        ),
    nineml.On("V > theta", do=["tspike = t", "V=c","U+=d",
                               nineml.SpikeOutputEvent]),
    name="subthreshold_regime"
    )

c1 = nineml.Component("Izhikevich", parameters,
                             initial_regime = subthreshold_regime )


c1.write("izhikevich2.xml")

c2 = nineml.parse("izhikevich2.xml")


## l1 = list(c1.regimes)
## l2 = list(c2.regimes)

## t1 = l1[0].nodes[2]
## t2 = l2[0].nodes[2]

## s1 = l1[1]
## s2 = l2[1]

t1 = c1.transitions
t2 = c2.transitions
