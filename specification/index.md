---
layout: archive
title: "Specification"
modified:
tags: []
image:
  feature:
  teaser:
---

The current 9ML specification is
[version 1.0](http://nineml-spec.readthedocs.io/en/latest). There is
also a [development branch](http://nineml-spec.readthedocs.io/en/develop),
which contains revisions to the specification that have been approved
by the [NineML committee](http://nineml.net/committee) but have not
been released.

There is a corresponding
[XML schema]({{site.url}}/9ML/1.0/NineML_v1.0.xsd) although
the schema only validates the model hierarchy and basic
types, i.e. not all the requirements described in the specification.

For full validation of 9ML models, please use the officially supported
[NineML Python Library](http://github.com/INCF/nineml-python).
Software written in Python, or with a Python interface, can also take
advantage of its convenient API in order to add support for 9ML to your
tool.  

For 9ML examples, please see the
[NineML Catalog](https://github.com/INCF/nineml-catalog).
