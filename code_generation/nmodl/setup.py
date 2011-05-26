#!/usr/bin/env python

from distutils.core import setup
from nineml2nmodl import __version__

setup(
    name = "9ml2nmodl",
    version = __version__,
    packages = ['nineml2nmodl', 'nineml2nmodl.test'],
    package_data = {'nineml2nmodl': ['nmodl_template.jinja']},
    scripts = ['bin/9ml2nmodl'],
    author = "Andrew P. Davison", # add your name here if you contribute to the code
    author_email = "andrewpdavison@gmail.com",
    description = "A tool for generating NMODL mechanisms for the NEURON simulator (http://www.neuron.yale.edu) from model descriptions in NineML (http://www.nineml.org/)",
    license = "CeCILL http://www.cecill.info",
    keywords = "computational neuroscience modeling interoperability XML NEURON NMODL",
    url = "http://nineml.incf.org",
    classifiers = ['Development Status :: 2 - Pre-Alpha',
                   'Environment :: Console',
                   'Intended Audience :: Science/Research',
                   'License :: Other/Proprietary License',
                   'Natural Language :: English',
                   'Operating System :: OS Independent',
                   'Programming Language :: Python :: 2',
                   'Topic :: Scientific/Engineering'],
    requires = ['nineml'],
)

