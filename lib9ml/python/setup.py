#!/usr/bin/env python

#from distutils.core import setup
from distribute_setup import use_setuptools
use_setuptools()
from setuptools import setup, find_packages
from nineml.__init__ import __version__

setup(
    name = "9ML",
    version = __version__,
    packages = ['nineml',
                'nineml.abstraction_layer',
                'nineml.abstraction_layer.component',
                'nineml.abstraction_layer.component.parse',
                'nineml.abstraction_layer.component_modifiers',
                'nineml.abstraction_layer.flattening',
                'nineml.abstraction_layer.readers',
                'nineml.abstraction_layer.testing_utils',
                'nineml.abstraction_layer.validators',
                'nineml.abstraction_layer.visitors',
                'nineml.abstraction_layer.writers',
                'nineml.exceptions',
                'nineml.maths',
                'nineml.utility'
                ],
    package_data = {'nineml': ['examples/AL/demos/*.py', "examples/AL/components_done/*.py"]},
    #packages = find_packages(),
    author = "Andrew P. Davison, Eilif Muller, Mike Hull", # add your name here if you contribute to the code
    author_email = "nineml-users@incf.org",
    description = "A tool for reading, writing and generally working with 9ML files.",
    long_description = open("README").read(),
    license = "BSD 3 License",
    keywords = "computational neuroscience modeling interoperability XML",
    url = "http://nineml.incf.org",
    classifiers = ['Development Status :: 3 - Alpha',
                   'Environment :: Console',
                   'Intended Audience :: Science/Research',
                   'License :: OSI Approved :: BSD License',
                   'Natural Language :: English',
                   'Operating System :: OS Independent',
                   'Programming Language :: Python :: 2',
                   'Topic :: Scientific/Engineering'],
    install_requires = ['lxml', 'ply'],
    tests_require = ['nose']
)

