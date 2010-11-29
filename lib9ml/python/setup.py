#!/usr/bin/env python

from distutils.core import setup
from nineml.__init__ import __version__
      
setup(
    name = "9ML",
    version = __version__,
    packages = ['nineml', 'nineml.abstraction_layer'],
    author = "Andrew P. Davison", # add your name here if you contribute to the code
    author_email = "andrewpdavison@gmail.com",
    description = "A tool reading, writing and generally working with 9ML files.",
    license = "CeCILL http://www.cecill.info",
    keywords = "computational neuroscience modeling interoperability XML",
    url = "http://nineml.incf.org",
    classifiers = ['Development Status :: 2 - Pre-Alpha',
                   'Environment :: Console',
                   'Intended Audience :: Science/Research',
                   'License :: OSI Approved :: BSD License',
                   'Natural Language :: English',
                   'Operating System :: OS Independent',
                   'Programming Language :: Python :: 2',
                   'Topic :: Scientific/Engineering'],
    requires = ['lxml'],
)

