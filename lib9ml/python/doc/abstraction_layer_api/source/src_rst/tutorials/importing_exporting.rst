Importing and Exporting
========================


Exporting 
---------

XML
~~~

To write a |COMPONENTCLASS| to a file, we can use the ``write`` method,
which takes the filename as a parameter::

     # Construct the component:
     c = ComponentClass(...)

     # Save the component as NineML-XML:
     c.write("test.xml")


``write`` is a simple wrapper around ``XMLWriter.write``. We could also
have written::

     # Construct the component:
     c = ComponentClass(...)

     # Save the component as NineML-XML:
     from nineml.abstraction_layer.writers import XMLWriter
     XMLWriter.write(c,"test.xml")


dot
~~~

To visualize the component, we can export it in dot format
(http://en.wikipedia.org/wiki/DOT_language)::

    from nineml.abstraction_layer.writers import DotWriter
    DotWriter.write(c,"test.dot")
    
Various tools can convert a dot file to an image, for example GraphViz
(http://graphviz.org/).  If you have GraphViz installed, then on the
command-line you can convert the dot files into various image formats, e.g.::

    $ dot -Tsvg test.dot -o test.svg
    $ dot -Tpng test.dot -o test.png


Text
~~~~~

It can be useful to export a component to an easy-to-read text file format.
This can be down with the ``TextWriter`` ::

    from nineml.abstraction_layer.writers import TextWriter
    TextWriter.write(c,"test.dot")

This is also a good starting point for anyone interested in writing code-generation back-ends.



Importing 
---------


XML
~~~

Since we can write to XML, we can also load files from XML. We do this with the ``XMLReader``::


    from nineml.abstraction_layer.readers import XMLReader
    c = XMLReader.read("test.xml")

This will work provided there is just a single component specified in the file. If more than one component is specified, then we need to explicitly name the component we want to load::


    from nineml.abstraction_layer.readers import XMLReader
    c = XMLReader.read("test.xml", component_name='MyComponent')

If we want to load all the components from a file, we can use::

    from nineml.abstraction_layer.readers import XMLReader
    component_list = XMLReader.read_components("test.xml")



