
from nineml.abstraction_layer import readers
from nineml.abstraction_layer import writers
from nineml.abstraction_layer import flattening
from nineml.abstraction_layer import validators
from nineml.utility import file_sha1_hexdigest

class TestXMLWriteReadWrite(object):
    
    @classmethod
    def test(cls, testable_component, build_dir):
        component = testable_component()
        print '  -- Testing One and a half trips...'

        if not component.is_flat():
            component = flattening.flatten(component)


        xmlfile1 = build_dir + component.name + '1.xml'
        xmlfile2 = build_dir + component.name + '2.xml'


        print '    -- Saving Component To XML:', xmlfile1
        writers.XMLWriter.write( component, xmlfile1 ) 

        print '    -- Loading Component To XML.'
        reloaded_comp = readers.XMLReader.read(xmlfile1)

        print '    -- Checking Components are identical'
        validators.ComponentEqualityChecker.check_equal( component, reloaded_comp)

        print '    -- Saving Reloaded Component to XML',xmlfile2
        writers.XMLWriter.write( reloaded_comp, xmlfile2 ) 

        print '    -- Checking the SHA1 Checksum of the two xml files:'
        hash1 = file_sha1_hexdigest(xmlfile1)
        hash2 = file_sha1_hexdigest(xmlfile2)
        print '      -->', hash1
        print '      -->', hash2

        if hash1 != hash2: 
            raise ValueError('XML files are different. This may not be an error but please report it to the developers')
