
import nineml.utility

class NamespaceAddress(object):

    @classmethod
    def create_root(cls):
        """Returns a empty (root) namespace address


        >>> nineml.abstraction_layer.NamespaceAddress.create_root()
        NameSpaceAddress: '//'    

        """

        return NamespaceAddress( loc=() )

    @classmethod
    def concat(cls,*args):
        """Concatenates all the Namespace Addresses.

        This method take all the arguments supplied, converts each one into a
        namespace object, then, produces a new namespace object which is the
        concatentation of all the arugements namespaces.
        
        For example:

        >>> NamespaceAddress.concat('first.second','third.forth','fifth.sixth')
            NameSpaceAddress: '/first/second/third/forth/fifth/sixth'


        """
        
        #Turn all the arguments into NamespaceAddress Objects:
        args = [ NamespaceAddress(a) for a in args]

        # Combine all the location tuples in each argument
        # into one long list.
        loc = nineml.utility.flatten_first_level( [ list(a.loctuple) for a in args  ] )

        # Create a namespace out of this long new tuple:
        return NamespaceAddress( loc=tuple(loc) )



    def __init__(self, loc):
        if isinstance(loc, basestring):
            self.loctuple = loc.split('.')
        elif isinstance(loc, tuple):
            self.loctuple = loc
        elif isinstance(loc, NamespaceAddress):
            self.loctuple = loc.loctuple
        else:
            print loc, type(loc)
            assert False

    # Since we often store Namespace addresses in dictionaries:
    def __hash__(self):
        return hash(self.loctuple)

    def __eq__(self,rhs):
        if not isinstance(rhs, self.__class__): 
            return False
        return self.loctuple == rhs.loctuple


    def __repr__(self):
        return "NameSpaceAddress: '" + "/" + "/".join( self.loctuple) + "/'"


    def get_subns_addr(self, component_name):
        """Returns the address of a subcomponent at this address.

        For example:

        >>> a = NamespaceAddress('level1.level2.level3')
        >>> a.get_subns_addr('subcomponent')
        NameSpaceAddress: '/level1/level2/level3/subcomponent/'

        """
        #comp_ns = NamespaceAddress(component_name)
        return NamespaceAddress.concat(self.loctuple, component_name )

    def get_parent_addr(self):
        """Return the address of an namespace higher
        
        >>> a = NamespaceAddress('level1.level2.level3')
        >>> a
        NameSpaceAddress: '/level1/level2/level3/'
        >>> a.get_parent_addr()
        NameSpaceAddress: '/level1/level2/'

        """

        assert len(self.loctuple) > 0
        return NamespaceAddress( loc = tuple(self.loctuple[:-1]) )



    def getstr(self, join_char='_'):
        """Returns the namespace address as a string.

        :param join_char: The character used to join the levels in the address.

        """
        return join_char.join( self.loctuple )

    def get_str_prefix(self, join_char='_'):
        """Returns the same as ``getstr``, but prepends the ``join_char`` to
        the end of the string, so that the string can be used to prefix
        variables.

        :param join_char: The character used to join the levels in the address.


        """
        return self.getstr(join_char=join_char) + join_char 

