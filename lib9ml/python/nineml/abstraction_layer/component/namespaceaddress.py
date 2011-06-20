
import nineml.utility

class NamespaceAddress(object):
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

    
    def __hash__(self):
        return hash(self.loctuple)

    def __eq__(self,rhs):
        if not isinstance(rhs, self.__class__): 
            return False
        return self.loctuple == rhs.loctuple


    def get_subns_addr(self, component_name):
        return NamespaceAddress( loc = tuple( list(self.loctuple) + [component_name] ) )
    def get_parent_addr(self):
        assert len(self.loctuple) > 0
        return NamespaceAddress( loc = self.loctuple[:-1] )

    def __repr__(self):
        return "NSAddr: '" + "//" + "/".join( self.loctuple) + "/'"

    @classmethod
    def create_root(cls):
        return NamespaceAddress( loc=() )

    def getstr(self):
        return "_".join( self.loctuple )

    def get_str_prefix(self):
        return self.getstr() + "_" 

    @classmethod
    def concat(cls,*args):
        
        args = [ NamespaceAddress(a) for a in args]
        #print 'Concatenating:', args
        loctuple = tuple( nineml.utility.flatten_first_level( [ list(a.loctuple) for a in args  ] ) )
        res = NamespaceAddress(loc=loctuple)
        #print 'yields:', res
        return res
