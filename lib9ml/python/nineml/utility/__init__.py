
from os.path import dirname, join, normpath

def join_norm(*args):
    return normpath( join(*args))


class LocationMgr(object):

    @classmethod
    def getRootDir(cls):
        localDir = dirname( __file__ )
        rootDir = join_norm( localDir, "../../../../") 
        return rootDir




    @classmethod
    def getCatalogDir(cls):
        return join_norm( cls.getRootDir(), "catalog/" )



