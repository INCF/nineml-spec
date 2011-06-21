import os

class ModFileWriter(object):

    @classmethod
    def write(cls, component, filename):

       from nineml2nmodl import write_nmodl, write_nmodldirect
       write_nmodldirect(component=component,mod_filename=filename, weight_variables={} )


    @classmethod
    def compile_modfiles(cls, directory):

        cwd = os.getcwd()
        os.chdir( directory )

        try:
            os.system('nrnivmodl')
        finally:
            os.chdir(cwd)


