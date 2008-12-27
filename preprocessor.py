'''This module provides a translator which can be used to replace LaTeX in the input with different LaTeX.

For example it can be used to handle commands defined by \newcommand.'''

import latex_access

class preprocessor(latex_access.translator):
    '''Preprocessor translator

    All translations done by this translator should use the general_command mechanism rather than custom functions.'''
    def __init__(self):
        self.table={}

    def add(self,command,translation):
        '''Add a translation to the table.'''
        self.table[command]=translation

    def write(self,file):
        '''Save preprocessor entries to a # delimited file.'''
        try:
            f=open(file,"w")
        except IOError:
            return False
        for (k, v) in self.table.iteritems():
            output="%s#%s\n" % (k,"#".join(v))
            f.write(output)
        f.close()
        return True
    

                  
    def read(self, filename):
        '''Reads preprocessor entries from a # delimited file.'''
        try:
            f=open(filename,"r")
        except IOError:
            return False
        for l in f:
            input=l.split("#")
            self.table[input[0]]=input[1:]
        f.close()
        return True
