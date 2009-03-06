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

    def add_from_string(self, command, args, translation_string):
        '''This adds a command to the preprocessor given its number of arguments as well as its output in the form of an argument to \newcommand.

        Therefore the final argument is a string using #n to denote the nth argument.'''
        translation=[]
        translation.append(args)
        l=translation_string.split("#")
        translation.append(l[0])
        for s in l[1:]:
            translation.append(int(s[0]))
            translation.append(s[1:])
        self.table[command]=translation
