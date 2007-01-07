'''This is part of the latex-access project.

This module provides a mechanism for processing a line of LaTeX before it is sent to the main translator.  This enable custom defined commands to be replaced with standard equivalents.'''


def process(input):
    '''This function processes the input string, and returns the processed version as a string.'''
    output = input
    for key in table.keys():
        output=output.replace(key,table[key])
    return output

#The following dictionary will contain pairs of string which will be replaced.  It will not have any entries hard-coded into it as these will be variable, depending on the document.

table={}
