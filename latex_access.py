'''This module provides translations of lines of LaTeX into either braille or spoken output.'''

import re
import types

# Regular expression to match LaTeX commands
latex_command=re.compile(r"\\(([a-zA-Z]+)|[,!;])")


def translate(input,translation_table):
    '''This translates the string in input using the translation table specified.
    The translation table should be a dictionary which maps string to either strings of functions.

    Returns string.'''

    output=""
    i=0
    while (i<len(input)):
        # Test if we have a LaTeX command
        if input[i] == "\\":
            match=latex_command.match(input[i:])
            if match:
                curr=match.group()
            else:
                curr="\\"
            
        else:
            curr=input[i]

        if curr in translation_table:
            i+=len(curr)
            if type(translation_table[curr]) == types.StringType:
                output += translation_table[curr]

            elif type(translation_table[curr]) == types.FunctionType:
                translation=translation_table[curr](input,i)
                output+=translation[0]
                i=translation[1]
            

        else:
            output += curr
            i += len(curr)
    return output

def get_arg(input,start):
    '''Returns the argument of a latex command, starting at start.

    Returns a touple containing the contents of the argument
    and the index of the next character after the argument.'''
    i=start
    #Skip space
    while input[i]==" " and i < len(input):
        i+=1
    if input[i] != "{":
        return (input[i],i+1)
    else:
        i+=1
        start=i
        j=1 #Variable to track nested braces
        while (j != 0) and i < len(input):
            if input[i] == "{":
                j+=1
            if input[i] == "}":
                j-=1
            i+=1
        return(input[start:i-1],i)



