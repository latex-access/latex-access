'''Module to provide speech output for latex_access.'''

 
import latex_access
from latex_access import get_arg
from latex_access import translate


def super(input,start):
    '''Translate  superscripts into speech.

    Returns a touple with translated string and index of
    first char after end of super.'''
    arg=get_arg(input,start)
    #Handle squared and cubed as special cases
    if arg[0] == "2":
        translation=" squared "
    elif arg[0]=="3":
        translation=" cubed "
    else:
        translation = " to the %s end super " % translate(arg[0],table)  
    return (translation,arg[1])



table={"^":super}
