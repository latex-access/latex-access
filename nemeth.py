'''Module to provide Nemeth translations for the latex_access module.'''

import latex_access
from latex_access import get_arg
from latex_access import translate


def nemeth_super(input,start):
    '''Translate  superscripts into Nemeth.

    Returns a tourple with translated string and index of
    first char after end of super.'''
    start+=1
    arg=get_arg(input,start)
    #Handle squared and cubed as special cases
    if arg[0] == "2":
        translation="<"
    elif arg[0]=="3":
        translation="%"
    else:
        translation = "~" + translate(arg[0],nemeth_table) + "\""
    return (translation,arg[1])
    


def nemeth_sub(input,start):
    '''Translates nemeth subscripts.

    Returns a touple, as above'''
    start+=1
    arg=get_arg(input,start)
    if arg[0].isdigit():
        translation=arg[0]
    else:
        translation = ";"+translate(arg[0],nemeth_table) + "\""
    return (translation,arg[1])

def nemeth_sqrt(input,start):
    '''Translatesroots in latex.

    Returns a touple as above.'''
    start+=5
    arg=get_arg(input,start)
    if arg[0].isdigit() or len(arg[0])==1:
        translation=">"+arg[0]
    else:
        translation=">"+translate(arg[0],nemeth_table)+"}"
    return (translation,arg[1])

def nemeth_frac(input,start):
    '''Translates fractions into Nemeth.

    Returnstouple as above'''
    start+=5
    numerator=get_arg(input,start)
    if numerator[1]<len(input):
        denominator=get_arg(input,numerator[1])
    else:
        denominator=("",numerator[1])
    if numerator[0].isdigit() and denominator[0].isdigit():
            translation=numerator[0]+"/"+denominator[0]
    else:
        translation="?"+translate(numerator[0],nemeth_table)+"/"+translate(denominator[0],nemeth_table)+"#"
    return (translation,denominator[1])

def nemeth_bold(input,start):
    '''Handles bold letters in equations, for example for vectors and matrices.

    Returns touple.'''
    start+=7
    arg=get_arg(input,start)
    translation="_%s" % translate(arg[0],nemeth_table)
    return (translation,arg[1]) 
        
def nemeth_colvec(input,start):
    '''Handles 2d column vectors created with a \colvec command.
    This is a custom command which I find useful to define.

    Returns touple as above.'''
    start+=7
    x=get_arg(input,start)
    if x[1]<len(input):
        y=get_arg(input,x[1])
    else:
        y=("",x[1])
    translation="{%s %so" % (translate(x[0],nemeth_table),translate(y[0],nemeth_table))
    return (translation,y[1])

def nemeth_tcolvec(input,start):
    '''This is intended to handle 3d column vectors created by a tcolvec command.
    Like colvec, I reckomend defining this command.

    Returns touple as above'''
    start+=8
    x=get_arg(input,start)
    if x[1]<len(input):
        y=get_arg(input,x[1])
    else:
        y=("",x[1])
    if y[1]<len(input):
        z=get_arg(input,y[1])
    else:
        z=("",y[1])
    translation="{%s %s %so" % (translate(x[0],nemeth_table),translate(y[0],nemeth_table),translate(z[0],nemeth_table))
    return (translation,z[1])

def nemeth_dot(input, start):
    '''Used to translate dot, as in differentiation

    returns touple.'''
    start+=4
    arg=get_arg(input,start)
    translation="%s'" % translate(arg[0],nemeth_table)
    return (translation,arg[1]) 


def nemeth_ddot(input, start):
    '''Used to translate ddot, as in differentiation

    returns touple.'''
    start+=5
    arg=get_arg(input,start)
    translation="%s''" % translate(arg[0],nemeth_table)
    return (translation,arg[1])

def nemeth_text(input, start):
    '''Used to translate text, as in mbox and text
    
    returns touple.'''
    start+=5
    arg=get_arg(input,start)
    translation="%s" % translate(arg[0],nemeth_table)
    return (translation,arg[1]) 

nemeth_table={"+":"+","-":"-","=":" .k ","\\times":"*","\\pm":"+-","\\cdot":"_*","\\wedge":"*",
              "<":" \"k ",">":" .1 ","\\leq":" \"k.k ","\\geq":" .1.k ", 
              "\\alpha":".a","\\beta":".b","\\theta":".?","\\pi":".p","\\phi":".f",
              "\\gamma":".g","\\delta":".d","\\lambda":".l","\\mu":".m","\\nu":".n","\\sigma":".s",
              "\\kappa":".k","\\rho":".r","\\tau":".t","\\omega":".w","\\psi":".y","\\epsilon":".e",
              "\\Alpha":"_a","\\Beta":"_b","\\Gamma":"_g","\\Delta":"_d","\\Omega":"_w","\\Sigma":"_s",
              "\\sin":"sin ","\\cos":"cos ","\\tan":"tan ",
              "\\Sec":"sec ","\\csc":"cosec ","\\cot":"cot ",
              "\\sinh":"sinh ","\\cosh":"cosh ","\\tanh":"tanh ",
              "\\rightarrow":" 33o","\\leftarrow":" {33","\\equiv":" _l ",
              "\\partial":"_d","\\int":"!","\\dot":nemeth_dot,"\\ddot":nemeth_ddot,
              
              "^":nemeth_super,"_":nemeth_sub,"\\sqrt":nemeth_sqrt,"\\frac":nemeth_frac,
              "\mathbf":nemeth_bold,"\\colvec":nemeth_colvec,"\\tcolvec":nemeth_tcolvec,
              "\\left":"","\\right":"","\\quad":"  ","\\qquad":"  ","\\mbox":nemeth_text,"\\text":nemeth_text}

