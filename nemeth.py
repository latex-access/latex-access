'''Module to provide Nemeth translations for the latex_access module.'''

import latex_access
from latex_access import get_arg
from latex_access import translate
from latex_access import text
from latex_access import remove


def super(input,start):
    '''Translate  superscripts into Nemeth.

    Returns a touple with translated string and index of
    first char after end of super.'''
    arg=get_arg(input,start)
    #Handle squared and cubed as special cases
    if arg[0] == "2":
        translation="<"
    elif arg[0]=="3":
        translation="%"
    else:
        translation = "~" + translate(arg[0],table) + "\""
    return (translation,arg[1])



def sub(input,start):
    '''Translates nemeth subscripts.

    Returns a touple, as above'''
    arg=get_arg(input,start)
    if arg[0].isdigit():
        translation=arg[0]
    else:
        translation = ";"+translate(arg[0],table) + "\""
    return (translation,arg[1])

def sqrt(input,start):
    '''Translatesroots in latex.

    Returns a touple as above.'''
    arg=get_arg(input,start)
    if arg[0].isdigit() or len(arg[0])==1:
        translation=">"+arg[0]
    else:
        translation=">"+translate(arg[0],table)+"}"
    return (translation,arg[1])

def frac(input,start):
    '''Translates fractions into Nemeth.

    Returnstouple as above'''
    numerator=get_arg(input,start)
    if numerator[1]<len(input):
        denominator=get_arg(input,numerator[1])
    else:
        denominator=("",numerator[1])
    if numerator[0].isdigit() and denominator[0].isdigit():
            translation=numerator[0]+"/"+denominator[0]
    else:
        translation="?"+translate(numerator[0],table)+"/"+translate(denominator[0],table)+"#"
    return (translation,denominator[1])

def bold(input,start):
    '''Handles bold letters in equations, for example for vectors and matrices.

    Returns touple.'''
    arg=get_arg(input,start)
    translation="_%s" % translate(arg[0],table)
    return (translation,arg[1])

def colvec(input,start):
    '''Handles 2d column vectors created with a \colvec command.
    This is a custom command which I find useful to define.

    Returns touple as above.'''
    x=get_arg(input,start)
    if x[1]<len(input):
        y=get_arg(input,x[1])
    else:
        y=("",x[1])
    translation="{%s %so" % (translate(x[0],table),translate(y[0],table))
    return (translation,y[1])

def tcolvec(input,start):
    '''This is intended to handle 3d column vectors created by a tcolvec command.
    Like colvec, I reckomend defining this command.

    Returns touple as above'''
    x=get_arg(input,start)
    if x[1]<len(input):
        y=get_arg(input,x[1])
    else:
        y=("",x[1])
    if y[1]<len(input):
        z=get_arg(input,y[1])
    else:
        z=("",y[1])
    translation="{%s %s %so" % (translate(x[0],table),translate(y[0],table),translate(z[0],table))
    return (translation,z[1])

def dot(input, start):
    '''Used to translate dot, as in differentiation

    returns touple.'''
    arg=get_arg(input,start)
    translation="%s`" % translate(arg[0],table)
    return (translation,arg[1])


def ddot(input, start):
    '''Used to translate ddot, as in differentiation

    returns touple.'''
    arg=get_arg(input,start)
    translation="%s``" % translate(arg[0],table)
    return (translation,arg[1])

def bar(input, start):
    '''Handles bar/overline.

    Returns toutple'''
    arg=get_arg(input,start)
    if len(arg[0])==1:
        translation=":%s" % arg[0]
    else:
        translation=":{%so" % translate(arg[0],table)
    return (translation,arg[1])




def displaystyle(input, start):
    '''Removes the displaystile command but translates its argument.
    
    Returns touple.'''
    arg=get_arg(input,start)
    return (translate(arg[0],table),arg[1])    


table={"+":"+","-":"-","=":" .k ","\\times":"*","\\pm":"+-","\\mp":"-+","\\cdot":"_*","\\circ":"_*","\\wedge":"*","\\ldots":"'''","\\cdots":"'''","\\dots":"'''","!":"&",
       "\\infty":"=","<":" \"k ",">":" .1 ","\\leq":" \"k.k ","\\geq":" .1.k ","\\le":" \"k.k ","\ge":" .1.k ","\\neq":" ./k ","\\ne":" ./k ",
              "\\alpha":".a","\\beta":".b","\\theta":".?","\\pi":".p","\\phi":".f",
              "\\gamma":".g","\\delta":".d","\\lambda":".l","\\mu":".m","\\nu":".n","\\sigma":".s",
              "\\kappa":".k","\\rho":".r","\\tau":".t","\\omega":".w","\\psi":".y","\\epsilon":".e",
              "\\Alpha":"_a","\\Beta":"_b","\\Gamma":"_g","\\Delta":"_d","\\Omega":"_w","\\Sigma":"_s",
              "\\sin":"sin ","\\cos":"cos ","\\tan":"tan ",
              "\\sec":"sec ","\\cosec":"cosec ","\\cot":"cot ",
              "\\sinh":"sinh ","\\cosh":"cosh ","\\tanh":"tanh ",
              "\\rightarrow":" 33o","\\Rightarrow":" 33o","\\leftarrow":" {33","\\leftrightarrow":" {33o ","\\equiv":" _l ",
       "\\partial":"$","\\int":"!","\\sum":".s","\\prod":"_p","\\dot":dot,"\\ddot":ddot,
              "^":super,"_":sub,"\\sqrt":sqrt,"\\frac":frac,
       "\\mathbf":bold,"\\colvec":colvec,"\\tcolvec":tcolvec,"\\bar":bar,"\\hat":bar,"\\overline":bar,
              "\\cup":".+","\\cap":".%","\\subseteq":"_\"k:","\\subset":"_\"k","\\supseteq":"_.1:","\\supset":"_.1",
              "\\setminus":"_*","\\emptyset":"_0",
       "(":"{",")":"o","\\left":"","\\right":"","\\quad":"  ","\\qquad":"  ","\\,":"","\\;":" ","\\:":" ","\\mbox":text,"\\text":text,"\\textrm":text,"\\mathrm":text,"\\textbf":text,"\\displaystyle":displaystyle,
       "\\phantom":remove}
