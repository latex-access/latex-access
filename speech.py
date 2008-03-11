'''Module to provide speech output for latex_access.'''

 
import latex_access
from latex_access import get_arg
from latex_access import translate
from latex_access import text
from latex_access import remove


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

def sub(input,start):
    '''Translates subscripts.  The output uses html style tags to denote start and end.
    
    Returns touple.'''
    arg=get_arg(input,start)
    translation="<sub>%s</sub>" % translate(arg[0],table)
    return (translation,arg[1])



def sqrt(input,start):
    '''Translates squareroots into speech.
     
    returns touple.'''
    arg=get_arg(input,start)
    if arg[0].isdigit() or len(arg[0])==1:
        translation=" root "+arg[0]
    else:
        translation=" begin root "+translate(arg[0],table)+" end root "
    return (translation,arg[1])

#Define a list of words to use as denominators of simple fractions
denominators=[" over zero"," over1 "," half"," third"," quarter"," fifth"," sixth"," seventh"," eight"," ninth"]

def frac(input,start):
    '''Translate fractions into speech.

    Returns touple.'''
    numerator=get_arg(input,start)
    if numerator[1]<len(input):
        denominator=get_arg(input,numerator[1])
    else:
        denominator=("",numerator[1])
    if len(numerator[0])==1 and len(denominator[0])==1:
        if numerator[0].isdigit() and denominator[0].isdigit():
            translation = numerator[0]+denominators[int(denominator[0])]
            if int(numerator[0])>1:
                translation+="s"
            translation+=" "
        else:
            translation =" %s over %s " % (numerator[0], denominator[0])
    else:
        translation=" begin frac %s over %s end frac " % (translate(numerator[0],table), translate(denominator[0],table))
    return (translation,denominator[1])

def integral(input,start):
    '''Translate integrals, including limits of integration.
    
    Returns touple.'''
    arg=get_arg(input,start)
    #Look for limits
    lower=None
    upper=None
    i=start
    while input[i]==" " and i < len(input):
        i+=1
    if input[i]=="_":
        lower=get_arg(input,i+1)
        i=lower[1]
        if i < len(input):
            while input[i]==" " and i < len(input):
                i+=1
            if input[i]=="^":
                upper=get_arg(input,i+1)
                i=upper[1]
    #Now repeat this the other way round in case upper limit comes first
    elif input[i]=="^":
        upper=get_arg(input,i+1)
        i=upper[1]
        if i < len(input):
            while input[i]==" " and i < len(input):
                i+=1
            if input[i]=="_":
                lower=get_arg(input,i+1)
                i=lower[1]
    #Now build output string
    output=" integral "
    if lower is not None:
        output+="from "
        output+=translate(lower[0],table)
    if upper is not None:
        output+=" to "
        output+=translate(upper[0],table)
    output+=" of "
    return (output,i)

def bold(input,start):
    '''Translates characters in bold.
    
    Returns toutple'''
    arg=get_arg(input,start)
    translation="<bold>%s</bold>" % translate(arg[0],table)
    return (translation,arg[1])

def hat(input,start):
    '''Translates \hat, as used for vectors.
    
    Returns touple.'''
    arg=get_arg(input,start)
    translation="%s hat " % translate(arg[0],table)
    return (translation, arg[1])

def bar(input,start):
    '''Translates the hat accent.
    
    Returns touple.'''
    arg=get_arg(input,start)
    translation=" %s bar " % translate(arg[0],table)
    return (translation, arg[1])

def dot(input,start):
    '''Translates the dot as used for time derivative.
    
    Returns touple.'''
    arg=get_arg(input,start)
    translation=" %s dot " % translate(arg[0],table)
    return (translation, arg[1])

def ddot(input,start):
    '''Translates ddot as used as 2nd time derivative.
    
    Returns touple.'''
    arg=get_arg(input,start)
    translation=" %s double dot  " % translate(arg[0],table)
    return (translation, arg[1])

def displaystyle(input, start):
    '''Removes the displaystile command but translates its argument.
    
    Returns touple.'''
    arg=get_arg(input,start)
    return (translate(arg[0],table),arg[1])



table={"+":" plus ","-":" minus ","\\pm":" plus or minus ","\\mp":" minus or plus ","\\times":" times ",
"=":" equals ","\\ne":" not equals ","\\neq":" not equals ","<":" less than ",">":" greater than ","\\le":" less than or equal to ","\\leq":" less than or equal to ","\\ge":" greater than or equal to ","\\geq":" greater than or equal to ",
"\\cdot":" dot ","\\ldots":" dot dot dot ","\\cdots":" dot dot dot ","\\dots":" dot dot dot ",
"^":super,"_":sub,"\\sqrt":sqrt,"\\frac":frac,"\\int":integral,"\\mathbf":bold,"\\mathbb":bold,
"\\mbox":text,"\\text":text,"\\mathrm":text,"\\textbf":text,"\\textit":text,"\\displaystyle":displaystyle,
       "\\,":" ","\\;":" ","\\quad":"","\\qquad":"","\\left":"","\\right":"",
"\\alpha":" alpha ","\\Alpha":" cap alpha ","\\beta":" beta ","\\Beta":" cap beta ","\\gamma":" gamma ","\\Gamma":" cap gamma ",
"\\delta":" delta ","\\Delta":" cap delta ","\\epsilon":" epsilon ","\\omega":" omega ","\\Omega":" cap omega ","\\phi":" phi ","\\Phi":" cap phi ","\\lambda":" lambda ","\\mu":" mu ","\\pi":" pi ",
"\\theta":" theta ","\\sigma":" sigma ","\\Sigma":" Sigma ","\\rho":" rho ",
       "\\zeta":" zeta ","\\eta":"  eta ",
"\\infty":" infinity ","\\rightarrow":" goes to ","\\leftrightarrow":" if and only if ","\\Rightarrow":" goes to","\\Leftrightarrow":" if and only if ",
"\\sin":" sine ","\\cos":" coz ","\\tan":"tan","\\sinh":" shine ","\\cosh":" cosh ","\\tanh":" thann ",
       "\\hat":hat,"\\widehat":hat,"\\bar":bar,"\\overline":bar,"\\dot":dot,"\\ddot":ddot,
"\\partial":" partial ","\\nabla":" nabla ","\\wedge":" vec "}
