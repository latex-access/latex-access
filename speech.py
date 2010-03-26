'''Module to provide speech output for latex_access.'''


 
import latex_access
from latex_access import get_arg

#Define a list of words to use as denominators of simple fractions
denominators=[" over zero"," over1 "," half"," third"," quarter"," fifth"," sixth"," seventh"," eight"," ninth"]

class speech(latex_access.translator):
    '''Speech translator class.'''
    def __init__(self):
        latex_access.translator.__init__(self)
        new_table={"$":self.dollar,"+":" plus ","-":" minus ","\\pm":" plus or minus ","\\mp":" minus or plus ","\\times":" times ",
                   "=":" equals ","\\equiv":" equiv ","\\ne":" not equals ","\\neq":" not equals ","<":" less than ",">":" greater than ","\\le":" less than or equal to ","\\leq":" less than or equal to ","\\ge":" greater than or equal to ","\\geq":" greater than or equal to ",
                   "\\cdot":" dot ","\\ldots":" dot dot dot ","\\cdots":" dot dot dot ","\\dots":" dot dot dot ",
                   "^":self.super,"_":("<sub>","</sub>"),"\\sqrt":self.sqrt,"\\frac":self.frac,"\\int":self.integral,"\\mathbf":("<bold>","</bold>"),"\\mathbb":("<bold>","</bold>"),
                   "\\cap":" intersection ","\\cup":" union ","\\emptyset":" empty set ","\\bigcup":" union ","\\bigcap":" intersection ","\\in":" in ",
                   "\\subset":" subset ","\\subseteq":" subset or equal to ","\\supset":" superset ","\\supseteq":" superset or equal to ","\\triangleleft":" normal subgroup ",
                   "\\,":" ","\\;":" ","\\quad":"","\\qquad":"","\\left":"","\\right":"",
                   "\\alpha":" alpha ","\\Alpha":" cap alpha ","\\beta":" beta ","\\Beta":" cap beta ","\\gamma":" gamma ","\\Gamma":" cap gamma ",
                   "\\delta":" delta ","\\Delta":" cap delta ","\\epsilon":" epsilon ","\\omega":" omega ","\\Omega":" cap omega ","\\phi":" phi ","\\Phi":" cap phi ","\\lambda":" lambda ","\\mu":" mu ","\\nu":" nu ","\\pi":" pi ",
                   "\\theta":" theta ","\\sigma":" sigma ","\\Sigma":" cap sigma ","\\rho":" rho ","\\tau":" tau ","\\Tau":" cap tau ",
                   "\\zeta":" zeta ","\\eta":"  eta ","\\psi":" psi ","\\Psi":" cap psi","\\Theta":" cap theta ","\\xi":" xi ","\\chi":" chi ","\\Lambda":"  cap lambda ",
                   "\\infty":" infinity ","\\rightarrow":" goes to ","\\leftrightarrow":" if and only if ","\\Rightarrow":" goes to","\\Leftrightarrow":" if and only if ",
                   "\\sin":" sine ","\\cos":" coz ","\\tan":"tan","\\sinh":" shine ","\\cosh":" cosh ","\\tanh":" thann ","\\log":" log ",
                   "\\hat":(" "," hat "),"\\widehat":(" "," hat "),"\\bar":(" "," bar "),"\\overline":(" "," bar "),"\\dot":(" "," dot "),"\\ddot":(" "," double dot "),
                   "\\partial":" partial ","\\nabla":" nabla ","\\wedge":" vec ","\\lim":" limit ","\\sum":" sum ","\\prod":" product ","\\therefore":" therefore ","\\forall":" for all "}
        for (k,v) in new_table.iteritems():
            self.table[k]=v        





    def super(self,input,start):
        '''Translate  superscripts into speech.

        Returns a touple with translated string and index of
        first char after end of super.'''
        arg=get_arg(input,start)
        #Handle squared and cubed as special cases
        if arg[0] == "2":
            translation=" squared "
        elif arg[0]=="3":
            translation=" cubed "
        #Handle primes
        elif latex_access.primes.match(arg[0]):
            translation=" prime "*arg[0].count("\\prime")
        else:
            translation = " to the %s end super " % self.translate(arg[0])  
        return (translation,arg[1])


    def sqrt(self,input,start):
        '''Translates squareroots into speech.
        
        returns touple.'''
        arg=get_arg(input,start)
        if arg[0].isdigit() or len(arg[0])==1:
            translation=" root "+arg[0]
        else:
            translation=" begin root "+self.translate(arg[0])+" end root "
        return (translation,arg[1])



    def frac(self,input,start):
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
            translation=" begin frac %s over %s end frac " % (self.translate(numerator[0]), self.translate(denominator[0]))
        return (translation,denominator[1])

    def integral(self,input,start):
        '''Translate integrals, including limits of integration.
    
        Returns touple.'''
        (lower,upper,i)=latex_access.get_subsuper(input,start)
        output=" integral "
        if lower is not None:
            output+="from "
            output+=self.translate(lower[0])
        if upper is not None:
            output+=" to "
            output+=self.translate(upper[0])
        output+=" of "
        return (output,i)

