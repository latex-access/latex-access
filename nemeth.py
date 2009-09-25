'''Module to provide Nemeth translations for the latex_access
module.'''

import latex_access
from latex_access import get_arg

class nemeth(latex_access.translator):
    '''Class for nemeth translations.'''

    def __init__(self):
        latex_access.translator.__init__(self)
        new_table={"$":self.dollar,"+":"+","-":"-","=":" .k ","\\times":"*","\\pm":"+-","\\mp":"-+","\\cdot":"_*","\\circ":"_*","\\wedge":"*","\\ldots":"'''","\\cdots":"'''","\\dots":"'''","!":"&",
                   "\\infty":"=","<":" \"k ",">":" .1 ","\\leq":" \"k.k ","\\geq":" .1.k ","\\le":" \"k.k ","\ge":" .1.k ","\\neq":" ./k ","\\ne":" ./k ",
                   "\\alpha":".a","\\beta":".b","\\theta":".?","\\pi":".p","\\phi":".f",
                   "\\gamma":".g","\\delta":".d","\\lambda":".l","\\mu":".m","\\nu":".n","\\sigma":".s",
                   "\\kappa":".k","\\rho":".r","\\tau":".t","\\omega":".w","\\psi":".y","\\epsilon":".e","\\eta":".h","\\zeta":".z",
                   "\\Alpha":"_a","\\Beta":"_b","\\Gamma":"_g","\\Delta":"_d","\\Omega":"_w","\\Sigma":"_s","\\Phi":"_f","\Psi":"_y","\\Theta":"_?","\\Lambda":"_l",
                   "\\sin":"sin ","\\cos":"cos ","\\tan":"tan ",
                   "\\sec":"sec ","\\cosec":"cosec ","\\cot":"cot ",
                   "\\sinh":"sinh ","\\cosh":"cosh ","\\tanh":"tanh ",
                   "\\rightarrow":" 33o","\\Rightarrow":" 33o","\\leftarrow":" {33","\\leftrightarrow":" {33o ","\\Leftrightarrow":" {33o ","\\equiv":" _l ",
                   "\\partial":"$","\\int":"!","\\sum":".s","\\prod":"_p","\\dot":("","`"),"\\ddot":("","``"),
                   "^":self.super,"_":self.sub,"\\sqrt":self.sqrt,"\\frac":self.frac,
                   "\\mathbf":("_",""),"\\mathbb":("_",""),"\\colvec":("{"," ","o"),"\\tcolvec":("{"," "," ","o"),"\\bar":self.bar,"\\hat":self.bar,"\\overline":self.bar,
                   "\\cup":".+","\\cap":".%","\\subseteq":"_\"k:","\\subset":"_\"k","\\supseteq":"_.1:","\\supset":"_.1",
                   "\\setminus":"_*","\\emptyset":"_0",
                   "(":"{",")":"o","\\left":"","\\right":"","\\quad":"  ","\\qquad":"  ","\\,":"","\\;":" ","\\:":" ",
                   "\\nabla":".$","\\therefore":" ,*","\\forall":"`&"}
        for (k,v) in new_table.iteritems():
            self.table[k]=v






    def super(self,input,start):
        '''Translate  superscripts into Nemeth.

        Returns a touple with translated string and index of
        first char after end of super.'''
        arg=get_arg(input,start)
        #Handle squared and cubed as special cases
        if arg[0] == "2":
            translation="<"
        elif arg[0]=="3":
            translation="%"
        #Handle primes
        elif latex_access.primes.match(arg[0]):
            translation="'"*arg[0].count("\\prime")
        else:
            translation = "~" + self.translate(arg[0]) + "\""
        return (translation,arg[1])



    def sub(self,input,start):
        '''Translates nemeth subscripts.

        Returns a touple, as above'''
        arg=get_arg(input,start)
        if arg[0].isdigit():
            translation=arg[0]
        else:
            translation = ";"+self.translate(arg[0]) + "\""
        return (translation,arg[1])

    def sqrt(self,input,start):
        '''Translatesroots in latex.

        Returns a touple as above.'''
        arg=get_arg(input,start)
        if arg[0].isdigit() or len(arg[0])==1:
            translation=">"+arg[0]
        else:
            translation=">"+self.translate(arg[0])+"}"
        return (translation,arg[1])

    def frac(self,input,start):
        '''Translates fractions into Nemeth.

        Returns touple as above'''
        numerator=get_arg(input,start)
        if numerator[1]<len(input):
            denominator=get_arg(input,numerator[1])
        else:
            denominator=("",numerator[1])
        if numerator[0].isdigit() and denominator[0].isdigit():
            translation=numerator[0]+"/"+denominator[0]
        else:
            translation="?"+self.translate(numerator[0])+"/"+self.translate(denominator[0])+"#"
        return (translation,denominator[1])


    def bar(self, input, start):
        '''Handles bar/overline.

        Returns toutple'''
        arg=get_arg(input,start)
        if len(arg[0])==1:
            translation=":%s" % arg[0]
        else:
            translation=":{%so" % self.translate(arg[0])
        return (translation,arg[1])







