# nemeth.py
#    A part of the latex-access project at http://latex-access.sourceforge.net/
#    Author: Alastair Irving <alastair.irving@sjc.ox.ac.uk>
#    Copyright (C) 2011 Alastair Irving/latex-access Contributors
#
#    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation;
#    either version 2 of the License, or (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#    See the GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License along with this program; if not, visit <http://www.gnu.org/licenses>
#
'''Module to provide Nemeth translations for the latex_access
module.'''


import latex_access
from latex_access import get_arg

class nemeth(latex_access.translator):
    '''Class for nemeth translations.'''

    def __init__(self):
        latex_access.translator.__init__(self)
        self.files.append("nemeth.table")
        self.load_files()
        new_table={"$":self.dollar,

                   "\\dot":("","`"),"\\ddot":("","``"),
                   "^":self.super,"_":self.sub,"\\sqrt":self.sqrt,"\\frac":self.frac,
                   "\\tag":self.tag,"\\mathbf":("_",""),"\\mathbb":("_",""),"\\colvec":("{"," ","o"),"\\tcolvec":("{"," "," ","o"),"\\bar":self.bar,"\\hat":self.bar,"\\overline":self.bar}

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

    def tag(self,input,start):
        '''Translate  tags into Nemeth.

        Returns a touple with translated string and index of
        first char after end of tag.'''
        arg=get_arg(input,start)
        translation="  {"+arg[0]+"}"
        return (translation,arg[1])
