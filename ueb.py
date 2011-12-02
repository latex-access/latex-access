# ueb.py
#    A part of the latex-access project at http://latex-access.sourceforge.net/
#    Author: Daniel Dalton <daniel.dalton10@gmail.com>
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
'''Module to provide UEB translations for the latex_access
module.'''


import latex_access
from latex_access import get_arg

class ueb(latex_access.translator):
    '''Class for ueb translations.'''

    def __init__(self):
        latex_access.translator.__init__(self)
        self.files.append("ueb.table")
        self.load_files()
        new_table={"$":self.dollar,

                   "\\dot":("","`"),"\\ddot":("","``"),
                   "^":self.super,"_":self.sub,"\\sqrt":self.sqrt,"\\frac":self.frac,
                   "\\tag":self.tag,"\\mathbf":("_",""),"\\mathbb":("_",""),"\\colvec":("{"," ","o"),"\\tcolvec":("{"," "," ","o"),"\\bar":self.bar,"\\hat":self.bar,"\\overline":self.bar}

        for (k,v) in new_table.iteritems():
            self.table[k]=v






    def super(self,input,start):
        '''Translate  superscripts into UEB.

        Returns a touple with translated string and index of
        first char after end of super.'''
        arg=get_arg(input,start)
        if len(self.translate(arg[0])) > 2:
            translation = ";9\"<" + self.translate(arg[0])+"\">"
        else:
            translation = ";9" + self.translate(arg[0])
        return (translation,arg[1])



    def sub(self,input,start):
        '''Translates ueb subscripts.

        Returns a touple, as above'''
        arg=get_arg(input,start)
        if len(arg[0]) > 1:
            translation = ";5\"<"+self.translate(arg[0]) + "\">"
        else:
            translation = ";5"+self.translate(arg[0])
        return (translation,arg[1])

    def sqrt(self,input,start):
        '''Translatesroots in latex.

        Returns a touple as above.'''
        arg=get_arg(input,start)
        translation="%"+self.translate(arg[0])+"+"
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
            translation=";("+self.translate(numerator[0])+"/"+self.translate(denominator[0])+";)"
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
        translation="  "<"+arg[0]+"">"
        return (translation,arg[1])

    def before (self, input):
        """This function is ran prior to the translation.

        Place anything here you wish to be done before the returned
        value of this function is passed to the translator."""
        return addHash (input) # Put in ueb number signs 

    def after (self,input):
        """Place any functions here that you wish to be called after the
        translation.

        This function is ran after the translation takes place to
        i.e. handle some complications of UEB."""
        out=capitalise(input) # handle caps 
        out=letterSign(out) # Put letter signs in 
        return out # Return our final translation 

def addHash (latex):
    """Put in ueb number signs.

    In UEB a hash sign is put before a set of letters indicating a
    number for instance the number 1 is written as #a so this function
    applies the rules of ueb, and puts in the relevant hash signs."""
    out=""
    approachingnumb = False # Are we inside a number 
    for x in latex: # Go through the latex source 
        if approachingnumb and not x.isdigit() and x not in '.,': # end of the number 
            out+=x
            approachingnumb = False
            continue
        if not approachingnumb and x.isdigit (): # we begin a number, insert a # sign 
            approachingnumb = True
            out+="#"+x
            continue
        out+=x # not inside a number and is not a digit, default handling 
    return out

def capitalise (input):
    """Put in capital signs in UEB translation.

    Add a dot 6 to indicate a capital letter, and make that letter lower
    case in the translation so we don't get annoying computer Braille
    dot 7..."""
    eol=False # not at end of line yet 
    incaps = False # Are we in capital mode 
    out=""
    incount=0
    for x in input: # move through input by char 
        if not x.isalpha (): # Pointless it's not a letter 
            incount +=1
            out+=x
            continue 
        if incount+1 >= len (input): # We are on the last char 
            eol=True
        if not x.islower() and not incaps: # We have capital letter
            incaps=True
            if eol: # finish end of line 
                out+=","+x.lower()
                break
            if input[incount+1].isalpha() and input[incount+1] != ' ' and not input[incount+1].islower(): # next letter is cap 
                out+=',,'+x.lower()
                incount +=1
                continue
            else: # Only this letter is cap next is not 
                out+=','+x.lower()
                incount +=1
                continue
        if x.islower (): # lower case 
            incount +=1
            out+=x
            incaps = False
            continue
        # We got nothing 
        out+=x.lower()
        incount+=1 
    return out

def letterSign (input):
    """Add UEB letter signs.

    This function adds a letter sign to stand alone letters so they are
    not confused for contractions."""
    count=0
    eol = False # end of line
    out = ""
    for x in input: # Move by char 
        if count+1 >= len (input): # we reach end of line 
            eol=True
        if not eol:
         # must be a letter, and either side space or consider caps 
            if x.isalpha () and input[count+1] == ' ' and input[count-1] in ' ,':
                if input[count-1] == ',':
                    temp = out[:-1]
                    out=temp+';,'+x # handle capital letters which need
# a letter sign 
                else:
                    out+=";"+x
                count+=1
                continue
            # Taking into account first char of line with space after it 
            if x.isalpha() and count == 0 and input[count+1] == ' ':
                out+=";"+x
                count+=1
                continue 
        else:
            out+=x
            break
        out+=x
        count+=1

    return out
