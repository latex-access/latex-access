# ueb.py
#    A part of the latex-access project at http://latex-access.sourceforge.net/
#    Author: Daniel Dalton <daniel.dalton10@gmail.com>
#    Copyright (C) 2011 Daniel Dalton/latex-access Contributors
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
from latex_access import get_optional_arg

class ueb(latex_access.translator):
    '''Class for ueb translations.'''

    def __init__(self):
        latex_access.translator.__init__(self)
        self.files.append("ueb.table")
        self.load_files()
        new_table={"[":self.bracket,"]":self.bracket,"$":self.uebDollar,

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
        arg=get_optional_arg(input,start)
        if arg:
            translation=";;%9"+self.translate(arg[0])
            arg=get_arg(input,arg[1])
            translation+=self.translate(arg[0])+"+"
        else:
            arg=get_arg(input,start)
            translation="%"+self.translate(arg[0])+"+"
        return (translation,arg[1])

    def frac(self,input,start):
        '''Translates fractions into ueb.

        Returns touple as above'''
        numerator=get_arg(input,start)
        denominator=get_arg(input,numerator[1])

        if str(numerator[0]).replace('#','').replace('@brl@','').replace('@/brl@','').isdigit() and str(denominator[0]).replace('#', '').replace('@brl@','').replace('@/brl@','').isdigit():
            translation=numerator[0]+"/"+denominator[0]
        else: # complex fraction 
            translation = ";("+self.translate(numerator[0])+"./"+self.translate(denominator[0])+";)"
        return (translation, denominator[1])


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
        translation = ' "<'+arg[0]+'">'
        return (translation, arg[1])

    def uebDollar (self, input, start):
        """Handle dollars.

        This function uses the self.dollars method to check if dollars
    should be removed and if not change the dollar to ueb, `s."""
        translation=self.dollar(input,start)
        if translation[0]:
            translationout="`s"
        else:
            translationout = ''
        translation=(translationout, translation[1])
        return translation
    
    def before (self, input):
        """This function is ran prior to the translation.

        Place anything here you wish to be done before the returned
        value of this function is passed to the translator."""
        output=self.markInput(input)
        output=self.addHash (output) # Put in ueb number signs
        return output 

    def after (self,input):
        """Place any functions here that you wish to be called after the
        translation.

        This function is ran after the translation takes place to
        i.e. handle some complications of UEB."""
        out=self.capitalise(input) # handle caps 
        out=self.letterSign(out) # Put letter signs in
        out=self.upperNumbers(out) # upper numbers
        out=self.stripUnwantedHash (out)
        out=out.replace("@brl@","")
        out=out.replace("@/brl@","")
        out=out.replace("\\{", "_<")
        out=out.replace("\\}", "_>")
        return out # Return our final translation 

    def addHash (self,latex):
        """Put in ueb number signs.

        In UEB a hash sign is put before a set of letters indicating a
        number for instance the number 1 is written as #a so this function
        applies the rules of ueb, and puts in the relevant hash signs."""
        out=""
        approachingnumb = False # Are we inside a number
        count=0
        for x in latex: # Go through the latex source
            if approachingnumb and not x.isdigit ():
                out+=x
                approachingnumb = False
                count+=1
                continue
            if not approachingnumb and x.isdigit (): # we begin a number, insert a # sign 
                approachingnumb = True
                out+="#"+x
                count+=1
                continue
            out+=x # not inside a number and is not a digit, default handling
            count+=1
        return out

    def capitalise (self,input):
        """Put in capital signs in UEB translation.

        Add a dot 6 to indicate a capital letter, and make that letter lower
        case in the translation so we don't get annoying computer Braille
        dot 7..."""
        eol=False # not at end of line yet 
        incaps = False # Are we in capital mode 
        out=""
        incount=0
        for x in input: # move through input by char
            if incaps and not x.isupper ():
                incaps = False
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

    def letterSign (self,input):
        """Add UEB letter signs.

        This function adds a letter sign to stand alone letters so they are
        not confused for contractions."""
        letters=("a","b","c","d","e","f","g","h","i","j")
        count=0
        out = ""
        for x in input: # Move by char
            # if line is only one char long, and not alpha numeric seems
            # to cause problems
            if len(input) == 1 and not input.isalpha ():
                return input 
            # Numbers, are a special case
            # Handle lower case letters after a number. Somehow, (I
            # don't understand why), capital letters are handled fine, so
            # I won't bother writing unnecessary code:)
            if x.isalpha() and self.followsNumber (input, count) and x in letters:
                count+=1
                out+=";"+x
                continue
            # Only one char on line 
            if len(input) == 1 and x.isalpha():
                out+=';'+x
                break
            elif len(input) == 2 and x in ',' and input[1].isalpha(): # line only cap letter
                out+=";,"+input[1]
                break
            if count+1 < len (input) and count-1 >= 0: # somewhere in the line
                # Do we have a letter after some Braille punctuation
                if not self.insideMarkup (input,count) and x.isalpha () and input[count-1].isdigit ():
                    out+=";"+x
                    count+=1
                    continue
                # We have either lower or upper case letter on it's own 
                if x.isalpha () and input[count-1] in ' ,' and input[count+1] == ' ':
                    if input[count-1] == ',': # handle cap
                        if count-2 >= 0:
                            if input[count-2].isalpha ():
                                out+=x
                                count+=1
                                continue 
                        out=out[:-1]+";,"+x # Letter sign then capital sign 
                    else: # no cap to worry about 
                        out+=";"+x
                    count+=1
                    continue
            elif count-1 < 0: # first char on line 
                if input[count+1] == ' ' and x.isalpha(): # its lower case on its own
                    out+=";"+x
                    count+=1
                    continue
            elif count+1 == len(input): # last char of line
                if not self.insideMarkup (input,count) and x.isalpha () and input[count-1].isdigit ():
                    out+=";"+x
                    count+=1
                    continue
                if x.isalpha () and input[count-1] in ' ,': # it's a char
                    if input[count-1] == ' ': # lowercase on it's own
                        out+=";"+x
                    else: # Upper case
                        if count-2 >= 0:
                            if input[count-2].isalpha ():
                                out+=x
                                count+=1
                                break
                        out=out[:-1]+";,"+x
                    break

            count+=1
            out+=x
        return out

    def upperNumbers (self,input):
        """Convert all numbers to upper numbers i.e. letters.
        
        UEB demands that numbers should be in the upper cells eg. 1 = a so
        lets just do that..."""
        number = False # is it a number? 
        numbers={"0":"j","1":"a", "2":"b", "3":"c", "4":"d", "5":"e", "6":"f", "7":"g", "8":"h", "9":"i"}
        out=""
        count=0
        for x in input:
            if not self.insideMarkup (input, count):
                count+=1
                out+=x
                continue
            try:
                if x.isdigit() and input[count-2] != '.' and input[count-1] == '/':
                    count+=1
                    out+=numbers[x]
                    continue
            except:
                pass
            if x in '#':
                number=True
            elif not x.isdigit () and x not in '#':
                number=False
            if x.isdigit () and number :
                out+=numbers[x]
            else:
                out+=x
            count+=1
        return out

    def markupInsert (self, input, start, end):
        """Insert markup around relevant latex source.

        Places the text in string input[start:end] in markup and returns
        a newly formatted string of input including the additional
        markup."""
        
        output = input[:start]+"@brl@"+input[start:end]+"@/brl@"+input[end:]
        return (output, end+11)
    
    def markInput (self, input):
        """Insert markup into a string of LaTeX.

        Move through a string of latex source, and determine what
        sections of the string must be surrounded by markup, and pass
        this information to the self.markupInsert function."""
        
        i=0
        output=''
        markup = False
        start = end = -1
        while i < len(input):
            if input[i].isdigit() and not markup: # mark start of a section where markup is required 
                markup = True
                start=i
            elif markup and (input[i] == '\n' or not input[i].isdigit ()): # Mark the end of the particular section which required markup 
                markup = False
                end = i 
                tmp=self.markupInsert (input, start, end)
                input=tmp[0]
                i=tmp[1]
            i+=1
        
        if markup: # handle markup right at the end of the text 
            input= self.markupInsert (input, start, len(input))[0]

        return input

    def insideMarkup (self, input, index):
        """Are we inside markup.

        Boolean, return True if we are inside markup, otherwise return
        value is False."""

        markup=("@brl@","@/brl@")
        start=input.rfind("@brl@", 0, index+1)
        end=input.find("@/brl@", index, len(input))
        if start == -1 or end == -1:
            return False
        else:
            start+=5
            startfind=input.find("@brl@", start, end+1)
            endfind=input.find("@/brl@", start, end+1)
            if startfind == -1 and endfind == -1:
                return True
            else:
                return False

    def followsNumber (self, input, start):
        try:
            if input[start-7].isdigit () and input[start-6:start] in '@/brl@':
                return True
        except:
            pass
        return False

    
    def endNumber (self, input, start):
        if start <= 6:
            return False
        try:
            if input[start-6:start] in '@/brl@':
                return True
        except:
            pass
        return False
                    
    def stripUnwantedHash (self, input):
        """Remove unecessary hash signs.

        Remove unwanted # signs."""

        out=''
        count=0
        removehash = False
        for x in input:
            if x == '\n': # handle newline
                count+=1
                out+=x
                removehash = False
                continue 
            try: # handle simple fractions
                if self.endNumber(input, count) and x == '/':
                    removehash = True
            except:
                pass
            if self.endNumber(input, count) and x in '14':
                removehash = True
            if x == '#' and removehash:
                removehash=False
                count+=1
                continue
            out+=x
            count+=1
        return out

    def bracket (self, input, start):
        """Translate square brackets into ueb.

        It will only be called for stand-alone brackets, or unrecognised
        latex commands, so the ueb brackets won't hurt anyone."""

        if input[start-1] == '[':
            return ('.<', start)
        else:
            return ('.>', start)
    
        
