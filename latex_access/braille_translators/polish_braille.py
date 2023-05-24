# -*- coding: utf-8 -*-
# polish_braille.py
#    A part of the latex-access project at http://latex-access.sourceforge.net/
#    Author: Alastair Irving <alastair.irving@sjc.ox.ac.uk>
#    Translation to polish braille created by Łukasz Golonka and Jakub Lukowicz
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
'''Module to provide  translations to Polish  braille for the latex_access.'''

from __future__ import absolute_import
from latex_access import latex_access
from latex_access.latex_access import get_arg
import re


sqrt_with_two_args =re.compile(r".*\\sqrt\[(.?)\]")

class BrailleTranslator(latex_access.translator):
    '''Class for translations to braille.'''

    upperNumbers = ('j', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i')
    _LOWERED_DIGITS = {
        "0": u"⠴", "1": u"⠂", "2": u"⠆",
        "3": u"⠒", "4": u"⠲", "5": u"⠢",
        "6": u"⠖", "7": u"⠶", "8": u"⠦",
        "9": u"⠔"
    }

    def __init__(self):
        latex_access.translator.__init__(self)
        self.files.append("polish_braille.table")
        self.load_files()
        new_table={"\\dot":("","`"),"\\ddot":("","``"),
                   "^":self.super,"_":self.sub,"\\sqrt":self.sqrt,"\\frac":self.frac,"\\dfrac":self.frac,"\\tfrac":self.frac,
                   "\\mathbf":("_",""),"\\mathbb":("_",""),"\\colvec":("{"," ","o"),"\\abs":(u"⠈l",u"⠸"),"\\tcolvec":("{"," "," ","o"),
                   "\\bar":self.bar,"\\hat":self.bar,"\\widehat":self.bar,"\\overline":self.bar,",":self.comma,"\\pmod":self.pmod,"\\log":self.log}

        for number in range (0,10): # add the numbers
            new_table[str(number)]=self.numbers
        for letter in range (65,91): # Ascii upper case
            new_table ["%c" % (letter)] = self.upperLetter
        for letter in range (97,123): # Ascii, lower case
            new_table["%c" % (letter)] = self.lowerLetter

        self.table.update(new_table)

    sqrt_with_two_args =re.compile(r".*\\sqrt\[(.*)\]+.*")

    def lower_digits(self, to_lower):
        """Converts provided digits to their lowered form.
        Used for example for degrees of rotts and indexes of powers.
        """
        return u"".join((self._LOWERED_DIGITS[digit] for digit in to_lower))

    def before (self):
        """Method ran before the translator at depth = 0.

        Place anything here to do before we begin translation to polish braille. Eg. set
        variables etc. Should NOT alter the actual input at all."""
        self.lastnumber = -1 # keep track of where the last latex number # was open i.e. after a digit . or , (for Polish braille)
    
    def super(self,input,start,rting=()):
        '''Translate  superscripts into polish braille.

        Returns a tuple with translated string and index of
        first char after end of super.'''
        arg=get_arg(input,start)
        index = arg[0]
        # Handle squared, degrees  and cubed as special cases
        if index == "2":
            translation=u"⠬⠆"
        elif index == "3":
            translation=u"⠬⠒"
        elif index == r"\circ":
            translation=u"⠴"
        # Handle primes
        elif latex_access.primes.match(index):
            translation=u"⠔"*arg[0].count("\\prime")
        else:
            translation = u"ó"
            if index[0] == "-":
                translation += u"-"
                index = index[1:]  # Skip minus sign
            if index.isdigit():
                # The entire index of the power is numeric, so just lower all the digits.
                translation += self.lower_digits(index)
                # Return early - the entire index was translated.
                return (translation, arg[1])
            if rting==():
                translation+= self.translate(index)
            else:
                if not index.startswith("\\"):
                    # If index is neither numeric nor a LaTeX command,
                    # it has to be prefixed by dot six
                    # to make sure whatever follows would not be mistaken for a number.
                    # Note that we don't need to check if index was not numeric here,
                    # since numeric indexes
                    # cause this method to return earlier.
                    translation+= u"⠠"
                translation += index
                translation = self.translate(translation,(rting[0]+arg[2],rting[1]+1))
        return (translation,arg[1])

    def sub(self,input,start,rting=()):
        '''Translates subscripts to Polish braille.

        Returns a tuple, as above'''
        arg=get_arg(input,start)
        if arg[0].isdigit():
            translation = u"ą"
            translation += self.lower_digits(arg[0])
        elif not arg[0].isdigit() and len(arg[0]) ==1:
            translation = u"ą" +self.translate(arg[0])
        #With two lines below uncommented translation doesn't work as expected
        #The conditional below always executes so numbers are never lowered
        #if rting!=():
            #for j in range(len(arg[0])): self.rt.append((rting[1]+j,rting[0]+arg[2]+j))
        else:
            if rting==():
                translation = u"ą"+self.translate(arg[0]) + u"ą"
            else:
                translation = u"ą"+self.translate(arg[0],(rting[0]+arg[2],rting[1]+1)) + u"ą"
        return (translation,arg[1])

    def sqrt(self,input,start,rting=()):
        '''Translatesroots in latex.

        Returns a tuple as above.'''
        first_arg =latex_access.get_optional_arg(input, start)
        if first_arg:
            second_arg=get_arg(input, first_arg[1])
            if first_arg[0] =="2":
                translation =u"⠌⠆"
            elif first_arg[0] =="3":
                translation =u"⠌⠒"
            elif first_arg[0].lower() =="n":
                translation =u"⠌⠝"
            elif first_arg[0].lower() =="x":
                translation =u"⠌⠭"
            elif first_arg[0] =="":
                translation =u""
            else:
                translation =u"⠌"
                translation += self.lower_digits(first_arg[0])
            self.lastnumber = -1
            translation += u"ć" + self.translate(second_arg[0])
            if len(second_arg[0]) > 1 and not second_arg[0].isdigit():
                # If the root is not numeric, or longer than a single char
                # We need to indicate where exactly it ends.
                translation +=u"ę"
            return (translation,second_arg[1])
        else:
            arg=get_arg(input,start)
            second_arg=""
        if arg[0].isdigit() or len(arg[0])==1:
            translation =u"ć" +arg[0] +u""
        else:
            translation =u"ć" +self.translate(arg[0]) +u"ę"
        return (translation,arg[1])

    def frac(self,input,start,rting=()):
        '''Translates fractions into polish braille.

        Returns tuple as above'''
        numerator=get_arg(input,start)
        if numerator[1]<len(input):
            denominator=get_arg(input,numerator[1])
        else:
            denominator=("",numerator[1],numerator[1])
        if numerator[0].isdigit() and denominator[0].isdigit():
            if start > 5 and input[start-6].isdigit():
                translation = "#"
                if len(numerator[0]) <= 1:
                    translation += self.upperNumbers[int(numerator[0])]
                else:
                    for k in range(len(numerator[0])) : translation+=self.upperNumbers[int(numerator[0][k])]
            else:
                translation=numerator[0]
            translation += self.lower_digits(denominator[0])
            if rting!=():
                for j in range(len(numerator[0])): self.rt.append((rting[1]+j,rting[0]+numerator[2]+j))
                for j in range(len(denominator[0])): self.rt.append((rting[1]+j+1+len(numerator[0]),rting[0]+denominator[2]+j))
        else:
            if rting==():
                if numerator[0][0] == "-":
                    self.lastnumber=-1
                    translation=u"⠆"+"-"+self.translate(numerator[0].lstrip("-"))
                    self.lastnumber=-1
                    translation+=u"⠳"+self.translate(denominator[0])+u"⠰"
                else:
                    self.lastnumber=-1
                    translation=u"⠆"+self.translate(numerator[0])
                    self.lastnumber=-1
                    translation+=u"⠳"+self.translate(denominator[0])+u"⠰"
            else:
                self.lastnumber=-1
                transnum=self.translate(numerator[0],(rting[0]+numerator[2],rting[1]+1))
                self.lastnumber=-1
                transden=self.translate(denominator[0],(rting[0]+denominator[2],rting[1]+len(transnum)+2))
                translation=u"⠆"+transnum+u"⠳"+transden+u"⠰"
        return (translation,denominator[1])

    def bar(self, input, start,rting=()):
        '''Handles bar/overline.

        Returns tuple'''
        arg=get_arg(input,start)
        if len(arg[0])==1:
            translation=":%s" % arg[0]
            if rting!=(): self.rt.append((rting[1]+1,rting[0]+arg[2]))
        else:
            if rting==(): translation=":{%so" % self.translate(arg[0])
            else: translation=":{%so" % self.translate(arg[0],(rting[0]+arg[2],rting[1]+2))
        return (translation,arg[1])

    def log(self,input,start,rting=()):
        '''Translate logs into polish braille.

        Returns a tuple with translated string and index of
        first char after end of entire logarithm.'''
        log=get_arg(input,start)
        translation=u"⠫l"
        if len(log[0]) < 1 or log[0][0] != "_": # \log by itself 
            return (translation, log[2]) # ignore the supposed command 
        
        translation=u"⠌"
        base=get_arg(input, log[1])
        if base[0].isdigit():
            translation += self.lower_digits(base[0])
        else:
            translation+= self.translate(base[0])
        translation+=u"⠫l"
        return (translation, base[1])

    def pmod(self,input,start,rting=()):
        '''Translates modules into polish braille.

        Returns a tuple, as above'''
        arg=get_arg(input,start)
        if arg[0].isdigit():
            translation=u"⠈l"+self.translate(arg[0])+u"⠸"
            if rting!=():
                for j in range(len(arg[0])): self.rt.append((rting[1]+j,rting[0]+arg[2]+j))
        else:
            if rting==():
                translation = u"⠈l"+self.translate(arg[0]) + u"⠸"
            else:
                translation = u"⠈l"+self.translate(arg[0],(rting[0]+arg[2],rting[1]+1)) + u"⠸"
        return (translation,arg[1])

    def letterSign (self,input,start):
        '''Determines whether the letter sign is necessary. 

        Returns a boolean.'''
        lettersign = False # no lettersign yet
        if input[start].lower() in 'aiouwz' and \
                (self.lastnumber <= 0 or start != self.lastnumber) and \
                ((start > 0 and len(input)-start>1 and input[start-1] == ' ' and input[start+1] == ' ') or
                (start == 0 and len(input)>1 and input[start+1] == ' ')): # special case no letter sign required, because these letter's are used as a separate words in polish
            return lettersign
# Last character was part of a number and letters are within range a-j 
        if self.lastnumber >= 0 and start == self.lastnumber and input[start].lower() in 'abcdefghij':
            lettersign = True

        try: # white space on either side of single char 
            if start > 0 and input[start-1] == ' ' and input[start+1] == ' ':
                lettersign = True
        except:
            pass
        try: # char on it's own at beginning of line 
            if start == 0 and input[start+1] == ' ':
                lettersign = True
        except:
            pass
        try: # char on it's own at end of line 
            if input[start-1] in ' .,$' and start+1 == len(input):
                lettersign = True
        except:
            pass
        try: # some punctuation after a letter eg. A.
            if input[start+1] in '.,$' and not input[start-1].isalpha ():
                lettersign = True
        except:
            pass

        try: # Letters on the left punctuation, on the right punctuation or space 
            if input[start-1] in '.,$}' and not input[start+1].isalpha ():
                lettersign = True
        except:
            pass

        try: # Letters on the left punctuation, on the right punctuation or space 
            if input[start-1] in '}':
                lettersign = True
        except:
            pass
            
        if len (input) == 1: # char by itself on line
            lettersign = True
        return lettersign 

    def lowerLetter (self,input,start):
        '''Translates lower case letters in latex.

        Returns a tuple as above.'''
        start=start-1 # We are manipulating current char
        if self.letterSign(input,start): # add the letter sign 
          translation = u"⠠"
        else:
            translation = ''
        if start > 1 and input[start-2:start].isupper () and input[start-2:start].isalpha () and input[start].islower (): # capital letters and now we are a lower case 
            translation+=u"⠠"
        translation += input[start]
        return (translation,start+1)

    def upperLetter (self, input, start):
        '''Translates upper case letters in latex.

        Returns a tuple as above.'''
        if self.capitalisation == '8dot':
            return (input[start-1], start)
        start=start-1 # We are focused on current char 
        translation= "" # The brf translation 
        cap = True # Provide a capital sign unless special case (below)
        doublecap = False # Do we represent consecutive capital letters by ⠨⠨
        lettersign = self.letterSign (input,start) # Do we provide a lettersign (⠠)
        try: # Letter before wasn't a cap, but letter after is so start consecutive capitals (⠨⠨)
            if input[start+1].isupper () and not input[start-1].isupper ():
                doublecap = True
                cap = False # And no need for single ⠨
        except:
            pass
        try: # Handle double Cap on start of line 
            if start == 0 and input[start+1].isupper ():
                doublecap = True
                cap = False # No single cap necessary 
        except:
            pass
        # the double ⠨⠨ has already been provided for this set of consecutive capital letters
        if start > 0 and input[start-1].isupper ():
            cap = False
            doublecap =False
        if start == 0: # Handle cap at start of line 
            cap = True
        if doublecap: # we add double capital sign 
            translation += u"⠨⠨"
        elif cap: # Otherwise just add single cap sign 
            translation += u"⠨"
        translation+=input[start].lower () # Now add the lowercase equivalent to avoid dot 7 in some tables 
        return (translation, start+1)
    
    def numbers(self,input,start):
        '''Translates numbers in latex.

        Returns a tuple as above.'''
        numberstart = start-1 # since it's not a latex command we are interested in the current char
        if self.lastnumber >= 0 and numberstart == self.lastnumber:
            translation ="" #inside a  number no hash "#" sign necessary
        else: 
            translation = '#'
        numberstart+=1
        translation += self.upperNumbers[int(input[start-1])] # and get the upper number eg. 3 = c
        self.lastnumber = numberstart # Record where last number is
        return (translation, numberstart)
        
    def comma (self, input, start):
        '''Translates commas (,) in latex.

        Returns a tuple as above.'''
        if self.lastnumber >= 0 and start-1 == self.lastnumber:
            self.lastnumber = start
        translation=','
        return (translation, start)
