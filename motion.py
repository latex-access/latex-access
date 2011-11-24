#    motion.py
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
# The purpose of this module is to provide some basic movement or motion
#    functions which can be used by a blind person under latex-access to
#    quickly move through mathematics. I intend to add more functions.

"""Some general functions which allow a blind user to more quickly move
through a latex document containing a high volume of mathematics.

These functions intend to be able to move the cursor to specific sections of
the document, for instance, the move by term functions should move to
the next/previous mathematical terms. This should save lots of reading
or arrowing..."""

import speech
import preprocessor 

s=speech.speech() 
p=preprocessor.preprocessor()

def NextTerm (line, cursor):
  """Move to the next mathematical term.

  It tries to move to the next mathematical term on the current line,
  failing this, it returns -1, otherwise if successful, it returns the
  new value where cursor should be placed in characters from the start
  of the line. 
  This function takes two arguments: line which should be of type
  string, containing only the currently focused line of latex in the
  editor.
  Cursor is an integer, and should be the number of characters the
  cursor is in from the start of the line, for instance if the cursor is
  sitting at the first character on line, that is line[0], the value of
  cursor is 1."""

# Define some symbols which can be considered term separators. 
  symbols=("+", "-", "&", "=", "<", ">", "\\leq", "\\geq")
# Next line sets up the python index which points to the character under
# the cursor which can be used to access that particular character in
# the python string. 
  charnumb = cursor-1

# Inefficient I know, but we want to find the first match of any valid
# separator
# Therefore, cycle through each character of string, onwards from where
# the cursor is situated, i.e. to the right. 
  for character in line[cursor-1:]:
    if character in symbols: # We got a match, we now have a new term
      return charnumb +2 # Return where the cursor should be moved too, i.e. just after the symbols from symbols(...)
    charnumb+=1 # no match next character 
  return -1 # No more terms to the right on current line. 

def PreviousTerm (line, cursor):
  """Move to the previous mathematical term.

  It tries to move to the previous mathematical term on the current line,
  failing this, it returns -1, otherwise if successful, it returns the
  new value where cursor should be placed in characters from the start
  of the line. 
  This function takes two arguments: line which should be of type
  string, containing only the currently focused line of latex in the
  editor.
  Cursor is an integer, and should be the number of characters the
  cursor is in from the start of the line, for instance if the cursor is
  sitting at the first character on line, that is line[0], the value of
  cursor is 1."""

# term separators 
  symbols=("+", "-", "&", "=", "<", ">", "\\leq", "\\geq")
# Where abouts in string do we start from, i.e. where the cursor is
# sitting, but subtract one since indexing in python starts at 0
  charnumb = cursor-1

  if line[charnumb] in symbols: # Are we on separator symbol 
    charnumb-=1 # Yes, we are, move back off the symbol 
  elif line[charnumb-1] in symbols: # We are on the first character of a term
    charnumb -=2 # Therefore, move back over the term separator 

  while charnumb >= 0: # Move backwards through the string 
    if line[charnumb] in symbols: # We have a match
      while charnumb >=0:
        if line[charnumb] in symbols: # we found the boundary of new term
          return charnumb+2 # includes separator symbol, put into form chars from start of line, where first char of line =1, place cursor just after separator symbol
        charnumb=-1 # go to previous char
      return charnumb +3 # After exiting loop charnumb will be -1 if we reach start of line hence +3
    charnumb-=1 # No match, move back a character and re-iterate 

  return -1 # No terms to the left of cursor 

def SpeakSegment (text, start, end):
  """Speak a particular section of a string of text.

  This function just returns a substring from the current string which
  can then be passed to the speech synthesizer to speak."""

  if end == -1: # should speak to end of list
    end=len(text)-1

  if start == -1: # Should speak from start of text
    start=0
  
  return s.translate(p.translate(text[start-1:end-1])) # Only speak the
# new term, not any parts of the following terms...
