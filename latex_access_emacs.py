#!/usr/bin/python

# latex_access_emacs.py
#
# A silly hack, so we don't need to worry about classes to access the
# translate functions.
# Used for communication via pyymacs from within emacs.
# Written by Daniel Dalton <daniel.dalton10@gmail.com>

"""Silly hack to access translate functions as pymacs was difficult to
interact with classes."""

import sys
import speech
import nemeth 
import preprocessor
import table

n=nemeth.nemeth()
s=speech.speech()
p=preprocessor.preprocessor()
nc=preprocessor.newcommands(p)

if __name__ == "__main__":
  print "This is just a module."
  exit (-1)

def transbrl (arg):
  """Translate latex code into Nemeth Braile.

  Unless you are using pymacs to call this, please use the function
  nemeth.nemeth.translate() instead. Found in nemeth.py."""
  return n.translate(p.translate(arg))

def transsp (arg):
  """Translate a line of LaTeX source into understandable speech.

  Unless calling with pymacs, please use
  speech.speech.translate(). Found in speech.py."""

  return s.translate(p.translate(arg))

def toggle_dollars_nemeth ():
  """Toggle Brailling of dollar signs.

  This function negotiates the class, because pymacs is challenging to
  deal with."""
  n.remove_dollars=not n.remove_dollars
  return n.remove_dollars

def toggle_dollars_speech ():
  """Toggle the speaking of dollar signs.

  This functions is here to negotiate the class, so that pymacs isn't
  able to cause us trouble."""
  s.remove_dollars=not s.remove_dollars
  return s.remove_dollars
  



def preprocessor_add(command,args,translation_string):
  '''A function to add entries to the preprocessor'''
  p.add_from_string(command,args,translation_string)

def preprocessor_from_string(input):
  '''Adds preprocessor entries from a LaTeX string containing \newcommand.'''
  nc.translate(input)

def preprocessor_write(filename):
  p.write(filename)

def preprocessor_read(filename):
  p.read(filename)

# Export the table.* functions to emacs.
def BuildHeaderString (text):
  """Enable access to the BuildHeaderString function for table
  manipulation.

  Consult the documentation in table.py."""

  return table.BuildHeaderString (text)

def WhereAmI (row, headers):
  """Access to the WhereAmI function in table.py.

  This exports the WhereAmI function to emacs -- consult documentation
  in table.py for more details."""

  return table.WhereAmI (row, headers)

def GetTableTopRow (latextable):
  """Make the GetTopRow function available to emacs for table
  manipulation.

  Consult the documentation in table.py for details."""

  return GetTableTopRow (latextable)

def GetTableCurrentRow (latextable):
  """Get the current row of a table.

  Export this function to emacs so to allow for latex table
  accessibility."""

  return GetTableCurrentRow (latextable)
