#!/usr/bin/python

# latex_access_emacs.py
#
# A silly hack, so we don't need to worry about classes to access the
# translate functions.
# Used for communication via pyymacs from within emacs.
# Written by Daniel Dalton <daniel.dalton10@gmail.com>

"""Silly hack to access translate functions as pymacs was difficult to
interact with classes."""

import speech
import nemeth 
from latex_access_com import latex_access_com

n=nemeth.nemeth()
s=speech.speech()
l=latex_access_com()

def transbrl (arg):
  """Translate latex code into Nemeth Braile.

  Unless you are using pymacs to call this, please use the function
  nemeth.nemeth.translate() instead. Found in nemeth.py."""
  if (l.nemeth_translator.remove_dollars):
    return n.translate(arg).replace("$", "")
  return n.translate(arg)

def transsp (arg):
  """Translate a line of LaTeX source into understandable speech.

  Unless calling with pymacs, please use
  speech.speech.translate(). Found in speech.py."""

  if(l.speech_translator.remove_dollars):
    return s.translate(arg).replace("$", "")
  return s.translate(arg)

def toggle_dollars_nemeth ():
  """Toggle Brailling of dollar signs.

  This function negotiates the class, because pymacs is challenging to
  deal with."""

  return l.toggle_dollars_nemeth()

def toggle_dollars_speech ():
  """Toggle the speaking of dollar signs.

  This functions is here to negotiate the class, so that pymacs isn't
  able to cause us trouble."""

  return l.toggle_dollars_speech ()
