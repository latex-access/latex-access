# brltty.py
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
# This module aims to provide all the functions necessary to receive a
#    smooth LaTeX Braille translation under Linux, mainly emacs, by
#    means of brltty (brlapi python bindings)
# This module may be used by other editors, provided the relevant
#    functions can be called at the appropriate times.

"""This module provides access to relevant brltty functions for
latex-access.

The idea behind this module is to set up a handle to the Braille display
so that editors under linux can easily write a Braille translation of
LaTeX to the display. Currently this works under emacs, and the
functions would allow it to work anywhere in the console provided there
was a daemon running in the back ground passing the relevant lines of
text to the functions in the class below. For now we use emacs to pass
the lines and ultimately allow this work. This ultimately makes it
better since things like side by side windows will work correctly,
i.e. if a daemon took the current line it'll translate the window next
to our's which emacs won't do... However, this probably won't allow you
to read with the display anywhere but inside the current buffer..."""

import brlapi

class braille:
  """Establish a connection to the Braille display and allow
  manipulation.
  
  This class provides a mecagnism to easily call commonly required
  functions of the Braille display, and to send a LaTeX line to the
  display, having it translated so it looks good on the display, no
  matter the size. Takes into account where the cursor is."""

  def __init__ (self):
    """Initialise the display.
    
    Initialise and connect to the Braille display and make a local class
    handle to the display, self.b."""
    self.b = brlapi.Connection ()

  def ttyMode (self,ttynumb):
    """Put the Braille display in tty mode.

    This function puts the Braille display in tty mode so we can
    actually read and write from it."""
    return self.b.enterTtyMode (ttynumb)

  def closeDisplay (self):
    """Leave tty mode.

    Take the display out of tty mode since we are finished for now, and
    allow brltty to resume control."""
    return self.b.leaveTtyMode ()

  def segmentToBraille (self,text, point=0):
    """Decide on what section of the current line should be Brailled.

    This function decides what text should be Brailled, given the length
    of the Braille display, and the location of the cursor."""
    panning = False # Are we > display length chars into current line
    display = BrailleDisplaySize () # length of display 
    reps = 0 # How many display lengths into current line 
    while point >= display-1: # Shift the Braille window until the
      # display shows the location of point 
      panning = True # Hence we are > one dsipaly length in 
      reps+=1 # How many display lengths into line are we
      point-=display # Reduce point until it fits on display 
    if panning: # Hack to get the starting char right 
      start=reps*display-1 
    else:
      start=reps*display

    end=reps*display+display

    return text[start:end].replace ("\n", "") # Remove newline chars 

  def braille (self,text):
    """Write some text to the display.

    Use brlapi to actually write the text to the Braille display. Note
    you'll probably want to do the translation and formatting first
    otherwise it'll look terrible."""
    return self.b.writeText(text)

def BrailleDisplaySize ():
  """Return the size of a Braille display.

  Use Brlapi to figure out the size of the connected Braille display."""

  try:
    bs=brlapi.Connection() # Connect to the display.
    return int(bs.displaySize[0]) # Return the number of cells of display.
  except:
    return -1 # Either brlapi not found or display not connected
