#!/usr/bin/python

# uninstall_latex_access.py
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
# Simple script to remove the latex_access stuff from .emacs
# Hence, disabling latex_access
# Does not remove any of our *.py or *.el files (no files are removed at
# all)
# Daniel Dalton <daniel.dalton10@gmail.com>

import os
import sys

defaultemacs = os.path.expanduser("~/.emacs") # default location of .emacs init file
latexdir=os.path.realpath("../") # get dir above this one
def uninstall (path):
  """Uninstall latex-access.

  Edit .emacs file removing latex-access lines. If this file has been
  edited manually or latex-access is not installed, this script
  shouldn't get here."""
  newfile = [] # define the list
  begin="; Emacs latex-access:\n"
  try:
    f=open(path, "r") # read file
  except:
    print "Couldn't open file...Aborting."
    exit (-1)
  print "Loaded file into memory..."
  oldfile=f.readlines() # make list of file
  f.close() # close the file we were working with
  try:
    i=oldfile.index(begin)
  except:
    print "Can't find latex-access installation!\nOr you have touched "\
        "the file manually.\n\nAborting..."
    exit (-1) 
  print "Removing latex-access installation..."
  newfile=newfile+oldfile[:i] # get everything before our code
  newfile=newfile+oldfile[(i+6):] # get rest of file
# after our code
  # The easy bit, write to the file.
  try:
    f=open(path, "w") # rewrite with everything
  except:
    print "Error openning file for writing.Aborting (Uninstall failed)"
    exit (-1)
  print "Committing changes..."
  for x in newfile:
    f.write(x) # write a line at a time
  f.close () # done

if len (sys.argv) > 1: # we have args
  if sys.argv[1] == '-h' or sys.argv[1] == '--help':
    print "Usage: python %s <path_to_.emacs_file>\n\nIf no arguments"\
        " supplied use ~/.emacs.\n\npython %s\nIs usually how the script should be"\
        " invoked." % (sys.argv[0], sys.argv[0])
    exit () # clean exit
  elif not os.path.isfile (sys.argv[1]):
    print "%s doesn't exist.\n\nCan't possibly be a latex-access "\
        "installation.\nRun\n%s -h\nfor help." % (sys.argv[1],\
                                                    sys.argv[0])
    exit (-1) # no file 
  else:
    uninstall(sys.argv[1]) # uninstall latex-access
    exit ()

uninstall(defaultemacs) # default uninstall
