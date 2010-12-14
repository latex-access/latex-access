#! /usr/bin/python

# setup.py
#
# Automatic installer for the emacs specific code which interacts with
# latex-access.
# Daniel Dalton <daniel.dalton10@gmail.com>

import sys
import os

defaultemacs = os.path.expanduser("~/.emacs") # default location of .emacs init file
latexdir=os.path.realpath("../") # get dir above this one

def do_install(initfile):
  """Install the relevant code into .emacs init file.

  This completes the installation of latex-access under emacs. pymacs
  should still be installed by the user. The relevant code must still be
  edited manually if either emacspeak or Braille is not present."""
  byte=raw_input("Attempt to byte compile emacs-latex-access (Y/N)? ")
  ext ='.elc' # do we use compiled or source file
  if byte.lower() == 'y':
    try:
      os.system("emacs -batch -f batch-byte-compile"\
                  " emacs-latex-access.el")
    except:
      print "Could not byte compile emacs-latex-access.el, continuing"\
        " anyway with source installation.\nThis will still work fine!"
      ext ='.el'
  else: # don't compile
    ext = '.el'
    print "Not byte compiling..."
  if ext == '.elc':
    print "Byte compiled emacs-latex-access.el"

  print "Installing to %s" % (initfile)

# Code to go in .emacs
  emacscode = (
  "; Emacs latex-access:\n",
  '(setq latex-access-path "'+latexdir+'")\n',
  '(load (concat latex-access-path "/emacs/emacs-latex-access'+ext+'"))\n',
  "(add-hook 'LaTeX-mode-hook 'latex-access-on) ; turn on when visiting a\n",
  "; buffer in latex-mode \n",
  "; End emacs Latex-access.\n")

  try:
    f=open(initfile, "a") # append to user's .emacs
  except IOError:
    print "IO Error: aborting."
    exit (-1) # io error 
  print "Writing to %s" % (initfile)
  for line in emacscode:
    f.write(line) # write 1 line at a time.
  f.close () # finished accessing file
  print "Installation successful!\n\nPlease add the following line to"\
      " your ~/.bash_profile or equivalent file, before emacs is started to set the global environment"\
      " variable PYTHONPATH.\n"\
      'export PYTHONPATH=%s:"${PYTHONPATH}"\n\n'\
      "Then install pymacs and restart emacs!" % (latexdir)
  return 

if len (sys.argv) > 1: # we have args
  if sys.argv[1] == '-h' or sys.argv[1] == '--help':
    print "Usage: python %s <path_to_.emacs_file>\n\nIf no arguments"\
        " supplied use ~/.emacs.\n\npython %s\nIs usually how the script should be"\
        " invoked." % (sys.argv[0], sys.argv[0])
    exit () # clean exit
  else:
    do_install(sys.argv[1])
    exit () 
# If we get to hear, use default location.
do_install(defaultemacs)
