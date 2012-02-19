#!/usr/bin/python
#    install.py
#    A part of the latex-access project at http://latex-access.sourceforge.net/
#    Author: Daniel Dalton <daniel.dalton10@gmail.com>
#    Copyright (C) 2012 Daniel Dalton/latex-access Contributors
#
#    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation;
#    either version 2 of the License, or (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#    See the GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License along with this program; if not, visit <http://www.gnu.org/licenses>

"""Latex-access installation script for unix based operating systems."""

from platform import python_version
import shutil, os, sys 

# Usage info 
usage="This script allows you to install latex-access.\n\nUsage:%s\nWhich installs to the default location, or:\n%s <dir to install latex-access to>" % (sys.argv[0], sys.argv[0])

pyver=python_version ()[:3] # the version of python 
print "Found python version %s" % (pyver)
outdir="/usr/lib/python"+pyver+"/site-packages/latex_access/"

if len(sys.argv) == 2:
  if sys.argv[1] == '-h' or sys.argv[1] == '--help':
    print usage
    exit ()
  else:
    outdir = sys.argv[1]

try:
  if not os.path.exists (outdir):
    os.mkdir (outdir)
    print "Created directory, %s" % (outdir)
except:
  print "Couldn't create directory %s." % (outdir)
  print "Installation failed."
  exit (-1)
  
print "Installing to %s." % (outdir)
  
try:
  for filename in os.listdir("latex_access/"): # copy the files into our path
    try:
      if filename[-3:] == '.py' or filename[-6:] == '.table': # we only want to install .py and .table files 
        shutil.copyfile (os.path.join("latex_access/",filename), os.path.join(outdir, filename))
        print "Copying %s" % (filename)
    except:
      continue 
  print "Copied files to %s." % (outdir)
except:
  print "Couldn't install latex-access to %s, perhaps you don't have permission?" % (outdir)
  exit (-1)
