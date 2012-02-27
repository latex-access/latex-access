#!/usr/bin/python
# latex_record.py
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

"""This module is designed to record a whole document of latex into an
audio file.

At the current time this is just experimental. The motivation for such a
tool is so that you can listen your notes for instance on some audio
player perhaps a smart phone or mp3 player. In conjunction with
fast-forward, rewind, pause and in some cases speed control, I'm hoping
this might be useful to quickly review notes while on the train or
similar."""

from latex_access import speech
import os
import sys
import wave

if len (sys.argv) < 2:
  print "Usage: %s <path-to-latex-document.text>" % (sys.argv[0])
  exit (-1)
if not os.path.exists (sys.argv[1]):
  print "Path %s does not exist." % (sys.argv[1])
  exit (-1)
if not os.path.isfile (sys.argv[1]):
  print "%s must be a file." % (sys.argv[1])
  exit (-1)
  
def compileFiles (number):
  """Compile all the separate wav files into one file.

  Takes all lines of input which were saved into wav files and saves
  them into one file in the right order of course."""

  infiles=[]
  for name in range (0,number):
    infiles.append("temp"+str(name))

  outfile = sys.argv[1]+".wav"
  data= []
  for infile in infiles:
    w = wave.open(infile, 'rb')
    data.append( [w.getparams(), w.readframes(w.getnframes())] )
    w.close()
    os.remove (infile) 

  output = wave.open(outfile, 'wb')
  output.setparams(data[0][0])
  for item in range(0, len(data)):
    output.writeframes(data[item][1])

  output.close()

s=speech.speech ()
latexFile=open(sys.argv[1], "r")
count=0
startRecording=False
for line in latexFile.readlines ():
  if not startRecording and '\\begin{document}' not in line:
    continue
  startRecording = True
  if line == '' or line.isspace ():
    continue # ignore blank lines which produce no audio 
  os.system('espeak -s 225 "'+s.translate(line.replace("$","\\$"))+'" -w temp'+str(count))
  count+=1
  os.system ('espeak -s 500 -p 95 "newline" -w temp'+str(count))
  count+=1  

compileFiles (count)
