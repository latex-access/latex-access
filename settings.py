# settings.py
#    A part of the latex-access project at http://latex-access.sourceforge.net/
#    Author: Alastair Irving <alastair.irving@sjc.ox.ac.uk>
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

globals
# Global settings for latex-access, these are the default values 
settings = {"brailletranslation":"True", "speechtranslation":"True","brailledollars":"True","speakdollars":"True"}

def loadSettings (file):
  """Read settings from file.

  This function reads the setting values from file. The settings are
  saved in the public dict settings. The file should be in the form
  settingname value

  Where settingname is a valid setting and value is the value of that
  setting. The file may be commented by use of ; but only at the start
  of a line! Blank lines are ignored."""

  f=open(file, "r")
  for line in f.readlines ():
    if line[0] == "\n" or line[0] == ";": # Skip some irrelevant stuff
      continue 
    words = line.split()
    if not words: # Ignore lines with spaces
      continue 
    settings[words[0]] = words[1]
  f.close()

def activateSettings (filename, instances):
  """Activate settings stored in a file.

  This function activates the settings in a file, which for the emacs
  module is ~/.latex-access.

  It also sets up the settings for active instances such as those as a
  result of the nemeth class eg. nemeth.nemeth () and sets those active
  sessions to the values specified in the config file. Note the
  activation or deactivation of speech and Braille must be controlled by
  each module independently, i.e. not here."""

  try: # Settings file doesn't necessarily exist 
    loadSettings (filename)
  except: 
    return False

# Now convert settings in the dictionary into the proper variables 

  instances["speech"].remove_dollars = not booleaniseSetting("speechdollars")
  instances["braille"].remove_dollars = not booleaniseSetting("brailledollars")
  return True # Settings activated 

def booleaniseSetting (setting):
  """Turn a setting value into a boolean type.
  
  As settings read from the config file are of type string, return a
  boolean representation of this. 'true' or 'True' = True, while any
  other string is False."""
  if str(setting).lower () == 'true':
    return True
  else:
    return False

def getSetting (setting):
  """Get the value of setting.

  This function searches for the particular setting in the settings
  dict, and if found, returns the settings' value."""

  if setting in settings.keys():
    return booleaniseSetting(settings[setting])
  else: # setting not found
    return False

