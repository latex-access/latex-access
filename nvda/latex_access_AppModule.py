#    latex_access_AppModule.py
#    A part of the latex-access project at http://latex-access.sourceforge.net/
#    Author: Nathaniel Schmidt <nathanieljsch@westnet.com.au>
#    Copyright (C) 2011 Nathaniel Schmidt/latex-access Contributors
#
#    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation;
#    either version 2 of the License, or (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#    See the GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License along with this program; if not, visit <http://www.gnu.org/licenses>

"""An AppModule for NVDA to provide optional translations of LaTeX math into nemeth braille and speech that is easier to understand, by way of latex-access.  See readme.txt for more information.

Features:
* Translating lines of LaTeX into nemeth braille and speech: under development.
* matrix browser for reading larger matrices: not completed.
* The preprocessor (support for custom defined LaTeX commands): not completed."""

from comtypes.client import *

import api
import braille, speech# for brailling/speaking messages in NVDA
import appModuleHandler
import textInfos# to get information such as the current line.

### Global variables:
### I know this is probably not the best way to do this, but I wasn't sure exactly how to successfully do it another way and keep the values of the variables
### the same throughout classes and other functions.
initialised = False
processMaths = False
latex_access = None
matrix = None
row = None
column = None
### End of global variable declarations

class AppModule (appModuleHandler.AppModule):
	"""main class for the latex-access AppModule, in which all key bindings and NVDA events are handled."""

	def __init__ (self):
		"""Constructor.  Here we initialise what we need: we use the initialised global variable, and we create the latex_access com object.  We interface with the matrix later."""
		global initialised
		global latex_access
		if not initialised:# is the latex_access com object created yet?
			latex_access = CreateObject ("latex_access")
			initialised = True

	def script_speakTranslation (self, gesture):
		# This should really be an event and not a script, but because of certain limitations at the moment with NVDA events and a problem with brailling messages,
		# we only press a keystroke to provide the translation of the current line instead of updating it all the time and toggling
		# between the translation and the default behaviour. I will fix this later on.

		global latex_access

		# get the current line:
		info = api.getFocusObject().makeTextInfo(textInfos.POSITION_CARET)
		info.expand(textInfos.UNIT_LINE)
		currentLine = info.text
		currentLine = latex_access.speech(currentLine)
		speech.speakMessage (currentLine)

	script_speakTranslation.__doc__ = _("speaks the current line that NVDA is positioned on, but translates it first into latex_access's speech")# for input help.

	def script_brailleTranslation (self, gesture):
		# same problem here, and another one added, so for now we will do the same thing like what is in script_speakTranslation ().

		global latex_access

		# get the current line:
		info = api.getFocusObject().makeTextInfo(textInfos.POSITION_CARET)
		info.expand(textInfos.UNIT_LINE)
		currentLine = info.text
		currentLine = latex_access.nemeth(currentLine)
		braille.handler.message (currentLine)

	script_brailleTranslation.__doc__ = _("brailles the current line that NVDA is focussed on, but passes it through latex_access's nemeth method so as to translate the LaTeX into nemeth braille code.")

	# For the key bindings:
	__gestures = {
		"kb:NVDA+alt+s": "speakTranslation",
		"kb:NVDA+alt+b": "brailleTranslation"
	}