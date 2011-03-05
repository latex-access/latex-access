#    latex_access.py
#    A part of the latex-access project at http://latex-access.sourceforge.net/
#    Author: Nathaniel Schmidt <nathanieljsch@westnet.com.au>
#    Copyright (C) 2011 Nathaniel Schmidt/latex-access Contributors
#
#    This program is free software; you can redistribute it
#    and/or modify it under the terms of the GNU General Public License as published
#    by the Free Software Foundation;
#    either version 2 of the License, or (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#    See the GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License along with this program; if not, visit <http://www.gnu.org/licenses>

"""A global plugin for NVDA to provide optional translations of LaTeX math into nemeth braille and speech that is easier to understand, by way of latex-access.  See readme.txt for more information.

Features:
	* Translating lines of LaTeX into nemeth braille and speech: under development.
	* matrix browser for reading larger matrices: not completed.
	* The preprocessor (support for custom defined LaTeX commands): not completed."""

from comtypes.client import CreateObject

import api
import braille, speech# for brailling/speaking messages in NVDA
import globalPluginHandler
import textInfos# to get information such as the current line.

### Global variables:
### I know this is probably not the best way to do this, but I wasn't sure exactly how to successfully do it another way and keep the values of the variables
### the same throughout classes and other functions.
### End of global variable declarations

class GlobalPlugin (globalPluginHandler.GlobalPlugin):
	"""main class for the global plugin, in which all key bindings/scripts and NVDA events are handled."""
	initialised = False
	processMaths = False
	latex_access = None
	currentLine = None
	matrix = None
	row = None
	column = None

	def __init__ (self):
		"""Constructor. Here we initialise what we need: we use the initialised global variable, and we create the latex_access com object.  We interface with the matrix later."""

		super (GlobalPlugin, self).__init__ ()

		if not GlobalPlugin.initialised:# is the latex_access com object created yet?
			GlobalPlugin.latex_access = CreateObject ("latex_access")
			GlobalPlugin.initialised = True

	def event_caret (self, obj, nextHandler):
		"""This event is called when the system caret moves, and it is being overidden so that latex-access speech translation can be used if the user wishes."""

		if GlobalPlugin.processMaths:
			GlobalPlugin.currentLine = self.GetLine ()
			if not GlobalPlugin.currentLine:# Is it a blank line?
				GlobalPlugin.currentLine = _("blank")
			else:
				GlobalPlugin.currentLine = GlobalPlugin.latex_access.speech (GlobalPlugin.currentLine)
			speech.speakMessage (GlobalPlugin.currentLine)

		else:
			self.SayLine ()

		nextHandler ()

	def script_toggleMaths (self, Gesture):
		"""A script to toggle the latex-access translation on or off.
		@param gesture: the gesture to be past through to NVDA (in this case, a keypress).
		@type gesture: keypress.
		"""

		if GlobalPlugin.processMaths:# is translation on?
			GlobalPlugin.processMaths = False
			speech.speakMessage (_("Maths to be read as plain latex"))
		else:
			GlobalPlugin.processMaths = True
			speech.speakMessage (_("Maths to be processed to a more verbal form"))

	script_toggleMaths.__doc__ = _("Toggles the speaking of mathematical expressions as either straight latex or a more verbal rendering.")

# Useful methods:

	def GetLine (self):
		"""Retrieves the line of text that the current navigator object is focussed on, then returns it."""
		info = api.getFocusObject().makeTextInfo(textInfos.POSITION_CARET)
		info.expand(textInfos.UNIT_LINE)
		currentLine = info.text
		return currentLine

	def SayLine (self):
		"""This function says the current line without any translation.  This is necessary so that we can return to NVDA's default behaviour when LaTeX translation is toggled off."""
		speech.speakMessage (self.GetLine())

	def BrailleLine (self):
		"""Brailles the current line.  This again is necessary so that we can return to NVDA's default behaviour."""
		braille.handler.message (self.GetLine())

	# For the key bindings:
	__gestures = {
		"kb:control+M": "toggleMaths",
	}