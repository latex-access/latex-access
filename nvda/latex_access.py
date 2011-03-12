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

class GlobalPlugin (globalPluginHandler.GlobalPlugin):
	"""main class for the global plugin, in which all key bindings/scripts and NVDA events are handled."""

	def __init__ (self):
		"""Constructor. Here we initialise what we need: we create the latex_access com object.  We interface with the matrix later."""

		super (GlobalPlugin, self).__init__ ()
		self.processMaths = False
		self.latex_access = CreateObject ("latex_access")

	def script_reportCurrentLine (self, gesture):
		"""This script reports the line that the current navigator object is focused on, and speaks/brailles it appropriately depending on the state of self.processMaths."""

		if self.processMaths:
			spokenLine = self.GetLine ()
			brailledLine = self.GetLine ()
			if not spokenLine and not brailledLine:# Is it a blank line?
				spokenLine = _("blank")
				brailledLine = _("blank")
			else:
				spokenLine = self.latex_access.speech (spokenLine)
				brailledLine = self.latex_access.nemeth (brailledLine)
			speech.speakMessage (spokenLine)
			braille.handler.message (brailledLine)

		else:
			self.SayLine ()

	script_reportCurrentLine.__doc__ = _("If latex-access translation is on, Translates the current line into nemeth braille and speech.  If translation is turned off, the current line is spoken as normal.")

	def script_toggleMaths (self, Gesture):
		"""A script to toggle the latex-access translation on or off.
		@param gesture: the gesture to be past through to NVDA (in this case, a keypress).
		@type gesture: keypress.
		"""

		if self.processMaths:# is translation on?
			self.processMaths = False
			speech.speakMessage (_("Maths to be read as plain latex"))
		else:
			self.processMaths = True
			speech.speakMessage (_("Maths to be processed to a more verbal form"))

	script_toggleMaths.__doc__ = _("Toggles the speaking of mathematical expressions as either straight latex or a more verbal rendering.")

# Useful methods:

	def GetLine (self):
		"""Retrieves the line of text that the current navigator object is focussed on, then returns it."""
		obj = api.getFocusObject()
		treeInterceptor = obj.treeInterceptor
		if hasattr (treeInterceptor, 'TextInfo') and not treeInterceptor.passThrough:
			obj = treeInterceptor
		try:
			info = obj.makeTextInfo (textInfos.POSITION_CARET)
		except (NotImplementedError, RuntimeError):
			info = obj.makeTextInfo (textInfos.POSITION_FIRST)
		info.expand (textInfos.UNIT_LINE)
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
		"kb:NVDA+upArrow": "reportCurrentLine",
	}