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

"""
A global plugin for NVDA to provide optional translations of LaTeX math into nemeth braille and speech that is easier to understand, by way of latex-access.  See readme.txt for more information.

Features:
	* Translating lines of LaTeX into nemeth braille and speech: under development.
	* matrix browser for reading larger matrices: not completed.
	* The preprocessor (support for custom defined LaTeX commands): not completed.
"""

from comtypes.client import CreateObject
from scriptHandler import isScriptWaiting# This is needed for the code copied from EditableText

import api
import braille, speech, ui# for brailling/speaking messages in NVDA
import config
import globalPluginHandler
import NVDAObjects
import textInfos# to get information such as caret position and the current line.

class EditableText (NVDAObjects.behaviors.EditableText):
	"""
	Provides latex-access support, but makes sure this is only in edit controls.
	
	This NVDAObject overlay class is used when NVDA enters accessible Editable text, and provides the user with all the events, scripts and gestures needed to use this plugin.
	
	See the l{__gestures} dict for all the key bindings that this plugin uses.  Some may also be found in the l{GlobalPlugin} class, in the same dict.
	
	Any method beginning with event_* is an NVDA event which gets fired on other system events.
	
	Any method that begins with script_* will get executed when the required key is pressed, button on the mouse is clicked, etc.
	"""

	processMaths=False
	latex_access = CreateObject ("latex_access")	

	def _caretScriptPostMovedHelper(self, speakUnit):
		if isScriptWaiting():
			return
		try:
			info = self.makeTextInfo(textInfos.POSITION_CARET)
		except:
			return
		if config.conf["reviewCursor"]["followCaret"] and api.getNavigatorObject() is self:
			api.setReviewPosition(info.copy())
		if speakUnit==textInfos.UNIT_LINE and EditableText.processMaths:
			spokenLine = GetLine ()
			brailledLine = GetLine ()
			if not spokenLine and not brailledLine:# Is it a blank line?
				spokenLine = _("blank")
				brailledLine = _("")
			else:
				spokenLine = EditableText.latex_access.speech (spokenLine)
				brailledLine = EditableText.latex_access.nemeth (brailledLine)
			speech.speakMessage (spokenLine)
			braille.handler.message (brailledLine)
		else:
			if speakUnit:
				info.expand(speakUnit)
				speech.speakTextInfo(info, unit=speakUnit, reason=speech.REASON_CARET)

	def script_reportCurrentLine (self, gesture):
		"""
		This script reports the line that the current navigator object is focused on, and speaks/brailles it appropriately depending on the state of l{self.processMaths}.
		@param gesture: the gesture to be passed through to NVDA (in this case, a keypress).
		@type gesture: l{inputCore.InputGesture}.
		"""

		if EditableText.processMaths:
			spokenLine = GetLine ()
			brailledLine = GetLine ()
			if not spokenLine and not brailledLine:# Is it a blank line?
				spokenLine = _("blank")
				brailledLine = _("")
			else:
				spokenLine = EditableText.latex_access.speech (spokenLine)
				brailledLine = EditableText.latex_access.nemeth (brailledLine)
			speech.speakMessage (spokenLine)
			braille.handler.message (brailledLine)

		else:
			SayLine ()

	script_reportCurrentLine.__doc__ = _("If latex-access translation is on, Translates the current line into nemeth braille and speech.  If translation is turned off, the current line is spoken as normal.  If this keystroke is pressed twice, the current line is spellt out.")

	def script_toggleDollars_nemeth (self, gesture):
		"""
		Toggles the state of whether dollar signs should be brailled in nemeth LaTeX translation.
		@param gesture: the gesture to be passed through to nvda (in this case, a keypress).
		@type gesture: l{inputCore.InputGesture}
		"""

		dollars = EditableText.latex_access.toggle_dollars_nemeth ()
		if dollars == -1:
			ui.message (_("nemeth dollars off"))

		else:
			ui.message (_("nemeth dollars on"))
			EditableText.latex_access.toggle_dollars_nemeth ()

	script_toggleDollars_nemeth.__doc__ = _("Toggles the state of whether dollar signs should be brailled in nemeth LaTeX translation.")

	def script_toggleDollars_speech (self, gesture):
		"""
		Toggles the state of whether dollar signs should be spoken in speech for LaTeX translation.
		@param gesture: the gesture to be passed through to nvda (in this case, a keypress).
		@type gesture: l{inputCore.InputGesture}
		"""

		dollars = EditableText.latex_access.toggle_dollars_speech ()
		if dollars == -1:
			ui.message (_("speech dollars off"))

		else:
			ui.message (_("speech dollars on"))

	script_toggleDollars_speech.__doc__ = _("Toggles the state of whether dollar signs should be spoken in speech for LaTeX translation.")

	def script_toggleMaths (self, Gesture):
		"""A script to toggle the latex-access translation on or off.
		@param gesture: the gesture to be past through to NVDA (in this case, a keypress).
		@type gesture: l{inputCore.InputGesture}.
		"""

		if EditableText.processMaths:# is translation on?
			EditableText.processMaths = False
			ui.message (_("Maths to be read as plain latex"))
		else:
			EditableText.processMaths = True# translation was off.
			ui.message (_("Maths to be processed to a more verbal form"))

	script_toggleMaths.__doc__ = _("Toggles the speaking of mathematical expressions as either straight latex or a more verbal rendering.")

	# For the input gestures:
	__gestures = {
		"kb:control+M": "toggleMaths",
		"kb:NVDA+UpArrow": "reportCurrentLine",
		"kb:control+shift+D": "toggleDollars_speech",
		"kb:control+D": "toggleDollars_nemeth",
	}

class GlobalPlugin (globalPluginHandler.GlobalPlugin):
	"""
	main class for the global plugin, in which some key bindings/scripts and NVDA events may be handled, however most of these (for this globalPlugin at least) should be in l{EditableText}.
	"""

	def chooseNVDAObjectOverlayClasses (self, obj, clsList):
		"""
		This is for the l{EditableText} object overlay class.
		"""

		windowClassName = obj.windowClassName
		if windowClassName == "Edit":
			clsList.insert (0, EditableText)

	# For the key bindings:
	__gestures = {
	}

# Useful functions:
def GetLine ():
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

def SayLine ():
		"""This function says the current line without any translation.  This is necessary so that we can return to NVDA's default behaviour when LaTeX translation is toggled off."""
		speech.speakMessage (GetLine())

def BrailleLine ():
		"""Brailles the current line.  This again is necessary so that we can return to NVDA's default behaviour."""
		braille.handler.message (GetLine())