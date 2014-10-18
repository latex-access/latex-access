#    latex_access.py
#    A part of the latex-access project at http://latex-access.sourceforge.net/
#    Author: Nathaniel Schmidt <nathanieljsch@westnet.com.au>
#    Copyright (C) 2011 and 2012 Nathaniel Schmidt/Latex-access Contributors
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
#    You should have received a copy of the GNU General Public License along with this program; if not, visit <http://www.gnu.org/licenses/old-licenses/gpl-2.0.html>

"""
A global plugin for NVDA to provide optional translations of LaTeX math into Nemeth and UEB Braille and speech that is easier to understand, by way of latex-access.  See readme.txt for more information.

Features:
	* Translating lines of LaTeX into nemeth braille and speech - status: completed.
	* matrix browser for reading larger matrices - status: under development.
	* The preprocessor (support for custom defined LaTeX commands) - status: not completed.
	* Access to tables - status: not completed.
	* access to motion for moving quickly through mathematical terms - status: N/A.
"""

from comtypes.client import CreateObject

import api
import braille, speech, ui# for brailling/speaking messages in NVDA
import config
import controlTypes
import globalPluginHandler
import NVDAObjects
from scriptHandler import isScriptWaiting, willSayAllResume, getLastScriptRepeatCount
import review
import textInfos# to get information such as caret position and the current line.

class EditableText (NVDAObjects.behaviors.EditableText):
	"""
	Provides latex-access support, but makes sure this is only in edit controls.  The normal editableText.EditableText class is not used any more in this plugin because we need to take advantage of selection changes for the matrix processor.
	
	This NVDAObject overlay class is used when NVDA enters accessible Editable text, and provides the user with all the events, scripts and gestures needed to use this plugin.
	
	See the l{__gestures} dict for all the key bindings that this plugin uses.  Some may also be found in the l{GlobalPlugin} class, in the same dict.
	
	Any method beginning with event_* is an NVDA event which gets fired on other system events.
	
	Any method that begins with script_* will get executed when the required l{InputGesture} is pressed, E.G. if a key is pressed, button on the mouse is clicked, etc.
	"""

	processMaths = False
	latex_access = CreateObject ("latex_access")

	# For the matrix:
	matrix = None
	row = None
	column = None

	def _caretScriptPostMovedHelper(self, speakUnit, gesture, info = None):
		"""
This method ensures that LaTeX translation occurs when the system caret moves, and also makes sure that normal behaviour occurs when l{processMaths} is off.
		"""

		if isScriptWaiting ():
			return

		if not info:
			try:
				info = self.makeTextInfo (textInfos.POSITION_CARET)
			except:
				return
		review.handleCaretMove(info)
		if speakUnit == textInfos.UNIT_LINE and EditableText.processMaths and not willSayAllResume(gesture):
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
			if speakUnit and not willSayAllResume(gesture):
				info.expand(speakUnit)
				speech.speakTextInfo(info, unit=speakUnit, reason=controlTypes.REASON_CARET)
		braille.handler.handleCaretMove(self)

	def script_reportCurrentLine (self, gesture):
		"""
		This script reports the line that the current navigator object is focused on, and speaks/brailles it appropriately depending on the state of l{processMaths}.  If pressed twice quickly, the current line is spelt out.
		@param gesture: the gesture to be passed through to NVDA (in this case, a keypress).
		@type gesture: l{inputCore.InputGesture}.
		"""

		obj=api.getFocusObject()
		treeInterceptor=obj.treeInterceptor
		if hasattr(treeInterceptor,'TextInfo') and not treeInterceptor.passThrough:
			obj=treeInterceptor
		try:
			info=obj.makeTextInfo(textInfos.POSITION_CARET)
		except (NotImplementedError, RuntimeError):
			info=obj.makeTextInfo(textInfos.POSITION_FIRST)
		info.expand(textInfos.UNIT_LINE)
		if getLastScriptRepeatCount()==0:
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
				speech.speakTextInfo(info,unit=textInfos.UNIT_LINE,reason=speech.REASON_CARET)
		else:
			speech.speakSpelling(info.text)
	script_reportCurrentLine.__doc__ = _("If latex-access translation is on, Translates the current line into nemeth braille and speech.  If translation is turned off, the current line is spoken as normal.  If this keystroke is pressed twice, the current line is spellt out.")

	def script_toggleDollars_nemeth (self, gesture):
		"""
		Toggles the state of whether dollar signs should be brailled in nemeth LaTeX translation.
		@param gesture: the gesture to be passed through to nvda (in this case, a keypress).
		@type gesture: l{inputCore.InputGesture}
		"""

		dollars = EditableText.latex_access.toggle_dollars_nemeth ()
		if dollars == True:
			ui.message (_("nemeth dollars off"))
		else:
			ui.message (_("nemeth dollars on"))
	script_toggleDollars_nemeth.__doc__ = _("Toggles the state of whether dollar signs should be brailled in nemeth LaTeX translation.")

	def script_toggleDollars_speech (self, gesture):
		"""
		Toggles the state of whether dollar signs should be spoken in speech for LaTeX translation.
		@param gesture: the gesture to be passed through to nvda (in this case, a keypress).
		@type gesture: l{inputCore.InputGesture}
		"""

		dollars = EditableText.latex_access.toggle_dollars_speech ()
		if dollars == True:
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

	def script_inputMatrix (self, gesture):
		"""
		This script creates the matrix COM Object, and initialises a matrix based on the text that is currently selected.
		@param gesture: the gesture to be passed through to NVDA (in this case, a keypress).
		@type gesture: l{inputCore.InputGesture}
		"""

		EditableText.matrix = CreateObject ("latex_access_matrix")
		EditableText.row = 1
		EditableText.column = 1
		EditableText.matrix.tex_init(getSelectedText ())
		# The msg variable is here to say how many rows and columns have been initialised.
		msg = "Initialised"
		msg = msg + str (EditableText.matrix.rows)
		msg = msg + " by "
		msg = msg + str(EditableText.matrix.columns)
		msg = msg + " matrix"
		ui.message (_(msg))
	script_inputMatrix.__doc__ = _ ("Initialises a matrix.  First highlight it and then run this script to have it as an object.")

	def script_matrixRight (self, gesture):
		"""
		Moves the matrix one cell to the right.
		@param gesture: the gesture to be passed through to NVDA (in this case, a keypress).
		@type gesture: l{inputCore.InputGesture}
		"""

		if EditableText.column < EditableText.matrix.columns:
			EditableText.column = EditableText.column + 1
			ui.message (_(EditableText.matrix.get_cell(EditableText.row, EditableText.column)))
		else:
			ui.message (_("End of row"))
	script_matrixRight.__doc__ = _ ("moves the matrix cursor right and then speaks and brailles the cell.")

	def script_matrixLeft (self, gesture):
		"""
		Moves the matrix one cell to the left.
		@param gesture: the gesture to be passed through to NVDA (in this case, a keypress).
		@type gesture: l{inputCore.InputGesture}
		"""

		if EditableText.column > 1:
			EditableText.column = EditableText.column - 1
			ui.message (_ (EditableText.matrix.get_cell (EditableText.row, EditableText.column)))
		else:
			ui.message (_ ("Start of row"))
	script_matrixLeft.__doc__ = _ ("Moves the matrix cursor to the left one cell, then speaks and brailles it")

	def script_matrixDown (self, gesture):
		"""
		Moves the matrix one cell down.
		@param gesture: the gesture to be passed through to NVDA (in this case, a keypress).
		@type gesture: l{inputCore.InputGesture}
		"""

		if EditableText.row < EditableText.matrix.rows:
			EditableText.row = EditableText.row + 1
			ui.message (_ (EditableText.matrix.get_cell (EditableText.row, EditableText.column)))
		else:
			ui.message (_ ("End of column"))
	script_matrixDown.__doc__ = _ ("Moves the matrix cursor down one cell, then speaks and brailles it.")

	def script_matrixUp (self, gesture):
		"""
		Moves the matrix one cell up.
		@param gesture: the gesture to be passed through to NVDA (in this case, a keypress).
		@type gesture: l{inputCore.InputGesture}
		"""

		if EditableText.row > 1:
			EditableText.row = EditableText.row - 1
			ui.message (_ (EditableText.matrix.get_cell (EditableText.row, EditableText.column)))
		else:
			ui.message (_ ("Start of column"))
	script_matrixUp.__doc__ = _ ("Moves the matrix down one cell and then speaks and brailles it.")

	# For the input gestures:
	__gestures = {
		"kb:control+M": "toggleMaths",
		"kb:NVDA+UpArrow": "reportCurrentLine",
		"kb:control+D": "toggleDollars_nemeth",
		"kb:control+shift+D": "toggleDollars_speech",
		"kb:control+shift+M":"inputMatrix",
		"kb:control+shift+L": "matrixRight",
		"kb:control+shift+J": "matrixLeft",
		"kb:control+shift+K": "matrixDown",
		"kb:control+shift+I": "matrixUp",
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
		if windowClassName == "Edit" or windowClassName == "Scintilla":
			clsList.insert (0, EditableText)

	# For the key bindings:
	__gestures = {
	}

# Useful functions:
def GetLine ():
	"""Retrieves the line of text that the current navigator object is focussed on, then returns it.
	@tryutndz; The current line under the cursor.
	@rtype: STR
	"""

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

def getSelectedText ():
	"""
	Retrieves, then returns the currently selected text.
	@returns: The text currently selected.
	@rtype: str
	"""

	obj = api.getFocusObject ()
	treeInterceptor = obj.treeInterceptor
	if hasattr (treeInterceptor, 'TextInfo') and not treeInterceptor.passThrough:
		obj = treeInterceptor
	try:
		info = obj.makeTextInfo(textInfos.POSITION_SELECTION)
	except (RuntimeError, NotImplementedError):
		info = None
	if not info or info.isCollapsed:
		return None
	else:
		return info.text
