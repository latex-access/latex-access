; latex.jss
;    A part of the latex-access project at http://latex-access.sourceforge.net/
;    Author: Alastair Irving <alastair.irving@sjc.ox.ac.uk>
;    Copyright (C) 2011 Alastair Irving/latex-access Contributors
;
;    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation;
;    either version 2 of the License, or (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;    See the GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License along with this program; if not, visit <http://www.gnu.org/licenses>
;

; Documentation by:  Jose Tamayo( jtblas@hotmail.com)
;
; history of changes 
;
; Needs:
; 1.  JSM file for messages
; 2.  proper variable naming 
; 3.  intro for functions.
; 4.  Hungarian notation 
; 5.  Move all literals and messages to the latex.jsm file.

include "hjconst.jsh"
 include "latex.jsm"

globals
int initialised,
int ProcessMaths,
object latex_access,
object matrix,
int row,
int column

Void Function AutoStartEvent ()
if !initialised then

let latex_access=CreateObject (o_latexAccess)
let initialised=true
endif
EndFunction

Script reInitialise ()
let latex_access=CreateObject (o_latexAccess)
EndScript

Void Function SayLine ()
if  ProcessMaths then
var
string input
let input = GetLine ()

if StringIsBlank(input) then
let input = "blank"
else
let input = latex_access.speech(input)
let input = StringReplaceSubstrings (input, "&", "&amp;")
let input = StringReplaceSubstrings (input, "<sub>", smmGetStartMarkupForAttributes (attrib_subscript|attrib_text))
let input = StringReplaceSubstrings (input, "</sub>", smmGetEndMarkupForAttributes (attrib_subscript|attrib_text))
let input = StringReplaceSubstrings (input, "<bold>", smmGetStartMarkupForAttributes (attrib_bold|attrib_text))
let input = StringReplaceSubstrings (input, "</bold>", smmGetEndMarkupForAttributes (attrib_bold|attrib_text))
let input=StringReplaceSubstrings (input, "<mathcal>", smmGetStartMarkupForAttributes (attrib_italic|attrib_text))
let input=StringReplaceSubstrings (input, "</mathcal>", smmGetEndMarkupForAttributes (attrib_italic|attrib_text))
endif
Say (input, ot_selected_item, true)
else
SayLine ()
endif
EndFunction

Script ToggleMaths ()
if ProcessMaths then
let ProcessMaths = false
SayMessage (ot_status, msgProcessingOff_L, msgProcessingOff_S)
else
let ProcessMaths = true
SayMessage(OT_STATUS,msgProcessingOn_L,msgProcessingOn_S)
endif

EndScript

Script ToggleDollarsNemeth ()
var
int result 
let result=latex_access.toggle_dollars_nemeth()
if result==-1 then 
SayMessage (ot_status, msgNemethDollarsOff_L, msgNemethDollarsOff_S)
else
SayMessage (ot_status, msgNemethDollarsOn_L, msgNemethDollarsOn_S)
endif
EndScript

Script ToggleDollarsSpeech ()
var
int result 
let result=latex_access.toggle_dollars_speech()
if result==-1 then 
SayMessage (ot_status, msgSpeechDollarsOff_L, msgSpeechDollarsOff_S)
else
SayMessage (ot_status, msgSpeechDollarsOn_L, msgSpeechDollarsOn_S)
endif
EndScript

Int Function BrailleBuildLine ()
if  ProcessMaths then
var
string input
let input = GetLine()
let input = StringReplaceSubstrings (input, sScrollDownSymbols , "")
let input=StringTrimTrailingBlanks (input)
let input = latex_access.nemeth(input)
; now sort out bad dots 456 
let input =StringReplaceSubstrings (input, "_", "\127") 
BrailleAddString (input, 0, 0, 0)

endif
return true
EndFunction

Script InputMatrix ()
let matrix=CreateObject (o_latex_access_matrix)
let row=1
let column=1
matrix.tex_init(GetSelectedText ())
; JT:  Replace the msg var with a message variable in the latex.jsm file 
var
string msg
let msg ="Initialised "
let msg=msg+inttostring(matrix.rows)
let msg=msg+sPadBy
let msg=msg+inttostring(matrix.columns)
let msg=msg +sPadMatrix
SayString(msg)
EndScript

Script HotKeyHelp ()
; check the virtual buffer and close it if active.
If UserBufferIsActive () Then 
	UserBufferDeactivate ()
EndIf
; Display the help text when the user presses JAWSKey+H
SayFormattedMessage(OT_USER_BUFFER, msgHotKeyHelp)
AddHotKeyLinks ()
EndScript

Script MatrixRight ()
if column < matrix.columns then
let column = column+1
saystring(matrix.get_cell(row,column))
else
saystring(sEndRow)
endif
EndScript

Script MatrixLeft ()
if column > 1 then 
let column = column - 1
saystring(matrix.get_cell(row,column))
else
saystring(sStartRow)
endif
EndScript

Script MatrixDown ()
if row < matrix.rows then
let row = row+1
saystring(matrix.get_cell(row,column))
else
saystring(sEndColumn)
endif
EndScript

Script MatrixUp ()
if row > 1 then
let row = row - 1
saystring(matrix.get_cell(row,column))
else
SayString(sStartColumn)
endif
EndScript

; JT:  This variable named i must be changed
Script SayRow (int i)
if i>0 && i <= matrix.rows then 
saystring(matrix.get_row(i," "))
else 
saystring(sInvalidRow)
endif
EndScript
; JT:  Find out what i is for and replace if needed.
Script SayColumn (int i)
if i>0 && i <=matrix.columns then
saystring(matrix.get_col(i," "))
else
saystring(sInvalidColumn)
endif
EndScript

Script preprocessorAdd ()
var
string input,
int args,
string strargs,
string translation
if InputBox (sCommandToRedefine, "Initial LaTeX", input) then 
if InputBox (sEnterCommandArguments, "Number of arguments", strargs) then 
if InputBox (sEnterCustomCommandDef, "Translation", translation) then 
let args=StringToInt (strargs)
latex_access.preprocessor_add(input,args,translation)
endif
endif
endif
EndScript

Script PreprocessorFromString ()
latex_access.preprocessor_from_string(GetSelectedText ())


EndScript

Script PreprocessorWrite ()
var
string filename
if InputBox (sEnterFileToSaveTo , "Filename", filename) then 
if FileExists (filename) then 
var
int result
let result=ExMessageBox (sFileExistError , sFileExistTitle , MB_YESNO)
if result==IDNO then 
return 
endif
endif
latex_access.preprocessor_write(filename)
endif
EndScript

Script PreprocessorRead ()
var
string filename
; JT: another literal to move to the latex.jsm file.
if InputBox ("enter full filename to read from ", "Filename", filename) then 
if FileExists (filename) then 
latex_access.preprocessor_read(filename)
else
MessageBox (sFileNoExist )
endif
endif
EndScript

Script ScriptFileName()
ScriptAndAppNames (msgLatexAccess)
if ProcessMaths then
Say (msgProcessingOn_S, OT_USER_REQUESTED_INFORMATION)
else
Say (msgProcessingOff_S, OT_USER_REQUESTED_INFORMATION)
EndIf
EndScript
