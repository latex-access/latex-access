;  latex-access scripts by Alastair Irving  ( alastairirving19@hotmail.com)
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

; to be uncommented later 
 include "latex.jsm"

; JT:  what is the purpose of initialised variable?
globals int initialised,
int ProcessMaths,
object latex_access,
object matrix,
int row,
int column


Void Function AutoStartEvent ()
if !initialised then

; JT: consider creating a constant for the latex_access object and then reference the constant instead. - done 2/25/2010
let latex_access=CreateObject (o_latexAccess)
let initialised=true
endif
EndFunction

Void Function SayLine ()
if  ProcessMaths then
; JT: what is the variable input for?
var string input
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
endif
Say (input, ot_selected_item, true)
else
SayLine ()
endif
EndFunction





Script ToggleMaths ()
if ProcessMaths then
let ProcessMaths = false
; JT : the following text content must be placed in a JSM file for consistency. - Done.
SayMessage (ot_status, msgProcessingOff_L, msgProcessingOff_S)
else
let ProcessMaths = true
; JT:  The string literal must also be placed in the latex.jsm file for better manageability.
SayMessage(OT_STATUS,msgProcessingOn_L,msgProcessingOn_S)
endif


EndScript

Script ToggleDollarsNemeth ()
var bool result 
let result=latex_access.toggle_dollars_nemeth()
if result then 
; JT:  Message to be placed in the JSM file for better manageability. - Done.
SayMessage (ot_status, msgNemethDollarsOff_L, msgNemethDollarsOff_S)
else
; JT :  This is another message that must be placed in the latex.jsm file - Done.
SayMessage (ot_status, msgNemethDollarsOn_L, msgNemethDollarsOn_S)

endif
EndScript


Script ToggleDollarsSpeech ()
var bool result 
let result=latex_access.toggle_dollars_speech()
; JT:  Two more messages that must be placed in the latex.jsm file.
if result then 
SayMessage (ot_status, msgSpeechDollarsOff_L, msgSpeechDollarsOff_S)
else
SayMessage (ot_status, msgSpeechDollarsOn_L, msgSpeechDollarsOn_S)
endif
EndScript





Int Function BrailleBuildLine ()
if  ProcessMaths then
var string input
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
; JT:  Place the object in a constant so that it can be more manageable 
let matrix=CreateObject (o_latex_access_matrix)
let row=1
let column=1
matrix.tex_init(GetSelectedText ())
; JT:  Replace the msg var with a message variable in the latex.jsm file 
var string msg
let msg ="Initialised "
let msg=msg+inttostring(matrix.rows)
; JT: another literal that must be replaced.  - Done February 21, 2011
let msg=msg+sPadBy
let msg=msg+inttostring(matrix.columns)
; JT:  another literal to be placed in latex.jsm  - Done February 21, 2011
let msg=msg +sPadMatrix
SayString(msg)
EndScript

Script HotKeyHelp ()
; check the virtual buffer and close it if active.
If UserBufferIsActive () Then 
	UserBufferDeactivate ()
EndIf
; Display the help text when the user presses JAWSKey+L
SayFormattedMessage(OT_USER_BUFFER, msgHotKeyHelp)


EndScript


Script MatrixRight ()
if column < matrix.columns then
let column = column+1
saystring(matrix.get_cell(row,column))
else
; JT:  another literal to be placed in the latex.jsm file. - Done February 21, 2011
saystring(sEndRow)
endif
EndScript


Script MatrixLeft ()
if column > 1 then 
let column = column - 1
saystring(matrix.get_cell(row,column))
else
; JT:  Another literal to be placed in latex.jsm 
saystring(sStartRow)
endif
EndScript


Script MatrixDown ()
if row < matrix.rows then
let row = row+1
saystring(matrix.get_cell(row,column))
else
; JT:  another literal to add to latex.jsm 
saystring(sEndColumn)
endif
EndScript


Script MatrixUp ()
if row > 1 then
let row = row - 1
saystring(matrix.get_cell(row,column))
else
; JT:  another latex.jsm literal to be added. - done February 21, 2011
SayString(sStartColumn)
endif
EndScript


; JT:  This variable named i must be changed
Script SayRow (int i)
if i>0 && i <= matrix.rows then 
saystring(matrix.get_row(i," "))
else 
; another literal to be moved to latex.jsm - done February 21, 2011
saystring(sInvalidRow)
endif
EndScript
; JT:  Find out what i is for and replace if needed.
Script SayColumn (int i)
if i>0 && i <=matrix.columns then
saystring(matrix.get_col(i," "))
else
; JT: literal that must be moved to the latex.jsm file 
saystring(sInvalidColumn)
endif
EndScript



Script preprocessorAdd ()
var string input, int args, string strargs, string translation
; JT:  Move this literal to the latex.jsm file.
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
var string filename
; JT:  another prompt to be moved to latex.jsm - done February 21, 2011
if InputBox (sEnterFileToSaveTo , "Filename", filename) then 
if FileExists (filename) then 
var int result
; JT:  literal string to be moved to latex.jsm  - done February 21, 2011
let result=ExMessageBox (sFileExistError , sFileExistTitle , MB_YESNO)
if result==IDNO then 
return 
endif
endif
latex_access.preprocessor_write(filename)
endif
EndScript


Script PreprocessorRead ()
var string filename
; JT: another literal to move to the latex.jsm file.
if InputBox ("enter full filename to read from ", "Filename", filename) then 
if FileExists (filename) then 
latex_access.preprocessor_read(filename)
else
; JT: move this literal to the latex.jsm file too.
MessageBox (sFileNoExist )
endif
endif
EndScript








