include "hjconst.jsh"

globals int ProcessMaths,
object latex_access,
object matrix,
int row,
int column


Void Function AutoStartEvent ()
let latex_access=CreateObject ("latex_access")
EndFunction

Void Function SayLine ()
var string input
let input = GetLine ()
if  ProcessMaths then
if stringisblank(input) then
let input = "blank"
else
let input = latex_access.speech(input)
endif
Say (input, ot_selected_item, true)
else
SayLine ()
endif
EndFunction





Script ToggleMaths ()
if ProcessMaths then
let ProcessMaths = false
SayMessage (ot_status, "maths to be read as plain latex", "Processing off")
else
let ProcessMaths = true
SayMessage(OT_STATUS,"Maths to be processed to a more verbal form","Processing on")
endif


EndScript



Int Function BrailleBuildLine ()
if  ProcessMaths then
var string input
let input = GetLine()
let input = latex_access.nemeth(input)
; BrailleAddString (SubString (input, 1, 1), 0, 0, ATTRIB_HIGHLIGHT)
; BrailleAddString (SubString (input, 2, StringLength (input)), 0, 0, 0)
BrailleAddString (input, 0, 0, 0)

endif
return true
EndFunction




Script InputMatrix ()
let matrix=CreateObject ("latex_access_matrix")
let row=1
let column=1
matrix.tex_init(GetSelectedText ())
var string msg
let msg ="Initialised "
let msg=msg+inttostring(matrix.rows)
let msg=msg+" by "
let msg=msg+inttostring(matrix.columns)
let msg=msg +" matrix"
saystring(msg)
EndScript


Script MatrixRight ()
if column < matrix.columns then
let column = column+1
saystring(matrix.get_cell(row,column))
else
saystring("end row")
endif
EndScript


Script MatrixLeft ()
if column > 1 then 
let column = column - 1
saystring(matrix.get_cell(row,column))
else
saystring("start row")
endif
EndScript


Script MatrixDown ()
if row < matrix.rows then
let row = row+1
saystring(matrix.get_cell(row,column))
else
saystring("end column")
endif
EndScript


Script MatrixUp ()
if row > 1 then
let row = row - 1
saystring(matrix.get_cell(row,column))
else
saystring("start columnd")
endif
EndScript



Script SayRow (int i)
if i>0 && i <= matrix.rows then 
saystring(matrix.get_row(i," "))
else 
saystring("Invalid row")
endif
EndScript

Script SayColumn (int i)
if i>0 && i <=matrix.columns then
saystring(matrix.get_col(i," "))
else
saystring("Invalid column")
endif
EndScript



Script preprocessorAdd ()
var string input, string translation
InputBox ("Enter the custom LaTeX you wish to re-define.", "Initial LaTeX", input)
InputBox ("Enter the definition of the custom command, that is, the standard LaTeX to which it is equivalent.", "Translation", translation)
latex_access.preprocessor_add(input,translation)
EndScript



Script PreprocessorCsv ()
var string filename
InputBox ("Enter the name of the CSV file you wish to load", "Filename", filename)
if FileExists (filename) then 
latex_access.load_csv(filename)
else 
saystring("File not found")
endif

EndScript
