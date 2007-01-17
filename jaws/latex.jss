include "hjconst.jsh"

globals int ProcessMaths,
object latex_access,
object matrix,
int row,
int column


Void Function AutoStartEvent ()
let latex_access=CreateObject ("latex_access")
EndFunction




const NumericalDenominators = "haf&third&quarter&fifth&sixth&seventh&eighth&ninth"

Int Function NextNonSpace (string text, int i)
while(SubString (text, i, 2)=="& " && i <= Stringlength(text))
let i =i +1
endwhile
return i
EndFunction




Void Function Process1argCommand (string ByRef Input, string command, string Beginning, string end, string short, int spaces)
var string output
let output = input
var int commands
let commands = StringContains (input, command)
var string space
if (spaces == true) then
let space = " "
else
let space = ""
endif
While (commands!=0)
let output = StringLeft (input, commands-1)
var int i
let i = commands+stringlength(command)
let i = NextNonSpace (input, i)
var string contents
let contents = GetContents (input, i)
if ((StringLength(contents) ==1 || IsInteger (contents)) && short !="") then
let output = output + space + short 
let output = output + space + contents
else
let output = output + space + Beginning
let output = output + space  +  contents
let output = output + space + end
endif
if SubString (input, i, 1)=="{" then
let output = output + space + stringChopLeft (input, i+stringlength(contents)+1)
else
let output = output + space + stringChopLeft (input, i+stringlength(contents)-1)
endif
let input = output
let commands= StringContains (input, command)
endwhile
return output
EndFunction

Void Function Process2ArgCommand (string ByRef input, string command, string beginning, string delimitor, string end, string short, int spaces)
var string output
let output = input
var int commands
let commands = StringContains (input, command)
var string space
if spaces == true then
let space = " "
else 
let space =""
endif
While (commands!=0)
let output = StringLeft (input, commands-1)
var int i
let i = commands+stringlength(command)
let i = nextnonspace(input,i)
var string contents
let contents = GetContents (input, i)
if SubString (input, i, 1)=="{" then
let i = i+stringlength(contents)+2
else
let i = i+stringlength(contents)
endif
let i = nextnonspace(input,i)
var string contents2
let contents2 = GetContents (input, i)
if (command=="\\frac" && spaces == true) then
let output = output + " " + ProcessFraction(contents,contents2,beginning,delimitor,end,short) + " "
else
if StringLength(contents) ==1 && StringLength(contents2)==1 && short !="" then
let output = output + space + contents + space  + short+space  + contents2 
else
let output = output + space + Beginning + space  + contents + space  + delimitor + space + contents2 + space + end
endif
endif
if SubString (input, i, 1)=="{" then
let output = output + space + stringChopLeft (input, i+stringlength(contents2)+1)
else
let output = output + space  + stringChopLeft (input, i+stringlength(contents2)-1)
endif
let commands= StringContains (output, command)
let input = output
endwhile
EndFunction

Void Function ProcessFromToCommand (string byref input, string command, string translation)
var string output
let output = input
var int commands
let commands = StringContains (input, command)
While (commands!=0)
let output = StringLeft (input, commands-1)
var int i
let i = commands+stringlength(command)
let i = NextNonSpace(input,i)
let output = output + " " + translation
if SubString (input, i, 1)== "_" then
var string contents
let i = i + 1
let contents = GetContents (input, i)
let output = output + " from " + contents
if substring(input,i,1) == "{" then
let i = i + StringLength (contents)+2
else
let i = i + stringlength(contents)
endif
endif
let i = NextNonSpace(input,i)
if substring(input,i,1) == "^" then
let i = i + 1
let contents = GetContents (input, i)
let output = output + " to " + contents
if substring(input,i,1) == "{" then
let i = i + StringLength (contents)+2
else
let i = i + stringlength(contents)
endif
endif
let output = output + " of "
let output = output + stringChopLeft (input, i-1)
let input = output
let commands= StringContains (output, command)
endwhile
return output
EndFunction

String Function GetContents (string input, int pos)
if (SubString (input, pos, 1) == "{") then
var string contents
var int j
let j = pos + 1
var int braces
let braces = 0
while (braces !=-1 && j <= Stringlength(input))
if (SubString (input, j, 1)== "{") then
let braces = braces + 1
elif (SubString (input, j, 1)== "}") then
let braces = braces - 1
endif
let j = j + 1
endwhile
let j = j-1
let contents = SubString (input, pos+1, j-pos-1)
else
let contents = SubString (input, pos, 1)
endif
return contents
EndFunction


String Function ProcessMaths (string input)
var string output
let output = input 
let output = smmReplaceSymbolsWithMarkup (output)
ProcessFromToCommand (output,"\\int","integral")
ProcessFromToCommand(output,"\\sum","sum")
Process1argCommand (output, "\\lim_", "Lim as", "then","",true )
Process1argCommand (output, "\\sqrt", "Begin root", "end root", "root",true)
Process1argCommand (output, "^", "Begin super", "End super", "To the",true)
Process1argCommand (output, "_", smmGetStartMarkupForAttributes (2049), smmGetEndMarkupForAttributes (2049), "",true)
Process1argCommand (output,"\\mathbf",smmGetStartMarkupForAttributes (3),smmGetEndMarkupForAttributes (3),"",true) 
Process1argCommand (output,"\\mathbb",smmGetStartMarkupForAttributes (3),smmGetEndMarkupForAttributes (3),"",true) 
process1argCommand(output,"\\mbox","","","",true)
Process1ArgCommand(output,"\\bar","","bar","",true)
Process1ArgCommand(output,"\\overline","","bar","",true)
;Process1argCommand (output, "\\dot", "", "dot", "", true)
Process1ArgCommand(output,"\\pmod","mod","","mod",true)
; Process1argCommand (output, "\\ddot", "", "double dot", "", true)

Process2ArgCommand(output,"\\frac","Fraction","over","End frac","over",true)
Process2ArgCommand (output, "\\colvec", "vector"+smmGetStartMarkupForAttributes (1025), smmGetEndMarkupForAttributes (1025)+smmGetStartMarkupForAttributes (2049), smmGetEndMarkupForAttributes (2049), "",true)
return output
EndFunction


Void Function SayLine ()
var string input
let input = GetLine ()
if  ProcessMaths then
if stringisblank(input) then
let input = "blank"
else
let input = ProcessMaths (input)
endif
Say (input, ot_selected_item, true)
else
SayLine ()
endif
EndFunction

String Function ProcessFraction (string numerator, string denominator, string beginning, string delimitor, string end, string short)
var string output
; process simple derivatives
if Stringlength(numerator) == 2 && StringLength(denominator) == 2 && Substring(numerator,1,1) == "d" && Substring(denominator,1,1)=="d" then
let numerator = substring(numerator,1,1) + " " + substring(numerator,2,1)
let denominator = substring(denominator,1,1) + " " + substring(denominator,2,1)
let output = numerator + " by " + denominator
; Process simple numerical fractions
elif stringlength(numerator) == 1 && stringlength(denominator) == 1 && numerator >=1 && numerator <=9 && denominator >=2 && denominator <=9 then
let output = numerator + " "
let output = output + StringSegment (NumericalDenominators, "&", StringToInt(denominator)-1)
; handle plurals
if numerator > 1 then
let output = output + "s"
endif
elif StringLength(numerator) ==1 && StringLength(denominator)==1 && short!=1 then
let output = output + " " + numerator + " " + short+" " + denominator
else
let output = output + " " + Beginning + " " + numerator + " " + delimitor + " " + denominator + " " + end
endif
return output
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
BrailleAddString (input, 0, 0, 0)
endif
return true
EndFunction

Int Function IsInteger (string number)
var int i
let i = 1
while (i <= stringlength(number))
if !(substring(number,i,1)  >= "\48" && substring(number,i,1) <="\57") then
return false
endif
let i = i +1
endwhile
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
