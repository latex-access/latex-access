; Created by Jose Tamayo on 2/25/2010
; this will allow for latex-access to load the messages and vars correctly.


const
o_latexAccess  = "latex_access",
o_latex_access_matrix = "latex_access_matrix",
; input variable literals 
sScrollDownSymbols = "scroll down symbol",
sPadBy = " by ", 
sPadMatrix = " matrix ",
sEndRow = "end row",
sStartRow = "start row",
sEndColumn = "end column",
sStartColumn = "start column",
sInvalidRow = "Invalid Row",
sInvalidColumn = "invalid column",
sCommandToReDefine = "Enter the command you wish to re-define.",
sEnterCommandArguments = "Enter the number of arguments of the command.",
sEnterCustomCommandDef = "Enter the definition of the custom command, that is, the standard LaTeX to which it is equivalent.",
sEnterFileToSaveTo = "enter full filename to save to",
sFileExistError = "The file you specified already exists.  Do you wish to replace it?",
sFileExistTitle = "File Exists",
sFileNoExist = "File does not exist",
;keystrokes
ks1 = "control+shift+d"  ;open default file

messages
@msgProcessingOn_S
Processing on
@@
@msgProcessingOn_L
Maths to be processed to a more verbal form
@@
@msgProcessingOff_S
Processing off
@@
@msgProcessingOff_L
maths to be read as plain latex
@@
@msgNemethDollarsOff_S
Nemeth Dollars off
@@
@msgNemethDollarsOff_L
Dollars will now be ignored in nemeth
@@
@msgNemethDollarsOn_S
Nemeth Dollars on
@@
@msgNemethDollarsOn_L
Dollars will now be shown in nemeth
@@

@msgSpeechDollarsOff_S
Speech Dollars off
@@
@msgSpeechDollarsOff_L
Dollars will now be ignored in speech
@@
@msgSpeechDollarsOn_S
Speech Dollars on
@@
@msgSpeechDollarsOn_L
Dollars will now be shown in Speech
@@


; JT HERE 
; Hot key help for latex-access
@msgHotKeyHelp
Welcome to the LaTeX Access Tools for JAWS.

NOTE:  There is currently no means of independently toggling of speech and braille.

Press %KeyFor(ToggleMaths ) to toggle processing of LaTeX on and off
Press %KeyFor(ToggleDollarsNemeth ) To toggle Nemeth dollars on or off
Press %KeyFor(ToggleDollarsSpeech ) To toggle Speech dollars on or off
Press %KeyFor(InputMatrix ) to create a custom matrix entry (see preprocessor section)

the Preprocessor 
LaTeX enables you to define custom commands.  The scripts can handle
this but they must be told what the custom commands are.  This is done
by means of the preprocessor.

Press %KeyFor(preprocessorAdd ) to add  a preprocessor command

In the first textbox, enter the custom command, in the next enter the number of arguments, 0 if there
are none, and in the 3rd box enter the translation of the custom
command.  The translation is the standard LaTeX equivalent of the
command, using #n to denote places where the nth argument should be
interpolated into the translation.  The 3 textboxes correspond to the
3 arguments to the \newcommand command used to define the custom
command.

Preprocessor commands are lost when JAWS is restarted.  You may load multiple preprocessor files.
Press %KeyFor(PreprocessorWrite ) to save the custom preprocessor commands to a file.
Press %KeyFor(PreprocessorRead ) to retreive custom preprocessor commands  previously saved

The Matrix Processor
To load a matrix into the processor, highlight its contents, (not
including any \begin and \end commands), and press %KeyFor(InputMatrix ) .  For
example you might highlight the following:

CTRL+SHIFT+j, K, L, or I act as arrows that navigate the matrix 
CTRL+SHIFT with a number reads that row 
cTRL+ALT with anumber reads that column.  

Press %KeyFor(HotKeyHelp ) to redisplay this message 

Press ESC to close this message
@@

EndMessages
