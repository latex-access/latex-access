JFW Script File                                                             �     autostartevent  $  initialised          latex_access      createobject    &  latex_access         &  initialised       �     $reinitialise       latex_access      createobject    &  latex_access            

 LaTeX addon has been re-initialised.     Re-initialised    saymessage        �    sayline $  processmaths            getline '      %     stringisblank       blank   '      $  latex_access      %     speech  '      %    &    &amp;     stringreplacesubstrings '      %    <sub>                
    smmgetstartmarkupforattributes    stringreplacesubstrings '      %    </sub>               
    smmgetendmarkupforattributes      stringreplacesubstrings '      %    <sup>                
    smmgetstartmarkupforattributes    stringreplacesubstrings '      %    </sup>               
    smmgetendmarkupforattributes      stringreplacesubstrings '      %    <bold>               
    smmgetstartmarkupforattributes    stringreplacesubstrings '      %    </bold>              
    smmgetendmarkupforattributes      stringreplacesubstrings '      %    <mathcal>                
    smmgetstartmarkupforattributes    stringreplacesubstrings '      %    </mathcal>               
    smmgetendmarkupforattributes      stringreplacesubstrings '         %     
          say            sayline          ,    $togglemaths    $  processmaths             &  processmaths             maths to be read as plain latex  Processing off    saymessage             &  processmaths             Maths to be processed to a more verbal form  Processing on     saymessage           <    $toggledollarsbraille   $  latex_access        toggle_dollars_nemeth   '   %     ����
             

 Dollars will now be ignored in braille   Braille Dollars off   saymessage                

 Dollars will now be shown in braille     Braille Dollars on    saymessage           8    $toggledollarsspeech    $  latex_access        toggle_dollars_speech   '   %     ����
             

 Dollars will now be ignored in speech    Speech Dollars off    saymessage                		 Dollars will now be shown in Speech  Speech Dollars on     saymessage           p    braillebuildline    $  processmaths            getline '      %    scroll down symbol         stringreplacesubstrings '      %     stringtrimtrailingblanks    '   $  latex_access      %     nemeth  '      %    _         stringreplacesubstrings '      %                       brailleaddstring                  	      �    $inputmatrix        latex_access_matrix   createobject    &  matrix       &  row      &  column  $  matrix         getselectedtext   tex_init        Initialised     '   %      $  matrix      rows      inttostring 
  '   %     by     
  '   %      $  matrix      columns   inttostring 
  '   %     matrix     
  '      %     saystring         �    $hotkeyhelp      userbufferisactive          userbufferdeactivate               (   ��Welcome to the LaTeX Access Tools for JAWS.

NOTE:  There is currently no means of independently toggling of speech and braille.

Press %KeyFor(ToggleMaths ) to toggle processing of LaTeX on and off
Press %KeyFor(ToggleDollarsBraille ) To toggle Braille dollars on or off
Press %KeyFor(ToggleDollarsSpeech ) To toggle Speech dollars on or off
Press %KeyFor(InputMatrix ) to create a matrix 

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

1 & 2\\
3 & 4\\

CTRL+SHIFT+j, K, L, or I act as arrows that navigate the matrix 
CTRL+SHIFT with a number reads that row 
cTRL+ALT with a number reads that column.  

Press %KeyFor(HotKeyHelp ) to redisplay this message   sayformattedmessage         addhotkeylinks        �     $matrixright    $  column  $  matrix      columns 
     $  column       
  &  column     $  matrix    $  row $  column    get_cell      saystring             end row   saystring            �     $matrixleft $  column       
     $  column       
  &  column     $  matrix    $  row $  column    get_cell      saystring             start row     saystring            �     $matrixdown $  row $  matrix      rows    
     $  row      
  &  row    $  matrix    $  row $  column    get_cell      saystring             end column    saystring            �     $matrixup   $  row      
     $  row      
  &  row    $  matrix    $  row $  column    get_cell      saystring             start column      saystring            �     $sayrow    %         
  # L %   $  matrix      rows    
  
        $  matrix    %          get_row   saystring             Invalid Row   saystring            �     $saycolumn     %         
  # P %   $  matrix      columns 
  
        $  matrix    %          get_col   saystring             invalid column    saystring            �    $preprocessoradd        Enter the command you wish to re-define.     Initial LaTeX   %     inputbox           Enter the number of arguments of the command.    Number of arguments %    inputbox           Enter the definition of the custom command, that is, the standard LaTeX to which it is equivalent.   Translation %    inputbox          %    stringtoint '  $  latex_access      %   %  %    preprocessor_add                   t     $preprocessorfromstring $  latex_access           getselectedtext   preprocessor_from_string          `    $preprocessorwrite      enter full filename to save to   Filename    %     inputbox          %     fileexists         The file you specified already exists.  Do you wish to replace it?   File Exists        exmessagebox    '  %       
     	         $  latex_access      %     preprocessor_write                $preprocessorread      		 enter full filename to read from     Filename    %     inputbox          %     fileexists     $  latex_access      %     preprocessor_read             File does not exist   messagebox              