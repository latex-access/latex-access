JFW Script File                                                           �  �     autostartevent  $  initialised          latex_access      createobject    &  latex_access         &  initialised       �    sayline $  processmaths            getline '      %     stringisblank       blank   '      $  latex_access      %     speech  '      %    &    &amp;     stringreplacesubstrings '      %    <sub>                
    smmgetstartmarkupforattributes    stringreplacesubstrings '      %    </sub>               
    smmgetendmarkupforattributes      stringreplacesubstrings '      %    <bold>               
    smmgetstartmarkupforattributes    stringreplacesubstrings '      %    </bold>              
    smmgetendmarkupforattributes      stringreplacesubstrings '      %    <mathcal>                
    smmgetstartmarkupforattributes    stringreplacesubstrings '      %    </mathcal>               
    smmgetendmarkupforattributes      stringreplacesubstrings '         %     
          say            sayline          ,    $togglemaths    $  processmaths             &  processmaths             maths to be read as plain latex  Processing off    saymessage             &  processmaths             Maths to be processed to a more verbal form  Processing on     saymessage           8    $toggledollarsnemeth    $  latex_access        toggle_dollars_nemeth   '   %     ����
             

 Dollars will now be ignored in nemeth    nemeth dollars off    saymessage                		 Dollars will now be shown in nemeth  nemeth dollars on     saymessage           8    $toggledollarsspeech    $  latex_access        toggle_dollars_speech   '   %     ����
             

 Dollars will now be ignored in speech    speech dollars off    saymessage                		 Dollars will now be shown in speech  speech dollars on     saymessage           p    braillebuildline    $  processmaths            getline '      %    scroll down symbol         stringreplacesubstrings '      %     stringtrimtrailingblanks    '   $  latex_access      %     nemeth  '      %    _         stringreplacesubstrings '      %                       brailleaddstring                  	      �    $inputmatrix        latex_access_matrix   createobject    &  matrix       &  row      &  column  $  matrix         getselectedtext   tex_init        Initialised     '   %      $  matrix      rows      inttostring 
  '   %     by     
  '   %      $  matrix      columns   inttostring 
  '   %     matrix 
  '      %     saystring         �     $matrixright    $  column  $  matrix      columns 
     $  column       
  &  column     $  matrix    $  row $  column    get_cell      saystring             end row   saystring            �     $matrixleft $  column       
 
    $  column       
  &  column     $  matrix    $  row $  column    get_cell      saystring             start row     saystring            �     $matrixdown $  row $  matrix      rows    
     $  row      
  &  row    $  matrix    $  row $  column    get_cell      saystring             end column    saystring            �     $matrixup   $  row      
 
    $  row      
  &  row    $  matrix    $  row $  column    get_cell      saystring             start columnd     saystring            �     $sayrow    %         
 
 # L %   $  matrix      rows    
  
        $  matrix    %          get_row   saystring             Invalid row   saystring            �     $saycolumn     %         
 
 # P %   $  matrix      columns 
  
        $  matrix    %          get_col   saystring             Invalid column    saystring            �    $preprocessoradd        Enter the command you wish to re-define.     Initial LaTeX   %     inputbox           Enter the number of arguments of the command.    Number of arguments %    inputbox           Enter the definition of the custom command, that is, the standard LaTeX to which it is equivalent.   Translation %    inputbox          %    stringtoint '  $  latex_access      %   %  %    preprocessor_add                   t     $preprocessorfromstring $  latex_access           getselectedtext   preprocessor_from_string          `    $preprocessorwrite      enter full filename to save to   Filename    %     inputbox          %     fileexists         The file you specified already exists.  Do you wish to replace it?   File Exists        exmessagebox    '  %       
     	         $  latex_access      %     preprocessor_write                $preprocessorread      		 enter full filename to read from     Filename    %     inputbox          %     fileexists     $  latex_access      %     preprocessor_read             File does not exist   messagebox              