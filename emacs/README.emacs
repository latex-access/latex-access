Emacs/Emacspeak and Braille implementation for latex-access

Daniel Dalton <daniel.dalton10@gmail.com>

ABOUT: 

Implement latex-access under gnu/emacs, so it is usable with emacs under
Linux.

The basic structure: 
* Normal behaviour takes place if latex-access is disabled. 
* Line navigation functions voice the line in question through the
latex-access translation. This means all line movement (with the arrows,
c-p, c-n, and c-e up/down in emacspeak all voice the line with the
latex-access translation.) c-u (prefix arguments are supported)
* The Braille display is updated as you type, or as the point is
moved.
* Speech is spoken via emacspeak, and Braille translation is provided in
the "other-window", that is directly to the right of your latex text. 
* Latex-access is enabled when latex-math-mode is enabled. It is
disabled when latex-math-mode is disabled. Of course you can enable/disable it
manually though. 
* Toggle of Brailling dollar signs in nemeth Braille translation (m-x toggle-latex-access-dollars-braille)
* Toggle of speaking dollar signs. (m-x toggle-latex-access-dollars-speech)
* Support for the preprocessor functions 
* Support for selecting an entire region and getting a Nemeth
translation in a separate buffer in another window. This functionality
is available from the latex-access-eq function. This is good for multi
line equations.
* Matrix support through emacspeak tables.
* Table WhereAmI functionality 

INSTALLATION:
1. Add 
(load "/path/to/emacs/emacs-latex-access.el")
in your init file eg. ~/.emacs. Replace /path/to/ with the path to where
you checked out this svn repository. 

Also set the PYTHONPATH variable eg. 

export PYTHONPATH=/path/to/latex-access 
(the path to where you checked out the svn repository), and set this
before starting emacs. Eg. place the export line in your ~/.bash_profile. 

2. Please install pymacs. 
On debian/ubuntu you may do:
sudo apt-get install pymacs 
Otherwise follow the instructions provided at:
http://pymacs.progiciels-bpi.ca/

Note: On Debian testing I believe pymacs is broken, it may still be,
hence personally I had to build from source. 


3. Restart emacs!
Now emacs should communicate correctly with latex-access.

SETTINGS

Place settings under the line 
(load "/path/to/emacs/emacs-latex-access.el")

Use (setq variable value) to set the following. 
* latex-access-use-braille -- When latex-access is initialised should
Braille support be enabled? Allowed values are nil or t 
* latex-access-use-speech -- When latex-access is initialised should
speech be enabled? Allowed values are nil and t 

HOW TO USE:

Using any form of line navigation including emacspeak commands will
speak the line in question as a latex-access translation. Braille is
updated in the other window (to the right of your latex text). You may
move the Braille display around in this window, i.e. up and down to view
previous/next line of Braille translation while keep point in the same
spot. 

Editing works as usual, just edit the LaTeX source, and of course cursor
routing works on the LaTeX source text. As you edit the echo area will
be updated with a Braille translation.

You can call most of the latex-access- functions interactively through
m-x. 

* toggle speech on/off use m-x latex-access-toggle-speech
* Toggle Braille on/off use m-x latex-access-toggle-braille 
* Or on the fly use latex-access-braille or latex-access-speech
  variables, or for perminent change set latex-access-use-* variables in
  your init file under the (load ".../emacs/emacs-latex-access.el") line.
* toggle dollars use m-x latex-access-toggle-dollars-speech or
  m-x latex-access-toggle-dollars-braille.
* The preprocessor functions are under latex-access-preprocessor-* --
  use tab completion to find out what functions are available.
* multiple line equations or just emacs regions for that matter may be
  all translated at once into a new buffer for you to review in a
  separate window to use this functionality.
1. Mark the beginning of the region with c-space (for example the
  beginning of a multiple line equation), but any region is ok. 
2. Move to the end of the area you wish to translate.
3. Type m-x latex-access-eq and review the multi line translation at
  your leasure in the buffer in other window. Note this buffer is erased
  each time latex-access-eq is called, furthermore focus is placed in
  this buffer when you call latex-access-eq.
* Matrix support, to use this functionality:
A. Mark the beginning of a Matrix (not the \begin line, but the line
  below).
B. Move the emacs point (cursor), to the end of the matrix.
C. m-x latex-access-matrix 
D. You are presented with an emacspeak table buffer. Use emacspeak table
  navigation commands to move about the matrix. Consult the emacspeak
  documentation if you are unsure, but here are a few to get you
  started:
  + up prior row 
  + down next row 
  + left backward cell 
  + right forward cell 
* c-u arguments are supported to repeat a certain command multiple
  times.
* latex-access-table-location -- Provide feedback about table location,
  and row header coordinates etc. Move to a point in the table and run m-x
  latex-access-table-location. 

UNINSTALL:

Comment or delete the (load ".../emacs/emacs-latex-access.el") line from
your init file, and any settings if you like. 

Last Updated: 21 November 2011 by Daniel Dalton
