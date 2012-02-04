Emacs/Emacspeak and Braille implementation for latex-access

Daniel Dalton <daniel.dalton10@gmail.com>

ABOUT: 

Implement latex-access under gnu/emacs, so it is usable with emacs under
Linux.

The basic structure: 
LaTeX-access is now an emacs minor-mode:

Latex-access can now be enabled or disabled by typing latex-access-mode. 

* Normal behaviour takes place if latex-access-mode is disabled. 
* Line navigation functions voice the line in question through the
latex-access speech translation. This means all line movement (with the arrows,
produces a spoken latex-access translation. c-n and c-p act differently
though, see below for details. 
(c-u (prefix arguments are supported)
* The Braille display is updated as you type, or as the point is
moved (if you switch on latex-access-brltty-toggle-type). See the
keymaps section at the end of this README as well. 
There are also many other features to shift the Braille display around
the document. 
* Speech is spoken via emacspeak, and Braille translation is provided on
your display by brltty. 
* Toggle of Brailling dollar signs in nemeth Braille translation (m-x latex-access-toggle-dollars-braille)
* Toggle of speaking dollar signs. (m-x latex-access-toggle-dollars-speech)
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

Open a latex document and type m-x latex-access <ret> and you should
hear/see the message 
"LaTeX-access mode enabled." in the echo area. 

Look at the mode line and if you see the words LaTeX-access then
latex-access is loaded correctly and working. (c-e M) will voice this
information through emacspeak. 

SETTINGS

Settings are now controlled by the latex-access configuration file. This
file should reside at ~/.latex-access. You can see a sample
latex-access.conf file in the root svn checkout directory - copy this to
~/.latex-access the instructions are pretty clear inside the file i.e.:
cp /path/to/svn/checkout/latex-access.conf ~/.latex-access

HOW TO USE:

Using any form of line navigation including emacspeak commands will
speak the line in question as a latex-access translation. Editing works as usual, just edit the LaTeX source, and of course cursor
routing works on the LaTeX source text. When latex-access Braille mode is disabled. 

You can call most of the latex-access- functions interactively through
m-x. There are also key bindings see below. 

* toggle speech on/off use m-x latex-access-toggle-speech
* Toggle Braille on/off use m-x latex-access-toggle-braille 
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
* latex-access-table-location -- Provide feedback about table location,
  and row header coordinates etc. Move to a point in the table and run m-x
  latex-access-table-location. 

BRAILLE 

Recently a new Braille framework was introduced by means of
communicating directly with brltty via brlapi. 
The way it works:
* You may use the following functions to manipulate the display 
  + latex-access-brltty-previous-line Move the Braille window up a line, point
  is not moved. 
  + latex-access-brltty-next-line move the Braille window down a line,
  point is not moved. 
  + latex-access-brltty-pan-left Pan the Braille window to the left 
  Point is not moved. 
  + latex-access-brltty-pan-right Pan the Braille window to the right 
  Point is not moved. 
  + latex-access-brltty-goto-point Move the Braille window to the
  location of point. Point is not moved. 
  + latex-access-brltty-goto-braille-cursor Move the emacs point to
  where the Braille cursor is sitting. 
  + latex-access-brltty-switch-cursor Move the Braille window between
  the locations of the Braille and emacs cursors. (this is a toggle,
  useful for jumping back and forth)
  + latex-access-brltty-toggle-type Turn on/off Braille window follows
  the emacs point. i.e. as you type or edit. 
  + latex-access-brltty-toggle Toggle Braille mode on/off, use this to
  exit Braille mode and return to brltty. 

Notes: 
1) You do not need to run latex-access-brltty-toggle every time to start
Braille mode, it'll just start Braille mode with the current line of
text. Feel free to use previous or next line or pan left/right to enter
Braille mode and move to the desired position. 
2) You must exit Braille mode before you can use BRLTTY normally again
i.e. you can't access the console or other areas of emacs with Braille
mode enabled. This is done with latex-access-brltty-toggle 
3) Don't worry about these long commands, there are key bindings to make
navigation much easier! See below in the keymap section. 
4) The keys on the display currently do nothing in Braille mode. So for
now you'll just need to use the emacs key maps below to shift the
display, but we are investigating adding Braille display key bindings. 

KEYMAPS 

Recently, now that latex-access is a minor mode we have a comprehensive
set of key bindings to execute most commonly used functions. You can
still of course, use m-x though. 

C-b		latex-access-brltty-pan-left
C-f		latex-access-brltty-pan-right
C-n		latex-access-brltty-next-line
C-p		latex-access-brltty-previous-line
C-c C-o .. C-c C-p		latex-access-brltty-goto-point
C-c C-t		latex-access-toggle-braille
C-c C-y		latex-access-toggle-speech
C-c c		latex-access-brltty-toggle-type
C-c d		latex-access-toggle-dollars-braille
C-c f		latex-access-toggle-dollars-speech
C-c g		latex-access-brltty-switch-cursors
C-c t		latex-access-brltty-toggle
C-c w		latex-access-table-location

Some of these bindings may look strange, the idea here is that emacs
uses many of the obvious bindings already, so I've tried to put as many
functions on keys which for instance their first letter
represents. However, I've also grouped functions that are common with
each other to keys that are next to each other. After a bit of use these
key bindings should come to you naturally. 

NOTES:

See the todo and bugs files for todo items and bugs respectively.

UNINSTALL:

Comment or delete the (load ".../emacs/emacs-latex-access.el") line from
your init file, and any settings if you like. 

Last Updated: 4 February 2012 by Daniel Dalton
