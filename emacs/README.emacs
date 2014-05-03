Emacs/Emacspeak for latex-access

Daniel Dalton <daniel.dalton10@gmail.com>

ABOUT: 

Implement latex-access under gnu/emacs, so it is usable with emacs under
Linux.

Braille is now controlled directly through brltty, giving a range of
advantages. See the README.BRLTTY file in the docs directory.

The basic structure: 
LaTeX-access is now an emacs minor-mode:

Latex-access can now be enabled or disabled by typing latex-access-mode. 

* Normal behaviour takes place if latex-access-mode is disabled. 
* Line navigation functions voice the line in question through the
latex-access speech translation. This means all line movement (with the arrows,
produces a spoken latex-access translation. 
(c-u (prefix arguments are supported)
* Speech is spoken via emacspeak, 
* Toggle of speaking dollar signs. (m-x latex-access-toggle-dollars-speech)
* Support for the preprocessor functions 
* Support for selecting an entire region and getting a Nemeth
translation in a separate buffer in another window. This functionality
is available from the latex-access-eq function. This is good for multi
line equations.
* Support for the matrix processor.
* Table WhereAmI functionality 

INSTALLATION:
1. Add 
(load "/path/to/emacs/emacs-latex-access.el")
in your init file eg. ~/.emacs. Replace /path/to/ with the path to where
you checked out this svn repository. 

Next run install.py as root which is located in the root directory where
you checked out latex-access, i.e. the one above this one. 
python install.py 
Use the -h or --help arguments to find out more powerful usage
functions. 
Remember to run install.py each time you update latex-access with svn up.

Alternatively, set the PYTHONPATH variable eg. 
export PYTHONPATH=/path/to/latex-access 
(the path to where you checked out the svn repository), and set this
before starting emacs. Eg. place the export line in your ~/.bash_profile. 
This is not necessary if you ran the install.py script.

2. Please install pymacs. 
On debian/ubuntu you may do:
sudo apt-get install pymacs 
Otherwise follow the instructions provided at:
http://pymacs.progiciels-bpi.ca/
3. Restart emacs!
Now emacs should communicate correctly with latex-access.

Open a latex document and type m-x latex-access-mode <ret> and you should
hear/see the message 
"LaTeX-access mode enabled." in the echo area. 

Look at the mode line and if you see the words LaTeX-access then
latex-access is loaded correctly and working. (c-e M) will voice this
information through emacspeak. Note, it is an upper case M.

SETTINGS

Settings are now controlled by the latex-access configuration file. This
file should reside at ~/.latex-access or /etc/latex-access.conf. You can see a sample
latex-access.conf file in the root svn checkout directory - copy this to
~/.latex-access or /etc/latex-access.conf the instructions are pretty clear inside the file i.e.:
cp /path/to/svn/checkout/latex-access.conf ~/.latex-access
or
cp /path/to/svn/checkout/latex-access.conf /etc/latex-access.conf

HOW TO USE:

Using any form of line navigation including emacspeak commands will
speak the line in question as a latex-access translation. Editing works as usual, just edit the LaTeX source.

You can call most of the latex-access- functions interactively through
m-x. There are also key bindings see below. 

* toggle dollars (speech) use m-x latex-access-toggle-dollars-speech
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
  Remember to put the display into 8 dot Braille.

* Matrix support, to use this functionality:
1. Mark the beginning of a Matrix (not the \begin line, but the line
  below).
2. Move the emacs point (cursor), to the end of the matrix.
3. m-x latex-access-matrix 
4. The matrix can now be virtually navigated using the
  latex-access-matrix-up/down/left/right commands.  These are bound to
  c-c up/down/left/right.  To avoid typing c-c you can run
  latex-access-matrix-mode, bound to c-c t, after which the matrix can
  be navigated using the arrow keys.  Calling latex-access-matrix-mode
  again exits the mode and restores normal functionality.
  It should be noted that the matrix navigation is purely virtual, the cursor is not moved in the LaTeX source.
  One can therefore navigate the matrix independently of the source.  
  
* latex-access-table-location -- Provide feedback about table location,
  and row header coordinates etc. Move to a point in the table and run m-x
  latex-access-table-location. 

KEYMAPS 

Recently, now that latex-access is a minor mode we have a comprehensive
set of key bindings to execute most commonly used functions. You can
still of course, use m-x though. 


C-c d		latex-access-toggle-dollars-speech
C-c w		latex-access-table-location

NOTES:

See the todo and bugs files for todo items and bugs respectively.

UNINSTALL:

Comment or delete the (load ".../emacs/emacs-latex-access.el") line from
your init file, and any settings if you like. 

Last Updated: 17 February 2012 by Daniel Dalton
