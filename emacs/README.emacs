Linux - Emacs/Emacspeak and Braille implementation for latex-access

Daniel Dalton <daniel.dalton10@gmail.com>

ABOUT: 

Implement latex-access under gnu/emacs, so it is usable with emacs under
Linux.

The basic structure: 
* The latex-access mode can be toggled with c-x \ 
* Normal behaviour takes place if latex-access is off. 
* If latex-access is enabled, the down/up arrows and c-n, c-p keys act
in a unique way. They still navigate by line or by lines depending on
argument, but instead of emacspeak speaking the line as-is, it is passed
to latex-access and emacspeak speaks the newly generated string from
latex-access.
* The line is also passed to the nemeth translation and nemeth output is
observed in the echo area i.e. at bottom line of screen. 
* C-e tab or c-e c-i will update latex-access with current line 
A. displaying Braille in echo area.
B. Speak current line with speech translation from latex-access.
* Functions which have been created are:
1. latex-access-on - switch on latex-access 
2. latex-access-off switch off latex access.
3. toggle-latex-access togle the state of latex-access (on/off)
4. latex-access-current-line - speaks and Brailles current line by using
latex-access translation.
5. next-latex-access-line -- Go to next line speaking the translation.
6. previous-latex-access-line - Move to, and translate the prior line. 
* An autohook exists to load this functionality automatically if we are
editing a LaTeX file. 
(of course you can toggle the functionality when desired though.)

INSTALLATION:

1. I hope to build an install script to make this procedure
automatic. For now:
A. Copy the emacs-latex-access.el file found in this directory to
anywhere of your choice, I chose ~/.emacs.d/.
B. Add the following lines to ~/.emacs:
; Emacs latex-access:
(load "~/.emacs.d/emacs-latex-access.el")
(add-hook 'LaTeX-mode-hook 'latex-access-on) ; turn on when visiting a
; buffer in latex-mode 
; End emacs Latex-access.

Note, these comments will be useful for my uninstall.py script which I
will develop at some point.

C. Copy the following files to any directory the sys.path variable in
python states. I chose /usr/lib/python2.6/
* latex_access.py
* nemeth.py
* speech.py 
* speech.table 
* nemeth.table
* latex_access_emacs.py

2. Please install pymacs. 
On debian/ubuntu you may do:
sudo apt-get install pymacs 
Otherwise follow the instructions provided at:
http://pymacs.progiciels-bpi.ca/

3. Restart emacs!
Now emacs should communicate correctly with latex-access.

GENERAL NOTES:

Currently Braille will work independently of emacspeak, but to avoid errors comment out these lines from emacs-latex-access.el:
after installation if emacspeak is absent!
(dtk-speak (latex_access_emacstranssp currline)) ; Speech to pass to emacspeak
(local-set-key (kbd "C-e C-i")
'emacspeak-table-display-table-in-region) 

If Braille is absent, then possibly consider commenting out Braille to avoid annoying symbols appearing in the echo area:
This is the line to comment:
(message (latex_access_emacstransbrl currline)) ; Braille translation

HOW TO USE:

Use line up/down functions or keys in emacs which will trigger latex-access
c-e c-i or c-e tab will offer a translation of the currently selected line.

* Emacspeak will automatically voice the translation.
* Braille translation appears at bottom of screen in the echo area.
* Sometimes, for long reading I turn off cursor-tracking,
leaving my display situated at the bottom of the screen, to receive updates in nemeth Braille in real time.
* Finally, to edit just edit the actual LaTeX source, and cursor routing keys do indeed work here.
* If desired, latex-access-current-line is available as an interactive
* function through m-x and the key c-e, c-i or c-e, tab.

UNINSTALL:

I hope to soon create a script to remove  the necessary *.py files, and make necessary modification
to .emacs, but for now you shall do it manually.
Just delete the latex-access related code from .emacs:
; Emacs latex-access:
(load "~/.emacs.d/emacs-latex-access.el")
(add-hook 'LaTeX-mode-hook 'latex-access-on) ; turn on when visiting a
; buffer in latex-mode 
; End emacs Latex-access.
The .py files are very small and insignificant, but you may remove them
from /usr/lib/python2.6/ if you like. (or wherever you chose to install
them.
The emacs-latex-access.el file is also insignificant and harmless, but
may be safely removed. 

FEATURES:

1. line up/down behaviour incorperates latex-access functionality
- when var "latex-access is true (t)
2. 
A. Speech implementation
B. Braille implementation 
3. I had great trouble working with the python class directly from emacs, so I have developed a silly hack found in
latex_access_emacs.py
4. A hook if minor mode is "LaTeX-mode" shall set latex-access "t"
5. c-e,c-i and tab are attached to a latex-access call on current line.
We may need to look into better key bindings, but
I can't find many unused keys!!!
6. On any line of latex math just press c-e tab or c-e c-i to have the Braille and speech translation updated.

TODO:

- Perhaps make latex-access implementation smoother with other
- navigation functions such as move by word, char, sentense etc.
- offer an installation script 
- Offer an uninstall script.
- Make the toggle-latex-access function accept user interactive input,
- and either be a global or local modification.

BUGS:

- For some reason emacspeak says "." (dot) before the translation in speech.
I disable the Braille and this still occurs, do other users see this?
It may be something I need to investigate when time permits -- not a huge issue though.

Last Updated: 12 December 2010

