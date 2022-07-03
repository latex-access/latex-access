# LaTeX-access

Python scripts for processing LaTeX source into Nemeth or UEB braille and
audible speech.

Scripts written by [Alastair Irving](https://github.com/ajirving)
<br>
[Website](https://latex-access.github.io/)

## Overview

These scripts, written in the Python language, are designed to
process a line of LaTeX source, and translate it into braille on a refreshable braille display. They only handle a single
line of LaTeX at any one time, however when scrolling through a
LaTeX document the braille is refreshed on the fly.  Currently, support for cursor routing buttons on Braille displays is only provided when working with Brltty under Linux.


The scripts also translate the current line into English speech wghich
  is then spoken by JAWS/emacspeak/NVDA.

The Python scripts provide output in a large area of
mathematics, such as fractions, superscripts and subscripts,
calculus notation, set theory notation, and a large number of
mathematical symbols.

The scripts currently interface to Jaws for Windows, (version 5 and
above), NVDA, (2011.1 or later), Brltty and Emacspeak.  For documentation regarding any 
of the interfaces to the previously mentioned screen readers/editors,
please refer to any of the readme files in the appropriate folder, or consult the latex-access manual.
