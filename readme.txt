LaTeX-access

Python scripts for processing LaTeX source into niemeth braille and
audible speech.

Scripts written by Alastair Irving: alastair.irving@sjc.ox.ac.uk

Website: http://latex-access.sourceforge.net

Overview

These scripts, written in the Python language, are designed to
process a line of LaTeX source, and translate it into Nemeth
braille on a refreshable braille display. They only handle a single
line of LaTeX at any one time, however when scrolling through a
LaTeX document the braille is refreshed on the fly.  At the present time, support
for cursor rooting facilities on braille displays is not supported,
and it is advised that the user does not try to edit documents using
the cursor rooting buttons, and should use standard arrow keys
instead. It is hoped that cursor rooting will be implemented in the future.

The scripts also translate the current line into English speech which
  is then spoken by JAWS/emacspeak/NVDA.

The Python scripts provide output in a large area of
mathematics, such as fractions, superscripts and subscripts,
calculus notation, set theory notation, and a large number of
mathematical symbols.

The scripts currently interface to Jaws for Windows, (version 5 and
above), NVDA, (2011.1 or later), as well
as emacspeak.  For documentation regarding any 
of the interfaces to the previously mentioned screen readers/editors,
please refer to any of the readme files in the appropriate folder, or consult the latex-access manual.
