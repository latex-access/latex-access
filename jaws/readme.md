# Latex-access scripts, for JAWS for Windows 5 or later.

This readme file describes how to install and use the Latex-access JAWS scripts for JAWS for Windows, version 5 and above.

## Installation
To install the scripts on a machine running windows and Jaws version 5 or higher, do the following.
1. Download the [latest stable release of version 2 of the Python software](https://www.python.org/downloads/release/python-2718/) and install it.
   
   Note: LaTeX-access has not been tested with python 3 and probably won't work with it.
2. Download the [pywin32 package](https://sourceforge.net/projects/pywin32), ensuring you have the correct file to match your version of python. Install this.
3. Git clone or svn checkout the latest version of latex-access from SVN to a directory on your computer. Any directory will work, but for the purposes of this readme we will assume it is
   ```
   c:\latex-access
   ```
4. Copy all of the files from within the jaws folder to the folder where your jaws scripts are located, with the exception of this readme file. On the now legacy Windows XP, These are/were usually in a path such as
   ```
   c:\documents and settings\%username%\application data\freedom scientific\jaws\%jaws version%\settings\enu
   ```
   But on machines running Windows Vista and beyond, it is in
   ```
   C:\Users\Nathaniel\AppData\Roaming\Freedom Scientific\JAWS\2022\Settings\enu
   ```
   (This can be reached using the explore jaws submenu of the jaws menu in your start menu).
5. Now open a command prompt by going to run, and typing 'cmd'. Switch to the latex-access directory by typing
   ```
   cd c:\latex-access\latex_access
   ```
6. Type
   ```
   latex_access_com.py
   ```
   you should hear 'latex_access registered'. Now register the matrix processor object by typing
   ```
      matrix_processor.py
   ```
   You should hear a similar message. Exit the command prompt by typing exit.
   
   Note: the object here is to run the specified python files with python, so the above will only work if python is the default program
associated with .py files. If it is not then try
   ```
   python latex_access_com.py
   ```
   and if this fails then use the full path to your python installation,
for example
   ```
   c:\python26\python.exe latex_access_com.py
   ```
   then repeat with matrix_processor.py (in later Python versions and installations of Windows, Python may be found in the `Program Files` directory)
7. Finally, open the confignames.ini file in your jaws scripts folder using a text editor such as notepad. After the line which reads
   ```
   [ConfigNames]
   ```
   enter a new line as described below.
   
   As many different applications can be used to read or write LaTeX, the scripts have a generic name `latex.j**`. Therefore, we use this file to add an alias, so that many different applications can be used without needing to change the name of each Jaws script to match the executable name of the application used to write LaTeX. So for example, as a user of winedt, I have
   ```
   winedt=latex
   ```.
   If you use notepad to write and read LaTeX, you should add 'notepad=latex' etc. If you are one of multiple users on the computer, you may have to search to find the confignames.ini file. The file will be found within the same file structure, within the all users (or something similar) directory, within the documents and settings directory.
   
   Note: with older versions of jaws you could just enter the lines at the bottom of the file, but in more recent versions the .ini file contains more sections and the lines must be inserted in the `[ConfigNames]` section.
8. Load the relevant program for writing/reading LaTeX, and press
   ```
   CTRL+M
   ```
   to initialise the scripts. Repeat this keystroke to turn them off. You should be able to navigate the document and listen to audible speech output, as well as reading the mathematical translation on the braille display.

## Using the scripts
As stated above, the processing of LaTeX is toggled by means of
```
   CTRL+M
```. There is currently no means of toggling speech and braille independently but this is easy to implement if required. 

It is possible to toggle the speaking and/or brailling of dollar signs. The keys for this are
```
   CTRL+Shift+D
```
for speech and
```
   CTRL+D
```
for Braille.

### The Preprocessor
LaTeX enables you to define custom commands. The scripts can handle this but they must be told what the custom commands are. This is done by means of the preprocessor.

To add a command press
```
   CTRL+Shift+A
```. In the first textbox, enter the custom command, in the next enter the number of arguments, 0 if there are none, and in the 3rd box enter the translation of the custom command. The translation is the standard LaTeX equivalent of the command, using #n to denote places where the nth argument should be interpolated into the translation. The 3 textboxes correspond to the 3 arguments to the \newcommand command used to define the custom command.

When you close jaws the preprocessor entries are lost. However, you can save them to a file. The keystroke for this is
```
   CTRL+Shift+W
```, after which you must enter a full file name including path. To load saved preprocessor data the keystroke is control+shift+r. You can in fact reload multiple preprocessor files and their entries will be merged, however if the result is then saved all the entries will be saved in one file.

### The Matrix Processor
Because matrices are spread over multiple lines it can be hard to perform calculations with them using only speech and a braille display. For this reason the scripts include a matrix processor. To load a matrix into the processor, highlight its contents, (not including any `\begin` and `\end` commands), and press
```
   CTRL+Shift+M
```. For example you might highlight the following:
```tex
   1 & 2 & 3\\
   4 & 5 & 6\\
   7 & 8 & 9\\
```
Jaws should say &ldquo;initialised `m` by `n` matrix&rdquo;, where `m` is the number of rows and `n` the number of columns. The matrix is now stored in memory, and it can be navigated independently of navigation and
editing in the LaTeX document. `CTRL+Shift` in conjunction with `j`, `k`, `l` and `i` act as arrow keys enabling you to navigate the matrix entry by entry. `CTRL+Shift` with a number reads that row and `CTRL+Alt` with a number reads that column.
