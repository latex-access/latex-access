;;; emacs-latex-access.el --- Latex-access implementation for emacs

;; Copyright (C) 2010,2011,2012,2013  Daniel Dalton

;; Author: Daniel Dalton <daniel.dalton10@gmail.com>
;; Keywords: Latex_access emacs implementation 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A module for gnu/emacs, which interfaces with latex-access providing
;; access to the python functions so that they can be manipulated from
;; within emacs.
;;; Code:
;; Note: pymacs is required for this to work.

; Ensure you have PYTHONPATH set correctly!

; Load pymacs, shouldn't really mater if this was done by .emacs first 
(require 'pymacs)

(pymacs-load "latex_access.latex_access_emacs" "latex_access_emacs") ; load the relevant modules 

(setq latex-access-mode nil)
(setq latex-access-matrix-mode nil)
; Voice definitions, customize these by customizing the <voice_name>-settings variable.
(if (featurep 'emacspeak) (progn 
			    (defvoice latex-access-voice-bold (list nil 1 6 6  nil)
			      "Voice used for bold commands")
			    (defvoice latex-access-voice-subscript (list nil 3 nil nil nil)
			      "Voice used for subscripts")
			    (defvoice latex-access-voice-mathcal (list nil 9  nil nil  nil)
			      "Voice used for mathcal commands")
			    
			    (setq latex-access-personality-alist (list (list "bold" 'latex-access-voice-bold)
								       (list "mathcal" 'latex-access-voice-mathcal)
								       (list "sub" 'latex-access-voice-subscript)))

; latex-access advice 
; Advise emacspeak to speak the latex-access (nicely spoken
; Mathematics), when desired.
; This will hook into the emacspeak-speak-line function, and is called
; for all line navigations, c-e l, c-e up/down,  up/down, c-p, c-n
; etc. c-u args are fully supported as the navigation is left to emacs.
			    (defadvice emacspeak-speak-line (around latex-access-speak-line)
			      "Intercept Say line function of emacspeak.
If latex-access-speech enabled, speak with speech provided by
latex-access. Otherwise pass straight through to the default
emacspeak-speak-line function. This means all line navigation with
emacs/emacspeak will call this function, hence, providing latex-access
output when applicable"
			      (make-local-variable 'latex-access-speech)
			      (when (listp arg) (setq arg (car arg )))
			      (if latex-access-mode
				  (cond 
				   ((null arg) (latex-access-speak (latex_access_emacstranssp
								    (thing-at-point 'line)))) ; Speech to pass to
				   ((> arg 0)
				    (save-excursion 
				      (let ((begposs (point))) 
					(end-of-line)
					(latex-access-speak (latex_access_emacstranssp
							     (buffer-substring-no-properties begposs (point))))))) ; Speak from (point) to end of line
				   ((< arg 0)
				    (save-excursion 
				      (let ((endposs (point))) 
					(beginning-of-line)
					(latex-access-speak (latex_access_emacstranssp
							     (buffer-substring-no-properties (point) endposs))))))) ; Speak from start of line to point 
				ad-do-it)))) ; else call default emacspeak line handler 

(defun latex-access ()
  "Set up latex-access." 
					; Enable speech (emacspeak advice)
  (if (featurep 'emacspeak) ; Load emacspeak...
      (progn 
	(ad-enable-advice 'emacspeak-speak-line 'around 'latex-access-speak-line) 
	(ad-activate 'emacspeak-speak-line))) ; Enable the advice. 
  (latex_access_emacsactivateSettings)) ; Activate user settings from file

(defun latex-access-toggle-dollars-speech ()
  "Toggle whether to speak dollar signs."
  (interactive)
  (message "Dollar signs will %s be spoken."
	   (if (latex_access_emacstoggle-dollars-speech) "not" "")))

; The preprocessor functions
(defun latex-access-preprocessor-read ()
  "Prompt user for a file and pass the path to the python function."
  (interactive)
  (let ((filename (read-file-name "Enter full
filename to read from: ")))
    (if (file-exists-p filename)
	(progn 
	  (message "Reading file %s..." filename)
	  (latex_access_emacspreprocessor-read filename))
      (error "File %s doesn't exist." filename))))

(defun latex-access-preprocessor-write ()
  "Takes user input for filename, then passes the file path to the python
function."
  (interactive)
  (let ((filename (read-file-name "Enter full
filename to save to: ")))
    (if (file-exists-p filename)
	(progn 
	  (let ((input (yes-or-no-p (concat "File " filename " exists, overwrite? "))))
	    (if input
		(progn 
		  (message "Overwriting...")
		  (latex_access_emacspreprocessor-write filename))
	      (message "Did not write to file %s!" filename))))
      (progn 
	(message "Writing to %s..." filename)
	(latex_access_emacspreprocessor-write filename)))))

(defun latex-access-preprocessor-from-string (beg end)
  "Pass the beginning and end of region to this function. Will pass the
text in region to the python processor-get-string function."
  (interactive "r") 
  (latex_access_emacspreprocessor-from-string
   (buffer-substring-no-properties beg end))
  (message "Passed region to the preprocessor"))

(defun latex-access-preprocessor-add (input strargs translation)
  "Preprocessor add function -- passes input to the python code."
  (interactive "sEnter the command you wish to re-define: 
nEnter the number of arguments of the command: 
sEnter the definition of the custom command, that is, the standard LaTex to which it is equivalent: ")
  (latex_access_emacspreprocessor-add input strargs
  translation)
  (message "Added string %s" input))

(if (featurep 'emacspeak)
    (defun latex-access-speak (text)
      "Convert the latex-access speech markup into text-properties on the
string and then speak, given speech is enabled."
      (let ((chunks (split-string text "[<>]")) ; elements of chunks with even index are latex and with odd index are speech commands.
	    (latex_chunks ())
	    (endpoints ())
	    (n 0)
	    (command)
	    (start)
	    (end))
	
	(dotimes (i (length chunks))
	  (if (= (% i 2) 0)
	      (progn (push (nth i chunks) latex_chunks)
		     (setq n (+ n (length (nth i chunks)))))
	    (push (list n (nth i chunks)) endpoints)))
	(setq text  (apply 'concat (reverse latex_chunks)))
	(setq endpoints (reverse endpoints))
	(dotimes (i (/ (length endpoints) 2))
	  (setq command (nth 1 (nth (* i 2) endpoints)))
	  (setq start (nth 0 (nth (* i 2) endpoints)))
	  (setq end (nth 0 (nth (+ (* i 2)  1) endpoints)))
	  (put-text-property start end 'personality (nth 1 (assoc command latex-access-personality-alist)) text)))
  
      (dtk-speak text)))

(defun latex-access-eq (beg end)
  "Grabs text between beg and end.
Translates all text between beg and end into Braille. Could do speech,
but this would be irritating!
For interactive input the active region is used. Feel free to use any
two points of a buffer though when calling from lisp."
  (interactive "r")
  (let ((latex-access-buff (get-buffer-create " latex-access buffer")))
    (save-excursion 
					; prepare our workspace 
      (set-buffer latex-access-buff)
      (make-local-variable 'buffer-read-only)
      (setq buffer-read-only nil)
      (erase-buffer) ; clear our workspace
      (insert "Braille translation of math in region follows:\n\n"))
    (let ((latex-access-currline (latex_access_emacstransbrl
				  (buffer-substring-no-properties beg
								  end)))) ; get the translation 
					; Write our work to the workspace buffer ready for reading in Braille
      (save-excursion
	(set-buffer latex-access-buff)
	(goto-char (point-max))
	(insert latex-access-currline) ; insert translation 
	(indent-region (point-min) (point-max) 0)
	(align-regexp (point-min) (point-max) "&" 0 0)
	(goto-char (point-min))
	(replace-string "&" " ")
	(goto-char (point-min))
	(replace-string "\\" "")))
    (switch-to-buffer-other-window latex-access-buff))
  (make-local-variable 'buffer-read-only) ; just to be safe, we don't want all buffers read-only:)
  (setq buffer-read-only t)
  (goto-char 49))

(if (featurep 'emacspeak)
    (defun latex-access-table-location ()
      "Provide information of current location in table."
      (interactive)
      (save-excursion 
	(let ((end (point)) (beg (progn (search-backward "\\begin")
					(move-end-of-line nil) (point))))
	  (let ((table (buffer-substring-no-properties beg end)))
	    (dtk-speak (latex_access_emacsWhereAmI
			(latex_access_emacsGetTableCurrentRow table)
			(latex_access_emacsBuildHeaderString
			 (latex_access_emacsGetTableTopRow table)) table)))))))

(defun latex-access-load-settings ()
  "Allow a user to load their latex-access settings stored in ~/.latex-access"
  (interactive)
  (if (latex_access_emacsactivateSettings)
      (message "Loaded settings")
    (message "No configuration file, continuing with defaults.")))

(defun latex-access-speak-line ()
  "Speak the current line via latex-access"
  (interactive)
  (dtk-speak (latex_access_emacstranssp (thing-at-point 'line))))

;;; The matrix 
(defun latex-access-matrix (beg end)
  "initialise the matrix."
  (interactive "r")
  (if (not (latex_access_emacsmatrixInit (buffer-substring-no-properties beg
								end)))
      (message "Table or matrix is not formatted correctly, all rows must have the same number of columns...")
  (message (latex_access_emacsmatrixInitialisedStats))))

(defun latex-access-matrix-up ()
  "Move up a row."
  (interactive)
  (dtk-speak (latex_access_emacstranssp (latex_access_emacsmatrixUp))))

(defun latex-access-matrix-down ()
  "Move down a row."
  (interactive)
  (dtk-speak (latex_access_emacstranssp
	      (latex_access_emacsmatrixDown))))

(defun latex-access-matrix-left ()
  "Move left a column."
  (interactive)
  (dtk-speak (latex_access_emacstranssp (latex_access_emacsmatrixLeft))))

(defun latex-access-matrix-right ()
  "Move right a column."
  (interactive)
  (dtk-speak (latex_access_emacstranssp
	      (latex_access_emacsmatrixRight))))

(defun latex-access-matrix-goto ()
  "Goto a specific cell in matrix."
  (interactive) 
  (let ((row (read-number "Enter row number: "))
	     (column (read-number "Enter column number: ")))
    (dtk-speak (latex_access_emacstranssp
		(latex_access_emacsmatrixGoto row column)))))

(define-minor-mode latex-access-matrix-mode
    "Toggle LaTeX-access matrix mode.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode.

This mode is strictly for convenience to navigate tables or matrices. It
allows the arrow keys to perform table navigation functions without
having to use the c-c prefix each time. 

\\{latex-access-matrix-mode-map}"
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " LaTeX-access-matrix"
  ;; The minor mode bindings.
  `(                                                                                                                                                      
    (,(kbd "<up>") . latex-access-matrix-up)
    (,(kbd "<down>") . latex-access-matrix-down)
    (,(kbd "<left>") . latex-access-matrix-left)
    (,(kbd "<right>") . latex-access-matrix-right)
    (,(kbd "C-m") . latex-access-matrix)
    (,(kbd "C-s") . latex-access-matrix-goto)))

(provide 'latex-access)
(latex-access) ; Set everything up

(define-minor-mode latex-access-mode
    "Toggle LaTeX-access mode.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode.

When LaTeX-access is enabled, it provides spoken
feedback for a blind user by translating LaTeX markup into Braille
mathematics and speaking the markup in a way which is much easier to
understand. It also has a number of other useful functions under the
name latex-access-*. See http://latex-access.sourceforge.net/ for details.

\\{latex-access-mode-map}"
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " LaTeX-access"
  ;; The minor mode bindings.
  `(                                                                                                                                                      
    (,(kbd "C-c d") . latex-access-toggle-dollars-speech)
    (,(kbd "C-c w") . latex-access-table-location)
    (,(kbd "C-c <up>") . latex-access-matrix-up)
    (,(kbd "C-c <down>") . latex-access-matrix-down)
    (,(kbd "C-c <left>") . latex-access-matrix-left)
    (,(kbd "C-c <right>") . latex-access-matrix-right)
    (,(kbd "C-c m") . latex-access-matrix)
    (,(kbd "C-c t") . latex-access-matrix-mode)
    (,(kbd "C-c s") . latex-access-matrix-goto)))

;;; emacs-latex-access.el ends here
