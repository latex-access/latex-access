;;; emacs-latex-access.el --- Latex-access implementation for emacs

;; Copyright (C) 2010  Daniel Dalton

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
(defcustom latex-access-linesabove 0
"This variable determines how many lines above the currently selected
line should be passed to the latex-access translator and Brailled. Set to 0 for just
the current line, 1 for the current line as well as the line above etc.") ; Set this to how many lines above the current one
					; you want Brailled! I find 1 useful for solving equations.
(pymacs-load "latex_access_emacs" "latex_access_emacs") ; load the
					; relevant modules 
(setq latex-access nil) ; set initial global value 
(global-set-key (kbd "C-x \\") 'toggle-latex-access) ; key binding for toggle

; latex-access advice 
; Advise emacspeak to speak the latex-access (nicely spoken
; Mathematics), when desired.
; This will hook into the emacspeak-speak-line function, and is called
; for all line navigations, c-e l, c-e up/down,  up/down, c-p, c-n
; etc. c-u args are fully supported as the navigation is left to emacs.

(defadvice emacspeak-speak-line (around latex-access-speak-line)
  "Intercept Say line function of emacspeak.
If latex-access enabled, speak with speech provided by
latex-access. Otherwise pass straight through to the default
emacspeak-speak-line function. This means all line navigation with
emacs/emacspeak will call this function, hence, providing latex-access
output when applicable"
  (make-local-variable 'latex-access)
  (if latex-access
      (dtk-speak (latex_access_emacstranssp (thing-at-point 'line))) ; Speech to pass to
    ad-do-it) ; else call default emacspeak line handler 
  )

(defun latex-access-off ()
  "Turn off latex-access."
  (interactive)
  (make-local-variable 'latex-access)
  (setq latex-access nil)
					; Comment these for now.
					; They should only be used if latex-access is disabled globally.
					; Code in advice determines how to act.
					; This code will be used when we have global change available.
					;  (ad-disable-advice 'emacspeak-speak-line 'around 'latex-access-speak-line)
					;  (ad-activate 'emacspeak-speak-line)
  (remove-hook 'post-command-hook 'latex-access-current-line-braille t)
  (latex-access-current-line-braille))

(defun latex-access-on ()
  "Turn on latex-access"
  (interactive)
  (message "Latex-access enabled.")
  (make-local-variable 'latex-access)
  (setq latex-access t)  
  (setq latex-access-initial t)
  (ad-enable-advice 'emacspeak-speak-line 'around 'latex-access-speak-line)
  (ad-activate 'emacspeak-speak-line)
  (add-hook 'post-command-hook 'latex-access-current-line-braille nil t)
  )

(defun toggle-latex-access ()
  "Toggle the state of latex-access."
  (interactive)
  (if latex-access 
      (latex-access-off) ; Switch off latex-access
    (latex-access-on) ; Switch on latex-access
    ))

(defun toggle-latex-access-dollars-braille ()
  "Toggle whether to Braille dollar signs."
  (interactive)
  (message "Dollar signs will %s be shown in Braille."
	   (if (latex_access_emacstoggle-dollars-nemeth) "not" "")))

(defun toggle-latex-access-dollars-speech ()
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
    (progn 
      (if (file-exists-p filename)
	  (progn 
	    (message "Reading file %s..." filename)
	    (latex_access_emacspreprocessor-read filename))
	(error "File %s doesn't exist." filename)))))

(defun latex-access-preprocessor-write ()
  "Takes user input for filename, then passes the file path to the python
function."
  (interactive)
  (let ((filename (read-file-name "Enter full
filename to save to: ")))
    (if (file-exists-p filename)
	(progn 
	  (let ((input (yes-or-no-p "File %s exists, overwrite? ")))
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
  (if (mark) 
      (progn 
	     (latex_access_emacspreprocessor-from-string
	      (buffer-substring-no-properties beg end))
	     (message "Passed region to the preprocessor"))
    (error "No region set.")))

(defun latex-access-preprocessor-add (input strargs translation)
  "Preprocessor add function -- passes input to the python code."
  (interactive "sEnter the command you wish to re-define: 
nEnter the number of arguments of the command: 
sEnter the definition of the custom command, that is, the standard LaTex to which it is equivalent: ")
  (latex_access_emacspreprocessor-add input strargs
  translation)
  (message "Added string %s" input))

(defun latex-access-current-line-braille ()
  "Braille the current line"
  (interactive)
  (if (and latex-access (not latex-access-initial))
      (let ((emacspeak-speak-messages nil))
	(latex-access-braille-line)))
  (if (and latex-access latex-access-initial)
      (progn (setq latex-access-initial nil)
	     (message "Latex-access enabled.")))
  (if (not latex-access) 
      (message "Latex-access disabled.")))

(defun latex-access-eq (beg end)
  "Grabs text between beg and end.
Translates all text between beg and end into Braille. Could do speech,
but this would be irritating!
For interactive input the active region is used. Feel free to use any
two points of a buffer though when calling from lisp."
  (interactive "r")
  (setq latex-access-buff (get-buffer-create " latex-access buffer"))
  (save-excursion 
    (set-buffer latex-access-buff)
    (erase-buffer)
    (insert "Braille translation of math in region follows:\n"))
					; Next run the region line by line through translator 
  (goto-char beg)
  (catch 'break
    (while (<= (point) end)
      (progn
	(if (= (point) (point-max))
	    (throw 'break nil))
	(let ((latex-access-currline (latex_access_emacstransbrl (thing-at-point 'line))))
	  (forward-line)
	  (save-excursion
	    (set-buffer latex-access-buff)
	    (goto-char (point-max))
	    (insert "\n" latex-access-currline))))))
  (switch-to-buffer-other-window latex-access-buff)
  (align-regexp (point-min) (point-max) "&" 0 0) ; align multy line equations
  (goto-char (point-min))
  (replace-string "&" " ")
  (goto-char 49) ; Shall remain consistant if first line doesn't change.
  )

(defun latex-access-braille-line ()
  "Braille a particular number of lines above the current one. Includes
the current line."
  (interactive)
					; get current line and set our counter
  (setq counter 0)
  (setq brltext (latex_access_emacstransbrl (thing-at-point 'line)))
					; Next get prior lines
  (save-excursion 
    (move-beginning-of-line nil)
    (let ((emacspeak-speak-messages nil))
      (catch 'break
	(while (< counter latex-access-linesabove)
	  (progn 
	    (forward-line -1)
	    (setq brltext (concat (latex_access_emacstransbrl (thing-at-point
							       'line)) "\n" brltext))
					; Next increment counter
	    (setq counter (1+ counter))
	    (if (= (point) (point-min))
		(throw 'break nil)))))))
  (message "%s" brltext))

;;; emacs-latex-access.el ends here
