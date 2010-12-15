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
(pymacs-load "latex_access_emacs" "latex_access_emacs") ; load the
					; relevant modules 
(setq latex-access nil) ; set initial global value 
(global-set-key (kbd "C-x \\") 'toggle-latex-access) ; key binding for toggle

(defun latex-access-off ()
  "Turn off latex-access."
  (interactive)
					; set default emacs key bindings
					; and latex-access is nil.
  (make-local-variable 'latex-access)
  (setq latex-access nil)
					; Comment following two lines if you do not have emacspeak.
  (local-set-key (kbd "C-e C-i")
		 'emacspeak-table-display-table-in-region) 
  (local-set-key (kbd "C-n") 'next-line)
  (local-set-key (kbd "C-p") 'previous-line)
  (local-set-key [down] 'next-line)
  (local-set-key [up] 'previous-line)
  (message "Latex-access disabled."))

(defun latex-access-on ()
  "Turn on latex-access"
  (interactive)
					; set latex-access var to true,
					; and adjust relevant keymaps.
  (make-local-variable 'latex-access)
  (setq latex-access t)  
  (local-set-key (kbd "C-e C-i") 'latex-access-current-line)
  (local-set-key (kbd "C-n") 'next-latex-access-line)
  (local-set-key (kbd "C-p") 'previous-latex-access-line)
  (local-set-key [down] 'next-latex-access-line)
  (local-set-key [up] 'previous-latex-access-line)
  (message "Latex-access enabled."))

(defun toggle-latex-access ()
  "Toggle the state of latex-access."
  (interactive)
  (if latex-access 
      (latex-access-off) ; Switch off latex-access
    (latex-access-on) ; Switch on latex-access
    ))

(defun latex-access-current-line ()
  "Grab the current line and pass to latex-access."
  "This function is called by some of the emacs navigation commands"
  "to provide useful Braille and speech output for LaTeX."
  (interactive)
  (setq currline (thing-at-point 'line)) ; We grab current line 
  ; Shut up emacspeak
  (let ((emacspeak-speak-messages nil))
    (progn 
      (message "%s" (latex_access_emacstransbrl currline)) ; Braille translation
					; -- appears in echo area 
      )
    (dtk-speak (latex_access_emacstranssp currline)) ; Speech to pass to
					; emacspeak 
  ))

(defun next-latex-access-line (lines)
  "The same as next-line, unless latex-access is enabled."
  "If Latex-access is enabled, offer Braille and speech translations,"
  "when moving down by line."
  (interactive "p")
  (forward-line lines) ; go to next line
  (latex-access-current-line) ; Translate new active line
  )

(defun previous-latex-access-line (lines)
  "The same as prior-line, unless latex-access is enabled."
  "If Latex-access is enabled, offer Braille and speech translations,"
  "when moving up by line."
  (interactive "p")
					; If lines is a positive number make it negative.
  (if (>= lines 0)
      (setq lines (- 0 lines)))
  (forward-line lines) ; go up x number of  lines.
  (latex-access-current-line) ; Translate new active line.
  )

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

;;; emacs-latex-access.el ends here
