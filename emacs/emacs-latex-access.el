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
(setq latex-access-speech nil) ; set initial global value 
(setq latex-access-braille nil) ; initial global value 
(setq latex-access-speech-initial nil) ; Used by the dmsg function
(setq latex-access-braille-initial nil) ; Used by the dmsg function

; Voice definitions, customize these by customizing the <voice_name>-settings variable.
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
  (if latex-access-speech
      (latex-access-speak (latex_access_emacstranssp (thing-at-point 'line))) ; Speech to pass to
    ad-do-it)) ; else call default emacspeak line handler 

(defun latex-access-braille-off ()
  "Disable latex-access Braille."
  (interactive)
  (make-local-variable 'latex-access-braille)
  (setq latex-access-braille nil)
  (remove-hook 'post-command-hook 'latex-access-dmsg t)
  (latex-access-dmsg t nil))

(defun latex-access-braille-on ()
  "Enable latex-access Braille."
  (interactive)
  (make-local-variable 'latex-access-braille)
  (setq latex-access-braille t)
  (add-hook 'post-command-hook 'latex-access-dmsg nil t)
  (setq latex-access-braille-initial t)) ; Next call to dmsg function should show the braille enabled message.

(defun latex-access-speech-off ()
  "Turn off latex-access speech."
  (interactive)
  (make-local-variable 'latex-access-speech)
  (setq latex-access-speech nil)
  (if (not latex-access-braille)
      (latex-access-dmsg nil t)
    (setq latex-access-speech-initial t))) ; Next call of dmsg function will show the relevant message.

(defun latex-access-speech-on ()
  "Turn on latex-access speech"
  (interactive)
  (make-local-variable 'latex-access-speech)
  (setq latex-access-speech t)  
					; enabled and activate the advice for emacspeak
  (ad-enable-advice 'emacspeak-speak-line 'around 'latex-access-speak-line)
  (ad-activate 'emacspeak-speak-line)
  (if (not latex-access-braille) 
      (latex-access-dmsg nil t)
    (setq latex-access-speech-initial t)))

(defun latex-access-toggle-speech ()
  "Toggle latex-access speech on/off."
  (interactive)
  (make-local-variable 'latex-access-speech)
  (if latex-access-speech 
      (latex-access-speech-off)
    (latex-access-speech-on))) ; Speech is off, turn it on.

(defun latex-access-toggle-braille ()
  "Toggle latex-access Braille on/off."
  (interactive)
  (make-local-variable 'latex-access-braille)
  (if latex-access-braille 
      (latex-access-braille-off)
    (latex-access-braille-on))) ; Braille is off now, turn it on.

(defun latex-access-toggle-dollars-braille ()
  "Toggle whether to Braille dollar signs."
  (interactive)
  (message "Dollar signs will %s be shown in Braille."
	   (if (latex_access_emacstoggle-dollars-nemeth) "not" "")))

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

(defun latex-access-speak (text)
  "Convert the latex-access speech markup into text-properties on the string and then speak."
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
  
  (dtk-speak text))


(defun latex-access-dmsg (&optional brlsetchange
						    &optional speechsetchange)
  "Braille the current line"
					; Hack encase the hook is trying to overwrite the first call of
					; function to reflect setting change
  (catch 'return 
					; Only do this hack if braille is on.
    (make-local-variable 'latex-access-braille)
    (if (and latex-access-braille latex-access-speech-initial)
	(progn 
	  (message "Latex-access speech %s." (if latex-access-speech
						 "enabled" "Disabled"))
	  (setq latex-access-speech-initial nil)
	  (throw 'return nil)))
					; Only do this if Braille is running 
    (if (and latex-access-braille-initial latex-access-braille)
	(progn 
	  (setq latex-access-braille-initial nil)
	  (message "Latex-access Braille %s." (if latex-access-braille
						  "enabled" "Disabled"))
	  (throw 'return nil)))
    ; this stuff should be executed if there is a setting change, but
    ; Braille isn't active.
    (if brlsetchange
	(progn 
	  (if latex-access-braille ; Braille just turned on
	      (message "Latex-access Braille enabled.")
	    (message "Latex-access Braille disabled."))
	  (throw 'return nil))) ; done with setting change.
					; Speech settings 
    (if speechsetchange
	(progn 
	  (if latex-access-speech 
	      (message "Latex-access speech enabled.")
	    (message "Latex-access speech disabled.")) ; else clause 
	  (throw 'return nil))) ; done 

					; next, if no settings changed, it's safe to Braille msg 
					; But only if latex-access-braille is enabled.
    (if latex-access-braille 
	(let ((emacspeak-speak-messages nil)) 
	  (latex-access-braille-line)))))

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

(defun latex-access-braille-line ()
  "Braille a particular number of lines above the current one. Includes
the current line."
  (interactive)
  ; first determine current location of point on screen
  (save-excursion 
    (let ((endpoint (progn (move-end-of-line nil) (point)))
	  (startpoint (progn (move-beginning-of-line nil) (forward-line
							   (- 0 latex-access-linesabove)) (point))))
      (message "%s" (latex_access_emacstransbrl
		     (buffer-substring-no-properties startpoint endpoint))))))

(defun latex-access-matrix (beg end)
  "Display a matrix in emacspeak table mode. 
Once the matrix is in emacspeak table mode, all emacspeak table commands
may be used to navigate the matrix."
  (interactive "r")
  (let ((matrix (replace-regexp-in-string "\\\\" ""
					  (buffer-substring-no-properties
					   beg end))) ; get rid of \\ chars
	(workspace (get-buffer-create "workspace-latex-access"))) ; create a workspace buffer
    (setq matrix (replace-regexp-in-string "&" "" matrix)) ; get rid of the & signs 
					; We now have a reasonably clean string. 
					; use a workspace buffer to pass emacspeak the matrix 
    (save-excursion 
      (set-buffer workspace)
      (insert matrix) ; place matrix in our workspace ready for manipulation
					; now hand to emacspeak
      (emacspeak-table-display-table-in-region (point-min) (point-max)))
    (kill-buffer workspace ))) ; Delete our workspace now.

;; This is silly, but convenient whilst we lack key bindings.
;; Ultimately, something of this nature could be of value, but the
;; implementation should take into account whether specific
;; functionality is allowed to be enabled. I.e. if the user doesn't have
;; Braille this shouldn't alter the Braille on/off settings...
(defun latex-access-hack (&optional prefix)
  "Disables both Braille and speech. When properly implemented checks
should determine what functionality the user actually has
available... For now, use if you use both Braille and speech, and
require a method to switch them on/off together.
If you supply an arg (c-u), the latex-access functionality will be
turned on, otherwise, with no arg, it is turned off."
  (interactive "P")
  (if (not prefix)
      (progn ; Disable 
	(latex-access-braille-off)
	(latex-access-speech-off))
    (progn ; Arg supplied, turn on
      (latex-access-speech-on)
      (latex-access-braille-on))))

(defun latex-access-table-location ()
  "Provide information of current location in table."
  (interactive)
  (save-excursion 
    (let ((end (point)) (beg (progn (search-backward "\\begin")
				    (move-end-of-line nil) (point))))
      (let ((table (buffer-substring-no-properties beg end)))
	(dtk-speak (latex_access_emacsWhereAmI
		    (latex_access_emacsGetTableCurrentRow table)
		    (latex_access_emacsBuildHeaderString (latex_access_emacsGetTableTopRow table))))))))

;;; emacs-latex-access.el ends here
