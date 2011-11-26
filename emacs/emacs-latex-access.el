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

; Load pymacs, shouldn't really mater if this was done by .emacs first 
(require 'pymacs)

; Add the hooks to start by default when in LaTeX-mode 
; Ideally I'd like to make this latex-math-mode, but I dn't see a hook
; for it anywhere? 
; Initialise latex-access and turn on/off speech or Braille depending on
; the latex-access-speech and latex-access-braille user customisable
; variables. By default turn both on. To override or change this setting
; just use the (setq) function after loading this file in your .emacs or
; init file. 

; Global value should always be nil because latex-access should never be
; running globally 
; Imagine how annoying it would potentially be while browsing the web,
; or in dired-mode :)
(defcustom latex-access-speech nil "Set this variable to determine
whether latex-access-speech should be enabled. A value of nil disables
latex-access-speech, while a value of t enables it.")
(defcustom latex-access-braille nil "Set this variable to determine
whether latex-access-braille should be enabled. A value of nil disables
latex-access-braille, while a value of t enables it.")
(defcustom latex-access-use-braille t "This variable controls what
latex-access will load eg. if this variable is set to t, Braille will be
loaded, while a value of nil will not load Braille. To load Braille if
this variable is nil you'd have to run m-x
latex-access-toggle-braille.")
(defcustom latex-access-use-speech t "This variable controls what
latex-access will load eg. if this variable is set to t, speech will be
loaded, while a value of nil will not load speech. To load speech if
this variable is nil you'd have to run m-x latex-access-toggle-speech.")

(pymacs-load "latex_access_emacs" "latex_access_emacs") ; load the
					; relevant modules 

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
  (when (listp arg) (setq arg (car arg )))
  (if latex-access-speech
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
    ad-do-it)) ; else call default emacspeak line handler 

(defadvice LaTeX-math-mode (before latex-access-auto-enable)
  "Auto enable or disable latex-access when latex-math-mode is toggled."
  (latex-access-auto-enabler))

(defun latex-access-auto-enabler ()
  "Toggle the state of speech and Braille, only if they are supposed to
  be automatically enabled though, eg. by latex-access-use-* variables."
  (make-local-variable 'latex-access-braille)
  (make-local-variable 'latex-access-speech)
  (make-local-variable 'latex-access-math-mode)
					; Toggle speech now 
  (if latex-access-use-speech (if latex-access-speech 
				  (setq latex-access-speech nil) 
				(setq latex-access-speech t)))
  ; Toggle Braille now 
  (if latex-access-use-braille (if latex-access-braille 
				   (setq latex-access-braille nil)
				 (setq latex-access-braille t)))
  (if latex-access-math-mode 
      (progn
	(setq latex-access-math-mode nil)
	(message "LaTeX-math-mode disabled"))
    (progn 
      (setq latex-access-math-mode t)
      (message "LaTeX-math-mode enabled"))))

(defun latex-access ()
  "Set up latex-access." 
					; Braille post-command hook so that Braille is displayed on the message line.
  (add-hook 'post-command-hook 'latex-access-braille-other-window nil nil)
					; Enable speech (emacspeak advice)
  (ad-enable-advice 'emacspeak-speak-line 'around 'latex-access-speak-line) 
  (ad-activate 'emacspeak-speak-line) ; Enable the advice. 
  ; Enable latex-math-mode latex-access toggle advice. 
  (ad-enable-advice 'LaTeX-math-mode 'before 'latex-access-auto-enable)
  (ad-activate 'LaTeX-math-mode) ; Enable the advice. 
  (setq latex-access-math-mode nil)) ; Assume latex-math-mode is disabled
				    ; at start up.  

(defun latex-access-toggle-speech ()
  "Toggle latex-access speech on/off."
  (interactive)
  (make-local-variable 'latex-access-speech)
  (if latex-access-speech 
      (progn (setq latex-access-speech nil)
	     (message "Latex-access speech disabled."))
    (progn (setq latex-access-speech t)
	   (message "Latex-access speech enabled."))))

(defun latex-access-toggle-braille ()
  "Toggle latex-access Braille on/off."
  (interactive)
  (make-local-variable 'latex-access-braille)
  (if latex-access-braille 
      (progn (setq latex-access-braille nil)
	     (message "Latex-access Braille disabled."))
    (progn (setq latex-access-braille t)
	   (message "Latex-access Braille enabled."))))

(defun latex-access-disable ()
  "Turn off latex-access entirely"
  (interactive)
  (make-local-variable 'latex-access-braille)
  (make-local-variable 'latex-access-speech)
  (setq latex-access-braille nil)
  (setq latex-access-speech nil)
  (message "Latex-access disabled."))

(defun latex-access-enable ()
  "Enable all features of latex-access disregarding user variables."
  (interactive) 
  (latex-access) ; initialise 
  (make-local-variable 'latex-access-braille)
  (make-local-variable 'latex-access-speech)
  (setq latex-access-speech t)
  (setq latex-access-braille t)
  (message "Latex-access enabled."))

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
  
  (dtk-speak text))

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

(defun latex-access-braille-other-window ()
  "Put the Braille translation of a LaTeX buffer in another buffer, and
show it in the other-window, provided that Braille is switched on.
This allows a blind user to read the LaTeX source, and directly to the
right of it should be the corresponding Braille (nemeth) translation
for each line, keeping lines in sync."

  (save-excursion 
					; is Braille on 
    (if latex-access-braille 
	(let ((emacspeak-speak-messages nil) ; Emacspeak shouldn't read Braille translation (is this still even necessary?)
					; Make the windows split side by side not top-bottom 
	      (split-width-threshold 80)
	      (split-height-threshold nil)
	      ; Make a buffer for our translation or load it if it
	      ; already exists 
	      (workspace (get-buffer-create "*translation.braille"))
	      ; So we can come back to our current place later 
	      (currentbuff (current-buffer))
					; Get the translation from the python code 
	      (translation (latex_access_emacstransbrl
			    (buffer-substring-no-properties
			     (window-start) (window-end)))))
					; Open the relevant buffer insert translation. 
	  (set-buffer workspace)
	  (setq buffer-read-only nil)
	  (erase-buffer) 
	  (insert translation)
	  (setq buffer-read-only t)
	  (switch-to-buffer-other-window workspace) ; Make sure the
					; other window holds
					; translation, not some other
					; random buffer 
					; Set translation window to begin from same point as latex
					; buffer is scrolled, hopefully keeping lines in sync. This is
					; still a bit problematic. It works ok quite often, but also
					; manages to make the lines out of sync frequently. 
					; Now move back to where we started
	  (switch-to-buffer-other-window currentbuff)))))

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
		     (latex_access_emacsGetTableTopRow table)) table))))))

;;; Experimental...
;;; move forward or back by a "mathematical term" 
;;; This could be useful for increasing efficiency for things like
;;; indicies but I'm just not sure 

(defun latex-access-next-term ()
  "Move forward a mathematical term on current line"
  (interactive)
    (let ((endtext (point)) ; Where cursor is now 
	  (begtext (progn (beginning-of-line) (point)))) ; Beginning of
					; line 
					; We must use the next few separate let statements so we can
					; reference variables we have already defined
					; Next line assigns value to cursor of character in from  the
					; start of line. If cursor was on the first character of line,
					; it's value would be 1. 
      (let ((cursor (+ (- endtext begtext) 1))
	  (currentline (thing-at-point 'line))) ; Grab current line of LaTeX
					; Find out where the cursor should move to as a result of
					; calling the python function. 
	(let ((newposs (latex_access_emacsNextTerm currentline cursor)))
      (if (not (= newposs -1)) ; Did we actually find a new term
					; Go to beginning of line, then move the specified number of
					; characters forward so we are in the updated cursor position
					; on current line. 
	  (progn 
	    (beginning-of-line)
	    (forward-char (- newposs 1))
					; Speak the new term...
	    (latex-access-speak (latex_access_emacsSpeakSegment currentline
			(- newposs 1)
			(- (latex_access_emacsNextTerm currentline newposs) 1))))
	(progn 
	  (goto-char endtext) ; Return the cursor to where it was
			      ; previously since no term found
	  (message "No more terms")))))))

(defun latex-access-previous-term ()
  "Move backward a mathematical term on current line"
  (interactive)
    (let ((endtext (point)) ; Where cursor is now 
	  (begtext (progn (beginning-of-line) (point)))) ; Beginning of
					; line 
					; We must use the next few separate let statements so we can
					; reference variables we have already defined
					; Next line assigns value to cursor of character in from  the
					; start of line. If cursor was on the first character of line,
					; it's value would be 1. 
      (let ((cursor (+ (- endtext begtext) 1))
	  (currentline (thing-at-point 'line))) ; Grab current line of LaTeX
					; Find out where the cursor should move to as a result of
					; calling the python function. 
	(let ((newposs (latex_access_emacsPreviousTerm currentline cursor)))
      (if (not (= newposs -1)) ; Did we actually find a new term
					; Go to beginning of line, then move the specified number of
					; characters forward so we are in the updated cursor position
					; on current line. 
	  (progn 
	    (beginning-of-line)
	    (forward-char (- newposs 1))
					; Speak the new term...
	    (latex-access-speak (latex_access_emacsSpeakSegment currentline
			(- newposs 1)
			(- (latex_access_emacsNextTerm currentline newposs) 1))))
	    (progn 
	  (goto-char endtext) ; Return the cursor to where it was
			      ; previously since no term found
	  (message "No more terms")))))))

(defun latex-access-skim-terms (&optional arg)
  "Skim through mathematical terms in latex using
emacspeak-execute-repeatedly function. Optionally choose a line to
perform this on by supplying a prefix arg or optional arg if calling
through lisp. A positive arg refers to lines above the point, while
negative args refer to lines below the point. The logic here is that
when working with mathematical working you'll normally want to review
the previous line..."
  (interactive "P") ; We accept c-u args
  (when (listp arg) (setq arg (car arg )))
  (save-excursion 
    (if (not (null arg))
					; Convert so it is suitable for forward-line
					; function, i.e. we expect positive arg to go up
					; so make it negative, since forward-line has
					; the reverse effect 
	(forward-line (- 0 arg)))
    (emacspeak-execute-repeatedly 'latex-access-next-term))) ; Skim the terms.

(defun latex-access-setup-source-window () 
  (interactive) 
  "Set the source window x characters wide given braille display width,
provided Braille is enabled of course."
(if latex-access-braille
    (enlarge-window-horizontally 
     (latex_access_emacsDetermineWindowSize (window-width)
					    (latex_access_emacsBrailleDisplaySize)))))

(latex-access) ; Set everything up

;;; emacs-latex-access.el ends here
