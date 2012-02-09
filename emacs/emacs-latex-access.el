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

(pymacs-load "latex_access_emacs" "latex_access_emacs") ; load the
					; relevant modules 

(setq latex-access-braille nil)
(setq latex-access-speech nil)

(setq latex-access-displaying nil) ; So we can toggle brailling of line
(setq latex-access-braille-cursor (point)) ; position of braille cursor 
(setq latex-access-braille-type nil) ; Should the display be updated as you type 
(setq latex-access-braille-shortcut t) ; Which cursor to switch to 

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
				ad-do-it)))) ; else call default emacspeak line handler 

(defun latex-access ()
  "Set up latex-access." 
					; Braille post-command hook so that Braille is displayed on the message line.
  ;(add-hook 'post-command-hook 'latex-access-braille-other-window nil nil)
					; Experimental braille implementation, uncomment for testing, but is
  ; far from perfect yet. 
  (add-hook 'post-command-hook 'latex-access-brltty-type nil nil)
					; Enable speech (emacspeak advice)
  (if (featurep 'emacspeak) ; Load emacspeak...
      (progn 
	(ad-enable-advice 'emacspeak-speak-line 'around 'latex-access-speak-line) 
	(ad-activate 'emacspeak-speak-line))) ; Enable the advice. 
  (latex_access_emacsactivateSettings)) ; Activate user settings from file

(defun latex-access-toggle-speech ()
  "Toggle latex-access speech on/off."
  (interactive)
  (make-local-variable 'latex-access-speech)
  (if latex-access-speech 
      (progn (latex-access-disable-speech) (message "LaTeX-access speech disabled."))
    (progn (latex-access-enable-speech) (message "LaTeX-access speech enabled."))))

(defun latex-access-toggle-braille ()
  "Toggle latex-access Braille on/off."
  (interactive)
  (make-local-variable 'latex-access-braille)
  (if latex-access-braille 
      (progn (latex-access-disable-braille) (message "LaTeX-access Braille disabled."))
    (progn (latex-access-enable-braille) (message "LaTeX-access Braille enabled."))))

(defun latex-access-disable ()
  "Turn off latex-access entirely"
  (latex-access-disable-braille)
  (latex-access-disable-speech))

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
	  (switch-to-buffer-other-window currentbuff)
	  (latex-access-setup-source-window))))) ; Set desireable width 

(if (featurep 'emacspeak)
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
	(kill-buffer workspace )))) ; Delete our workspace now.

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

(defun latex-access-setup-source-window () 
  "Set the source window x characters wide given braille display width,
provided Braille is enabled of course."
(if latex-access-braille
    (enlarge-window-horizontally 
     (- (latex_access_emacsDetermineWindowSize (window-width)
					    (latex_access_emacsBrailleDisplaySize)) 1))))

(defun latex-access-load-settings ()
  "Allow a user to load their latex-access settings stored in ~/.latex-access"
  (interactive)
  (if (latex_access_emacsactivateSettings)
      (message "Loaded settings")
    (message "No configuration file, continuing with defaults.")))

(defun latex-access-enable-braille ()
  "Enable latex-access Braille"
  (make-local-variable 'latex-access-braille)
  (setq latex-access-braille t)) 

(defun latex-access-disable-braille ()
  "Disable latex-access Braille"
  (make-local-variable 'latex-access-braille)
  (latex-access-close-display)
  (setq latex-access-braille nil))

(defun latex-access-enable-speech ()
  "Enable latex-access speech"
  (make-local-variable 'latex-access-speech)
  (setq latex-access-speech t))

(defun latex-access-disable-speech ()
  "Disables latex-access speech."
  (make-local-variable 'latex-access-speech)
  (setq latex-access-speech nil))

(defun latex-access-decide-braille ()
  "Decide if Braille should be enabled."
  (if (latex_access_emacsgetSetting "brailletranslation")
      (latex-access-enable-braille)))

(defun latex-access-decide-speech ()
  "Decide if speech should be enabled."
  (if (latex_access_emacsgetSetting "speechtranslation")
      (latex-access-enable-speech)))

(defun latex-access-brltty (&optional speak)
  "Braille a line of latex in nemeth on the Braille display relative to the
position of point."
  (interactive)
  (make-local-variable 'latex-access-braille)
  (save-excursion
    (if latex-access-braille 
	(let ((currentposs (point))
	      (begposs (progn (beginning-of-line) (point)))) ; get the
					; position of beginning of line 
	  (latex_access_emacsbrailleRegion (thing-at-point 'line) (-
								   currentposs begposs)) ; Braille the current line, passing the line
					; number to the python functions and the
					; difference between start of line and the
					; cursor position. 
	  (if (and speak (featurep 'emacspeak)) (emacspeak-speak-line)))
      (latex-access-close-display))))

(defun latex-access-close-display ()
  "Close the display so brltty can regain control."
  (interactive)
  (latex_access_emacscloseDisplay))

(defun latex-access-brltty-toggle ()
  "Toggle displaying of current line via latex-access through brltty"
  (interactive)
  (if (equal latex-access-displaying nil) (progn 
					    (message "LaTeX-access Braille mode enabled.")
					    (setq latex-access-displaying t)
					    (setq latex-access-braille-cursor (point))
					    (latex-access-brltty))
    (progn 
      (message "LaTeX-access Braille mode disabled.")
      (setq latex-access-displaying nil)
	   (setq latex-access-braille-cursor (point))
	   (setq latex-access-braille-type nil) ; Disable follows emacs point first otherwise the display can't move 
	   (latex-access-close-display))))

(defun latex-access-brltty-previous-line (arg)
  "Braille the prior line, with usual c-u args acting same as the
previous-line function.."
  (interactive "P")
  (setq latex-access-braille-type nil) ; Disable follows emacs point first otherwise the display can't move 
  (save-excursion 
    (if (equal latex-access-displaying t) (goto-char latex-access-braille-cursor) (goto-char (point)))
    (setq latex-access-displaying t)
    (let ((dtk-quiet nil))
      (previous-line arg))
    (setq latex-access-braille-cursor (point))
    (latex-access-brltty t)))

(defun latex-access-brltty-next-line (arg)
  "Braille the next line, with usual c-u args acting same as the
next-line function.."
  (interactive "P")
  (setq latex-access-braille-type nil) ; Disable follows emacs point first otherwise the display can't move 
  (save-excursion
    (if (equal latex-access-displaying t) (goto-char latex-access-braille-cursor) (goto-char (point)))
    (setq latex-access-displaying t)
    (let ((dtk-quiet nil))
      (next-line arg))
    (setq latex-access-braille-cursor (point))
    (latex-access-brltty t)))

(defun latex-access-brltty-pan-right ()
  "Move one braille display length to the right."
  (interactive)
  (setq latex-access-braille-type nil) ; Disable follows emacs point first otherwise the display can't move 
  (save-excursion 
    (if (equal latex-access-displaying t) (goto-char latex-access-braille-cursor) (goto-char (point)))
    (setq latex-access-displaying t)
  (let ((cursor (- (point) (line-beginning-position))) ; cursor location in terms of current line 
	(display (latex_access_emacsBrailleDisplaySize)) ; Braille
					; display length 
	(line (- (line-end-position) (line-beginning-position)))) ; line
					; length 
    (if (< (+ cursor display) line) 
	(forward-char display) (end-of-line))
    (setq latex-access-braille-cursor (point))
    (latex-access-brltty))))

(defun latex-access-brltty-pan-left ()
  "Move one braille display length to the right."
  (interactive)
  (setq latex-access-braille-type nil) ; Disable follows emacs point first otherwise the display can't move 
  (save-excursion 
    (if (equal latex-access-displaying t) (goto-char latex-access-braille-cursor) (goto-char (point)))
    (setq latex-access-displaying t)
  (let ((cursor (- (point) (line-beginning-position))) ; cursor location in terms of current line 
	(display (latex_access_emacsBrailleDisplaySize))) ; Braille
					; display length 

    (if (> (- cursor display) 0) 
	(forward-char (- 0 display)) (beginning-of-line))
    (setq latex-access-braille-cursor (point))
    (latex-access-brltty))))

(defun latex-access-brltty-goto-point ()
  "Return Braille window to emacs point."
  (interactive)
  (setq latex-access-braille-cursor (point))
  (latex-access-brltty))

(defun latex-access-brltty-goto-braille-cursor ()
  "Move emacs point to the braille window"
  (interactive)
  (goto-char latex-access-braille-cursor)
  (if (featurep 'emacspeak) (emacspeak-speak-line)))

(defun latex-access-brltty-toggle-type ()
  "Toggle Braille translation as you type"
  (interactive) 
  (if (not latex-access-braille-type) (progn 
					(message "Braille updated as you type.")
					(setq latex-access-displaying t)
					(setq latex-access-braille-type t))
    (progn 
      (message "Braille will not be updated as you type.")
      (setq latex-access-braille-type nil))))

(defun latex-access-brltty-type ()
  "Braille translation as you type."
  (if latex-access-braille-type (progn 
				  (setq latex-access-braille-cursor (point))
				  (latex-access-brltty))))

(defun latex-access-brltty-switch-cursors ()
  "Allows you to quickly toggle between the Braille position and point
position. Useful to focus on a particular area of the screen as you
type, but also to keep track of your input."
  (interactive)
  (setq latex-access-displaying t)
  (if latex-access-braille-shortcut 
      (save-excursion 
	(setq latex-access-braille-save latex-access-braille-cursor)
	(setq latex-access-braille-shortcut nil)
	(goto-char (point)) (latex-access-brltty) 
	(setq latex-access-braille-cursor (point))
	(message "Moved to emacs point."))
    (save-excursion 
      (goto-char latex-access-braille-save)
      (latex-access-brltty)
      (setq latex-access-braille-shortcut t)
      (setq latex-access-braille-cursor (point))
      (message "Moved back to Braille cursor."))))

(provide 'latex-access)
(latex-access) ; Set everything up

(define-minor-mode latex-access-mode
    "Toggle LaTeX-access mode.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode.

When LaTeX-access is enabled, it provides both spoken and Braille
feedback for a blind user by translating LaTeX markup into Braille
mathematics and speaking the markup in a way which is much easier to
understand. See http://latex-access.sourceforge.net/ for details.

\\{latex-access-mode-map}"
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " LaTeX-access"
  ;; The minor mode bindings.
  `(                                                                                                                                                      
    (,(kbd "C-p") . latex-access-brltty-previous-line)
    (,(kbd "C-n") . latex-access-brltty-next-line)
    (,(kbd "C-f") . latex-access-brltty-pan-right)
    (,(kbd "C-b") . latex-access-brltty-pan-left)
    (,(kbd "C-c t") . latex-access-brltty-toggle)
    (,(kbd "C-c c") . latex-access-brltty-toggle-type)
    (,(kbd "C-c g") . latex-access-brltty-switch-cursors)
    (,(kbd "C-c C-p") . latex-access-brltty-goto-point)
    (,(kbd "C-c C-o") . latex-access-brltty-goto-point)
    (,(kbd "C-c d") . latex-access-toggle-dollars-braille)
    (,(kbd "C-c f") . latex-access-toggle-dollars-speech)
    (,(kbd "C-c f") . latex-access-toggle-dollars-speech)
    (,(kbd "C-c C-t") . latex-access-toggle-braille)
    (,(kbd "C-c C-y") . latex-access-toggle-speech)
    (,(kbd "C-c w") . latex-access-table-location))
  (if latex-access-mode
      (progn 
	(latex-access-decide-speech) (latex-access-decide-braille))
    (latex-access-disable)))

;;; emacs-latex-access.el ends here
