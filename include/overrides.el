;;; override.el --- Various overrides.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;;-----------------------------------------------------------------------------
;; Override from "simple.el": avoid the "helpful" octal/hexadecimal if it goes
;; into the buffer

(defun eval-expression-print-format (value)
  "Format VALUE as a result of evaluated expression.
Return a formatted string which is displayed in the echo area
in addition to the value printed by prin1 in functions which
display the result of expression evaluation."
  (if (and (integerp value)
           (not (bufferp standard-output)) ;ELI
           (or (not (memq this-command '(eval-last-sexp eval-print-last-sexp)))
               (eq this-command last-command)
               (if (boundp 'edebug-active) edebug-active)))
      (let ((char-string
             (if (or (if (boundp 'edebug-active) edebug-active)
		     (memq this-command '(eval-last-sexp eval-print-last-sexp)))
                 (prin1-char value))))
        (if char-string
            (format " (#o%o, #x%x, %s)" value value char-string)
          (format " (#o%o, #x%x)" value value)))))

;;-----------------------------------------------------------------------------
;; Overrides from "window.el"

;; Messes up the count when the window is not shown yet (example: breaks
;; resizing of the electric-bubber-list), so add a redisplay.  Actually,
;; it's unclear when this happens, so disable it for now.
'(defun count-screen-lines (&optional beg end count-final-newline window)
  "Return the number of screen lines in the region.
The number of screen lines may be different from the number of actual lines,
due to line breaking, display table, etc.

Optional arguments BEG and END default to `point-min' and `point-max'
respectively.

If region ends with a newline, ignore it unless optional third argument
COUNT-FINAL-NEWLINE is non-nil.

The optional fourth argument WINDOW specifies the window used for obtaining
parameters such as width, horizontal scrolling, and so on.  The default is
to use the selected window's parameters.

Like `vertical-motion', `count-screen-lines' always uses the current buffer,
regardless of which buffer is displayed in WINDOW.  This makes possible to use
`count-screen-lines' in any buffer, whether or not it is currently displayed
in some window."
  (unless beg
    (setq beg (point-min)))
  (unless end
    (setq end (point-max)))
  (if (= beg end)
      0
    (save-excursion
      (save-restriction
        (widen)
        (narrow-to-region (min beg end)
                          (if (and (not count-final-newline)
                                   (= ?\n (char-before (max beg end))))
                              (1- (max beg end))
                            (max beg end)))
        (goto-char (point-min))
        ;; (redisplay) ; ELI
        ;; (save-excursion (vertical-motion (buffer-size) window)) ; ELI
        (1+ (vertical-motion (buffer-size) window))))))

;;-----------------------------------------------------------------------------
;; Overrides from "help.el", and showing temporary buffers
;; (Works better than ehelp.)

;; Don't show a message, only set return-method.
(defun help-print-return-message (&optional function)
  "Display or return message saying how to restore windows after help command.
This function assumes that `standard-output' is the help buffer.
It computes a message, and applies the optional argument FUNCTION to it.
If FUNCTION is nil, it applies `message', thus displaying the message.
In addition, this function sets up `help-return-method', which see, that
specifies what to do when the user exits the help buffer."
  (and (not (get-buffer-window standard-output))
       (let ((first-message
	      (cond ((or
		      pop-up-frames
		      (special-display-p (buffer-name standard-output)))
		     (setq help-return-method (cons (selected-window) t))
		     ;; If the help output buffer is a special display buffer,
		     ;; don't say anything about how to get rid of it.
		     ;; First of all, the user will do that with the window
		     ;; manager, not with Emacs.
		     ;; Secondly, the buffer has not been displayed yet,
		     ;; so we don't know whether its frame will be selected.
		     nil)
		    ((not (one-window-p t))
		     (setq help-return-method
			   (cons (selected-window) 'quit-window))
		     "Type \\[display-buffer] RET to restore the other window.")
		    (pop-up-windows
		     (setq help-return-method (cons (selected-window) t))
		     "Type \\[delete-other-windows] to remove help window.")
		    (t
		     (setq help-return-method
			   (list (selected-window) (window-buffer)
				 (window-start) (window-point)))
		     "Type \\[switch-to-buffer] RET to remove help window."))))
	 (funcall (or function 'identity) ;ELI
		  (concat
		   (if first-message
		       (substitute-command-keys first-message))
		   (if first-message "  ")
		   ;; If the help buffer will go in a separate frame,
		   ;; it's no use mentioning a command to scroll, so don't.
		   (if (or pop-up-windows
			   (special-display-p (buffer-name standard-output)))
		       nil
		     (if (same-window-p (buffer-name standard-output))
			 ;; Say how to scroll this window.
			 (substitute-command-keys
			  "\\[scroll-up] to scroll the help.")
		       ;; Say how to scroll some other window.
		       (substitute-command-keys
			"\\[scroll-other-window] to scroll the help."))))))))

(defvar eli-popup-exit-conf nil
  "Window configuration to restore after quitting viewing a temp buffer.")
(make-variable-buffer-local 'eli-popup-exit-conf)

(defun buffer-op-and-restore-winconf (buf op)
  (let ((conf (buffer-local-value 'eli-popup-exit-conf buf)))
    (unless (window-minibuffer-p (selected-window)) (funcall op buf))
    (when (and conf (eq (selected-frame) (window-configuration-frame conf)))
      (set-window-configuration conf))))

;; Kill help windows when done
(add-hook 'help-mode-hook
  (lambda ()
    (setq view-exit-action
          (lambda (buf) (buffer-op-and-restore-winconf buf 'kill-buffer)))))
;; Other possible options:
;; (add-hook 'help-mode-hook (lambda () (setq view-exit-action 'kill-buffer)))
;; (setq-default view-exit-action 'bury-buffer) ; always!

;; Bury apropos windows
(add-hook 'apropos-mode-hook
  (lambda ()
    (setq view-exit-action
          (lambda (buf) (buffer-op-and-restore-winconf buf 'bury-buffer)))))

(temp-buffer-resize-mode 1)

;; Stay in temp buffers (for navigation)
(defun eli-temp-buffer-show-function (buf &optional alist)
  ;; A rough Lisp version of the code in temp_output_buffer_show, which leaves
  ;; us in the new window unless it is a completions window; also take an
  ;; optional alist argument and return t so it can be used with
  ;; `display-buffer-alist'.
  (let* ((origbuf (current-buffer))
         (origwin (get-buffer-window origbuf))
         (temp-buffer-show-function nil)
         (display-buffer-alist nil)
         (conf (current-window-configuration))
         (win (display-buffer buf)))
    (unless (eq (selected-frame) (window-frame win))
      (make-frame-visible (window-frame win)))
    (setq minibuffer-scroll-window win)
    (set-window-start win 0)
    (set-window-hscroll win 0)
    (unless (equal (buffer-name buf) "*Completions*")
      (select-window win)
      (set-buffer buf)
      ;; save the window configuration here, while we can (but not if there is
      ;; one already, eg -- with help links)
      (unless (and (eq buf origbuf) (eq win origwin))
        (setq eli-popup-exit-conf conf))
      (run-hooks 'temp-buffer-show-hook)
      (select-window win)
      (view-mode 1)))
  t)
(setq temp-buffer-show-function 'eli-temp-buffer-show-function)

(setq display-buffer-alist
      '(("^[*]Shell Command Output[*]$" eli-temp-buffer-show-function)))

;;-----------------------------------------------------------------------------
;; Override from "userlock.el": make a changed file automatically reload

(when eli-auto-revert-on-change

(defvar inside-ask-user-about-supersession-threat nil)

;; do it after loading, so the new definition isn't replaced by the original
(eval-after-load "userlock" '(progn

(defun ask-user-about-supersession-threat (fn)
  "(DON'T) Ask a user who is about to modify an obsolete buffer what to do.
This function has two choices: it can return, in which case the modification
of the buffer will proceed, or it can (signal 'file-supersession (file)),
in which case the proposed buffer modification will not be made.

You can rewrite this to use any criterion you like to choose which one to do.
The buffer in question is current when this function is called.

THIS IS A MODIFIED VERSION THAT AUTOMATICALLY RELOADS THE FILE."
  (unless inside-ask-user-about-supersession-threat
    (let ((inside-ask-user-about-supersession-threat t)
          (opoint (point))
          (strt   (window-start (get-buffer-window (current-buffer))))
          (inhibit-read-only t))
      (discard-input)
      ;; hack: make the previous version available with undo
      (undo-boundary) (erase-buffer)
      (let ((saved-undo buffer-undo-list))
        (setq buffer-undo-list nil)
        (revert-buffer t t t)
        (setq buffer-undo-list
              (cons (cons (point-min) (point-max)) saved-undo)))
      (set-window-start (get-buffer-window (current-buffer)) strt)
      (goto-char opoint)
      (error "%s was modifed on disk, reloaded" (file-name-nondirectory fn)))))

))

)

;;-----------------------------------------------------------------------------
;; Override from "files.el": don't ask about reverting a buffer, just say it

(when eli-auto-revert-on-change

;; do it after loading, so the new definition isn't replaced by the original
(eval-after-load "files" '(progn

(defun find-file-noselect (filename &optional nowarn rawfile wildcards)
  "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one, but
verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller.
Optional second arg NOWARN non-nil means suppress any warning messages.
Optional third arg RAWFILE non-nil means the file is read literally.
Optional fourth arg WILDCARDS non-nil means do wildcard processing
and visit all the matching files.  When wildcards are actually
used and expanded, return a list of buffers that are visiting
the various files."
  (setq filename
	(abbreviate-file-name
	 (expand-file-name filename)))
  (if (file-directory-p filename)
      (or (and find-file-run-dired
	       (run-hook-with-args-until-success
		'find-directory-functions
		(if find-file-visit-truename
		    (abbreviate-file-name (file-truename filename))
		  filename)))
	  (error "%s is a directory" filename))
    (if (and wildcards
	     find-file-wildcards
	     (not (string-match "\\`/:" filename))
	     (string-match "[[*?]" filename))
	(let ((files (condition-case nil
			 (file-expand-wildcards filename t)
		       (error (list filename))))
	      (find-file-wildcards nil))
	  (if (null files)
	      (find-file-noselect filename)
	    (mapcar #'find-file-noselect files)))
      (let* ((buf (get-file-buffer filename))
	     (truename (abbreviate-file-name (file-truename filename)))
	     (attributes (file-attributes truename))
	     (number (nthcdr 10 attributes))
	     ;; Find any buffer for a file which has same truename.
	     (other (and (not buf) (find-buffer-visiting filename))))
	;; Let user know if there is a buffer with the same truename.
	(if other
	    (progn
	      (or nowarn
		  find-file-suppress-same-file-warnings
		  (string-equal filename (buffer-file-name other))
		  (message "%s and %s are the same file"
			   filename (buffer-file-name other)))
	      ;; Optionally also find that buffer.
	      (if (or find-file-existing-other-name find-file-visit-truename)
		  (setq buf other))))
	;; Check to see if the file looks uncommonly large.
	(when (not (or buf nowarn))
	  (abort-if-file-too-large (nth 7 attributes) "open" filename))
	(if buf
	    ;; We are using an existing buffer.
	    (let (nonexistent)
	      (or nowarn
		  (verify-visited-file-modtime buf)
		  (cond ((not (file-exists-p filename))
			 (setq nonexistent t)
			 (message "File %s no longer exists!" filename))
			;; Certain files should be reverted automatically
			;; if they have changed on disk and not in the buffer.
			((and (not (buffer-modified-p buf))
			      (let ((tail revert-without-query)
				    (found nil))
				(while tail
				  (if (string-match (car tail) filename)
				      (setq found t))
				  (setq tail (cdr tail)))
				found))
			 (with-current-buffer buf
			   (message "Reverting file %s..." filename)
			   (revert-buffer t t)
			   (message "Reverting file %s...done" filename)))
                        ;;ELI:
                        (t
                         (message "File %s changed on disk!%s"
                                  (file-name-nondirectory filename)
                                  (if (buffer-modified-p buf)
                                    " (note: local modifications)"
                                    " (touch to revert)"))
                         (ding t))
			((yes-or-no-p
			  (if (string= (file-name-nondirectory filename)
				       (buffer-name buf))
			      (format
			       (if (buffer-modified-p buf)
				   "File %s changed on disk.  Discard your edits? "
				 "File %s changed on disk.  Reread from disk? ")
			       (file-name-nondirectory filename))
			    (format
			     (if (buffer-modified-p buf)
				 "File %s changed on disk.  Discard your edits in %s? "
			       "File %s changed on disk.  Reread from disk into %s? ")
			     (file-name-nondirectory filename)
			     (buffer-name buf))))
			 (with-current-buffer buf
			   (revert-buffer t t)))))
	      (with-current-buffer buf

		;; Check if a formerly read-only file has become
		;; writable and vice versa, but if the buffer agrees
		;; with the new state of the file, that is ok too.
		(let ((read-only (not (file-writable-p buffer-file-name))))
		  (unless (or nonexistent
			      (eq read-only buffer-file-read-only)
			      (eq read-only buffer-read-only))
		    (when (or nowarn
			      (let ((question
				     (format "File %s is %s on disk.  Change buffer mode? "
					     buffer-file-name
					     (if read-only "read-only" "writable"))))
				(y-or-n-p question)))
		      (setq buffer-read-only read-only)))
		  (setq buffer-file-read-only read-only))

		(when (and (not (eq (not (null rawfile))
				    (not (null find-file-literally))))
			   (not nonexistent)
			   ;; It is confusing to ask whether to visit
			   ;; non-literally if they have the file in
			   ;; hexl-mode or image-mode.
			   (not (memq major-mode '(hexl-mode image-mode))))
		  (if (buffer-modified-p)
		      (if (y-or-n-p
			   (format
			    (if rawfile
				"The file %s is already visited normally,
and you have edited the buffer.  Now you have asked to visit it literally,
meaning no coding system handling, format conversion, or local variables.
Emacs can only visit a file in one way at a time.

Do you want to save the file, and visit it literally instead? "
				"The file %s is already visited literally,
meaning no coding system handling, format conversion, or local variables.
You have edited the buffer.  Now you have asked to visit the file normally,
but Emacs can only visit a file in one way at a time.

Do you want to save the file, and visit it normally instead? ")
			    (file-name-nondirectory filename)))
			  (progn
			    (save-buffer)
			    (find-file-noselect-1 buf filename nowarn
						  rawfile truename number))
			(if (y-or-n-p
			     (format
			      (if rawfile
				  "\
Do you want to discard your changes, and visit the file literally now? "
				"\
Do you want to discard your changes, and visit the file normally now? ")))
			    (find-file-noselect-1 buf filename nowarn
						  rawfile truename number)
			  (error (if rawfile "File already visited non-literally"
				   "File already visited literally"))))
		    (if (y-or-n-p
			 (format
			  (if rawfile
			      "The file %s is already visited normally.
You have asked to visit it literally,
meaning no coding system decoding, format conversion, or local variables.
But Emacs can only visit a file in one way at a time.

Do you want to revisit the file literally now? "
			    "The file %s is already visited literally,
meaning no coding system decoding, format conversion, or local variables.
You have asked to visit it normally,
but Emacs can only visit a file in one way at a time.

Do you want to revisit the file normally now? ")
			  (file-name-nondirectory filename)))
			(find-file-noselect-1 buf filename nowarn
					      rawfile truename number)
		      (error (if rawfile "File already visited non-literally"
			       "File already visited literally"))))))
	      ;; Return the buffer we are using.
	      buf)
	  ;; Create a new buffer.
	  (setq buf (create-file-buffer filename))
	  ;; find-file-noselect-1 may use a different buffer.
	  (find-file-noselect-1 buf filename nowarn
				rawfile truename number))))))

)))

;;-----------------------------------------------------------------------------
;; Override from "files.el": make this respect case-folding

(defun file-expand-wildcards (pattern &optional full)
  "Expand wildcard pattern PATTERN.
This returns a list of file names which match the pattern.

If PATTERN is written as an absolute file name,
the values are absolute also.

If PATTERN is written as a relative file name, it is interpreted
relative to the current default directory, `default-directory'.
The file names returned are normally also relative to the current
default directory.  However, if FULL is non-nil, they are absolute."
  (save-match-data
    (let* ((nondir (file-name-nondirectory pattern))
	   (dirpart (file-name-directory pattern))
	   ;; A list of all dirs that DIRPART specifies.
	   ;; This can be more than one dir
	   ;; if DIRPART contains wildcards.
	   (dirs (if (and dirpart
			  (string-match "[[*?]"
					(or (file-remote-p dirpart 'localname)
					    dirpart)))
		     (mapcar 'file-name-as-directory
			     (file-expand-wildcards (directory-file-name dirpart)))
		   (list dirpart)))
	   contents)
      (while dirs
	(when (or (null (car dirs))	; Possible if DIRPART is not wild.
		  (and (file-directory-p (directory-file-name (car dirs)))
		       (file-readable-p (car dirs))))
	  (let ((this-dir-contents
		 ;; Filter out "." and ".."
		 (delq nil
		       (mapcar #'(lambda (name)
				   (let ((n (file-name-nondirectory name)))
                                     (and (not (member n '("." "..")))
                                          ;; ELI: do the wildcard filtering
                                          ;; here, not in directory files which
                                          ;; doesn't respect case folding
                                          (string-match (wildcard-to-regexp nondir)
                                                        n)
                                          name)))
			       (directory-files (or (car dirs) ".") full
						(wildcard-to-regexp nondir))))))
	    (setq contents
		  (nconc
		   (if (and (car dirs) (not full))
		       (mapcar (function (lambda (name) (concat (car dirs) name)))
			       this-dir-contents)
		     this-dir-contents)
		   contents))))
	(setq dirs (cdr dirs)))
      contents)))

;;-----------------------------------------------------------------------------
;; Override from "comint.el": remove the completions window on *any* key
;; other than the one used to show the completions (tab, usually).

(eval-after-load "comint" '(progn

(defun comint-dynamic-list-completions (completions &optional common-substring)
  "Display a list of sorted COMPLETIONS.
The meaning of COMMON-SUBSTRING is the same as in `display-completion-list'.
Typing any key flushes the completions buffer."
  (let ((window (get-buffer-window "*Completions*" 0)))
    (setq completions (sort completions 'string-lessp))
    (if (and (eq last-command this-command)
	     window (window-live-p window) (window-buffer window)
	     (buffer-name (window-buffer window))
	     ;; The above tests are not sufficient to detect the case where we
	     ;; should scroll, because the top-level interactive command may
	     ;; not have displayed a completions window the last time it was
	     ;; invoked, and there may be such a window left over from a
	     ;; previous completion command with a different set of
	     ;; completions.  To detect that case, we also test that the set
	     ;; of displayed completions is in fact the same as the previously
	     ;; displayed set.
	     (equal completions
		    (buffer-local-value 'comint-displayed-dynamic-completions
					(window-buffer window))))
	;; If this command was repeated, and
	;; there's a fresh completion window with a live buffer,
	;; and this command is repeated, scroll that window.
	(with-current-buffer (window-buffer window)
	  (if (pos-visible-in-window-p (point-max) window)
	      (set-window-start window (point-min))
	    (save-selected-window
	      (select-window window)
	      (scroll-up))))

      ;; Display a completion list for the first time.
      (setq comint-dynamic-list-completions-config
	    (current-window-configuration))
      (with-output-to-temp-buffer "*Completions*"
	(display-completion-list completions common-substring))
      (if (window-minibuffer-p (selected-window))
	  (minibuffer-message "Type anything to flush; repeat completion command to scroll")
	(message "Type space to flush; repeat completion command to scroll")))

    ;; Read the next key, to process SPC.
    (let (key first)
      (if (with-current-buffer (get-buffer "*Completions*")
	    (set (make-local-variable 'comint-displayed-dynamic-completions)
		 completions)
	    (setq key (read-key-sequence nil)
		  first (aref key 0))
	    (and (consp first) (consp (event-start first))
		 (eq (window-buffer (posn-window (event-start first)))
		     (get-buffer "*Completions*"))
		 (eq (key-binding key) 'mouse-choose-completion)))
	  ;; If the user does mouse-choose-completion with the mouse,
	  ;; execute the command, then delete the completion window.
	  (progn
	    (choose-completion first)
	    (set-window-configuration comint-dynamic-list-completions-config))
        ;; ELI: replacing this code
	;; (if (eq first ?\s)
	;;     (set-window-configuration comint-dynamic-list-completions-config)
	;;   (setq unread-command-events (listify-key-sequence key)))
        (if (not (eq first ?\t))
          (set-window-configuration comint-dynamic-list-completions-config))
        (setq unread-command-events (listify-key-sequence key))))))

))

;;-----------------------------------------------------------------------------
;; Overrides from "isearch.el": make `isearch-allow-scroll' not remove shift
;; modifiers, so it plays nicely with cua-mode (have an active region, then do
;; a search, then do a movement with shift: the region should not be lost).
;; Also, make scroll keys simply leave isearch if the point goes out of the
;; screen.

(defun isearch-reread-key-sequence-naturally (keylist)
  "Reread key sequence KEYLIST with an inactive Isearch-mode keymap.
Return the key sequence as a string/vector."
  (isearch-unread-key-sequence keylist)
  (let (overriding-terminal-local-map)
    ;; This will go through function-key-map, if nec.
    (read-key-sequence nil nil t))) ;ELI: Add the t

(defun isearch-other-meta-char (&optional arg)
  "Process a miscellaneous key sequence in Isearch mode.

Try to convert the current key-sequence to something usable in Isearch
mode, either by converting it with `function-key-map', downcasing a
key with C-<upper case>, or finding a \"scrolling command\" bound to
it.  \(In the last case, we may have to read more events.)  If so,
either unread the converted sequence or execute the command.

Otherwise, if `search-exit-option' is non-nil (the default) unread the
key-sequence and exit the search normally.  If it is the symbol
`edit', the search string is edited in the minibuffer and the meta
character is unread so that it applies to editing the string.

ARG is the prefix argument.  It will be transmitted through to the
scrolling command or to the command whose key-sequence exits
Isearch mode."
  (interactive "P")
  (let* ((key (if current-prefix-arg    ; not nec the same as ARG
                  (substring (this-command-keys) universal-argument-num-events)
                (this-command-keys)))
	 (main-event (aref key 0))
	 (keylist (listify-key-sequence key))
         scroll-command isearch-point)
    (cond ((and (= (length key) 1)
		(let ((lookup (lookup-key local-function-key-map key)))
		  (not (or (null lookup) (integerp lookup)
			   (keymapp lookup)))))
	   ;; Handle a function key that translates into something else.
	   ;; If the key has a global definition too,
	   ;; exit and unread the key itself, so its global definition runs.
	   ;; Otherwise, unread the translation,
	   ;; so that the translated key takes effect within isearch.
	   (cancel-kbd-macro-events)
	   (if (lookup-key global-map key)
	       (progn
		 (isearch-done)
		 (setq prefix-arg arg)
		 (apply 'isearch-unread keylist))
	     (setq keylist
		   (listify-key-sequence
		    (lookup-key local-function-key-map key)))
	     (while keylist
	       (setq key (car keylist))
	       ;; If KEY is a printing char, we handle it here
	       ;; directly to avoid the input method and keyboard
	       ;; coding system translating it.
	       (if (and (integerp key)
			(>= key ?\s) (/= key 127) (< key 256))
		   (progn
		     ;; Ensure that the processed char is recorded in
		     ;; the keyboard macro, if any (Bug#4894)
		     (store-kbd-macro-event key)
		     (isearch-process-search-char key)
		     (setq keylist (cdr keylist)))
		 ;; As the remaining keys in KEYLIST can't be handled
		 ;; here, we must reread them.
		 (setq prefix-arg arg)
		 (apply 'isearch-unread keylist)
		 (setq keylist nil)))))
	  (
	   ;; Handle an undefined shifted control character
	   ;; by downshifting it if that makes it defined.
	   ;; (As read-key-sequence would normally do,
	   ;; if we didn't have a default definition.)
	   (let ((mods (event-modifiers main-event)))
	     (and (integerp main-event)
		  (memq 'shift mods)
		  (memq 'control mods)
		  (not (memq (lookup-key isearch-mode-map
					 (let ((copy (copy-sequence key)))
					   (aset copy 0
						 (- main-event
						    (- ?\C-\S-a ?\C-a)))
					   copy)
					 nil)
			     '(nil
			       isearch-other-control-char)))))
	   (setcar keylist (- main-event (- ?\C-\S-a ?\C-a)))
	   (cancel-kbd-macro-events)
	   (setq prefix-arg arg)
	   (apply 'isearch-unread keylist))
	  ((eq search-exit-option 'edit)
	   (setq prefix-arg arg)
	   (apply 'isearch-unread keylist)
	   (isearch-edit-string))
          ;; ELI: The following branch is moved here for convenience
	  ;; A mouse click on the isearch message starts editing the search string
	  ((and (eq (car-safe main-event) 'down-mouse-1)
		(window-minibuffer-p (posn-window (event-start main-event))))
	   ;; Swallow the up-event.
	   (read-event)
	   (isearch-edit-string))
          ;; Handle a scrolling function.
          ((and isearch-allow-scroll
                (progn (setq key (isearch-reread-key-sequence-naturally keylist))
                       (setq keylist (listify-key-sequence key))
                       (setq main-event (aref key 0))
                       (setq scroll-command (isearch-lookup-scroll-key key))))
           ;; From this point onwards, KEY, KEYLIST and MAIN-EVENT hold a
           ;; complete key sequence, possibly as modified by function-key-map,
           ;; not merely the one or two event fragment which invoked
           ;; isearch-other-meta-char in the first place.
           (setq isearch-point (point))
           (setq prefix-arg arg)
           (command-execute scroll-command)
           (let ((ab-bel (isearch-string-out-of-window isearch-point)))
             (if ab-bel
                 ;; ELI: if got out of window -- get out of isearch
                 ;; (isearch-back-into-window (eq ab-bel 'above) isearch-point)
                 ;; This is an exact copy of the next case, except for the
                 ;; keyboard stuff
                 (let (window)
                   ;; (setq prefix-arg arg)
                   ;; (isearch-unread-key-sequence keylist)
                   ;; (setq main-event (car unread-command-events))
                   (if (and (not isearch-mode)
                            (listp main-event)
                            (setq window (posn-window (event-start main-event)))
                            (windowp window)
                            (or (> (minibuffer-depth) 0)
                                (not (window-minibuffer-p window))))
                     (with-current-buffer (window-buffer window)
                       (isearch-done)
                       (isearch-clean-overlays))
                     (isearch-done)
                     (isearch-clean-overlays)
                     (setq prefix-arg arg)))
               (goto-char isearch-point)
               (isearch-update)))) ; ELI: moved inside here
	  (search-exit-option
	   (let (window)
	     (setq prefix-arg arg)
             (isearch-unread-key-sequence keylist)
             (setq main-event (car unread-command-events))

	     ;; If we got a mouse click event, that event contains the
	     ;; window clicked on. maybe it was read with the buffer
	     ;; it was clicked on.  If so, that buffer, not the current one,
	     ;; is in isearch mode.  So end the search in that buffer.

	     ;; ??? I have no idea what this if checks for, but it's
	     ;; obviously wrong for the case that a down-mouse event
	     ;; on another window invokes this function.  The event
	     ;; will contain the window clicked on and that window's
	     ;; buffer is certainly not always in Isearch mode.
	     ;;
	     ;; Leave the code in, but check for current buffer not
	     ;; being in Isearch mode for now, until someone tells
	     ;; what it's really supposed to do.
	     ;;
	     ;; --gerd 2001-08-10.

	     (if (and (not isearch-mode)
		      (listp main-event)
		      (setq window (posn-window (event-start main-event)))
		      (windowp window)
		      (or (> (minibuffer-depth) 0)
			  (not (window-minibuffer-p window))))
		 (with-current-buffer (window-buffer window)
		   (isearch-done)
		   (isearch-clean-overlays))
	       (isearch-done)
	       (isearch-clean-overlays)
               (setq prefix-arg arg))))
          (t;; otherwise nil
	   (isearch-process-search-string key key)))))

;;; override.el ends here
