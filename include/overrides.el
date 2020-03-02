;;; override.el --- Various overrides.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

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
specifies what to do when the user exits the help buffer.

Do not call this in the scope of `with-help-window'."
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

(defvar-local eli-popup-exit-conf nil
  "Window configuration to restore after quitting viewing a temp buffer.")

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
  ;; `display-buffer-alist'.  Well, while we're at it, allow a `truncate-lines'
  ;; in the alist to do the obvious thing.
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
      (let ((tl (assq 'truncate-lines alist)))
        (when tl (setq truncate-lines (cdr tl))))
      (select-window win)
      (view-mode 1)))
  t)
(setq temp-buffer-show-function 'eli-temp-buffer-show-function)

;;-----------------------------------------------------------------------------
;; Override from "userlock.el": make a changed file automatically reload

(defvar-local eli-buffer-reverted 0)

(when eli-auto-revert-on-change

(defvar inside-ask-user-about-supersession-threat nil)

;; do it after loading, so the new definition isn't replaced by the original
(eval-after-load "userlock" '(progn

(defun ask-user-about-supersession-threat (fn)
  "(DON'T!) Ask a user who is about to modify an obsolete buffer what to do.
This function has two choices: it can return, in which case the modification
of the buffer will proceed, or it can (signal \\='file-supersession (file)),
in which case the proposed buffer modification will not be made.

You can rewrite this to use any criterion you like to choose which one to do.
The buffer in question is current when this function is called.

THIS IS A MODIFIED VERSION THAT AUTOMATICALLY RELOADS THE FILE."
  (unless inside-ask-user-about-supersession-threat
    (let* ((inside-ask-user-about-supersession-threat t)
           (win    (get-buffer-window (current-buffer)))
           (opoint (point))
           (wstart (window-start win))
           (inhibit-read-only t)
           (has-undo (listp buffer-undo-list)))
      (discard-input)
      ;; hack: make the previous version available with undo
      (undo-boundary)
      (let ((saved-undo (and has-undo buffer-undo-list))
            (bufstr (buffer-string)))
        (when has-undo (setq buffer-undo-list t))
        (erase-buffer)
        (revert-buffer t t t)
        (set-window-start win (min wstart (point-max)))
        (goto-char (min opoint (point-max)))
        (when has-undo
          (push opoint saved-undo)                         ; cursor position
          (push `(,bufstr . ,(- (point-min))) saved-undo)  ; buffer deleted
          (push `(,(point-min) . ,(point-max)) saved-undo) ; new text inserted
          (setq buffer-undo-list saved-undo)))
      (undo-boundary)
      (setq eli-buffer-reverted (1+ eli-buffer-reverted))
      (signal 'file-supersession
              (list "File reverted" (file-name-nondirectory fn))))))

))

)

;;-----------------------------------------------------------------------------
;; Override from "files.el": expand ".../**/..." to all dirs

(eval-after-load "files" '(progn

(defun eli-join-paths (p1 p2)
  (if (file-name-absolute-p p2) p2
      (concat (file-name-as-directory p1) p2)))

(defun directory-dirtree (directory &optional full)
  (let* ((loop #'(lambda (name)
                   (and (not (string-match "\\`\\.\\.?\\'"
                                           (file-name-nondirectory name)))
                        (let ((name (eli-join-paths directory name)))
                          (and (file-accessible-directory-p name)
                               (directory-dirtree name full))))))
         (subs (mapcar loop (directory-files directory full))))
    (cons directory (apply #'nconc (delq nil subs)))))

(defun file-expand-wildcards (pattern &optional full)
  "Expand wildcard pattern PATTERN.
This returns a list of file names which match the pattern.
Files are sorted in `string<' order.

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
			  (string-match "[[*?]" ;(file-local-name dirpart)
					(or (file-remote-p dirpart 'localname)
					    dirpart)))
		     (mapcar 'file-name-as-directory
			     (file-expand-wildcards (directory-file-name dirpart)))
		   (list dirpart)))
	   contents)
      (dolist (dir dirs)
	(when (or (null dir)	; Possible if DIRPART is not wild.
		  (file-accessible-directory-p dir))
	  (let ((this-dir-contents
		 ;; Filter out "." and ".."
		 (delq nil
		       (mapcar #'(lambda (name)
				   (unless (string-match "\\`\\.\\.?\\'"
							 (file-name-nondirectory name))
				     name))
			       (if (equal "**" nondir)
                                 (directory-dirtree (or dir ".") full)
                                 (directory-files (or dir ".") full
						  (wildcard-to-regexp nondir)))))))
	    (setq contents
		  (nconc
		   (if (and dir (not full))
		       (mapcar #'(lambda (name) (eli-join-paths dir name))
			       this-dir-contents)
		     this-dir-contents)
		   contents)))))
      contents)))

))

;;-----------------------------------------------------------------------------
;; Override from "files.el": don't ask about reverting a buffer, just say it

(when eli-auto-revert-on-change

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
		  (files--message "%s and %s are the same file"
                                  filename (buffer-file-name other)))
	      ;; Optionally also find that buffer.
	      (if (or find-file-existing-other-name find-file-visit-truename)
		  (setq buf other))))
	;; Check to see if the file looks uncommonly large.
	(when (not (or buf nowarn))
	  (abort-if-file-too-large (nth 7 attributes) "open" filename)
	  (warn-maybe-out-of-memory (nth 7 attributes)))
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
			      (let* ((new-status
				      (if read-only "read-only" "writable"))
				     (question
				      (format "File %s is %s on disk.  Make buffer %s, too? "
					      buffer-file-name
					      new-status new-status)))
				(y-or-n-p question)))
		      (setq buffer-read-only read-only)))
		  (setq buffer-file-read-only read-only))

		(unless (or (eq (null rawfile) (null find-file-literally))
			    nonexistent
			    ;; It is confusing to ask whether to visit
			    ;; non-literally if they have the file in
			    ;; hexl-mode or image-mode.
			    (memq major-mode '(hexl-mode image-mode)))
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
;; Override from "isearch.el": Also, make scroll keys simply leave isearch if
;; the point goes out of the screen.

(defun isearch-post-command-hook ()
  (when isearch-pre-scroll-point
    (let ((ab-bel (isearch-string-out-of-window isearch-pre-scroll-point)))
      ;; ELI: add the visibility check (which the above doesn't detect)
      (if (or ab-bel (not (pos-visible-in-window-p isearch-pre-scroll-point)))
	  ;; ELI: disable the following, and exit isearch instead
	  ;; (isearch-back-into-window (eq ab-bel 'above) isearch-pre-scroll-point)
	  (progn (setq isearch-pre-scroll-point nil)
                 (isearch-exit))
	;; ELI: also drag the rest of the function here
	(progn (goto-char isearch-pre-scroll-point)
	       (setq isearch-pre-scroll-point nil)
	       (isearch-update))))))

;;-----------------------------------------------------------------------------
;; Override from "comint.el": don't leave point where it was, let it
;; move to the EOL.  (Feature request for such an option submitted.)

(eval-after-load "comint" '(progn

(defun comint-previous-matching-input-from-input (n)
  "Search backwards through input history for match for current input.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, search forwards for the -Nth following match."
  (interactive "p")
  (let (; (opoint (point)) ELI: unused
        )
    (unless (memq last-command '(comint-previous-matching-input-from-input
				 comint-next-matching-input-from-input))
      ;; Starting a new search
      (setq comint-matching-input-from-input-string
	    (buffer-substring
	     (or (marker-position comint-accum-marker)
		 (process-mark (get-buffer-process (current-buffer))))
	     (point))
	    comint-input-ring-index nil))
    (comint-previous-matching-input
     (concat "^" (regexp-quote comint-matching-input-from-input-string))
     n)
    ;; ELI: don't do this: (goto-char opoint)
    ))

))

;;-----------------------------------------------------------------------------
;; General tool to add ^ to interactive specs, and using in a few places

(defmacro add-^-to-interactive-command (name)
  `(advice-add ',name :filter-return
     (lambda (x)
       "Add ^ to the interactive spec of this function"
       (interactive (lambda (o) (advice-eval-interactive-spec (concat "^" o))))
       x)))

(add-^-to-interactive-command forward-page)
(add-^-to-interactive-command backward-page)

(eval-after-load "dired" '(progn
  (add-^-to-interactive-command dired-next-dirline)
  (add-^-to-interactive-command dired-prev-dirline)))

;;-----------------------------------------------------------------------------
;; Override from "server.el": don't ask questions for unmodified buffers.

(eval-after-load "server" '(progn

(defun server-kill-emacs-query-function ()
  "Ask before exiting Emacs if it has live clients."
  (or (not (let (live-client)
             (dolist (proc server-clients)
               ;; ELI: also check modifications and ignore unmodified buffers
               (when (memq t (mapcar (lambda (buf) (and (buffer-live-p buf)
                                                        (buffer-modified-p buf)))
                                     (process-get proc 'buffers)))
                 (setq live-client t)))
             live-client))
      (yes-or-no-p "This Emacs session has clients; exit anyway? ")))

(defun server-kill-buffer-query-function ()
  "Ask before killing a server buffer."
  (or (not server-buffer-clients)
      ;; ELI: don't ask if the buffer was saved
      (not (buffer-modified-p))
      (let ((res t))
	(dolist (proc server-buffer-clients)
          (when (and (memq proc server-clients)
                     (eq (process-status proc) 'open))
            (setq res nil)))
         res)
      (yes-or-no-p (format "Buffer `%s' still has clients; kill it? "
			   (buffer-name (current-buffer))))))

))

;;-----------------------------------------------------------------------------
;; Override from "delsel.el": arrange for a single undo boundary.

(defvar-local delsel-undo-to-tweak t)

(eval-after-load "delsel" '(progn

(define-minor-mode delete-selection-mode
  "Toggle Delete Selection mode.
Interactively, with a prefix argument, enable
Delete Selection mode if the prefix argument is positive,
and disable it otherwise.  If called from Lisp, toggle
the mode if ARG is `toggle', disable the mode if ARG is
a non-positive integer, and enable the mode otherwise
\(including if ARG is omitted or nil or a positive integer).

When Delete Selection mode is enabled, typed text replaces the selection
if the selection is active.  Otherwise, typed text is just inserted at
point regardless of any selection.

See `delete-selection-helper' and `delete-selection-pre-hook' for
information on adapting behavior of commands in Delete Selection mode."
  :global t :group 'editing-basics
  (if (not delete-selection-mode)
      (progn (remove-hook 'pre-command-hook 'delete-selection-pre-hook)
             (remove-hook 'post-command-hook 'delete-selection-post-hook)) ;ELI
    (progn (add-hook 'pre-command-hook 'delete-selection-pre-hook)
           (add-hook 'post-command-hook 'delete-selection-post-hook))))    ;ELI

(defun delete-selection-pre-hook ()
  "Function run before commands that delete selections are executed.
Commands which will delete the selection need a `delete-selection'
property on their symbol; commands which insert text but don't
have this property won't delete the selection.
See `delete-selection-helper'."
  (when (and delete-selection-mode (use-region-p)
	     (not buffer-read-only))
    (setq delsel-undo-to-tweak buffer-undo-list)     ;ELI
    (let ((old-buffer-reverted eli-buffer-reverted)) ;ELI
      (delete-selection-helper (and (symbolp this-command)
                                    (get this-command 'delete-selection)))
      ;;ELI:
      (unless (equal old-buffer-reverted eli-buffer-reverted)
        (setq delsel-undo-to-tweak t)
        (setq this-command 'delete-selection-command-after-revert))
      (when (eq delsel-undo-to-tweak buffer-undo-list)
        (setq delsel-undo-to-tweak t)))))

(defun delete-selection-command-after-revert (&rest _)
  (interactive)
  (message "Buffer reverted, edit canceled"))

(defun delete-selection-post-hook ()
  "Function run after commands to make `delete-selection' deletions not
have an extra undo boundary."
  (interactive)
  (when (and (listp buffer-undo-list) (listp delsel-undo-to-tweak))
    (let ((undos buffer-undo-list) (p '(nil)) (seen-nil nil))
      (while (and undos (not (eq undos delsel-undo-to-tweak)))
        (if (car undos) (push (car undos) p) (setq seen-nil t))
        (setq undos (cdr undos)))
      (when (and seen-nil (eq undos delsel-undo-to-tweak))
        (while p (push (pop p) undos))
        (setq buffer-undo-list undos)))
    (setq delsel-undo-to-tweak t)))

))

;;; override.el ends here
