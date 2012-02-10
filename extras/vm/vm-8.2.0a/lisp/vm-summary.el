;;; vm-summary.el --- Summary gathering and formatting routines for VM
;;
;; This file is part of VM
;;
;; Copyright (C) 1989-1995, 2000 Kyle E. Jones
;; Copyright (C) 2003-2006 Robert Widhopf-Fenk
;; Copyright (C) 2009-2010 Uday S Reddy
;; Copyright (C) 2010 Arik Mitschang
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Code:

(provide 'vm-summary)

(eval-when-compile
  (require 'vm-misc)
  (require 'vm-crypto)
  (require 'vm-folder)
  (require 'vm-window)
  (require 'vm-menu)
  (require 'vm-toolbar)
  (require 'vm-mouse)
  (require 'vm-motion)
  (require 'vm-mime)
  (require 'vm-thread)
  (require 'vm-pop)
  (require 'vm-summary-faces)
)

(declare-function set-specifier "vm-xemacs" 
		  (specifier value &optional locale tag-set how-to-add))
(declare-function rfc822-addresses "ext:rfc822" (header-text))

(declare-function vm-visit-folder "vm.el" (folder &optional read-only))

(defvar scrollbar-height)		; defined for XEmacs


(defsubst vm-summary-message-at-point ()
  "Returns the message of the current summary line."
  (save-excursion
    (forward-line 0)
    ;; The point often ends up preceding the invisible stuff.  Skip it.
    (while (get-text-property (point) 'invisible)
      (forward-char))
    (get-text-property (+ (point) 3) 'vm-message)))

(defsubst vm-summary-padded-thread-count (m)
  "Returns a formatted thread count of the message M, usable in
summary display."
  (let ((count (vm-thread-count m)))
    (if (> count 1)
	(format "+%-2s" (1- (vm-thread-count m)))
      "   ")))

(defsubst vm-summary-message-number-thread-descendant (m)
  "Returns the message number of M, padded with spaces to display as
an interior message of a thread."
  (concat "  " (vm-padded-number-of m) " "))

(defsubst vm-expanded-root-p (m)
  "Returns t if M is the root of a thread that is currently shown
expanded (using the folded attribute of the message)."
  (and (vm-thread-root-p m)
       (null (vm-folded-flag m))))

(defsubst vm-collapsed-root-p (m)
  "Returns t if M is the root fo a thread that is currently shown
  collapsed (usint the folded attribute of the message)."
  (and (vm-thread-root-p m)
       (vm-folded-flag m)))

(defsubst vm-summary-mark-root-collapsed (m)
  "Mark a thread root message M as collapsed."
  (vm-set-folded-flag m t))

(defsubst vm-summary-mark-root-expanded (m)
  "Mark a thread root message M as expanded."
  (vm-set-folded-flag m nil))

(defsubst vm-visible-message (m)
  (apply 'vm-vs-or m vm-summary-visible))

(defun vm-summary-mode-internal ()
  (setq mode-name "VM Summary"
	major-mode 'vm-summary-mode
	mode-line-format vm-mode-line-format
	;; must come after the setting of major-mode
	mode-popup-menu (and vm-use-menus
			     (vm-menu-support-possible-p)
			     (vm-menu-mode-menu))
	buffer-read-only t
	vm-summary-pointer nil
	vm-summary-=> (if (stringp vm-summary-arrow) vm-summary-arrow "")
	vm-summary-no-=> (make-string (length vm-summary-=>) ? )
	truncate-lines t)
  ;; horizontal scrollbar off by default
  ;; user can turn it on in summary hook if desired.
  (when (and vm-xemacs-p (featurep 'scrollbar))
    (set-specifier scrollbar-height (cons (current-buffer) 0)))
  (use-local-map vm-summary-mode-map)
  (when (vm-menu-support-possible-p)
    (vm-menu-install-menus))
;; using the 'mouse-face property gives faster highlighting than this.
;;  (and vm-mouse-track-summary
;;       (vm-mouse-support-possible-p)
;;       (vm-mouse-xemacs-mouse-p)
;;       (add-hook 'mode-motion-hook 'mode-motion-highlight-line))
  (when (and vm-mutable-frames (or vm-frame-per-folder vm-frame-per-summary))
    (vm-set-hooks-for-frame-deletion))
  (run-hooks 'vm-summary-mode-hook)
  ;; Lucid Emacs apparently used this name
  (run-hooks 'vm-summary-mode-hooks))

(fset 'vm-summary-mode 'vm-mode)
(put 'vm-summary-mode 'mode-class 'special)

;;;###autoload
(defun vm-summarize (&optional display raise)
  "Summarize the contents of the folder in a summary buffer.
The format is as described by the variable `vm-summary-format'.  Generally
one line per message is most pleasing to the eye but this is not
mandatory."
  (interactive "p\np")
  (vm-select-folder-buffer-and-validate 0 (interactive-p))
  (if (null vm-summary-buffer)
      (let ((b (current-buffer))
	    (read-only vm-folder-read-only)
	    (summary-buffer-name (format "%s Summary" (buffer-name))))
	(setq vm-summary-buffer
	      (or (get-buffer summary-buffer-name)
		  (vm-generate-new-multibyte-buffer summary-buffer-name)))
	(save-excursion
	  (set-buffer vm-summary-buffer)
	  (abbrev-mode 0)
	  (auto-fill-mode 0)
	  (vm-fsfemacs-nonmule-display-8bit-chars)
	  (if (fboundp 'buffer-disable-undo)
	      (buffer-disable-undo (current-buffer))
	    ;; obfuscation to make the v19 compiler not whine
	    ;; about obsolete functions.
	    (let ((x 'buffer-flush-undo))
	      (funcall x (current-buffer))))
	  (setq vm-mail-buffer b
		vm-folder-read-only read-only)
	  (vm-summary-mode-internal))
	(vm-set-summary-redo-start-point t)))
  (if display
      (save-excursion
	(vm-goto-new-summary-frame-maybe)
	(vm-display vm-summary-buffer t
		    '(vm-summarize
		      vm-summarize-other-frame)
		    (list this-command) (not raise))
	;; need to do this after any frame creation because the
	;; toolbar sets frame-specific height and width specifiers.
	(set-buffer vm-summary-buffer)
	(vm-toolbar-install-or-uninstall-toolbar))
    (vm-display nil nil '(vm-summarize vm-summarize-other-frame)
		(list this-command)))
  (vm-update-summary-and-mode-line))

;;;###autoload
(defun vm-summarize-other-frame (&optional display)
  "Like vm-summarize, but run in a newly created frame."
  (interactive "p")
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'summary))
  (vm-summarize display)
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

(defun vm-do-summary (&optional start-point)
  "Generate summary lines for all the messages in the optional
argument START-POINT (a list of messages) or, if it is nil, all
the messages in the current folder."
  (let ((m-list (or start-point vm-message-list))
	mp m root
	(n 0)
	(modulus 100)
	(do-mouse-track (or (and vm-mouse-track-summary
				 (vm-mouse-support-possible-p))
			    vm-summary-enable-faces)))
    (setq mp m-list)
    (save-excursion
      (set-buffer vm-summary-buffer)
      (setq line-move-ignore-invisible vm-summary-show-threads)
      (let ((buffer-read-only nil)
	    (modified (buffer-modified-p))
	    (debug nil) ; vm-summary-debug, if necessary
	    o
	    )
	(unwind-protect
	    (progn
	      (if start-point
		  (if (vm-su-start-of (car mp))
		      (progn
			(goto-char (vm-su-start-of (car mp)))
			(vm-disable-extents (point) (point-max))
			(delete-region (point) (point-max)))
		    (goto-char (point-max)))
		(goto-char (point-min))
		(vm-disable-extents)
		(erase-buffer)
		(setq vm-summary-pointer nil))
	      ;; avoid doing long runs down the marker chain while
	      ;; building the summary.  use integers to store positions
	      ;; and then convert them to markers after all the
	      ;; insertions are done.  Likewise, detach overlays and
	      ;; re-establish them afterwards.
	      (vm-inform 7 "Generating summary... %d" n)
	      (overlay-recenter (point))
	      (setq mp m-list)
	      (while mp
		(setq m (car mp))
		(when (markerp (vm-su-start-of m))
		  (set-marker (vm-su-start-of m) nil))
		(when (markerp (vm-su-end-of m))
		  (set-marker (vm-su-end-of m) nil))
		(when (setq o (vm-su-summary-mouse-track-overlay-of m))
		  (vm-detach-extent o))
		(setq mp (cdr mp)))

	      (overlay-recenter (point-max))

	      (setq mp m-list)
	      (while mp
                (setq m (car mp))
		(if (and vm-debug
			 (member (vm-number-of m) vm-summary-traced-messages))
		    (debug))
		(vm-set-su-start-of m (point))
		(insert vm-summary-no-=>)
		(vm-tokenized-summary-insert m (vm-su-summary m))
		(vm-set-su-end-of m (point))
                (let ((s (vm-su-start-of m)) (e (vm-su-end-of m)))
		  (when s
		    (put-text-property s e 'vm-message m)
		    (when (and vm-summary-enable-thread-folding
			       vm-summary-show-threads)
		      (if (= (vm-thread-indentation-of m) 0)
			  (when (> (vm-thread-count m) 1)
			    (if vm-summary-threads-collapsed
				(vm-summary-mark-root-collapsed m)
			      (vm-summary-mark-root-expanded m)))
			(setq root (vm-thread-root m))
			(when (and root (vm-collapsed-root-p root))
			  (unless (vm-visible-message m)
			    (put-text-property s e 'invisible t))
			  ;; why mess with the root here?  USR, 2010-07-20
			  ;; (vm-summary-mark-root-collapsed root)
			  )))))
		(setq mp (cdr mp) n (1+ n))
		(when (zerop (% n modulus))
		  (vm-inform 7 "Generating summary... %d" n)
		  (if debug (debug "vm-debug-summary: Generating summary"))
		  (setq debug nil)))
	      ;; now convert the ints to markers.
	      (setq mp m-list)
	      (while mp
		(setq m (car mp))
		(when do-mouse-track
		  (vm-set-su-summary-mouse-track-overlay-of
		   m
		   (vm-mouse-set-mouse-track-highlight
		    (vm-su-start-of m)
		    (vm-su-end-of m)
		    (vm-su-summary-mouse-track-overlay-of m))))
		(vm-set-su-start-of m (vm-marker (vm-su-start-of m)))
		(vm-set-su-end-of m (vm-marker (vm-su-end-of m)))
		(when vm-summary-enable-faces (vm-summary-faces-add m))
		(setq mp (cdr mp))))
	  (set-buffer-modified-p modified))
	(run-hooks 'vm-summary-redo-hook)))
    (if (>= n modulus)
	(unless vm-summary-debug 
	  (vm-inform 7 "Generating summary... done")))))

(defun vm-expand-thread (&optional root)
  "Expand the thread associated with the message at point. This
will make visible all invisible elements of the thread tree and
place a '-' character at the pointer position indicating that the
thread can be collapsed.

In a Lisp program, you should call it with an argument ROOT, which
is the root of the thread you want expanded."
  (interactive)
  (unless vm-summary-enable-thread-folding 
    (error "Thread folding not enabled"))
  (when (interactive-p)
    (vm-select-folder-buffer-and-validate 1 (interactive-p))
    (unless vm-summary-show-threads
      (error "Summary is not sorted by threads"))
    (vm-follow-summary-cursor)
    (set-buffer vm-summary-buffer))
  (let ((buffer-read-only nil))
    (unless root
      (setq root (vm-thread-root (vm-summary-message-at-point))))
    (when (> (vm-thread-count root) 1)
      (vm-summary-mark-root-expanded root)
      (vm-mark-for-summary-update root)
      (mapc
       (lambda (m) 
	 (put-text-property 
	  (vm-su-start-of m) (vm-su-end-of m) 'invisible nil))
       (vm-thread-subtree (vm-thread-symbol root)))
      (when (interactive-p)
	(vm-update-summary-and-mode-line)))))

(defun vm-collapse-thread (&optional nomove root)
  "Collapse the thread associated with the message at point. This
will make invisible all read and non-new elements of the thread
tree and will place a '+' character at the pointer position
indicating the thread can be expanded. Optional argument nomove
directs vm-collapse-thread to not take the default action of
moving the pointer to the thread root after collapsing.

In a Lisp program, you should call it with an additional argument
ROOT, which is the root of the thread you want collapsed."
  (interactive "P")
  (unless vm-summary-enable-thread-folding 
    (error "Thread folding not enabled"))
  (when (interactive-p)
    (vm-select-folder-buffer-and-validate 1 (interactive-p))
    (unless vm-summary-show-threads
      (error "Summary is not sorted by threads"))
    (vm-follow-summary-cursor)
    (set-buffer vm-summary-buffer))
  (let ((buffer-read-only nil)
	(msg nil))
    (unless root
      (setq msg (vm-summary-message-at-point))
      (setq root (vm-thread-root msg)))
    (when (> (vm-thread-count root) 1)
      (vm-summary-mark-root-collapsed root)
      (vm-mark-for-summary-update root)
      (mapc
       (lambda (m) 
	 (unless (or (eq m root) (vm-visible-message m))
	   (put-text-property 
	    (vm-su-start-of m) (vm-su-end-of m) 'invisible t)))
       (vm-thread-subtree (vm-thread-symbol root)))
      ;; move to the parent thread only when not
      ;; instructed not to, AND when the currently
      ;; selected message will become invisible
      (when (interactive-p)
	(unless nomove
	  (when (get-text-property (+ (vm-su-start-of msg) 3) 'invisible)
	    (goto-char (vm-su-start-of root))))
	(vm-update-summary-and-mode-line)))))
	
(defun vm-expand-all-threads ()
  "Expand all threads in the folder, which might have been collapsed
 (folded) earlier."
  (interactive)
  (vm-select-folder-buffer-and-validate 0 (interactive-p))
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (unless vm-summary-show-threads
    (error "Summary is not sorted by threads"))
  (let ((ml vm-message-list))
    (with-current-buffer vm-summary-buffer
      (save-excursion
	(mapc (lambda (m)
		(when (and (eq m (vm-thread-root m))
			   (> (vm-thread-count m) 1))
		  (vm-expand-thread m)))
	      ml))))
  (setq vm-summary-threads-collapsed nil)
  (when (interactive-p)
    (vm-update-summary-and-mode-line)))

(defun vm-collapse-all-threads ()
  "Collapse (fold) all threads in the folder so that only the roots of
the threads are shown in the Summary window."
  (interactive)
  (vm-select-folder-buffer-and-validate 0 (interactive-p))
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (unless vm-summary-show-threads
    (error "Summary is not sorted by threads"))
  (let ((ml vm-message-list)
	msg root)
    (with-current-buffer vm-summary-buffer
      (setq msg (vm-summary-message-at-point))
      (setq root (vm-thread-root msg))
      (save-excursion
	(mapc (lambda (m)
		(when (and (eq m (vm-thread-root m))
			   (> (vm-thread-count m) 1))
		  (vm-collapse-thread t m)))
	      ml))
      (when (interactive-p)
	(when (get-text-property (+ (vm-su-start-of msg) 3) 'invisible)
	  (goto-char (vm-su-start-of root))))))
  (setq vm-summary-threads-collapsed t)
  (when (interactive-p)
    (vm-update-summary-and-mode-line)))
      
(defun vm-toggle-thread ()
  "Toggle collapse/expand thread associated with message at point.
see `vm-expand-thread' and `vm-collapse-thread' for a description
of action."
  (interactive)
  (when (and vm-summary-enable-thread-folding vm-summary-show-threads)
    (vm-select-folder-buffer-and-validate 1 (interactive-p))
    (if (interactive-p)
	(vm-follow-summary-cursor))
    (when vm-summary-buffer
      (set-buffer vm-summary-buffer)
      (let ((buffer-read-only nil)
	    root next)
	(setq root (vm-thread-root (vm-summary-message-at-point)))
	(if (vm-expanded-root-p root)
	    (call-interactively 'vm-collapse-thread)
	  (call-interactively 'vm-expand-thread))
	))))

(defun vm-do-needed-summary-rebuild ()
  "Rebuild the summary lines of all the messages starting at
`vm-summary-redo-start-point'.  Also, reset the summary pointer
to the current message.  Do the latter anyway if
`vm-need-summary-pointer-update' is non-NIL.  All this, only if
the Summary buffer exists. "
  (if (and vm-summary-redo-start-point vm-summary-buffer)
      (progn
	(vm-copy-local-variables vm-summary-buffer 'vm-summary-show-threads)
	(vm-do-summary (and (consp vm-summary-redo-start-point)
			    vm-summary-redo-start-point))
	(setq vm-summary-redo-start-point nil)
	(when vm-message-pointer
	  (vm-set-summary-pointer (car vm-message-pointer)))
	(setq vm-need-summary-pointer-update nil))
    (when (and vm-need-summary-pointer-update
	       vm-summary-buffer
	       vm-message-pointer)
      (vm-set-summary-pointer (car vm-message-pointer))
      (setq vm-need-summary-pointer-update nil))))

(defun vm-update-message-summary (m)
  "Replace the summary line of the message M in the summary
buffer by a regenerated summary line."
  (if (and vm-debug (member (vm-number-of m) vm-summary-traced-messages))
      (debug))
  (if (and (vm-su-start-of m)
	   (marker-buffer (vm-su-start-of m)))
      (let ((modified (buffer-modified-p)) ; Folder or Presentation
	    (do-mouse-track
	     (or (and vm-mouse-track-summary
		      (vm-mouse-support-possible-p))
		 vm-summary-enable-faces))
	    summary)
	(save-excursion
	  (setq summary (vm-su-summary m))
	  (set-buffer (marker-buffer (vm-su-start-of m)))
	  (let ((buffer-read-only nil)
		s e i
		(selected nil)
		(indicator nil)
		(modified (buffer-modified-p))) ; Summary buffer
	    (unwind-protect
		(save-excursion
		  (goto-char (vm-su-start-of m))
		  (setq selected (looking-at "[+-]>"))
		  (if (and vm-summary-show-threads
			   (eq m (vm-thread-root m))
			   (> (vm-thread-count m) 1))
		      (setq indicator (if (vm-collapsed-root-p m) "+" "-"))
		    (setq indicator nil))
		  ;; We do a little dance to update the text in
		  ;; order to make the markers in the text do
		  ;; what we want.
		  ;;
		  ;; 1. We need to avoid having the su-start-of
		  ;;    and su-end-of markers clumping together at
		  ;;    the start position.
		  ;;
		  ;; 2. We want the window point marker (w->pointm
		  ;;    in the Emacs display code) to move to the
		  ;;    start of the summary entry if it is
		  ;;    anywhere within the su-start-of to
		  ;;    su-end-of region.
		  ;;
		  ;; We achieve (2) by deleting before inserting.
		  ;; Reversing the order of insertion/deletion
		  ;; pushes the point marker into the next
		  ;; summary entry. We achieve (1) by inserting a
		  ;; placeholder character at the end of the
		  ;; summary entry before deleting the region.		  
                  (goto-char (vm-su-end-of m)) 		
                  (insert-before-markers "z")
		  (goto-char (vm-su-start-of m))
		  (setq s (vm-su-start-of m))
		  (setq e (vm-su-end-of m))
		  (setq i (get-text-property (+ s 2) 'invisible))
		  (delete-region (point) (1- (vm-su-end-of m)))		  
		  (if (not selected)		     
		      (insert (concat (or indicator " ") " "))
		    (if indicator
			(insert (concat indicator ">"))
		      (insert vm-summary-=>)))
		  (vm-tokenized-summary-insert m (vm-su-summary m))
	          (delete-char 1)	; delete "z"
		  (run-hooks 'vm-summary-update-hook)
		  (when do-mouse-track
		    (vm-mouse-set-mouse-track-highlight
		     (vm-su-start-of m)
		     (vm-su-end-of m)
		     (vm-su-summary-mouse-track-overlay-of m)))
		  (if vm-summary-enable-faces
		      (vm-summary-faces-add m)
		    (if (and selected 
			     (facep vm-summary-highlight-face))
			(vm-summary-highlight-region 
			 (vm-su-start-of m) (point)
			 vm-summary-highlight-face))))
	      (when s
		(put-text-property s e 'vm-message m)
		(put-text-property s e 'invisible i))
	      (vm-reset-buffer-modified-p  ; Summary buffer
	       modified (current-buffer))
	      ))))))

(defun vm-set-summary-pointer (m)
  "Set the summary-pointer in the summary window to the message M.
Also move the cursor (point and window-point)."
  (if vm-summary-buffer
      (let ((w (vm-get-visible-buffer-window vm-summary-buffer))
	    (do-mouse-track
	     (or (and vm-mouse-track-summary
		      (vm-mouse-support-possible-p))
		 vm-summary-enable-faces))
	    (old-window nil))
	(with-current-buffer vm-summary-buffer
	  (when w
	    (setq old-window (selected-window))
	    (select-window w))
	  (unwind-protect
	      (let ((buffer-read-only nil))
		(when (and vm-summary-pointer
			   (vm-su-start-of vm-summary-pointer))
		  (goto-char (vm-su-start-of vm-summary-pointer))
		  (if (not (get-text-property (+ (point) 3) 'invisible))
		      (let ((msg (vm-summary-message-at-point)))
			(if (and vm-summary-show-threads
				 vm-summary-enable-thread-folding
				 (eq msg (vm-thread-root msg))
				 (> (vm-thread-count msg) 1))
			    (if (vm-collapsed-root-p msg)
				(progn (insert "+ ") 
				       (delete-char (length vm-summary-=>)))
			      (progn (insert "- ")
				     (delete-char (length vm-summary-=>))))
			  (insert vm-summary-no-=>)
			  (delete-char (length vm-summary-=>))))
		    (delete-char (length vm-summary-=>))
		    (insert vm-summary-no-=>)
		    ;; re-invisible it so we dont have problems
		    (put-text-property 
		     (- (point) (length vm-summary-no-=>)) (point) 
		     'invisible t))
		  (when do-mouse-track
		    (vm-mouse-set-mouse-track-highlight
		     (vm-su-start-of vm-summary-pointer)
		     (vm-su-end-of vm-summary-pointer)
		     (vm-su-summary-mouse-track-overlay-of
		      vm-summary-pointer)))
		  (when vm-summary-enable-faces 
		    (vm-summary-faces-add vm-summary-pointer)))

		(setq vm-summary-pointer m)
		(goto-char (vm-su-start-of m))
		(let ((modified (buffer-modified-p)))
		  (unwind-protect
		      (progn
			;;
			;; when we move the cursor, the thread-state
			;; indicator should have already changed,
			;; check now to see if we should set the
			;; cursor with indicator
			;;
			;; if, somehow, the cursor became on an
			;; invisible message in a collapsed thread,
			;; assume that there is a good reason for
			;; this and expand the thread (e.g in
			;; visiting a folder with bookmark on
			;; sub-thread
			;;
			(if vm-summary-show-threads
			    (if (vm-collapsed-root-p m)
				(insert "+>")
			      (if (get-text-property 
				   (+ (vm-su-start-of m) 3) 'invisible)
				  (progn (insert vm-summary-=>)
					 (vm-expand-thread 
					  (vm-thread-root m)))
				(insert vm-summary-=>)))
			  (insert vm-summary-=>))
			(delete-char (length vm-summary-=>))

			(when do-mouse-track
			  (vm-mouse-set-mouse-track-highlight
			   (vm-su-start-of m) (vm-su-end-of m)
			   (vm-su-summary-mouse-track-overlay-of m)))
			(when vm-summary-enable-faces 
			  (vm-summary-faces-add m)))
		    (set-buffer-modified-p modified)))
		(forward-char (- (length vm-summary-=>)))
		(when vm-summary-highlight-face
		  (vm-summary-highlight-region
		   (vm-su-start-of m) (vm-su-end-of m)
		   vm-summary-highlight-face))
		(when (and w vm-auto-center-summary)
		  (vm-auto-center-summary))
		(run-hooks 'vm-summary-pointer-update-hook))
	    ;; unwind-protections
	    (when old-window (select-window old-window)))))))

(defun vm-summary-highlight-region (start end face)
  (vm-summary-xxxx-highlight-region start end face 'vm-summary-overlay))

(defun vm-folders-summary-highlight-region (start end face)
  (vm-summary-xxxx-highlight-region start end face
				    'vm-folders-summary-overlay))

(defun vm-summary-xxxx-highlight-region (start end face var)
  (let ((ooo (symbol-value var)))
    (cond (vm-fsfemacs-p
	   (if (and ooo (overlay-buffer ooo))
	       (move-overlay ooo start end)
	     (setq ooo (make-overlay start end))
	     (set var ooo)
	     (overlay-put ooo 'evaporate nil)
	     (overlay-put ooo 'face face)))
	  (vm-xemacs-p
	   (if (and ooo (vm-extent-end-position ooo))
	       (vm-set-extent-endpoints ooo start end)
	     (setq ooo (vm-make-extent start end))
	     (set var ooo)
	     ;; the reason this isn't needed under FSF Emacs is
	     ;; that insert-before-markers also inserts before
	     ;; overlays!  so a summary update of an entry just
	     ;; before this overlay in the summary buffer won't
	     ;; leak into the overlay, but it _will_ leak into an
	     ;; XEmacs extent.
	     (vm-set-extent-property ooo 'start-open t)
	     (vm-set-extent-property ooo 'detachable nil)
	     (vm-set-extent-property ooo 'face face))))))

(defun vm-auto-center-summary ()
  (if vm-auto-center-summary
      (if (or (eq vm-auto-center-summary t) (not (one-window-p t)))
	  (recenter '(4)))))

(defun vm-summary-sprintf (format message &optional tokenize)
  "Generates a summary in FORMAT for MESSAGE and return the
result.  The optional argument TOKENIZE says whether the summary
should be in tokenized form.  If so, the result is a list of
tokens, including strings in mime-decoded form with text-properties.
Otherwise, it is a string in mime-decoded form with text-properties.
						  USR, 2010-05-13" 
  ;; compile the format into an eval'able s-expression
  ;; if it hasn't been compiled already.
  (let* ((alist-var (if tokenize
			'vm-summary-tokenized-compiled-format-alist
		      'vm-summary-untokenized-compiled-format-alist))
	 (match (assoc format (symbol-value alist-var))))
    (unless match
      (vm-summary-compile-format format tokenize)
      (setq match (assoc format (symbol-value alist-var))))
    ;; The local variable name `vm-su-message' is mandatory here for
    ;; the format s-expression to work.
    (let ((vm-su-message message))
      (if (or tokenize (null vm-display-using-mime))
	  (eval (cdr match))
	(vm-decode-mime-encoded-words-in-string (eval (cdr match)))))))

(defun vm-summary-compile-format (format tokenize)
  "Compile FORMAT into an eval'able expression that generates the
summary.  If TOKENIZE is t, the the summary generated will be a
list of tokens.  Otherwise it is a string in mime-decoded form
with text-propertiies.				USR, 2010-05-13."

  (let ((return-value (nth 1 (vm-summary-compile-format-1 format tokenize))))
    (if tokenize
	(setq vm-summary-tokenized-compiled-format-alist
	      (cons (cons format return-value)
		    vm-summary-tokenized-compiled-format-alist))
      (setq vm-summary-untokenized-compiled-format-alist
	    (cons (cons format return-value)
		  vm-summary-untokenized-compiled-format-alist)))))

;; Inserts the summary line for MESSAGE created from TOKENS, which is
;; a list of tokens.  A token is one of 
;; - string, which is inserted literally, 
;; - 'number, meaning message number,
;; - 'mark, meaning the message mark indicator,
;; - 'thread-indent, meaning the indentation space for the message
;; - 'group-begin and 'group-end

(defun vm-tokenized-summary-insert (message tokens)
  "Insert a summary line for MESSAGE in the current buffer, using the
tokenized summary TOKENS."
  (if (stringp tokens)
      (insert tokens)
    (let (token group-list)
      (while tokens
	(setq token (car tokens))
	(cond ((stringp token)
	       (if vm-display-using-mime
		   (let ((vm-mime-qp-decoder-program nil) ; speed up decoding
			 (vm-mime-base64-decoder-program nil))
		     (insert (vm-decode-mime-encoded-words-in-string token)))
		 (insert token)))
	      ((eq token 'group-begin)
	       (setq group-list (cons (list (point) (nth 1 tokens)
					    (nth 2 tokens))
				      group-list)
		     tokens (cdr (cdr tokens))))
	      ((eq token 'group-end)
	       (let* ((space (string-to-char " "))
		      (blob (car group-list))
		      (start (car blob))
		      (field-width (nth 1 blob))
		      (precision (nth 2 blob))
		      (end (vm-marker (point))))
		 (if (integerp field-width)
		     (if (< (- end start) (vm-abs field-width))
			 (if (< field-width 0)
			     (insert-char space (vm-abs (+ field-width
							   (- end start))))
			   (save-excursion
			     (goto-char start)
			     (insert-char space (- field-width
						   (- end start)))))))
		 (if (integerp precision)
		     (if (> (- end start) (vm-abs precision))
			 (if (> precision 0)
			     (delete-char (- precision (- end start)))
			   (save-excursion
			     (goto-char start)
			     (delete-char (vm-abs (+ precision
						     (- end start))))))))
		 (setq group-list (cdr group-list))))
	      ((eq token 'number)
	       (if (and vm-summary-enable-thread-folding
			vm-summary-show-threads
			vm-summary-show-thread-count)
		   (if (= (vm-thread-indentation message) 0)
		       (insert
			(concat (vm-padded-number-of message) 
				(vm-summary-padded-thread-count message)))
		     (insert
		      (vm-summary-message-number-thread-descendant message)))
		 (insert (vm-padded-number-of message))))
	      ((eq token 'mark)
	       (insert (vm-su-mark message)))
	      ((eq token 'thread-indent)
	       (if (and vm-summary-show-threads
			(natnump vm-summary-thread-indent-level))
		   (insert-char 
		    ?\ 
		    (* vm-summary-thread-indent-level
		       (min vm-summary-maximum-thread-indentation
			    (vm-thread-indentation message)))))))
	(setq tokens (cdr tokens))))))

(defun vm-reencode-mime-encoded-words-in-tokenized-summary (summary)
  "Given a tokenized SUMMARY, with tokens including mime-decoded
strings, returns another version where the strings are reencoded in
mime.  It is used for writing summary lines to disk.   USR, 2010-05-13."
  (mapcar
   (function (lambda (token)
	       (if (stringp token)
		   (vm-reencode-mime-encoded-words-in-string token)
		 token)))
   summary))

(defun vm-summary-compile-format-1 (format &optional tokenize start-index)
  (or start-index (setq start-index 0))
  (let ((case-fold-search nil)
	(finished-parsing-format nil)
	(list nil)
	(sexp nil)
	(sexp-fmt nil)
	(saw-close-group nil)
	(last-match-end start-index)
	new-match-end token conv-spec splice)
    (store-match-data nil)
    (while (and (not saw-close-group) (not finished-parsing-format))
      (setq token nil
	    splice nil)
      (while
	  (and (not saw-close-group) (not token)
	       (string-match
		"%\\(-\\)?\\([0-9]+\\)?\\(\\.\\(-?[0-9]+\\)\\)?\\([()pPaAbcSdfFhHiIlLmMnstTwyz*%]\\|U[A-Za-z]\\)"
		format last-match-end))
	(setq conv-spec (aref format (match-beginning 5)))
	(setq new-match-end (match-end 0))
	(if (and (memq conv-spec '(?\( ?\) ?p ?P ?a ?A ?b ?c ?S ?d ?f ?F ?h ?H ?i ?I
				   ?l ?L ?M ?m ?n ?s ?t ?T ?U ?w ?y ?z ?* ))
		 ;; for the non-tokenized path, we don't want
		 ;; the close group spcifier processed here, we
		 ;; want to just bail out and return, which is
		 ;; accomplished by setting a flag in the other
		 ;; branch of this 'if'.
		 (or tokenize (not (= conv-spec ?\)))))
	    (progn
	      (cond ((= conv-spec ?\()
		     (if (not tokenize)
			 (save-match-data
			   (let ((retval (vm-summary-compile-format-1
					  format tokenize (match-end 5))))
			     (setq sexp (cons (nth 1 retval) sexp)
				   new-match-end (car retval))))
		       (setq token `('group-begin
				     ,(if (match-beginning 2)
					  (string-to-number
					   (concat (match-string 1 format)
						   (match-string 2 format))))
				     ,(string-to-number
				       (match-string 4 format)))
			     splice t)))
		    ((= conv-spec ?\))
		     (setq token ''group-end))
		    ((= conv-spec ?p)
		     (setq sexp (cons (list 'vm-su-postponed-indicator
					    'vm-su-message) sexp)))
		    ((= conv-spec ?P)
		     (setq sexp (cons (list 'vm-su-attachment-indicator
					    'vm-su-message) sexp)))
		    ((= conv-spec ?a)
		     (setq sexp (cons (list 'vm-su-attribute-indicators
					    'vm-su-message) sexp)))
		    ((= conv-spec ?A)
		     (setq sexp (cons (list 'vm-su-attribute-indicators-long
					    'vm-su-message) sexp)))
		    ((= conv-spec ?b)
		     (setq sexp (cons (list 'vm-su-attribute-indicators-short
					    'vm-su-message) sexp)))
		    ((= conv-spec ?c)
		     (setq sexp (cons (list 'vm-su-byte-count
					    'vm-su-message) sexp)))
		    ((= conv-spec ?S)
		     (setq sexp (cons (list 'vm-su-size
					    'vm-su-message) sexp)))
		    ((= conv-spec ?d)
		     (setq sexp (cons (list 'vm-su-monthday
					    'vm-su-message) sexp)))
		    ((= conv-spec ?f)
		     (setq sexp (cons (list 'vm-su-interesting-from
					    'vm-su-message) sexp)))
		    ((= conv-spec ?F)
		     (setq sexp (cons (list 'vm-su-interesting-full-name
					    'vm-su-message) sexp)))
		    ((= conv-spec ?h)
		     (setq sexp (cons (list 'vm-su-hour
					    'vm-su-message) sexp)))
		    ((= conv-spec ?H)
		     (setq sexp (cons (list 'vm-su-hour-short
					    'vm-su-message) sexp)))
		    ((= conv-spec ?i)
		     (setq sexp (cons (list 'vm-su-message-id
					    'vm-su-message) sexp)))
		    ((= conv-spec ?I)
		     (if tokenize
			 (setq token ''thread-indent)
		       (setq sexp (cons (list 'vm-su-thread-indent
					      'vm-su-message) sexp))))
		    ((= conv-spec ?l)
		     (setq sexp (cons (list 'vm-su-line-count
					    'vm-su-message) sexp)))
		    ((= conv-spec ?L)
		     (setq sexp (cons (list 'vm-su-labels
					    'vm-su-message) sexp)))
		    ((= conv-spec ?m)
		     (setq sexp (cons (list 'vm-su-month
					    'vm-su-message) sexp)))
		    ((= conv-spec ?M)
		     (setq sexp (cons (list 'vm-su-month-number
					    'vm-su-message) sexp)))
		    ((= conv-spec ?n)
		     (if tokenize
			 (setq token ''number)
		       (setq sexp (cons (list 'vm-padded-number-of
					      'vm-su-message) sexp))))
		    ((= conv-spec ?s)
		     (setq sexp (cons (list 'vm-su-subject
					    'vm-su-message) sexp)))
		    ((= conv-spec ?T)
		     (setq sexp (cons (list 'vm-su-to-names
					    'vm-su-message) sexp)))
		    ((= conv-spec ?t)
		     (setq sexp (cons (list 'vm-su-to
					    'vm-su-message) sexp)))
		    ((= conv-spec ?U)
		     (setq sexp
			   (cons (list 'vm-run-user-summary-function
				       (list 'quote
					     (intern
					      (concat
					       "vm-summary-function-"
					       (substring
						format
						(1+ (match-beginning 5))
						(+ 2 (match-beginning 5))))))
				       'vm-su-message) sexp)))
		    ((= conv-spec ?w)
		     (setq sexp (cons (list 'vm-su-weekday
					    'vm-su-message) sexp)))
		    ((= conv-spec ?y)
		     (setq sexp (cons (list 'vm-su-year
					    'vm-su-message) sexp)))
		    ((= conv-spec ?z)
		     (setq sexp (cons (list 'vm-su-zone
					    'vm-su-message) sexp)))
		    ((= conv-spec ?*)
		     (if tokenize
			 (setq token ''mark)
		       (setq sexp (cons (list 'vm-su-mark
					      'vm-su-message) sexp)))))
	      (cond ((and (not token) vm-display-using-mime)
		     ;; strings might have been already mime-decoded,
		     ;; but there is no harm in doing it again. USR, 2010-05-13
		     (setcar sexp
			     (list 'vm-decode-mime-encoded-words-in-string
				   (car sexp)))))
	      (cond ((and (not token) (match-beginning 1) (match-beginning 2))
		     (setcar sexp
			     (list
			      (if (eq (aref format (match-beginning 2)) ?0)
				  'vm-numeric-left-justify-string
				'vm-left-justify-string)
			      (car sexp)
			      (string-to-number
			       (substring format
					  (match-beginning 2)
					  (match-end 2))))))
		    ((and (not token) (match-beginning 2))
		     (setcar sexp
			     (list
			      (if (eq (aref format (match-beginning 2)) ?0)
				  'vm-numeric-right-justify-string
				'vm-right-justify-string)
			      (car sexp)
			      (string-to-number
			       (substring format
					  (match-beginning 2)
					  (match-end 2)))))))
	      (cond ((and (not token) (match-beginning 3))
		     (setcar sexp
			     (list 'vm-truncate-string (car sexp)
				   (string-to-number
				    (substring format
					       (match-beginning 4)
					       (match-end 4)))))))
	      ;; Why do we reencode decoded strings?  USR, 2010-05-12
;; 	      (cond ((and (not token) vm-display-using-mime)
;; 		     (setcar sexp
;; 			     (list 'vm-reencode-mime-encoded-words-in-string
;; 				   (car sexp)))))
	      (setq sexp-fmt
		    (cons (if token "" "%s")
			  (cons (substring format
					   last-match-end
					   (match-beginning 0))
				sexp-fmt))))
	  (setq sexp-fmt
		(cons (if (eq conv-spec ?\))
			  (prog1 "" (setq saw-close-group t))
			"%%")
		      (cons (substring format
				       (or last-match-end 0)
				       (match-beginning 0))
			    sexp-fmt))))
	  (setq last-match-end new-match-end))
      (if (and (not saw-close-group) (not token))
	  (setq sexp-fmt
		(cons (substring format last-match-end (length format))
		      sexp-fmt)
		finished-parsing-format t))
      (setq sexp-fmt (apply 'concat (nreverse sexp-fmt)))
      (if sexp
	  (setq sexp (cons 'format (cons sexp-fmt (nreverse sexp))))
	(setq sexp sexp-fmt))
      (if tokenize
	  (setq list (nconc list (if (equal sexp "") nil (list sexp))
			    (and token (if splice token (list token))))
		sexp nil
		sexp-fmt nil)))
    (list last-match-end (if list (cons 'list list) sexp))))

;;;###autoload
(defun vm-get-header-contents (message header-name-regexp &optional clump-sep)
  "Return the header field of MESSAGE with the header name matching
HEADER-NAME-REGEXP.  The result will be a string that is
mime-encoded.  The optional argument CLUMP-SEP, if present, should be
a string, which can be used as a separator to concatenate the fields
of multiple header lines which might match HEADER-NAME-REGEXP.
							USR, 2010-05-13."
  (let ((contents nil)
	(regexp (concat "^\\(" header-name-regexp "\\)")))
    (setq message (vm-real-message-of message))
    (save-excursion
      (set-buffer (vm-buffer-of (vm-real-message-of message)))
      (save-restriction
	(widen)
	(goto-char (vm-headers-of message))
	(let ((case-fold-search t))
	  (while (and (or (null contents) clump-sep)
		      (re-search-forward regexp (vm-text-of message) t)
		      (save-excursion (goto-char (match-beginning 0))
				      (vm-match-header)))
	    (if contents
		(setq contents
		      (concat contents clump-sep (vm-matched-header-contents)))
	      (setq contents (vm-matched-header-contents))))))
      contents )))

;; Do not use Emacs 20's string-width here.
;; It does not consider buffer-display-table.
(defun vm-string-width (string)
  (if (not (fboundp 'char-width))
      (length string)
    (let ((i 0)
	  (lim (length string))
	  (total 0))
      (while (< i lim)
	(setq total (+ total (char-width (aref string i)))
	      i (1+ i)))
      total )))

(defun vm-left-justify-string (string width)
  (let ((sw (vm-string-width string)))
    (if (>= sw width)
	string
      (concat string (make-string (- width sw) ?\ )))))

(defun vm-right-justify-string (string width)
  (let ((sw (vm-string-width string)))
    (if (>= sw width)
	string
      (concat (make-string (- width sw) ?\ ) string))))

;; I don't think number glyphs ever have a width > 1
(defun vm-numeric-left-justify-string (string width)
  (let ((sw (length string)))
    (if (>= sw width)
	string
      (concat string (make-string (- width sw) ?0)))))

;; I don't think number glyphs ever have a width > 1
(defun vm-numeric-right-justify-string (string width)
  (let ((sw (length string)))
    (if (>= sw width)
	string
      (concat (make-string (- width sw) ?0) string))))

(defun vm-truncate-string (string width)
  (cond ((fboundp 'char-width)
	 (cond ((> width 0)
		(let ((i 0)
		      (lim (length string))
		      (total 0))
		  (while (and (< i lim) (< total width))
		    (setq total (+ total (char-width (aref string i)))
			  i (1+ i)))
		  (if (< total width)
		      string
		    (substring string 0 i))))
	       (t
		(let ((i (1- (length string)))
		      (lim -1)
		      (total 0))
		  (setq width (- width))
		  (while (and (> i lim) (< total width))
		    (setq total (+ total (char-width (aref string i)))
			  i (1- i)))
		  (if (< total width)
		      string
		    (substring string (1+ i)))))))
	(t (vm-truncate-roman-string string width))))

(defun vm-truncate-roman-string (string width)
  (cond ((<= (length string) (vm-abs width))
	 string)
	((< width 0)
	 (substring string width))
	(t
	 (substring string 0 width))))

(defvar vm-postponed-header)		; defined vm-pine.el

(defun vm-su-postponed-indicator (msg)
  "Given a MESSAGE, ruturns a string indicating whether the
message is a postponed draft that still needs to be sent.  The
indicator string is that defined by the variable
`vm-summary-postponed-indicator'.  		USR, 2010-05-13."
  (if (vm-get-header-contents msg vm-postponed-header)
      vm-summary-postponed-indicator
    ""))

(defun vm-su-attachment-indicator (msg)
  "Given a MESSAGE, ruturns a string indicating whether the
message has attachments.  The indicator string is the value of
`vm-summary-attachment-indicator' followed by the number of
attachments.  					USR, 2010-05-13."
  (let ((attachments 0))
    (setq msg (vm-real-message-of msg))
    ;; If this calls back vm-update-summary-and-mode-line
    ;; an infinite regress happens!
    (vm-mime-action-on-all-attachments
     nil
     (lambda (msg layout type file)
       (setq attachments (1+ attachments)))
     vm-summary-attachment-mime-types
     vm-summary-attachment-mime-type-exceptions
     (list msg)
     t)
    (if (= attachments 0)
        ""
      (if (stringp vm-summary-attachment-indicator)
          vm-summary-attachment-indicator
        (format "%s%d" vm-summary-attachment-indicator attachments)))))

(defun vm-su-attribute-indicators (m)
  "Given a MESSAGE, ruturns a short string showing the attributes of the
message.  The string is 4 characters long. 		USR, 2010-05-13."
  (concat
   (cond ((vm-deleted-flag m) "D")
	 ((vm-new-flag m) "N")
	 ((vm-unread-flag m) "U")
	 ((vm-flagged-flag m) "!")
	 (t " "))
   (cond ((vm-filed-flag m) "F")
	 ((vm-written-flag m) "W")
	 (t " "))
   (cond ((vm-replied-flag m) "R")
	 ((vm-forwarded-flag m) "Z")
	 ((vm-redistributed-flag m) "B")
	 (t " "))
   (cond ((vm-edited-flag m) "E")
	 (t " "))))

(defun vm-su-attribute-indicators-short (m)
  "Given a MESSAGE, ruturns a short string showing the attributes of the
message.  The string is 1 character long. 		USR, 2011-01-08."
  (concat
   (cond ((vm-deleted-flag m) "D")
	 ((vm-new-flag m) "N")
	 ((vm-unread-flag m) "U")
	 ((vm-flagged-flag m) "!")
	 (t " "))))

(defun vm-su-attribute-indicators-long (m)
  "Given a MESSAGE, ruturns a long string showing the attributes of the
message.  The string is 7 characters long. 		USR, 2010-05-13."
  (concat
   (cond ((vm-deleted-flag m) "D")
	 ((vm-new-flag m) "N")
	 ((vm-unread-flag m) "U")
	 ((vm-flagged-flag m) "!")
	 (t " "))
   (if (vm-replied-flag m) "r" " ")
   (if (vm-forwarded-flag m) "z" " ")
   (if (vm-redistributed-flag m) "b" " ")
   (if (vm-filed-flag m) "f" " ")
   (if (vm-written-flag m) "w" " ")
   (if (vm-edited-flag m) "e" " ")))

(defun vm-su-byte-count (m)
  "Given a MESSAGE, ruturns a string showing the length of the
message in bytes.  				USR, 2010-05-13."
  (or (vm-byte-count-of m)
      (vm-set-byte-count-of
       m
       (int-to-string
	(- (vm-text-end-of (vm-real-message-of m))
	   (vm-text-of (vm-real-message-of m)))))))

(defun vm-su-size (msg)
  "Given a MESSAGE, return a string showing the the size of the
message in bytes, kilobytes or megabytes.      USR, 2010-05.13"
  (let ((size (string-to-number (vm-su-byte-count msg))))
    (cond ((< size 1024)
           (format "%d" size))
          ((< size 1048576)
           (setq size (/ size 1024))
           (format "%dK" size))
          (t
           (setq size (/ size 1048576))
           (format "%dM" size)))))

(defun vm-su-spam-score-aux (m)
  "Return the numeric spam level for M."
  (let ((spam-status (vm-get-header-contents m "X-Spam-Status:")))
    (if (and spam-status
	     (string-match "\\(hits\\|score\\)=\\([+-]?[0-9.]+\\)" spam-status))
        (string-to-number (match-string 2 spam-status))
      0)))

(defun vm-su-spam-score (m)
  "Return the numeric spam level for M (possibly using the cached-data)."
  (or (vm-spam-score-of m)
      (vm-set-spam-score-of m (vm-su-spam-score-aux m))))

(defun vm-su-weekday (m)
  "Given a MESSAGE, returns a string showing the week day on which it
was sent.                                                  USR, 2010-05-13"
  (or (vm-weekday-of m)
      (progn (vm-su-do-date m) (vm-weekday-of m))))

(defun vm-su-monthday (m)
  "Given a MESSAGE, returns a string showing the month day on which it
was sent.                                                  USR, 2010-05-13"
  (or (vm-monthday-of m)
      (progn (vm-su-do-date m) (vm-monthday-of m))))

(defun vm-su-month (m)
  (or (vm-month-of m)
      (progn (vm-su-do-date m) (vm-month-of m))))

(defun vm-su-month-number (m)
  (or (vm-month-number-of m)
      (progn (vm-su-do-date m) (vm-month-number-of m))))

(defun vm-su-year (m)
  (or (vm-year-of m)
      (progn (vm-su-do-date m) (vm-year-of m))))

(defun vm-su-hour-short (m)
  (let ((string (vm-su-hour m)))
    (if (> (length string) 5)
	(substring string 0 5)
      string)))

(defun vm-su-hour (m)
  (or (vm-hour-of m)
      (progn (vm-su-do-date m) (vm-hour-of m))))

(defun vm-su-zone (m)
  (or (vm-zone-of m)
      (progn (vm-su-do-date m) (vm-zone-of m))))

(defun vm-su-mark (m) (if (vm-mark-of m) "*" " "))

;; Some yogurt-headed delivery agents don't provide a Date: header.
(defun vm-grok-From_-date (message)
  ;; This works only on the From_ types, obviously
  (if (not (memq (vm-message-type-of message)
		 '(BellFrom_ From_ From_-with-Content-Length)))
      nil
    (save-excursion
      (set-buffer (vm-buffer-of (vm-real-message-of message)))
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (vm-start-of message))
	  (let ((case-fold-search nil))
	    (if (or (looking-at
		     ;; special case this so that the "remote from blah"
		     ;; isn't included.
		     "From [^ \t\n]*[ \t]+\\([^ \t\n].*\\) remote from .*")
		    (looking-at "From [^ \t\n]*[ \t]+\\([^ \t\n].*\\)"))
		(vm-buffer-substring-no-properties
		 (match-beginning 1)
		 (match-end 1)))))))))

(defun vm-su-do-date (m)
  (let ((case-fold-search t)
	vector date)
    (setq date (or (vm-get-header-contents m "Date:") (vm-grok-From_-date m)))
    (cond
     ((null date)
      (vm-set-weekday-of m "")
      (vm-set-monthday-of m "")
      (vm-set-month-of m "")
      (vm-set-month-number-of m "")
      (vm-set-year-of m "")
      (vm-set-hour-of m "")
      (vm-set-zone-of m ""))
     ((string-match
;; The date format recognized here is the one specified in RFC 822.
;; Some slop is allowed e.g. dashes between the monthday, month and year
;; because such malformed headers have been observed.
"\\(\\([a-z][a-z][a-z]\\),\\)?[ \t\n]*\\([0-9][0-9]?\\)[ \t\n---]*\\([a-z][a-z][a-z]\\)[ \t\n---]*\\([0-9]*[0-9][0-9]\\)[ \t\n]*\\([0-9:]+\\)[ \t\n]*\\([a-z][a-z]?[a-z]?\\|\\(-\\|\\+\\)[01][0-9][0-9][0-9]\\)"
       date)
      (if (match-beginning 2)
	  (vm-su-do-weekday m (substring date (match-beginning 2)
					    (match-end 2)))
	(vm-set-weekday-of m ""))
      (vm-set-monthday-of m (substring date (match-beginning 3) (match-end 3)))
      (vm-su-do-month m (substring date (match-beginning 4) (match-end 4)))
      (vm-set-year-of m (substring date (match-beginning 5) (match-end 5)))
      (if (= 2 (length (vm-year-of m)))
	  (save-match-data
	    (cond ((string-match "^[0-6]" (vm-year-of m))
		   (vm-set-year-of m (concat "20" (vm-year-of m))))
		  (t
		   (vm-set-year-of m (concat "19" (vm-year-of m)))))))
      (vm-set-hour-of m (substring date (match-beginning 6) (match-end 6)))
      (vm-set-zone-of m (substring date (match-beginning 7) (match-end 7))))
     ((string-match
;; UNIX ctime(3) format, with slop allowed in the whitespace, and we allow for
;; the possibility of a timezone at the end.
"\\([a-z][a-z][a-z]\\)[ \t\n]*\\([a-z][a-z][a-z]\\)[ \t\n]*\\([0-9][0-9]?\\)[ \t\n]*\\([0-9:]+\\)[ \t\n]*\\([0-9][0-9][0-9][0-9]\\)[ \t\n]*\\([a-z][a-z]?[a-z]?\\|\\(-\\|\\+\\)[01][0-9][0-9][0-9]\\)?"
       date)
      (vm-su-do-weekday m (substring date (match-beginning 1)
				     (match-end 1)))
      (vm-su-do-month m (substring date (match-beginning 2) (match-end 2)))
      (vm-set-monthday-of m (substring date (match-beginning 3) (match-end 3)))
      (vm-set-hour-of m (substring date (match-beginning 4) (match-end 4)))
      (vm-set-year-of m (substring date (match-beginning 5) (match-end 5)))
      (if (match-beginning 6)
	  (vm-set-zone-of m (substring date (match-beginning 6)
				       (match-end 6)))
	(vm-set-zone-of m "")))
     (t
      (setq vector (vm-parse-date date))
      (vm-su-do-weekday m (elt vector 0))
      (vm-set-monthday-of m (elt vector 1))
      (vm-su-do-month m (elt vector 2))
      (vm-set-year-of m (elt vector 3))
      (vm-set-hour-of m (elt vector 4))
      (vm-set-zone-of m (elt vector 5)))))

  ;; Normalize all hour and date specifications to avoid jagged margins.
  ;; If the hour is " 3:..." or "3:...", turn it into "03:...".
  ;; If the date is "03", turn it into " 3".
  (cond ((null (vm-hour-of m)) nil)
	((string-match "\\`[0-9]:" (vm-hour-of m))
	 (vm-set-hour-of m (concat "0" (vm-hour-of m)))))
  (cond ((null (vm-monthday-of m)) nil)
	((string-match "\\`0[0-9]\\'" (vm-monthday-of m))
	 (vm-set-monthday-of m (substring (vm-monthday-of m) 1 2))))
  )

(defun vm-su-do-month (m month-abbrev)
  (let ((val (assoc (downcase month-abbrev) vm-month-alist)))
    (if val
	(progn (vm-set-month-of m (nth 1 val))
	       (vm-set-month-number-of m (nth 2 val)))
      (vm-set-month-of m "")
      (vm-set-month-number-of m ""))))

(defun vm-su-do-weekday (m weekday-abbrev)
  (let ((val (assoc (downcase weekday-abbrev) vm-weekday-alist)))
    (if val
	(vm-set-weekday-of m (nth 1 val))
      (vm-set-weekday-of m ""))))

(defun vm-run-user-summary-function (function message)
  ;; (condition-case nil
  (let ((m (vm-real-message-of message)))
    (save-excursion
      (set-buffer (vm-buffer-of m))
      (save-restriction
	(widen)
	(save-excursion
	  (narrow-to-region (vm-headers-of m) (vm-text-end-of m))
	  (funcall function m)))))
  ;; (error " "))
  )

(defun vm-su-full-name (m)
  "Returns the author name of M as a string, either from
the stored entry (vm-full-name-of) or recalculating it if necessary.
The result is a mime-decoded string with text-properties.
							USR 2010-05-13"
  (or (vm-full-name-of m)
      (progn (vm-su-do-author m) (vm-full-name-of m))))

(defun vm-su-interesting-full-name (m)
  "Returns the author name of M as a string, but if the author is
\"uninteresting\" then returns the value of
`vm-summary-uninteresting-senders-arrow' followed by recipient
names.  The result is a mime-decoded string with text properties.
							  USR 2010-05-13"
  (if vm-summary-uninteresting-senders
      (let ((case-fold-search nil))
	(if (string-match vm-summary-uninteresting-senders (vm-su-from m))
	    (concat vm-summary-uninteresting-senders-arrow (vm-su-to-names m))
	  (vm-su-full-name m)))
    (vm-su-full-name m)))

(defun vm-su-from (m)
  "Returns the author address of M as a string, either from
the stored entry (vm-from-of) or recalculating it if necessary.
The result is a mime-encoded string, but this is not certain.
							USR 2010-05-13"
  (or (vm-from-of m)
      (progn (vm-su-do-author m) (vm-from-of m))))

(defun vm-su-interesting-from (m)
  "Returns the author address of M as a string, but if the author is
\"uninteresting\" then returns the value of
`vm-summary-uninteresting-senders-arrow' followed by recipient
addresses.  The result is a mime-encoded string, but this not certain.
							  USR 2010-05-13"
  (if vm-summary-uninteresting-senders
      (let ((case-fold-search nil))
	(if (string-match vm-summary-uninteresting-senders (vm-su-from m))
	    (concat vm-summary-uninteresting-senders-arrow (vm-su-to m))
	  (vm-su-from m)))
    (vm-su-from m)))

;; Some yogurt-headed delivery agents don't even provide a From: header.
(defun vm-grok-From_-author (message)
  ;; This works only on the From_ types, obviously
  (if (not (memq (vm-message-type-of message)
		 '(From_ BellFrom_ From_-with-Content-Length)))
      nil
    (save-excursion
      (set-buffer (vm-buffer-of message))
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (vm-start-of message))
	  (let ((case-fold-search nil))
	    (if (looking-at "From \\([^ \t\n]+\\)")
		(vm-buffer-substring-no-properties
		 (match-beginning 1)
		 (match-end 1)))))))))

(defun vm-su-do-author (m)
  "Parses the From headers of the message M and stores the results in
the from and full-name entries of the cached-data vector.   USR, 2010-05-13"
  (let ((full-name (vm-get-header-contents m "Full-Name:"))
	(from (or (vm-get-header-contents m "From:" ", ")
		  (vm-grok-From_-author m)))
	pair i)
    (if (and full-name (string-match "^[ \t]*$" full-name))
	(setq full-name nil))
    (if (null from)
	(progn
	  (setq from "???")
	  (if (null full-name)
	      (setq full-name "???")))
      (setq pair (funcall vm-chop-full-name-function from)
	    from (or (nth 1 pair) from)
	    full-name (or full-name (nth 0 pair) from)))
    (if (string-match "\\`\"\\([^\"]+\\)\"\\'" full-name)
 	(setq full-name
 	      (substring full-name (match-beginning 1) (match-end 1))))
    (while (setq i (string-match "\n" full-name i))
      (aset full-name i ?\ ))
    (vm-set-full-name-of m (vm-decode-mime-encoded-words-in-string full-name))
    (vm-set-from-of m (vm-decode-mime-encoded-words-in-string from))))

(defun vm-default-chop-full-name (address)
  (let ((from address)
	(full-name nil))
    (cond ((string-match
"\\`[ \t\n]*\\([^< \t\n]+\\([ \t\n]+[^< \t\n]+\\)*\\)?[ \t\n]*<\\([^>]+\\)>[ \t\n]*\\'"
			 address)
	   (if (match-beginning 1)
	       (setq full-name
		     (substring address (match-beginning 1) (match-end 1))))
	   (setq from
		 (substring address (match-beginning 3) (match-end 3))))
	  ((string-match
"\\`[ \t\n]*\\(\\(\"[^\"]+\"\\|[^\"( \t\n]\\)+\\)[ \t\n]*(\\([^ \t\n]+\\([ \t\n]+[^ \t\n]+\\)*\\)?)[ \t\n]*\\'"
			 address)
	   (if (match-beginning 3)
	       (setq full-name
		     (substring address (match-beginning 3) (match-end 3))))
	   (setq from
		 (substring address (match-beginning 1) (match-end 1)))))
    (list full-name from)))

;; test for existence and functionality of mail-extract-address-components
;; there are versions out there that don't work right, so we run
;; some test data through it to see if we can trust it.
(defun vm-choose-chop-full-name-function (address)
  (let ((test-data '(("kyle@uunet.uu.net" .
		      (nil "kyle@uunet.uu.net"))
		     ("c++std=lib@inet.research.att.com" .
		      (nil "c++std=lib@inet.research.att.com"))
		     ("\"Piet.Rypens\" <rypens@reks.uia.ac.be>" .
		      ("Piet Rypens" "rypens@reks.uia.ac.be"))
		     ("makke@wins.uia.ac.be (Marc.Gemis)" .
		      ("Marc Gemis" "makke@wins.uia.ac.be"))
		     ("" . (nil nil))))
	(failed nil)
	result)
    (while test-data
      (setq result (condition-case nil
		       (mail-extract-address-components (car (car test-data)))
		     (error nil)))
      (if (not (equal result (cdr (car test-data))))
	  ;; failed test, use default
	  (setq failed t
		test-data nil)
	(setq test-data (cdr test-data))))
    (if failed
	;; it failed, use default
	(setq vm-chop-full-name-function 'vm-default-chop-full-name)
      ;; it passed the tests
      (setq vm-chop-full-name-function 'mail-extract-address-components))
    (funcall vm-chop-full-name-function address)))

(defun vm-su-do-recipients (m)
  (let ((mail-use-rfc822 t) i names addresses to cc all list full-name)
    (setq to (or (vm-get-header-contents m "To:" ", ")
		 (vm-get-header-contents m "Apparently-To:" ", ")
		 (vm-get-header-contents m "Newsgroups:" ", ")
		 ;; desperation....
		 (user-login-name))
          cc (or (vm-get-header-contents m "Cc:" ", ")
                 (vm-get-header-contents m "Bcc:" ", "))
	  all to
	  all (if all (concat all ", " cc) cc)
	  addresses (condition-case err
                        (rfc822-addresses all)
                      (error
                       (vm-warn 0 5 err)
                       (list "corrupted-header"))))
    (setq list (vm-parse-addresses all)) ; adds text properties for charsets
    (while list
      ;; Just like vm-su-do-author:
      (setq full-name (or (nth 0 (funcall vm-chop-full-name-function
					  (car list)))
			  (car list)))
      ;; If double quotes are around the full name, fish the name out.
      (if (string-match "\\`\"\\([^\"]+\\)\"\\'" full-name)
	  (setq full-name
		(substring full-name (match-beginning 1) (match-end 1))))
      (while (setq i (string-match "\n" full-name i))
	(aset full-name i ?\ ))
      (setq names (cons full-name names))
      (setq list (cdr list)))
    (setq names (nreverse names))
    ;; added by jwz for fixed vm-parse-addresses
    (vm-set-to-of m (mapconcat 'identity addresses ", "))
    (vm-set-to-names-of m (mapconcat 'identity names ", "))))

(defun vm-su-to (m)
  "Returns the recipient addresses of M as a string, either from
the stored entry (vm-to-of) or recalculating them if necessary.
The result is a mime-decoded string with text properties.  
							USR 2010-05-13"
  (or (vm-to-of m) (progn (vm-su-do-recipients m) (vm-to-of m))))

(defun vm-su-to-names (m)
  "Returns the recipient names of M as a string, either from
the stored entry (vm-to-names-of) or recalculating them if necessary.
The result is a mime-decoded string with text properties.  
							USR 2010-05-13"
  (or (vm-to-names-of m) (progn (vm-su-do-recipients m) (vm-to-names-of m))))
				  
;;;###autoload
(defun vm-su-message-id (m)
  "Returns the subject string of M, either from the stored
entry (vm-subject-of) or recalculating it if necessary.  It is a
mime-encoded string with text properties.  USR 2010-05-13"
  (or (vm-message-id-of m)
      (vm-set-message-id-of
       m
       (or (let ((id (vm-get-header-contents m "Message-Id:")))
	     (and id (car (vm-parse id "[^<]*\\(<[^>]+>\\)"))))
	   ;; try running md5 on the message body to produce an ID
	   ;; better than nothing.
	   (save-excursion
	     (set-buffer (vm-buffer-of (vm-real-message-of m)))
	     (save-restriction
	       (widen)
	       (condition-case nil
		   (concat "<fake-VM-id."
			   (vm-md5-string
			    (buffer-substring
			     (vm-headers-of (vm-real-message-of m))
			     (vm-text-of (vm-real-message-of m))))
			   "@talos.iv>")
		 (error nil))))
	   (concat "<" (int-to-string (vm-abs (random))) "@toto.iv>")))))

(defun vm-su-line-count (m)
  "Returns the line count of M as a string, either from the stored
entry (vm-line-count-of) or recalculating it if necessary.  USR 2010-05-13"
  (or (vm-line-count-of m)
      (vm-set-line-count-of
       m
       (save-excursion
	 (set-buffer (vm-buffer-of (vm-real-message-of m)))
	 (save-restriction
	   (widen)
	   (int-to-string
	    (count-lines (vm-text-of (vm-real-message-of m))
			 (vm-text-end-of (vm-real-message-of m)))))))))

;;;###autoload
(defun vm-su-subject (m)
  "Returns the subject string of M, either from the stored
entry (vm-subject-of) or recalculating it if necessary.  It is a
mime-decoded string with text properties.  USR 2010-05-13"
  (or (vm-subject-of m)
      (vm-set-subject-of
       m
       (let ((subject (vm-decode-mime-encoded-words-in-string
                       (or (vm-get-header-contents m "Subject:") "")))
	     (i nil))
	 (while (string-match "\n[ \t]*" subject)
	   (setq subject (replace-match " " nil t subject)))
	 subject ))))

(defun vm-su-summary (m)
  "Returns the tokenized summary line of M, either from the
stored entry (vm-summary-of) or recalculating it if necessary.
The summary line is a mime-decoded string with text properties.
						  USR 2010-05-13"
  (if (and (vm-virtual-message-p m) (not (vm-virtual-messages-of m)))
      (or (vm-virtual-summary-of m)
	  (save-excursion
	    (vm-select-folder-buffer)
	    (vm-set-virtual-summary-of m (vm-summary-sprintf
					  vm-summary-format m t))
	    (vm-virtual-summary-of m)))
    (or (vm-summary-of m)
	(save-excursion
	  (vm-select-folder-buffer)
	  (vm-set-summary-of m (vm-summary-sprintf vm-summary-format m t))
	  (vm-summary-of m)))))

;;;###autoload
(defun vm-fix-my-summary (&optional kill-local-summary)
  "Rebuild the summary.
Call this function if you made changes to `vm-summary-format'."
  (interactive "P")
  (vm-select-folder-buffer-and-validate 1 (interactive-p))
  (if kill-local-summary
      (kill-local-variable 'vm-summary-format))
  (vm-inform 5 "Fixing your summary... %s" vm-summary-format)
  (let ((mp vm-message-list))
    ;; Erase all the cached summary and threading data
    (while mp
      (vm-set-summary-of (car mp) nil)
      (vm-set-thread-indentation-of (car mp) nil)
      (vm-set-thread-list-of (car mp) nil)
      (vm-set-thread-subtree-of (car mp) nil)
      (vm-mark-for-summary-update (car mp))
      (vm-set-stuff-flag-of (car mp) t)
      (setq mp (cdr mp)))
    ;; Erase threading information
    (setq vm-thread-obarray 'bonk
	  vm-thread-subject-obarray 'bonk)
    ;; Generate fresh summary data and stuff it
;;     (vm-inform 7 "Stuffing cached data...")
;;     (vm-stuff-folder-data nil)
;;     (vm-inform 7 "Stuffing cached data... done")
;;     (set-buffer-modified-p t)
    ;; Regenerate the summary
    (vm-inform 5 "Recreating summary...")
    (vm-update-summary-and-mode-line)
    (unless vm-summary-debug
      (vm-inform 5 "Recreating summary... done")))
  (if vm-thread-debug
      (vm-check-thread-integrity))
  (vm-inform 5 "Fixing your summary... done"))

(defun vm-su-thread-indent (m)
  (if (and vm-summary-show-threads (natnump vm-summary-thread-indent-level))
      (make-string (* (vm-thread-indentation m)
		      vm-summary-thread-indent-level)
		   ?\ )
    "" ))

(defun vm-su-labels (m)
  (or (vm-label-string-of m)
      (vm-set-label-string-of
       m
       (mapconcat 'identity (sort (vm-labels-of m) 'string-lessp) ","))
      (vm-label-string-of m)))

(defun vm-make-folder-summary ()
  (make-vector vm-folder-summary-vector-length nil))

(defun vm-fs-folder-of (fs) (aref fs 0))
(defun vm-fs-total-count-of (fs) (aref fs 1))
(defun vm-fs-new-count-of (fs) (aref fs 2))
(defun vm-fs-unread-count-of (fs) (aref fs 3))
(defun vm-fs-deleted-count-of (fs) (aref fs 4))
(defun vm-fs-start-of (fs) (aref fs 5))
(defun vm-fs-end-of (fs) (aref fs 6))
(defun vm-fs-folder-key-of (fs) (aref fs 7))
(defun vm-fs-mouse-track-overlay-of (fs) (aref fs 8))
(defun vm-fs-short-folder-of (fs) (aref fs 9))
(defun vm-fs-modflag-of (fs) (aref fs 10))

(defun vm-set-fs-folder-of (fs x) (aset fs 0 x))
(defun vm-set-fs-total-count-of (fs x) (aset fs 1 x))
(defun vm-set-fs-new-count-of (fs x) (aset fs 2 x))
(defun vm-set-fs-unread-count-of (fs x) (aset fs 3 x))
(defun vm-set-fs-deleted-count-of (fs x) (aset fs 4 x))
(defun vm-set-fs-start-of (fs x) (aset fs 5 x))
(defun vm-set-fs-end-of (fs x) (aset fs 6 x))
(defun vm-set-fs-folder-key-of (fs x) (aset fs 7 x))
(defun vm-set-fs-mouse-track-overlay-of (fs x) (aset fs 8 x))
(defun vm-set-fs-short-folder-of (fs x) (aset fs 9 x))
(defun vm-set-fs-modflag-of (fs x) (aset fs 10 x))

(defun vm-fs-spooled (fs)
  (let ((count 0)
	(list (symbol-value
	       (intern-soft (vm-fs-folder-key-of fs)
			    vm-folders-summary-folder-hash))))
    (while list
      (setq count (+ count (car (vm-get-folder-totals (car list))))
	    list (cdr list)))
    (int-to-string count)))

(defun vm-make-folders-summary-key (folder &optional dir)
  (cond ((and (stringp vm-recognize-pop-maildrops)
	      (string-match vm-recognize-pop-maildrops folder))
	 (vm-safe-popdrop-string folder))
	((and (stringp vm-recognize-imap-maildrops)
	      (string-match vm-recognize-imap-maildrops folder))
	 (vm-safe-imapdrop-string folder))
	(t
	 (concat "folder-summary0:"
		 (file-truename
		  (expand-file-name folder (or dir vm-folder-directory)))))))

(defun vm-open-folders-summary-database (mode)
  (condition-case data
      (open-database vm-folders-summary-database 'berkeley-db 'hash mode)
    (error (vm-warn 0 2 "open-database signaled: %S" data)
	   nil )))

(defun vm-get-folder-totals (folder)
  (let ((default "(0 0 0 0)") fs db key data)
    (catch 'done
      (if (null vm-folders-summary-database)
	  (throw 'done (read default)))
      (if (not (featurep 'berkeley-db))
	  (throw 'done (read default)))
      (if (null (setq db (vm-open-folders-summary-database "rw+")))
	  (throw 'done (read default)))
      (setq key (vm-make-folders-summary-key folder)
	    data (read (get-database key db default)))
      (close-database db)
      data )))

(defun vm-store-folder-totals (folder totals)
  (let (fs db key data)
    (catch 'done
      (if (null vm-folders-summary-database)
	  (throw 'done nil))
      (if (not (featurep 'berkeley-db))
	  (throw 'done nil))
      (if (null (setq db (vm-open-folders-summary-database "rw+")))
	  (throw 'done nil))
      (setq key (vm-make-folders-summary-key folder)
	    data (prin1-to-string totals))
      (put-database key data db t)
      (close-database db)
      (if (null vm-folders-summary-hash)
	  nil
	(setq fs (intern-soft key vm-folders-summary-hash)
	      fs (symbol-value fs))
	(if (null fs)
	    nil
	  (vm-set-fs-total-count-of fs (int-to-string (car totals)))
	  (vm-set-fs-new-count-of fs (int-to-string (nth 1 totals)))
	  (vm-set-fs-unread-count-of fs (int-to-string (nth 2 totals)))
	  (vm-set-fs-deleted-count-of fs (int-to-string (nth 3 totals)))))
      (vm-mark-for-folders-summary-update folder))))

(defun vm-modify-folder-totals (folder action &rest objects)
  (let (fs db totals key data)
    (catch 'done
      (if (null vm-folders-summary-database)
	  (throw 'done nil))
      (if (not (featurep 'berkeley-db))
	  (throw 'done nil))
      (if (null (setq db (vm-open-folders-summary-database "r")))
	  (throw 'done nil))
      (setq key (vm-make-folders-summary-key folder))
      (setq totals (get-database key db))
      (close-database db)
      (if (null totals)
	  (throw 'done nil))
      (setq totals (read totals))
      (cond ((eq action 'arrived)
	     (let ((arrived (car objects)) c n)
	       (setcar totals (+ (car totals) arrived))
	       (setq c (cdr totals))
	       (setcar c (+ (car c) arrived))))
	    ((eq action 'saved)
	     (let ((arrived (car objects))
		   (m (nth 1 objects)) c n)
	       (setcar totals (+ (car totals) arrived))
	       ;; increment new and unread counts if necessary.
	       ;; messages are never saved with the deleted flag
	       ;; set no need to check that.
	       (setq c (cdr totals))
	       (if (eq (car c) -1)
		   nil
		 (if (vm-new-flag m)
		     (setcar c (+ (car c) arrived))))
	       (setq c (cdr c))
	       (if (eq (car c) -1)
		   nil
		 (if (vm-unread-flag m)
		     (setcar c (+ (car c) arrived)))))))
      (setq data (prin1-to-string totals))
      (if (null (setq db (vm-open-folders-summary-database "rw+")))
	  (throw 'done nil))
      (put-database key data db t)
      (close-database db)
      (if (null vm-folders-summary-hash)
	  nil
	(setq fs (intern-soft key vm-folders-summary-hash)
	      fs (symbol-value fs))
	(if (null fs)
	    nil
	  (vm-set-fs-total-count-of fs (int-to-string (car totals)))
	  (vm-set-fs-new-count-of fs (int-to-string (nth 1 totals)))
	  (vm-set-fs-unread-count-of fs (int-to-string (nth 2 totals)))
	  (vm-set-fs-deleted-count-of fs (int-to-string (nth 3 totals)))))
      (vm-mark-for-folders-summary-update folder))))

(defun vm-folders-summary-sprintf (format layout)
  ;; compile the format into an eval'able s-expression
  ;; if it hasn't been compiled already.
  (let ((match (assoc format vm-folders-summary-compiled-format-alist)))
    (if (null match)
	(progn
	  (vm-folders-summary-compile-format format)
	  (setq match
		(assoc format vm-folders-summary-compiled-format-alist))))
    ;; The local variable name `vm-folder-summary' is mandatory here for
    ;; the format s-expression to work.
    (let ((vm-folder-summary layout))
      (eval (cdr match)))))

(defun vm-folders-summary-compile-format (format)
  (let ((return-value (vm-folders-summary-compile-format-1 format 0)))
    (setq vm-folders-summary-compiled-format-alist
	  (cons (cons format (nth 1 return-value))
		vm-folders-summary-compiled-format-alist))))

(defun vm-folders-summary-compile-format-1 (format start-index)
  (let ((case-fold-search nil)
	(done nil)
	(sexp nil)
	(sexp-fmt nil)
	(last-match-end start-index)
	new-match-end conv-spec)
    (store-match-data nil)
    (while (not done)
      (while
	  (and (not done)
	       (string-match
		"%\\(-\\)?\\([0-9]+\\)?\\(\\.\\(-?[0-9]+\\)\\)?\\([()dfnstu%]\\)"
		format last-match-end))
	(setq conv-spec (aref format (match-beginning 5)))
	(setq new-match-end (match-end 0))
	(if (memq conv-spec '(?\( ?d ?f ?n ?s ?t ?u))
	    (progn
	      (cond ((= conv-spec ?\()
		     (save-match-data
		       (let ((retval
			      (vm-folders-summary-compile-format-1
			       format
			       (match-end 5))))
			 (setq sexp (cons (nth 1 retval) sexp)
			       new-match-end (car retval)))))
		    ((= conv-spec ?d)
		     (setq sexp (cons (list 'vm-fs-deleted-count-of
					    'vm-folder-summary) sexp)))
		    ((= conv-spec ?f)
		     (setq sexp (cons (list 'vm-fs-short-folder-of
					    'vm-folder-summary) sexp)))
		    ((= conv-spec ?n)
		     (setq sexp (cons (list 'vm-fs-new-count-of
					    'vm-folder-summary) sexp)))
		    ((= conv-spec ?t)
		     (setq sexp (cons (list 'vm-fs-total-count-of
					    'vm-folder-summary) sexp)))
		    ((= conv-spec ?s)
		     (setq sexp (cons (list 'vm-fs-spooled
					    'vm-folder-summary) sexp)))
		    ((= conv-spec ?u)
		     (setq sexp (cons (list 'vm-fs-unread-count-of
					    'vm-folder-summary) sexp))))
	      (cond ((and (match-beginning 1) (match-beginning 2))
		     (setcar sexp
			     (list
			      (if (eq (aref format (match-beginning 2)) ?0)
				  'vm-numeric-left-justify-string
				'vm-left-justify-string)
			      (car sexp)
			      (string-to-number
			       (substring format
					  (match-beginning 2)
					  (match-end 2))))))
		    ((match-beginning 2)
		     (setcar sexp
			     (list
			      (if (eq (aref format (match-beginning 2)) ?0)
				  'vm-numeric-right-justify-string
				'vm-right-justify-string)
			      (car sexp)
			      (string-to-number
			       (substring format
					  (match-beginning 2)
					  (match-end 2)))))))
	      (cond ((match-beginning 3)
		     (setcar sexp
			     (list 'vm-truncate-string (car sexp)
				   (string-to-number
				    (substring format
					       (match-beginning 4)
					       (match-end 4)))))))
	      (setq sexp-fmt
		    (cons "%s"
			  (cons (substring format
					   last-match-end
					   (match-beginning 0))
				sexp-fmt))))
	  (setq sexp-fmt
		(cons (if (eq conv-spec ?\))
			  (prog1 "" (setq done t))
			"%%")
		      (cons (substring format
				       (or last-match-end 0)
				       (match-beginning 0))
			    sexp-fmt))))
	(setq last-match-end new-match-end))
      (if (not done)
	  (setq sexp-fmt
		(cons (substring format last-match-end (length format))
		      sexp-fmt)
		done t))
      (setq sexp-fmt (apply 'concat (nreverse sexp-fmt)))
      (if sexp
	  (setq sexp (cons 'format (cons sexp-fmt (nreverse sexp))))
	(setq sexp sexp-fmt)))
    (list last-match-end sexp)))

(defun vm-update-folders-summary-entry (fs)
  (if (and (vm-fs-start-of fs)
	   (marker-buffer (vm-fs-start-of fs)))
      (let ((modified (buffer-modified-p))
	    (do-mouse-track
	     (or (and vm-mouse-track-summary
		      (vm-mouse-support-possible-p))
		 vm-summary-enable-faces))
	    summary)
	(save-excursion
	  (set-buffer (marker-buffer (vm-fs-start-of fs)))
	  (let ((buffer-read-only nil))
	    (unwind-protect
		(save-excursion
		  (goto-char (vm-fs-start-of fs))
		  ;; We do a little dance to update the text in
		  ;; order to make the markers in the text do
		  ;; what we want.
		  ;;
		  ;; 1. We need to avoid having the start
		  ;;    and end markers clumping together at
		  ;;    the start position.
		  ;;
		  ;; 2. We want the window point marker (w->pointm
		  ;;    in the Emacs display code) to move to the
		  ;;    start of the summary entry if it is
		  ;;    anywhere within the su-start-of to
		  ;;    su-end-of region.
		  ;;
		  ;; We achieve (2) by deleting before inserting.
		  ;; Reversing the order of insertion/deletion
		  ;; pushes the point marker into the next
		  ;; summary entry. We achieve (1) by inserting a
		  ;; placeholder character at the end of the
		  ;; summary entry before deleting the region.
		  (goto-char (vm-fs-end-of fs))
		  (insert-before-markers "z")
		  (goto-char (vm-fs-start-of fs))
		  (delete-region (point) (1- (vm-fs-end-of fs)))
		  (insert
		   (vm-folders-summary-sprintf vm-folders-summary-format fs))
		  (delete-char 1)
		  (when do-mouse-track
		    (vm-mouse-set-mouse-track-highlight
		     (vm-fs-start-of fs)
		     (vm-fs-end-of fs)
		     (vm-fs-mouse-track-overlay-of fs)))
		  ;; VM Summary Faces may not work for this yet
		  ;; (when vm-summary-enable-faces
		  ;;   (vm-summary-faces-add fs))
		  )
	      (set-buffer-modified-p modified)))))))

(defun vm-folders-summary-mode-internal ()
  (setq mode-name "VM Folders Summary"
	major-mode 'vm-folders-summary-mode
	mode-line-format '("     %b")
	;; must come after the setting of major-mode
	mode-popup-menu (and vm-use-menus
			     (vm-menu-support-possible-p)
			     (vm-menu-mode-menu))
	buffer-read-only t
	buffer-offer-save nil
	truncate-lines t)
  (when (and vm-xemacs-p (featurep 'scrollbar))
    (set-specifier scrollbar-height (cons (current-buffer) 0)))
  (use-local-map vm-folders-summary-mode-map)
  (when (vm-menu-support-possible-p)
    (vm-menu-install-menus))
  (when (and vm-mutable-frames vm-frame-per-folders-summary)
    (vm-set-hooks-for-frame-deletion))
  (run-hooks 'vm-folders-summary-mode-hook))

(defun vm-do-folders-summary ()
  (catch 'done
    (let ((fs-hash (make-vector 89 0)) db dp fp f key fs totals
          (format vm-folders-summary-format)
	  (do-mouse-track (or (and vm-mouse-track-summary
				   (vm-mouse-support-possible-p))
			      vm-summary-enable-faces)))
      (save-excursion
	(set-buffer vm-folders-summary-buffer)
	(erase-buffer)
	(let ((buffer-read-only nil))
	  (if (null vm-folders-summary-database)
	      (throw 'done nil))
	  (if (not (featurep 'berkeley-db))
	      (throw 'done nil))
	  (if (null (setq db (vm-open-folders-summary-database "r")))
	      (throw 'done nil))
	  (setq dp vm-folders-summary-directories)
	  (while dp
	    (if (cdr vm-folders-summary-directories)
		(insert (car dp) ":\n"))
	    (let ((default-directory (car dp)))
	      (setq fp (sort (vm-delete-backup-file-names
			      (vm-delete-auto-save-file-names
			       (vm-delete-index-file-names
				(vm-delete-directory-names
				 (directory-files (car dp))))))
			     (function string-lessp))))
	    (while fp
	      (setq f (car fp)
		    key (vm-make-folders-summary-key f (car dp))
		    totals (get-database key db))
	      (if (null totals)
		  (let ((ff (expand-file-name f (car dp))))
		    (setq totals (list (or (vm-count-messages-in-file ff) -1)
				       -1 -1 -1))
		    (if (eq (car totals) -1)
			nil
		      (vm-store-folder-totals ff totals)))
		(setq totals (read totals)))
	      (if (eq (car totals) -1)
		  nil
		(setq fs (vm-make-folder-summary))
		(vm-set-fs-folder-of fs (expand-file-name f (car dp)))
		(vm-set-fs-short-folder-of fs f)
		(vm-set-fs-total-count-of fs (vm-nonneg-string (car totals)))
		(vm-set-fs-new-count-of fs (vm-nonneg-string (nth 1 totals)))
		(vm-set-fs-unread-count-of fs (vm-nonneg-string
					       (nth 2 totals)))
		(vm-set-fs-deleted-count-of fs (vm-nonneg-string
						(nth 3 totals)))
		(vm-set-fs-folder-key-of fs key)
		(vm-set-fs-start-of fs (vm-marker (point)))
		(insert (vm-folders-summary-sprintf format fs))
		(vm-set-fs-end-of fs (vm-marker (point)))
		(when do-mouse-track
		  (vm-set-fs-mouse-track-overlay-of
		   fs
		   (vm-mouse-set-mouse-track-highlight
		    (vm-fs-start-of fs)
		    (vm-fs-end-of fs))))
		;; VM Summary Faces may not work here yet
		;; (when vm-summary-enable-faces
		;;   (vm-summary-faces-add fs))
		(set (intern key fs-hash) fs))
	      (setq fp (cdr fp)))
	    (setq dp (cdr dp)))
	  (close-database db)
	  (setq vm-folders-summary-hash fs-hash))
	(goto-char (point-min))))))

(defun vm-update-folders-summary-highlight ()
  (if (or (null vm-mail-buffer)
	  (null (buffer-file-name vm-mail-buffer))
	  (null vm-folders-summary-hash))
      (progn
	(and vm-folders-summary-overlay
	     (vm-set-extent-endpoints vm-folders-summary-overlay 1 1))
	(setq vm-mail-buffer nil))
    (let ((ooo vm-folders-summary-overlay)
	  (fs (symbol-value (intern-soft (vm-make-folders-summary-key
					  (buffer-file-name vm-mail-buffer))
					 vm-folders-summary-hash))))
      (if (and fs
	       (or (null ooo)
		   (null (vm-extent-object ooo))
		   (/= (vm-extent-end-position ooo)
		       (vm-fs-end-of fs))))
	  (vm-folders-summary-highlight-region
	   (vm-fs-start-of fs) (vm-fs-end-of fs)
	   vm-summary-highlight-face)))))

(defun vm-do-needed-folders-summary-update ()
  (if (null vm-folders-summary-buffer)
      nil
    (save-excursion
      (set-buffer vm-folders-summary-buffer)
      (if (or (eq vm-modification-counter vm-flushed-modification-counter)
	      (null vm-folders-summary-hash))
	  nil
	(mapatoms
	 (function
	  (lambda (sym)
	    (let ((fs (symbol-value sym)))
	      (if (null (vm-fs-modflag-of fs))
		  nil
		(vm-update-folders-summary-entry fs)
		(vm-set-fs-modflag-of fs nil)))))
	  vm-folders-summary-hash)
	(vm-update-folders-summary-highlight)
	(setq vm-flushed-modification-counter vm-modification-counter)))))

(defun vm-mark-for-folders-summary-update (folder &optional dont-descend)
  (let ((key (vm-make-folders-summary-key folder))
	(hash vm-folders-summary-hash)
	(spool-hash vm-folders-summary-spool-hash)
	list fs )
    (setq fs (symbol-value (intern-soft key hash)))
    (if (not fs)
	nil
      (vm-set-fs-modflag-of fs t)
      (vm-check-for-killed-summary)
      (if vm-folders-summary-buffer
	  (save-excursion
	    (set-buffer vm-folders-summary-buffer)
	    (vm-increment vm-modification-counter))))
    (if dont-descend
	nil
      (setq list (symbol-value (intern-soft key spool-hash)))
      (while list
	(vm-mark-for-folders-summary-update (car list) t)
	(setq list (cdr list))))))

(defun vm-make-folders-summary-associative-hashes ()
  (let ((triples (vm-compute-spool-files t))
	(spool-hash (make-vector 61 0))
	(folder-hash (make-vector 61 0))
	s-list f-list folder-key spool-key)
    (while triples
      (setq folder-key (vm-make-folders-summary-key (car (car triples)))
	    spool-key (vm-make-folders-summary-key (nth 1 (car triples)))
	    s-list (symbol-value (intern-soft spool-key spool-hash))
	    s-list (cons (car (car triples)) s-list)
	    f-list (symbol-value (intern-soft folder-key folder-hash))
	    f-list (cons (nth 1 (car triples)) f-list)
	    triples (cdr triples))
      (set (intern spool-key spool-hash) s-list)
      (set (intern folder-key folder-hash) f-list))
    (setq vm-folders-summary-spool-hash spool-hash)
    (setq vm-folders-summary-folder-hash folder-hash)))

(defun vm-follow-folders-summary-cursor ()
  (if (or (not (eq major-mode 'vm-folders-summary-mode))
	  (null vm-folders-summary-hash))
      nil
    (catch 'done
      (mapatoms
       (function
	(lambda (sym)
	  (let ((fs (symbol-value sym)))
	    (if (and (>= (point) (vm-fs-start-of fs))
		     (< (point) (vm-fs-end-of fs))
		     (or (null vm-mail-buffer)
			 (not (eq vm-mail-buffer
				  (vm-get-file-buffer (vm-fs-folder-of fs))))))
		(progn
		  (setq vm-mail-buffer
			(save-excursion
			  (vm-visit-folder (vm-fs-folder-of fs))
			  (current-buffer)))
		  (vm-increment vm-modification-counter)
		  (vm-update-summary-and-mode-line)
		  (throw 'done t))))))
       vm-folders-summary-hash)
      nil )))


;;; vm-summary.el ends here
