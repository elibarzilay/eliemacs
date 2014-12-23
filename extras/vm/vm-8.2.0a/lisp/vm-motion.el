;;; vm-motion.el --- Commands to move around in a VM folder
;;
;; This file is part of VM
;;
;; Copyright (C) 1989-1997 Kyle E. Jones
;; Copyright (C) 2003-2006 Robert Widhopf-Fenk
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

(provide 'vm-motion)

(eval-when-compile
  (require 'vm-misc)
  (require 'vm-minibuf)
  (require 'vm-folder)
  (require 'vm-summary)
  (require 'vm-thread)
  (require 'vm-window)
  (require 'vm-page)
  )

(declare-function vm-so-sortable-subject "vm-sort" (message))

(defun vm-record-and-change-message-pointer (old new)
  "Change the `vm-message-pointer' of the folder from OLD to NEW, both
of which must be pointers into the `vm-message-list'."
  (intern (buffer-name) vm-buffers-needing-display-update)
  (vm-garbage-collect-message)
  (setq vm-last-message-pointer old
	vm-message-pointer new
	vm-need-summary-pointer-update t))

;;;###autoload
(defun vm-goto-message (n)
  "Go to the message numbered N.
Interactively N is the prefix argument.  If no prefix arg is provided
N is prompted for in the minibuffer.

If vm-follow-summary-cursor is non-nil this command will go to
the message under the cursor in the summary buffer if the summary
window is selected.  This only happens if no prefix argument is
given."
  (interactive
   (list
    (cond (current-prefix-arg (prefix-numeric-value current-prefix-arg))
	  ((vm-follow-summary-cursor) nil)
	  ((vm-follow-folders-summary-cursor) nil)
	  (t
	   (let ((last-command last-command)
		 (this-command this-command))
	     (vm-read-number "Go to message: "))))))
  (if (null n)
      ()				; nil means work has been done already
    (vm-select-folder-buffer-and-validate 1 (interactive-p))
    (vm-display nil nil '(vm-goto-message) '(vm-goto-message))
    (let ((cons (nthcdr (1- n) vm-message-list)))
      (if (null cons)
	  (error "No such message."))
      (if (eq vm-message-pointer cons)
	  (vm-present-current-message)
	(vm-record-and-change-message-pointer vm-message-pointer cons)
	(vm-present-current-message)
	;;(vm-inform 0 "start of message you want is: %s"
	;; (vm-su-start-of (car vm-message-pointer)))
	(if (and (vm-summary-operation-p)
		 vm-summary-show-threads
		 (get-text-property 
		  (+ (vm-su-start-of (car vm-message-pointer)) 2)
		  'invisible vm-summary-buffer))
	    (vm-expand-thread (vm-thread-root (car vm-message-pointer))))
	))))

;;;###autoload
(defun vm-goto-message-last-seen ()
  "Go to the message last previewed."
  (interactive)
  (vm-select-folder-buffer-and-validate 1 (interactive-p))
  (vm-display nil nil '(vm-goto-message-last-seen)
	      '(vm-goto-message-last-seen))
  (if vm-last-message-pointer
      (progn
	(vm-record-and-change-message-pointer vm-message-pointer
					      vm-last-message-pointer)
	(vm-present-current-message))))
(defalias 'vm-goto-last-message-seen 'vm-goto-message-last-seen)

;;;###autoload
(defun vm-goto-parent-message ()
  "Go to the parent of the current message."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (interactive-p))
  (vm-build-threads-if-unbuilt)
  (vm-display nil nil '(vm-goto-parent-message)
	      '(vm-goto-parent-message))
  (let ((lineage (cdr (reverse (vm-thread-list (car vm-message-pointer)))))
	(message nil))
    (cond ((null lineage)
	   (vm-inform 5 "Message has no parent listed."))
	  ((vm-th-messages-of (car lineage))
	   (setq message (car lineage)))
	  ((y-or-n-p (concat "Parent message is not in this folder. "
			     "Go to the next ancestor? "))
	   (while (and lineage (null (vm-th-messages-of (car lineage))))
	     (setq lineage (cdr lineage)))
	   (if (null lineage)
	       (vm-inform 5 "Message has no ancestors in this folder")
	     (setq message (car lineage)))))
    (when message
      (setq message (car (vm-th-messages-of (car lineage))))
      (vm-record-and-change-message-pointer vm-message-pointer
					    (vm-message-position message))
      (vm-present-current-message))))

(defun vm-check-count (count)
  (if (>= count 0)
      (if (< (length vm-message-pointer) count)
	  (signal 'end-of-folder nil))
    (if (< (1+ (- (length vm-message-list) (length vm-message-pointer)))
	   (vm-abs count))
	(signal 'beginning-of-folder nil))))

(defun vm-move-message-pointer (direction)
  "Move vm-message-pointer along DIRECTION by one position.  DIRECTION
is one of 'forward and 'backward.                     USR, 2011-01-18"
  (let ((mp vm-message-pointer))
    (if (eq direction 'forward)
	(progn
	  (setq mp (cdr mp))
	  (if (null mp)
	      (if vm-circular-folders
		  (setq mp vm-message-list)
		(signal 'end-of-folder nil))))
      (setq mp (vm-reverse-link-of (car mp)))
      (if (null mp)
	  (if vm-circular-folders
	      (setq mp (vm-last vm-message-list))
	    (signal 'beginning-of-folder nil))))
    (setq vm-message-pointer mp)))

(defun vm-should-skip-message (mp &optional skip-dogmatically)
  "Checks various preference settings and message attributes to
determine whether the current message should be skipped during
movement.  The first argument MP is a pointer into the message-list.
The optional argument SKIP-DOGMATICALLY asks it to follow a strong
interpretation of the preferences.                  USR, 2011-01-18"
  (or (and (if skip-dogmatically 
	       vm-skip-deleted-messages
	     (eq vm-skip-deleted-messages t))
	   (vm-deleted-flag (car mp)))
      (vm-should-skip-hidden-message mp)
      (and (if skip-dogmatically 
	       vm-skip-read-messages
	     (eq vm-skip-read-messages t))
	   (or (vm-deleted-flag (car mp))
	       (not (or (vm-new-flag (car mp))
			(vm-unread-flag (car mp))))))
      (and (eq last-command 'vm-next-command-uses-marks)
	   (null (vm-mark-of (car mp))))))

(defun vm-should-skip-hidden-message (mp)
  "Checks if the current message in MP should be skipped as a hidden
message in the summary buffer."
  (and vm-summary-buffer
       (with-current-buffer vm-summary-buffer
	 (and vm-skip-collapsed-sub-threads
	      vm-summary-enable-thread-folding
	      vm-summary-show-threads
	      (> (vm-thread-indentation (car mp)) 0)
	      (vm-collapsed-root-p (vm-thread-root (car mp)))
	      (get-text-property (vm-su-start-of (car mp)) 'invisible)))))

;;;###autoload
(defun vm-next-message (&optional count retry signal-errors)
  "Go forward one message and preview it.
With prefix arg (optional first argument) COUNT, go forward COUNT
messages.  A negative COUNT means go backward.  If the absolute
value of COUNT is greater than 1, then the values of the variables
vm-skip-deleted-messages and vm-skip-read-messages are ignored.

When invoked on marked messages (via `vm-next-command-uses-marks')
this command 'sees' marked messages as it moves."
  ;; second arg RETRY non-nil means retry a failed move, giving
  ;; not nil-or-t values of the vm-skip variables a chance to
  ;; work.
  ;;
  ;; third arg SIGNAL-ERRORS non-nil means that if after
  ;; everything we still have bashed into the end or beginning of
  ;; folder before completing the move, signal
  ;; beginning-of-folder or end-of-folder.  Otherwise no error
  ;; will be signaled.
  ;;
  ;; Note that interactively all args are 1, so error signaling
  ;; and retries apply to all interactive moves.
  (interactive "p\np\np")  
  ;;(vm-inform 8 "running vm next message")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate (if signal-errors 1 0) (interactive-p))
  ;; include other commands that call vm-next-message so that the
  ;; correct window configuration is applied for these particular
  ;; non-interactive calls.
  (vm-display nil nil '(vm-next-message
			vm-delete-message
			vm-undelete-message
			vm-scroll-forward)
	      (list this-command))
  (or count (setq count 1))
  (let ((oldmp vm-message-pointer)
	(use-marks (eq last-command 'vm-next-command-uses-marks))
	(error)
	(direction (if (> count 0) 'forward 'backward))
	(count (vm-abs count)))
    (cond
     ((null vm-message-pointer)
      (setq vm-message-pointer vm-message-list))
     ((/= count 1)
      (condition-case ()
	  (let ((oldmp oldmp))
	    (while (not (zerop count))
	      (vm-move-message-pointer direction)
	      (if (and use-marks (null (vm-mark-of (car vm-message-pointer))))
		  (progn
		    (while (and (not (eq vm-message-pointer oldmp))
				(null (vm-mark-of (car vm-message-pointer))))
		      (vm-move-message-pointer direction))
		    (if (eq vm-message-pointer oldmp)
			;; terminate the loop
			(setq count 1)
		      ;; reset for next pass
		      (setq oldmp vm-message-pointer))))
	      (if (not (vm-should-skip-hidden-message vm-message-pointer))
		  (vm-decrement count))))
	(beginning-of-folder (setq error 'beginning-of-folder))
	(end-of-folder (setq error 'end-of-folder))))
     (t
      (condition-case ()
	  (progn	    
	    (vm-move-message-pointer direction)
	    (while (and (not (eq oldmp vm-message-pointer))
			(vm-should-skip-message vm-message-pointer t))
	      (vm-move-message-pointer direction))
	    ;; Retry the move if we've gone a complete circle and
	    ;; retries are allowed and there are other messages
	    ;; besides this one.
	    (and (eq vm-message-pointer oldmp) retry (cdr vm-message-list)
		 (progn
		   (vm-move-message-pointer direction)
		   (while (and (not (eq oldmp vm-message-pointer))
			       (vm-should-skip-message vm-message-pointer))
		     (vm-move-message-pointer direction)))))
	(beginning-of-folder
	 ;; we bumped into the beginning of the folder without finding
	 ;; a suitable stopping point; retry the move if we're allowed.
	 (setq vm-message-pointer oldmp)
	 ;; if the retry fails, we make sure the message pointer
	 ;; is restored to its old value.
	 (if retry
	     (setq vm-message-pointer
		   (condition-case ()
		       (let ((vm-message-pointer vm-message-pointer))
			 (vm-move-message-pointer direction)
			 (while (vm-should-skip-message vm-message-pointer)
			   (vm-move-message-pointer direction))
			 vm-message-pointer )
		     (beginning-of-folder
		      (setq error 'beginning-of-folder)
		      oldmp )))
	   (setq error 'beginning-of-folder)))
	(end-of-folder
	 ;; we bumped into the end of the folder without finding
	 ;; a suitable stopping point; retry the move if we're allowed.
	 (when (and (vm-summary-operation-p)
		    (get-text-property 
		     (vm-su-start-of (car vm-message-pointer))
		     'invisible vm-summary-buffer))
	   (setq error 'end-of-folder)
	   (setq retry nil))

	 (setq vm-message-pointer oldmp)
	 ;; if the retry fails, we make sure the message pointer
	 ;; is restored to its old value.
	 (if retry
	     (setq vm-message-pointer
		   (condition-case ()
		       (let ((vm-message-pointer vm-message-pointer))
			 (vm-move-message-pointer direction)
			 (while (vm-should-skip-message vm-message-pointer)
			   (vm-move-message-pointer direction))
			 vm-message-pointer )
		     (end-of-folder
		      (setq error 'end-of-folder)
		      oldmp )))
	   (setq error 'end-of-folder))))))
    (unless (eq vm-message-pointer oldmp)
      (vm-record-and-change-message-pointer oldmp vm-message-pointer)
      (vm-present-current-message))
    (when (and error signal-errors)
	 (signal error nil))))

;;;###autoload
(defun vm-previous-message (&optional count retry signal-errors)
  "Go back one message and preview it.
With prefix arg COUNT, go backward COUNT messages.  A negative COUNT
means go forward.  If the absolute value of COUNT > 1 the values of the
variables vm-skip-deleted-messages and vm-skip-read-messages are
ignored."
  (interactive "p\np\np")
  (or count (setq count 1))
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 1 t)
  (vm-display nil nil '(vm-previous-message) '(vm-previous-message))
  (vm-next-message (- count) retry signal-errors))

;;;###autoload
(defun vm-next-message-no-skip (&optional count)
  "Like vm-next-message but will not skip deleted or read messages."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 1 (interactive-p))
  (vm-display nil nil '(vm-next-message-no-skip)
	      '(vm-next-message-no-skip))
  (when (and vm-summary-enable-thread-folding
	     vm-summary-show-threads
	     vm-summary-thread-folding-on-motion)    
    (with-current-buffer vm-summary-buffer
      (let ((msg (vm-summary-message-at-point))
	    (root (vm-thread-root (vm-summary-message-at-point))))
	;; if last message collapse (and do not move)
	(if (= (string-to-number (vm-number-of msg))
	       (+ (string-to-number (vm-number-of root)) 
		  (- (vm-thread-count root) 1)))
	    (vm-collapse-thread t)))))
  (let ((vm-skip-deleted-messages nil)
	(vm-skip-read-messages nil)
	(vm-skip-collapsed-sub-threads 
	 (not vm-summary-thread-folding-on-motion)))
    (vm-next-message count nil t)))

;; backward compatibility
(fset 'vm-Next-message 'vm-next-message-no-skip)

;;;###autoload
(defun vm-previous-message-no-skip (&optional count)
  "Like vm-previous-message but will not skip deleted or read messages."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 1 (interactive-p))
  (vm-display nil nil '(vm-previous-message-no-skip)
	      '(vm-previous-message-no-skip))
  (when (and vm-summary-enable-thread-folding
	     vm-summary-show-threads
	     vm-summary-thread-folding-on-motion)    
    (with-current-buffer vm-summary-buffer
      (let ((msg (vm-summary-message-at-point))
	    (root (vm-thread-root (vm-summary-message-at-point))))
	;; if root message collapse (moving up)
	(if (eq msg root)
	    (vm-collapse-thread t)))))
  (let ((vm-skip-deleted-messages nil)
	(vm-skip-read-messages nil)
	(vm-skip-collapsed-sub-threads 
	 (not vm-summary-thread-folding-on-motion)))
    (vm-previous-message count)))

;; backward compatibility
(fset 'vm-Previous-message 'vm-previous-message-no-skip)

;;;###autoload
(defun vm-next-unread-message ()
  "Move forward to the nearest new or unread message, if there is one."
  (interactive)
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 0 (interactive-p))
  (vm-display nil nil '(vm-next-unread-message) '(vm-next-unread-message))
  (condition-case ()
      (let ((vm-skip-read-messages t)
	    (oldmp vm-message-pointer))
	(vm-next-message 1 nil t)
	;; in case vm-circular-folders is non-nil
	(and (eq vm-message-pointer oldmp) (signal 'end-of-folder nil)))
    (end-of-folder (vm-inform 5 "No next unread message"))))

;;;###autoload
(defun vm-previous-unread-message ()
  "Move backward to the nearest new or unread message, if there is one."
  (interactive)
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 0 (interactive-p))
  (vm-display nil nil '(vm-previous-unread-message)
	      '(vm-previous-unread-message))
  (condition-case ()
      (let ((vm-skip-read-messages t)
	    (oldmp vm-message-pointer))
	(vm-previous-message)
	;; in case vm-circular-folders is non-nil
	(and (eq vm-message-pointer oldmp) (signal 'beginning-of-folder nil)))
    (beginning-of-folder (vm-inform 5 "No previous unread message"))))

;;;###autoload
(defun vm-next-message-same-subject ()
  "Move forward to the nearest message with the same subject.
vm-subject-ignored-prefix and vm-subject-ignored-suffix will apply
to the subject comparisons."
  (interactive)
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 0 (interactive-p))
  (vm-display nil nil '(vm-next-message-same-subject)
	      '(vm-next-message-same-subject))
  (let ((oldmp vm-message-pointer)
	(done nil)
	(subject (vm-so-sortable-subject (car vm-message-pointer))))
    (condition-case ()
	(progn
	  (while (not done)
	    (vm-move-message-pointer 'forward)
	    (if (eq oldmp vm-message-pointer)
		(signal 'end-of-folder nil))
	    (if (equal subject
		       (vm-so-sortable-subject (car vm-message-pointer)))
		(setq done t)))
	  (vm-record-and-change-message-pointer oldmp vm-message-pointer)
	  (vm-present-current-message))
      (end-of-folder
       (setq vm-message-pointer oldmp)
       (vm-inform 5 "No next message with the same subject")))))

;;;###autoload
(defun vm-previous-message-same-subject ()
  "Move backward to the nearest message with the same subject.
vm-subject-ignored-prefix and vm-subject-ignored-suffix will apply
to the subject comparisons."
  (interactive)
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 0 (interactive-p))
  (vm-display nil nil '(vm-previous-message-same-subject)
	      '(vm-previous-message-same-subject))
  (let ((oldmp vm-message-pointer)
	(done nil)
	(subject (vm-so-sortable-subject (car vm-message-pointer))))
    (condition-case ()
	(progn
	  (while (not done)
	    (vm-move-message-pointer 'backward)
	    (if (eq oldmp vm-message-pointer)
		(signal 'beginning-of-folder nil))
	    (if (equal subject
		       (vm-so-sortable-subject (car vm-message-pointer)))
		(setq done t)))
	  (vm-record-and-change-message-pointer oldmp vm-message-pointer)
	  (vm-present-current-message))
      (beginning-of-folder
       (setq vm-message-pointer oldmp)
       (vm-inform 5 "No previous message with the same subject")))))

(defun vm-find-first-unread-message (new)
  (let (mp unread-mp)
    (setq mp vm-message-list)
    (if new
	(while mp
	  (if (and (vm-new-flag (car mp)) (not (vm-deleted-flag (car mp))))
	      (setq unread-mp mp mp nil)
	    (setq mp (cdr mp))))
      (while mp
	(if (and (or (vm-new-flag (car mp)) (vm-unread-flag (car mp)))
		 (not (vm-deleted-flag (car mp))))
	    (setq unread-mp mp mp nil)
	  (setq mp (cdr mp)))))
    unread-mp ))

(defun vm-thoughtfully-select-message ()
  "Select a message in the current folder for the cursor position,
which should be the first new message, if there is any, the first
unread message, if there is any, or the position the cursor was at
the last time the folder was visited.  USR, 2010-03-08"
  (let ((new (and vm-jump-to-new-messages (vm-find-first-unread-message t)))
	(unread (and vm-jump-to-unread-messages
		     (vm-find-first-unread-message nil)))
	fix mp)
    (if (null vm-message-pointer)
	(setq fix (vm-last vm-message-list)))
    (setq mp (or new unread fix))
    (if (and mp (not (eq mp vm-message-pointer)))
	(progn
	  (vm-record-and-change-message-pointer vm-message-pointer mp)
	  mp )
      nil )))

;;;###autoload
(defun vm-follow-summary-cursor ()
  "Select the message under the cursor in the summary window before
executing commands that operate on the current message.  This occurs
only when the summary buffer window is the selected window.  

If a new message is selected then return t, otherwise nil. USR, 2010-03-08" 
  (and vm-follow-summary-cursor (eq major-mode 'vm-summary-mode)
       (let ((point (point))
	     message-pointer message-list mp)
	 (save-excursion
	   (set-buffer vm-mail-buffer)
	   (setq message-pointer vm-message-pointer
		 message-list vm-message-list))
	 (cond ((or (null message-pointer)
		    (and (>= point (vm-su-start-of (car message-pointer)))
			 (< point (vm-su-end-of (car message-pointer)))))
		nil )
	       ;; the position at eob belongs to the last message
	       ((and (eobp) (= (vm-su-end-of (car message-pointer)) point))
		nil )
	       ((eobp)
		(save-excursion
		  (while (get-text-property (- (point) 3) 'invisible)
		    (goto-char 
		     (- (vm-su-start-of (get-text-property 
					 (- (point) 3) 'vm-message))
			3)))
		  (vm-goto-message 
		   (string-to-number 
		    (vm-number-of 
		     (get-text-property (- (point) 3) 'vm-message)))))
		t)
	       ;; make the position at eob belong to the last message
	       ;; ((eobp)
	       ;; 	(while (get-text-property (point) 'invisible)
	       ;; 	  (goto-char (1- (point)))
	       ;; 	  setq mp 
	       ;; 	  ;;(setq mp (vm-last message-pointer))
	       ;; 	(save-excursion
	       ;; 	  (set-buffer vm-mail-buffer)
	       ;; 	  (vm-record-and-change-message-pointer 
	       ;;		vm-message-pointer mp)
	       ;; 	  (vm-present-current-message)
	       ;; 	  ;; return non-nil so the caller will know that
	       ;; 	  ;; a new message was selected.
	       ;; 	  t ))
	       (t
		(if (< point (vm-su-start-of (car message-pointer)))
		    (setq mp message-list)
		  (setq mp (cdr message-pointer) message-pointer nil))
		(while (or (and (not (eq mp message-pointer))
				(>= point (vm-su-end-of (car mp))))
			   (get-text-property 
			    (+ (vm-su-start-of (car mp)) 3) 'invisible))
		  (setq mp (cdr mp)))
		(if (not (eq mp message-pointer))
		    (save-excursion
		      (set-buffer vm-mail-buffer)
		      (vm-record-and-change-message-pointer
		       vm-message-pointer mp)
		      ;; preview disabled to avoid message
		      ;; loading. USR, 2010-09-30
		      ;; (vm-present-current-message)
		      ;; return non-nil so the caller will know that
		      ;; a new message was selected.
		      t )))))))

;;; vm-motion.el ends here
