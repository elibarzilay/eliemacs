;;; vm-thread.el ---  Thread support for VM
;;
;; This file is part of VM
;;
;; Copyright (C) 1994, 2001 Kyle E. Jones
;; Copyright (C) 2003-2006 Robert Widhopf-Fenk
;; Copyright (C) 2010 Uday S. Reddy
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

(provide 'vm-thread)

;; For function declarations

(eval-when-compile
  (require 'vm-misc)
  (require 'vm-folder)
  (require 'vm-summary)
  (require 'vm-sort)
)

;; --------------------------------------------------------------------------
;; Top-level operations
;;
;; vm-toggle-threads-display: interactive () -> none
;; vm-build-threads : (message list) -> none
;; vm-build-thread-lists : () -> none
;; vm-unthread-message : (message &optional bool) -> none
;;
;; vm-thread-mark-for-summary-update : message list -> none
;;
;; vm-parent: (message) -> message
;; vm-references: (message) -> string list
;;
;; vm-th-thread-symbol : (message) -> symbol
;; vm-th-thread-list : (message) -> symbol list
;; vm-th-thread-root : (message or symbol) -> message
;; vm-th-thread-root-sym : (message or symbol) -> symbol
;; vm-th-thread-root-p : (message) -> bool
;; vm-th-thread-indentation : (message) -> integer
;; vm-th-thread-subtree : (message or symbol) -> message list
;; vm-th-thread-count : (message or symbol) -> integer
;;
;; The thread-obarray and thread-subject-obarray properties
;;
;; vm-th-messages-of : symbol -> message list
;; vm-th-message-of : symbol -> message or nil
;; vm-th-children-of : symbol -> symbol list
;; vm-th-child-messages-of : symbol -> message list
;; vm-th-parent-of : symbol -> symbol
;; vm-th-date-of : symbol -> string
;; vm-th-youngest-date-of : symbol -> string
;; vm-th-oldest-date-of : symbol -> string
;; vm-th-thread-date-of : symbol X criterion-symbol -> string
;; vm-th-thread-subtree-of : symbol -> message list
;;	-- this field is now defunct, the thread-subtree field of
;; 	-- messages are being used.
;; vm-th-canonical-message-p : message -> bool
;;
;; vm-ts-subject-symbol : message -> symbol
;; vm-ts-root-of : symbol -> symbol
;; vm-ts-root-date-of : symbol -> date
;; vm-ts-members-of : symbol -> symbol list
;; vm-ts-messages-of : symbol -> message list
;;
;; Higher level operations
;; 
;; vm-th-thread-root : message or symbol -> message
;; vm-th-thread-subtree : message or symbol -> message list
;; vm-th-thread-count : message or symbol -> integer
;;
;; Message-level operations which appear here for some obscure reason
;; 
;; vm-parent : message -> string
;;      (aliased to vm-th-parent)
;; vm-references : message -> string list
;;	(aliased to vm-th-references)
;; vm-th-thread-indentation : message -> integer  
;; vm-th-thread-list : message -> symbol list
;; --------------------------------------------------------------------------

(defvar vm-traced-message-ids nil)

(if (fboundp 'define-error)
    (define-error 'vm-thread-error "VM internal threading error")
  (put 'vm-thread-error 'error-conditions
       '(vm-thread-error error))
  (put 'vm-thread-error 'error-message "VM internal threading error")
  )

;;;###autoload
(defun vm-th-thread-symbol (m)
  "Returns the interned symbol of message M which carries the
threading information.  Threads should have been built before this.
Otherwise nil is returned."
  (with-current-buffer (vm-buffer-of m)
    (and (vectorp vm-thread-obarray)
	 (intern (vm-su-message-id m) vm-thread-obarray))))

;;;###autoload
(defun vm-ts-subject-symbol (m)
  "Returns the interned symbol of message M which carries the
subject-based threading information.  Threads should have been built
before this.  Otherwise nil is returned."
  (with-current-buffer (vm-buffer-of m)
    (and (vectorp vm-thread-subject-obarray)
	 (intern (vm-su-subject m) vm-thread-subject-obarray))))


(defun vm-th-youngest-date-of (id-sym)
  (get id-sym 'youngest-date))

(defun vm-th-set-youngest-date-of (id-sym date)
  (put id-sym 'youngest-date date))

(defun vm-th-oldest-date-of (id-sym)
  (get id-sym 'oldest-date))

(defun vm-th-set-oldest-date-of (id-sym date)
  (put id-sym 'oldest-date date))

(defun vm-th-thread-date-of (id-sym criterion)
  "For the message with the interned symbol ID-SYM, return the
youngest or oldest date in its thread.  CRITERION must be one of
'youngest-date and 'oldest-date"
  (get id-sym criterion))

(defsubst vm-th-message-of (id-sym)
  (and (boundp id-sym) (symbol-value id-sym)))

(defsubst vm-th-set-message-of (id-sym m)
  (set id-sym m))

(defsubst vm-th-messages-of (id-sym)
  (get id-sym 'messages))

;; (defsubst vm-th-message (id-sym)
;;   (and (vm-th-messages-of id-sym)
;;        (vm-last-elem (vm-th-messages-of id-sym))))

(defsubst vm-th-set-messages-of (id-sym ml)
  (put id-sym 'messages ml))

(defsubst vm-th-parent-of (id-sym)
  (get id-sym 'parent))

(defsubst vm-th-set-parent-of (id-sym p-sym)
  ;; For safety, set the symbol-value to nil
  (unless (boundp id-sym)
    (set id-sym nil))
  (put id-sym 'parent p-sym))

(defsubst vm-th-children-of (id-sym)
  (get id-sym 'children))

(defun vm-th-child-messages-of (id-sym)
  (let ((kids (vm-th-children-of id-sym))
	(result nil)
	m)
    (while kids
      (setq m (vm-th-message-of (car kids)))
      (if m
	  (setq result (cons m result)))
      (setq kids (cdr kids)))
    (nreverse result)))

(defsubst vm-th-set-children-of (id-sym ml)
  (put id-sym 'children ml))

(defsubst vm-th-date-of (id-sym)
  (get id-sym 'date))

(defsubst vm-th-set-date-of (id-sym date)
  (put id-sym 'date date))

;; (defsubst vm-th-thread-subtree-of (id-sym)
;;   (get id-sym 'thread-subtree))

;; (defsubst vm-th-set-thread-subtree-of (id-sym ml)
;;   (put id-sym 'thread-subtree ml))

(defsubst vm-ts-root-of (subject-sym)
  (aref (symbol-value subject-sym) 0))

(defsubst vm-ts-root-date-of (subject-sym)
  (aref (symbol-value subject-sym) 1))

(defsubst vm-ts-members-of (subject-sym)
  (aref (symbol-value subject-sym) 2))

(defsubst vm-ts-messages-of (subject-sym)
  (aref (symbol-value subject-sym) 3))

(defsubst vm-ts-set-root-of (subject-sym id-sym)
  (aset (symbol-value subject-sym) 0 id-sym))

(defsubst vm-ts-set-root-date-of (subject-sym date)
  (aset (symbol-value subject-sym) 1 date))

(defsubst vm-ts-set-members-of (subject-sym ml)
  (aset (symbol-value subject-sym) 2 ml))

(defsubst vm-ts-set-messages-of (subject-sym ml)
  (aset (symbol-value subject-sym) 3 ml))



;;;###autoload
(defun vm-toggle-threads-display ()
  "Toggle the threads display on and off.
When the threads display is on, the folder will be sorted by
thread activity and thread indentation (via the %I summary format
specifier) will be visible."
  (interactive)
  (vm-select-folder-buffer-and-validate 0 (interactive-p))
  ;; get numbering of new messages done now
  ;; so that the sort code only has to worry about the
  ;; changes it needs to make.
  (vm-update-summary-and-mode-line)
  (vm-set-summary-redo-start-point t)
  (setq vm-summary-show-threads (not vm-summary-show-threads))
  ;; Toggle between "physical-order" and "activity" sort-keys.
  ;; This would have been better if vm-ml-sort-keys was a list of
  ;; sort-keys, but it is a string and this is a quick fix.
  (cond ((equal vm-ml-sort-keys "physical-order")
	 (setq vm-ml-sort-keys "activity"))
	((equal vm-ml-sort-keys "activity")
	 (setq vm-ml-sort-keys "physical-order"))
	((equal vm-ml-sort-keys "reversed-physical-order")
	 (setq vm-ml-sort-keys "reversed-activity"))
	((equal vm-ml-sort-keys "reversed-activity")
	 (setq vm-ml-sort-keys "reversed-physical-order")))
  (if vm-summary-show-threads
      (vm-sort-messages (or vm-ml-sort-keys "activity"))
    (vm-sort-messages (or vm-ml-sort-keys "physical-order"))))

(defun vm-build-reference-threads (mp schedule-reindents)
  (let ((n 0)
	(modulus 10)
	m parent parent-sym id id-sym date refs old-parent-sym)
    (while mp
      (setq m (car mp)
	    parent (vm-parent m)
	    id (vm-su-message-id m)
	    id-sym (intern id vm-thread-obarray)
	    date (vm-so-sortable-datestring m))
      (vm-th-set-message-of id-sym m)
      (vm-th-set-messages-of id-sym (cons m (vm-th-messages-of id-sym)))
      (vm-th-set-date-of id-sym date)
      (if (and schedule-reindents (null (cdr (vm-th-messages-of id-sym))))
	  (vm-thread-mark-for-summary-update 
	   (cons m (vm-th-child-messages-of id-sym))))
      (vm-th-set-parent-of id-sym nil)
      (when parent
	(setq parent-sym (intern parent vm-thread-obarray))
	(when (vm-th-message-of parent-sym)
	  (vm-set-thread-subtree-of
	   (vm-th-message-of parent-sym) nil))	; force it to be rebuilt
	(cond ((or (null (vm-th-parent-of id-sym))
		   (eq (vm-th-parent-of id-sym) parent-sym))
	       (vm-th-set-parent-of id-sym parent-sym))
	      (t
	       (let ((kids (vm-th-children-of old-parent-sym ))
		     (msgs (vm-th-messages-of id-sym))
		     (msg-sym nil))
		 (setq old-parent-sym (vm-th-parent-of id-sym))
		 (while msgs
		   (setq msg-sym (vm-th-thread-symbol (car msgs)))
		   (setq kids (delq msg-sym kids)
			 msgs (cdr msgs)))
		 (vm-th-set-children-of old-parent-sym kids)
		 (vm-th-set-parent-of id-sym parent-sym)
		 (if schedule-reindents
		     (vm-thread-mark-for-summary-update
		      (vm-th-messages-of id-sym))))))
	(vm-th-set-children-of 
	 parent-sym (cons id-sym (vm-th-children-of parent-sym))))
	;; FIXME is the parent property automatically set to nil?
        ;; (if (not (boundp id-sym))
	;;   (vm-th-set-parent-of id-sym nil))
      ;; use the references header to set parenting information
      ;; for ancestors of this message.  This does not override
      ;; a parent pointer for a message if it already exists.
      (if (cdr (setq refs (vm-references m)))
	  (let (parent-sym id-sym msgs msg-syms)
	    (setq parent-sym (intern (car refs) vm-thread-obarray)
		  refs (cdr refs))
	    (when (vm-th-message-of parent-sym)	
	      (vm-set-thread-subtree-of	; force it to be rebuilt
	       (vm-th-message-of parent-sym) nil))
	    (while refs
	      (setq id-sym (intern (car refs) vm-thread-obarray))
	      (if (vm-th-parent-of id-sym)
		  nil
		(vm-th-set-parent-of id-sym parent-sym)
		(setq msgs (vm-th-messages-of id-sym))
		(setq msg-syms 
		      (mapcar 'vm-th-thread-symbol msgs))
;; 		(if msgs
;; 		    (vm-th-set-children-of 
;; 		     parent-sym 
;; 		     (append msg-syms (vm-th-children-of parent-sym))
;; 		     ))
		(vm-th-set-children-of 
		 parent-sym 
		 (cons id-sym (vm-th-children-of parent-sym)))
		(if schedule-reindents
		    (vm-thread-mark-for-summary-update msgs)))
	      (when (vm-th-message-of id-sym)
		(vm-set-thread-subtree-of
		 (vm-th-message-of id-sym) nil))	; force it to be rebuilt
	      (setq parent-sym id-sym
		    refs (cdr refs)))))
      (setq mp (cdr mp) n (1+ n))
      (if (zerop (% n modulus))
	  (message "Building threads (by reference)... %d" n)))))

(defun vm-build-subject-threads (mp schedule-reindents)
  (let ((n 0)
	(modulus 10)
	m parent parent-sym id id-sym date refs old-parent-sym
	subject subject-sym)
    (while mp
      (setq m (car mp)
	    parent (vm-parent m)
	    id (vm-su-message-id m)
	    id-sym (intern id vm-thread-obarray)
	    date (vm-so-sortable-datestring m))
      (setq subject (vm-so-sortable-subject m)
	    subject-sym (intern subject vm-thread-subject-obarray))
      ;; inhibit-quit because we need to make sure the asets
      ;; below are an atomic group.
      (let* ((inhibit-quit t))
	;; if this subject was never seen before create the
	;; information vector.
	(if (not (boundp subject-sym))
	    (set subject-sym
		 (vector id-sym date nil (list m)))
	  ;; this subject seen before 
	  (vm-ts-set-messages-of subject-sym
				 (cons m (vm-ts-messages-of subject-sym)))
	  (if (string< date (vm-ts-root-date-of subject-sym))
	      (let* ((vect (symbol-value subject-sym))
		     (i-sym (vm-ts-root-of subject-sym)))
		;; optimization: if we know that this message
		;; already has a parent, then don't bother
		;; adding it to the list of child messages
		;; since we know that it will be threaded and
		;; unthreaded using the parent information.
		(unless (and (vm-th-parent-of i-sym)
			     (vm-th-messages-of (vm-th-parent-of i-sym)))
		  (vm-ts-set-members-of 
		   subject-sym (cons i-sym (vm-ts-members-of subject-sym))))
		(vm-ts-set-root-of subject-sym id-sym)
		(vm-ts-set-root-date-of subject-sym date)
		;; this loops _and_ recurses and I'm worried
		;; about it going into a spin someday.  So I
		;; unblock interrupts here.  It's not critical
		;; that it finish... the summary will just be out
		;; of sync.
		(when schedule-reindents
		  (let ((inhibit-quit nil))
		    (vm-thread-mark-for-summary-update 
		     (vm-ts-messages-of subject-sym)))))
	    ;; optimization: if we know that this message
	    ;; already has a parent, then don't bother adding
	    ;; it to the list of child messages, since we
	    ;; know that it will be threaded and unthreaded
	    ;; using the parent information.
	    (unless (and parent 
			 (vm-th-messages-of 
			  (intern parent vm-thread-obarray)))
	      (vm-ts-set-members-of 
	       subject-sym (cons id-sym (vm-ts-members-of subject-sym)))))))
      (setq mp (cdr mp) n (1+ n))
      (when (zerop (% n modulus))
	(message "Building threads (by subject)... %d" n)))))

;;;###autoload
(defun vm-build-threads (message-list)
  "For all messages in MESSAGE-LIST, build thread information in the
`vm-thread-obarray' and `vm-thread-subject-obarray'.  If MESSAGE-LIST
is nil, do it for all the messages in the folder.  USR, 2010-07-15"
  (if (not (vectorp vm-thread-obarray))
      (setq vm-thread-obarray (make-vector 641 0)
	    vm-thread-subject-obarray (make-vector 641 0)))
  (let ((mp (or message-list vm-message-list))
	(n 0)
	;; Just for laughs, make the update interval vary.
	(modulus (+ (% (vm-abs (random)) 11) 40))
	;; no need to schedule reindents of reparented messages
	;; unless there were already messages present.
	(schedule-reindents message-list)
	m parent parent-sym id id-sym date refs old-parent-sym)
    ;; Build threads using references
    (vm-build-reference-threads mp schedule-reindents)
    ;; Build threads using subject
    (when vm-thread-using-subject
      (vm-build-subject-threads mp schedule-reindents))
    ;; Calculate thread-subtrees for all the known message ID's
    (mapatoms
     (lambda (id-sym)
       (when (vm-th-message-of id-sym)
	 (vm-th-thread-subtree id-sym)))
     vm-thread-obarray)
    (when (> n modulus)
      (message "Building threads... done"))))

;; used by the thread sort code.
;;
;; vm-th-thread-list initializes the oldest-date property on
;; the message-id symbols.  Since this property is used as an
;; ordering key by the thread sort the oldest-date properties
;; must be computed before the sort begins, not during it.
;; Otherwise the sort won't be stable and there will be chaos.

;;;###autoload
(defun vm-build-thread-lists ()
  (let ((mp vm-message-list))
    (while mp
      (vm-th-thread-list (car mp))
      (setq mp (cdr mp))))
  (if vm-debug
      (vm-th-check-thread-integrity vm-message-list))
  )

(defun vm-thread-mark-for-summary-update (message-list)
  (let (m)
    (while message-list
      (setq m (car message-list))
      ;; if thread-list is null then we've already marked this
      ;; message, or it doesn't need marking.
      (if (null (vm-thread-list-of m))
	  nil
	(mapc (lambda (a)
		(when (vm-th-message-of a)
		  (vm-set-thread-subtree-of (vm-th-message-of a) nil)))
	      (vm-thread-list-of m))
	(vm-mark-for-summary-update m t)
	(vm-set-thread-list-of m nil)
	(vm-set-thread-indentation-of m nil)
	(vm-thread-mark-for-summary-update
	 (vm-th-child-messages-of (vm-th-thread-symbol m))))
      (setq message-list (cdr message-list)))))

(defun vm-thread-list (message)
  "Returns the thread-list, i.e., the lineage of MESSAGE, as a list of
symbols interned in vm-thread-obarray."
  (let ((done nil)
	(m message)
	(loop-recovery-point nil)
	(date (vm-so-sortable-datestring message))
	thread-list id-sym subject-sym loop-sym root-date youngest-date
	root ancestors)
    (save-excursion
      (set-buffer (vm-buffer-of m))
      (fillarray vm-thread-loop-obarray 0)
      (setq id-sym (intern (vm-su-message-id m) vm-thread-obarray)
	    thread-list (list id-sym))
      (if (and vm-debug (member (symbol-name id-sym) vm-traced-message-ids))
	  (debug id-sym))
      ;; if m is a non-canonical message for its message ID, give it
      ;; an artificial thread-list
      ;; But, does this make sense?
      ;; (unless (eq m (vm-th-message-of id-sym))
      ;; 	(setq thread-list (list id-sym id-sym))
      ;; 	(setq done t))
      (set (intern (symbol-name id-sym) vm-thread-loop-obarray) t)
      (while (not done)
	;; save the date of the oldest message in this thread
	(setq root-date (vm-th-oldest-date-of id-sym))
	(if (or (null root-date)
		(string< date root-date))
	    (vm-th-set-oldest-date-of id-sym date))
	;; save the date of the youngest message in this thread
	(setq youngest-date (vm-th-youngest-date-of id-sym))
	(if (or (null root-date)
		(string< youngest-date date))
	    (vm-th-set-youngest-date-of id-sym date))
	(cond ((vm-th-parent-of id-sym)
	       (setq id-sym (vm-th-parent-of id-sym)
		     loop-sym (intern (symbol-name id-sym)
				      vm-thread-loop-obarray))
	       (if (boundp loop-sym)
		   ;; loop detected, bail...
		   (setq done t
			 thread-list (or loop-recovery-point thread-list))
		 (set loop-sym t)
		 (setq thread-list (cons id-sym thread-list))
		 (when (vm-th-messages-of id-sym)
		     (setq m (vm-th-message-of id-sym)))))
	      ((null m)
	       (setq done t))
	      ((null vm-thread-using-subject)
	       (setq done t))
	      ((and (setq subject-sym
			  (intern (vm-so-sortable-subject m)
				  vm-thread-subject-obarray))
		    (or (not (boundp subject-sym))
			(and (eq (vm-ts-root-of subject-sym) id-sym)
			     (eq m (vm-th-message-of id-sym)))))
	       (setq done t))
	      (t
	       (setq id-sym (vm-ts-root-of subject-sym))
	       ;; seems to cause more trouble than it fixes
	       ;; revisit this later.
	       ;; (setq loop-recovery-point (or loop-recovery-point
	       ;;	 		        thread-list))
	       (setq loop-sym (intern (symbol-name id-sym)
				      vm-thread-loop-obarray))
	       (if (boundp loop-sym)
		   ;; loop detected, bail...
		   (setq done t 
			 thread-list (or loop-recovery-point thread-list))
		 (setq root (vm-th-message-of id-sym))
		 ;; the ancestors of id-sym will be added.
		 ;; remove them if they were already added.
		 (setq ancestors (remq id-sym (vm-th-thread-list root)))
		 (mapc (lambda (a)
			 (setq thread-list (remq a thread-list))
			 (makunbound (intern (symbol-name a)
					     vm-thread-loop-obarray)))
		       ancestors)
		 (set loop-sym t)
		 (setq thread-list (cons id-sym thread-list)
		       m (vm-th-message-of id-sym))))))
      thread-list )))

;; remove message struct from thread data.
;;
;; optional second arg non-nil means forget information that
;; might be different if the message contents changed.
;;
;; message must be a real (non-virtual) message

;;;###autoload
(defun vm-unthread-message (message &optional message-changing)
  "Removes MESSAGE and all its mirrored messages from their
current threads.  If optional argument MESSAGE-CHANGING is
non-nil, then forget information that might be different if the
message contents changed.  (What does this mean?)

MESSAGE should be a real (non-virtual) message.

The full functionality of this function is not entirely clear.  
						USR, 2010-07-24"
  (save-excursion
    (let ((mp (cons message (vm-virtual-messages-of message)))
	  m date id-sym s-sym p-sym)
      (while mp
	(setq m (car mp))
	(set-buffer (vm-buffer-of m))
	(if (not (vectorp vm-thread-obarray))
	    nil
	  (let ((inhibit-quit t))
	    ;; handles for the thread and thread-subject databases
	    (setq id-sym (intern (vm-su-message-id m) vm-thread-obarray))
	    (setq s-sym (intern (vm-so-sortable-subject m)
				vm-thread-subject-obarray))
	    (if (and vm-debug 
		     (member (symbol-name id-sym) vm-traced-message-ids))
		(debug id-sym))
	    ;; discard cached thread properties
	    (mapc (lambda (a)
		    (when (vm-th-message-of a)
		      (vm-set-thread-subtree-of (vm-th-message-of a) nil)))
		  (vm-thread-list-of m))
	    (vm-set-thread-list-of m nil)
	    (vm-set-thread-indentation-of m nil)
	    ;; remove the message from its erstwhile thread
	    (when (boundp id-sym)
	      ;; remove m from its thread node
	      (vm-th-set-messages-of 
	       id-sym (remq m (vm-th-messages-of id-sym)))
	      (vm-thread-mark-for-summary-update 
	       (vm-th-child-messages-of id-sym))
	      ;; reset the thread dates of m
	      (setq date (vm-so-sortable-datestring message))
	      (vm-th-set-youngest-date-of id-sym date)
	      (vm-th-set-oldest-date-of id-sym date)
	      ;; if message changed, remove it from the thread tree
	      ;; not clear what is going on.  USR, 2010-07-24
	      (when message-changing
		(setq p-sym (vm-th-parent-of id-sym))
		(when p-sym 
		  (vm-th-set-children-of 
		   p-sym (remq id-sym (vm-th-children-of p-sym))))
		(vm-th-set-parent-of id-sym nil)))

	    ;; remove the message from its erstwhile subject thread
	    (when (boundp s-sym)
	      (if (eq id-sym (vm-ts-root-of s-sym))
		  ;; (when message-changing
		  (if (null (cdr (vm-ts-messages-of s-sym)))
		      (makunbound s-sym)
		    (let ((p (vm-ts-messages-of s-sym))
			  oldest-msg oldest-date children)
		      (setq oldest-msg (car p))
		      (setq oldest-date (vm-so-sortable-datestring (car p)))
		      (setq p (cdr p))
		      (while p
			(when (and (string-lessp 
				  (vm-so-sortable-datestring (car p))
				  oldest-date)
				 (not (eq m (car p))))
			  (setq oldest-msg (car p)
				oldest-date 
				(vm-so-sortable-datestring (car p))))
			(setq p (cdr p)))
		      (vm-ts-set-root-of 
		       s-sym (intern (vm-su-message-id oldest-msg)
				     vm-thread-obarray))
		      (vm-ts-set-root-date-of s-sym oldest-date)
		      (setq children (remq oldest-msg (vm-ts-members-of s-sym)))
		      (vm-ts-set-members-of s-sym children)
		      (vm-ts-set-messages-of
		       s-sym (remq m (vm-ts-messages-of s-sym)))
		      ;; I'm not sure there aren't situations
		      ;; where this might loop forever.
		      (let ((inhibit-quit nil))
			(mapc (lambda (c-sym)
				(vm-thread-mark-for-summary-update 
				 (vm-th-messages-of c-sym)))
			      children))))
		      ;; )
		(vm-ts-set-members-of 
		 s-sym (remq id-sym (vm-ts-members-of s-sym)))
		(vm-ts-set-messages-of 
		 s-sym (remq m (vm-ts-messages-of s-sym)))
		))))
	(setq mp (cdr mp))))))

;;;###autoload
(defun vm-references (m)
  "Returns the cached references list of message M.  If the cache is
nil, retrieves the references list from the headers and caches it.
USR, 2010-03-13"
  (or (vm-references-of m)
      (vm-set-references-of
       m
       (let (references)
	 (setq references (vm-get-header-contents m "References:" " "))
	 (and references (vm-parse references "[^<]*\\(<[^>]+>\\)"))))))
(fset 'vm-th-references 'vm-references)

;;;###autoload
(defun vm-parent (m)
  "Returns the cached parent message of message M (in its thread).  If
the cache is nil, calculates the parent and caches it.  USR, 2010-03-13"
  (or (vm-parent-of m)
      (vm-set-parent-of
       m
       (or (vm-last-elem (vm-references m))
	   (let (in-reply-to ids id)
	     (setq in-reply-to (vm-get-header-contents m "In-Reply-To:" " ")
		   ids (and in-reply-to (vm-parse in-reply-to
						  "[^<]*\\(<[^>]+>\\)")))
	     (while ids
	       (when (< (length id) (length (car ids)))
		   (setq id (car ids)))
	       (setq ids (cdr ids)))
	     (and id (vm-set-references-of m (list id)))
	     id )))))
(fset 'vm-th-parent 'vm-parent)

;;;###autoload
(defun vm-th-thread-indentation (m)
  "Returns the cached thread-indentation of message M.  If the cache is
nil, calculates the thread-indentation and caches it.  USR, 2010-03-13"
  (or (vm-thread-indentation-of m)
      (let ((p (vm-th-thread-list m))
	    (n 0))
	(catch 'done
	  (while p 
	    (cond ((null (vm-th-messages-of (car p)))
		   (setq p (cdr p)))
		  (vm-summary-thread-indentation-by-references
		   (setq n (length p))
		   (throw 'done nil))
		  (t
		   (setq n (1+ n)
			 p (cdr p))))))
	(if (and (eq (car p) (vm-th-thread-symbol m))
		 (not (eq (vm-th-message-of (car p)) m)))
	    ;; thread root is a duplicate of m
	    (vm-set-thread-indentation-of m n)
	  (vm-set-thread-indentation-of m (1- n)))
	(vm-thread-indentation-of m))))

;;;###autoload
(defun vm-th-thread-list (m)
  "Returns the cached thread-list of message M.  If the cache is nil,
calculates the thread-list and caches it.  USR, 2010-03-13"
  (or (vm-thread-list-of m)
      (progn
	(vm-set-thread-list-of m (vm-thread-list m))
	;; reset the thread-subtrees, forcing them to be rebuilt
	(mapc (lambda (a)
		(when (vm-th-message-of a)
		  (vm-set-thread-subtree-of (vm-th-message-of a) nil)))
	      (vm-thread-list-of m))
	(vm-thread-list-of m))))

;;;###autoload
(defun vm-th-thread-root (m)
  "Returns the root message of M.  M can be either a message or
the interned symbol of a message.  If there are multiple messages with
the same root message ID, one of them is chosen arbitrarily.  Threads
should have been built for this function to work."
  (let (m-sym list id-sym)
    (cond ((symbolp m) 
	   (setq m-sym m)
	   (setq m (vm-th-message-of m-sym)))
	  (t
	   (setq m-sym (vm-th-thread-symbol m))))
    (if (and vm-debug (member (symbol-name m-sym) vm-traced-message-ids))
	(debug m-sym))
    (unless m-sym
      (signal 'vm-thread-error (list 'vm-th-thread-root)))
    (setq list (vm-th-thread-list m))
    (catch 'return
      (while list
	(setq id-sym (car list))
	(when (vm-th-messages-of id-sym)
	    (throw 'return (vm-th-message-of id-sym)))
	(setq list (cdr list)))
      nil)))

;;;###autoload
(defun vm-th-thread-root-sym (m)
  "Returns interned symbol of the root message of M.  M can be
either a message or the interned symbol of M.  Threads should
have been built for this function to work.  

See also: `vm-th-thread-root'."
  (let (m-sym list id-sym)
    (cond ((symbolp m) 
	   (setq m-sym m)
	   (setq m (vm-th-message-of m-sym)))
	  (t
	   (setq m-sym (vm-th-thread-symbol m))))
    (if (and vm-debug (member (symbol-name m-sym) vm-traced-message-ids))
	(debug m-sym))
    (unless m-sym
      (signal 'vm-thread-error (list 'vm-th-thread-root)))
    (setq list (vm-th-thread-list m))
    (catch 'return
      (while list
	(setq id-sym (car list))
	(when (vm-th-messages-of id-sym)
	    (throw 'return id-sym))
	(setq list (cdr list)))
      nil)))

;;;###autoload
(defun vm-th-thread-root-p (m)
  "Returns t if message M is known to be a thread root, nil
otherwise."
  (condition-case err
      (eq m (vm-th-thread-root m))
    (vm-thread-error
     nil)))

;;;###autoload
(defun vm-th-thread-subtree (msg)
  "Returns the list of messages in the thread subtree of MSG.
MSG can be a message or the interned symbol of a message.
Threads should have been built for this function to work."
  (let (m-sym)
    (if (symbolp msg)
	(setq m-sym msg
	      msg (vm-th-message-of msg))
      (setq m-sym (vm-th-thread-symbol msg)))
    (unless m-sym
      (signal 'vm-thread-error (list 'vm-th-thread-subtree)))
    (if (eq msg (vm-th-message-of m-sym))
	;; canonical message for this message ID
	(or (vm-thread-subtree-of msg)
	    ;; otherwise calcuate the thread-subtree
	    (let ((list (list m-sym))
		  (loop-obarray (make-vector 29 0))
		  subject-sym id-sym
		  result)
	      (while list
		(setq id-sym (car list))
		(when (and (vm-th-messages-of id-sym)
			   (not (memq (vm-th-message-of id-sym)
				      result)))
		  (setq result (append (vm-th-messages-of id-sym) result)))
		(when (null (intern-soft (symbol-name id-sym) loop-obarray))
		  (intern (symbol-name id-sym) loop-obarray)
		  (nconc list (copy-sequence (vm-th-children-of id-sym)))
		  (mapc
		   (lambda (m)
		     (setq subject-sym 
			   (intern (vm-so-sortable-subject m)
				   (with-current-buffer (vm-buffer-of m)
				     vm-thread-subject-obarray)))
		     (if (and (boundp subject-sym) 
			      (eq id-sym (vm-ts-root-of subject-sym)))
			 (nconc list (copy-sequence
				      (vm-ts-members-of subject-sym))))
		     )
		   (vm-th-messages-of id-sym)))
		(setq list (cdr list))
		)
	      (when msg
		(vm-set-thread-subtree-of msg result))
	      result))
      ;; non-canonical message for this message ID
      (vm-set-thread-subtree-of msg (list msg))
      (list msg))))

;;;###autoload
(defun vm-th-thread-count (m)
  "Returns the number of messages in the thread-subtree of message M.
M can be a message or the interned symbol of M.  Threads should
have been built for this function to work."
  (length (vm-th-thread-subtree m)))

(defun vm-th-check-thread-integrity (&optional ml)
  "Check that all messages are members of their thread subtrees.
Conversely, all members of thread subtrees should actually belong
to the thread.  Used for testing purposes."
  (interactive)
  (unless ml
    (with-current-buffer (or vm-mail-buffer (current-buffer))
      (setq ml vm-message-list)))
  ;; Check that all messages belong to their respective subtrees
  (mapc
   (lambda (m)
     (let* ((root (vm-th-thread-root-sym m))
	    (subtree (vm-th-thread-subtree root)))
       (with-current-buffer (vm-buffer-of m)
	 (unless (eq root 
		     (intern-soft (symbol-name root) vm-thread-obarray))
	   (debug 'interned-in-wrong-buffer m)))
       (unless (memq m subtree)
	 (debug 'missing m))))
   ml)
  ;; Check that all subtrees have correct messages
  (mapc
   (lambda (subroot)
     (let* ((subtree (vm-th-thread-subtree subroot))
	    (buf (vm-buffer-of subroot)))
       (mapc
	(lambda (m)
	  (unless (and (vm-th-thread-root m)
		       (eq (vm-th-thread-root m) 
			   (vm-th-thread-root subroot)))
	    (debug 'spurious m))
	  (unless (eq buf (vm-buffer-of m))
	    (debug 'wrong-buffer m)))
	subtree)))
   ml)
  )

;;; vm-thread.el ends here
