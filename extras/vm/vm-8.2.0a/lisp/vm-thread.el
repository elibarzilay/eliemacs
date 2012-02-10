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
  (require 'vm-motion)
  (require 'vm-summary)
  (require 'vm-sort)
)

;; --------------------------------------------------------------------------
;; Top-level operations
;;
;; vm-toggle-threads-display: interactive () -> none
;; vm-build-threads : (message list) -> none
;; vm-build-thread-lists : () -> none
;; vm-unthread-message-and-mirrors : (message &key
;;                                    :message-changing bool) -> none
;; vm-unthread-message : (message &key
;;                                    :message-changing bool) -> none
;;
;; vm-check-thread-integrity: (&optional message list) -> none
;;
;; vm-thread-mark-for-summary-update : message list -> none
;;
;; vm-parent: (message) -> message
;; vm-references: (message) -> string list
;;
;; vm-thread-symbol : (message) -> symbol
;; vm-thread-list : (message) -> symbol list
;; vm-thread-root : (message or symbol) -> message
;; vm-thread-root-sym : (message or symbol) -> symbol
;; vm-thread-root-p : (message) -> bool
;; vm-thread-indentation : (message) -> integer
;; vm-thread-subtree : (message or symbol) -> message list
;; vm-thread-count : (message or symbol) -> integer
;; vm-subject-symbol: (message) -> symbol
;;
;; The thread-obarray and thread-subject-obarray properties
;;
;; vm-th-thread-symbol: (message) -> symbol
;; vm-th-messages-of : symbol -> message list
;; vm-th-message-of : symbol -> message or nil
;; vm-th-children-of : symbol -> symbol list
;; vm-th-child-messages-of : symbol -> message list
;; vm-th-parent-of : symbol -> symbol
;; vm-th-date-of : symbol -> string
;; vm-th-youngest-date-of : symbol -> string
;; vm-th-oldest-date-of : symbol -> string
;; vm-th-thread-date-of : symbol X criterion-symbol -> string
;; vm-th-canonical-message-p : message -> bool
;; vm-th-root : symbol -> message
;;
;; vm-th-new-thread-symbol: message -> symbol
;; vm-th-add-message-to-symbol: symbol X message -> void
;; vm-th-remove-message-from-symbol: symbol X message -> void
;; vm-th-init-thread-symbol: symbol X message -> void
;; vm-th-set-parent : symbol X symbol -> void
;; vm-th-add-child: symbol X symbol -> void
;; vm-th-delete-child: symbol X symbol -> void
;; 
;; vm-th-clear-cached-data: symbol X symbol -> void
;;
;;
;; vm-ts-subject-symbol : symbol -> symbol
;; vm-ts-root-of : symbol -> symbol
;; vm-ts-root-date-of : symbol -> date
;; vm-ts-members-of : symbol -> symbol list
;; vm-ts-messages-of : symbol -> message list
;;
;; vm-ts-add-member: symbol X symbol -> void
;; vm-ts-add-message: symbol X message -> void
;;
;; vm-ts-clear-cached-data: symbol X symbol -> void
;;
;; vm-th-parent : message -> string
;;      (aliased to vm-parent)
;; vm-th-references : message -> string list
;;	(aliased to vm-references)
;; vm-th-thread-indentation : message -> integer
;;	(aliased to vm-thread-indentation)
;; --------------------------------------------------------------------------

(if (fboundp 'define-error)
    (define-error 'vm-thread-error "VM internal threading error")
  (put 'vm-thread-error 'error-conditions
       '(vm-thread-error error))
  (put 'vm-thread-error 'error-message "VM internal threading error")
  )

(defsubst vm-thread-debug (message &rest args)
  (if (and vm-thread-debug vm-summary-show-threads (vectorp vm-thread-obarray))
      (apply 'debug message args)))


;;;###autoload
(defun vm-thread-symbol (m)
  "Returns the interned symbol of message M which carries the
threading information.  Threads should have been built before this.
Otherwise nil is returned."
  (with-current-buffer (vm-buffer-of m)
    (and (vectorp vm-thread-obarray)
	 (intern (vm-su-message-id m) vm-thread-obarray))))

;;;###autoload
(defun vm-subject-symbol (m)
  "Returns the interned symbol of message M which carries the
subject-based threading information.  Threads should have been built
before this.  Otherwise nil is returned."
  (with-current-buffer (vm-buffer-of m)
    (and (vectorp vm-thread-subject-obarray)
	 (intern (vm-so-sortable-subject m) vm-thread-subject-obarray))))


(defsubst vm-th-thread-symbol (m)
  (intern (vm-su-message-id m) vm-thread-obarray))

(defsubst vm-th-youngest-date-of (id-sym)
  (get id-sym 'youngest-date))

(defsubst vm-th-set-youngest-date-of (id-sym date)
  (put id-sym 'youngest-date date))

(defsubst vm-th-oldest-date-of (id-sym)
  (get id-sym 'oldest-date))

(defsubst vm-th-set-oldest-date-of (id-sym date)
  (put id-sym 'oldest-date date))

(defsubst vm-th-thread-date-of (id-sym criterion)
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

(defun vm-th-add-child (parent-sym id-sym)
  (if (member (symbol-name id-sym) (car vm-traced-message-ids))
      (vm-thread-debug 'vm-th-add-child id-sym))
  (unless (member id-sym (vm-th-children-of parent-sym))
    (vm-th-set-children-of
     parent-sym (cons id-sym (vm-th-children-of parent-sym)))))

(defun vm-th-delete-child (parent-sym id-sym)
  (if (member (symbol-name id-sym) (car vm-traced-message-ids) )
      (vm-thread-debug 'vm-th-delete-child id-sym))
  (let ((kids (vm-th-children-of parent-sym)))
    (vm-th-set-children-of parent-sym (remq id-sym kids))))

(defsubst vm-th-date-of (id-sym)
  (get id-sym 'date))

(defsubst vm-th-set-date-of (id-sym date)
  (put id-sym 'date date))

(defsubst vm-ts-subject-symbol (id-sym)
  ;; the subject symbol is calculated from the canonical message of
  ;; ID-SYM, just in case the different copies have different subjects.
  (intern (vm-so-sortable-subject (vm-th-message-of id-sym))
	  vm-thread-subject-obarray))

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


;;; thread tree - basic operations

(defun vm-th-new-thread-symbol (m)
  "Create a new thread symbol for message M and intitialize its parent
and child pointers."
  (let ((id-sym (vm-th-thread-symbol m)))
    (vm-th-set-parent-of id-sym nil)
    (vm-th-set-children-of id-sym nil)
    id-sym))

(defsubst vm-th-add-message-to-symbol (id-sym m)
  "Add message M to ID-SYM as one of the messages with its id."
  (unless (memq m (vm-th-messages-of id-sym))
    (vm-th-set-messages-of id-sym (cons m (vm-th-messages-of id-sym)))))

(defsubst vm-th-remove-message-from-symbol (id-sym m)
  "Delete message M from ID-SYM as one of the messages with its id."
  (vm-th-set-messages-of id-sym (remq m (vm-th-messages-of id-sym)))
  (if (eq m (vm-th-message-of id-sym))
      (vm-th-set-message-of id-sym (car (vm-th-messages-of id-sym)))))

(defsubst vm-th-init-thread-symbol (id-sym m)
  "Initialize thread symbol ID-SYM to the message M."
  (vm-th-set-message-of id-sym m)
  (vm-th-set-messages-of id-sym (list m))
  (vm-th-set-date-of id-sym (vm-so-sortable-datestring m)))

(defsubst vm-th-set-parent (id-sym parent-sym)
  "Set the parent of ID-SYM to PARENT-SYM."
  (vm-th-set-parent-of id-sym parent-sym)
  (vm-th-add-child parent-sym id-sym))

(defsubst vm-th-clear-cached-data (id-sym parent-sym)
  "Clear the cached thread-subtree and thread-list information that is
invalidated by setting the parent of ID-SYM to PARENT-SYM.  This
involves the thread-subtrees of PARENT-SYM and all its ancestors.
It also invovles thread-lists of ID-SYM and all its descendants."
  (vm-th-clear-subtree parent-sym)
  (vm-th-clear-thread-lists id-sym))

(defsubst vm-ts-add-member (subject-sym id-sym)
  "Add ID-SYM as a member of SUBJECT-SYM."
  (unless (memq id-sym (vm-ts-members-of subject-sym))
    (vm-ts-set-members-of 
     subject-sym (cons id-sym (vm-ts-members-of subject-sym)))))

(defsubst vm-ts-add-message (subject-sym m)
  "Add M as a message in the subject thread of SUBJECT-SYM."
  (vm-ts-set-messages-of 
   subject-sym (cons m (vm-ts-messages-of subject-sym))))

(defsubst vm-ts-clear-cached-data (id-sym subject-sym)
  "Clear the cached thread-subtree and thread-list information
for ID-SYM, which is the subject root of SUBJECT-SYM.  This
involves clearing the thread-subtree of ID-SYM and the
thread-lists of all members of SUBJEC-SYM. (not entirely clear if this
is right).                                           USR, 2011-04-08"
  (vm-th-clear-subtree id-sym)
  (mapc 'vm-th-clear-thread-lists 
	(vm-ts-members-of subject-sym)))


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

;;;###autoload
(defun vm-promote-subthread (n)
  "Decrease the thread indentation of the current message and its
subthread by $N$ steps (provided as a prefix argument).  

The case $N$ being 0 is a special case.  It means to decrease the
indentation all the way to 0."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (interactive-p))
  (let ((modified (buffer-modified-p))
	(msg (car vm-message-pointer))
	(indent 0))
    (if (= n 0)				; special case, set to 0
      (let ((indent (or (vm-thread-indentation-of msg) 0)))
	(mapc (lambda (m)
		(vm-set-thread-indentation-offset-of m (- indent)))
	      (vm-thread-subtree msg)))
      (mapc (lambda (m)
	      (vm-set-thread-indentation-offset-of 
	       m (- (or (vm-thread-indentation-offset-of m) 0)
		      n)))
	    (vm-thread-subtree msg)))
    (vm-thread-mark-for-summary-update (list msg))
    (vm-update-summary-and-mode-line)))

;;;###autoload
(defun vm-demote-subthread (n)
  "Increase the thread indentation of the current message and its
subthread by $N$ steps (provided as a prefix argument).  

The case $N$ being 0 is a special case.  It means to reset the
indentation back to the normal indentation, i.e., no offset is used."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (interactive-p))
  (let ((modified (buffer-modified-p))
	(msg (car vm-message-pointer)))
    (if (= n 0)
	(mapc (lambda (m) (vm-set-thread-indentation-offset-of m 0))
	      (vm-thread-subtree msg))
      (mapc (lambda (m)
	      (vm-set-thread-indentation-offset-of 
	       m (+ (or (vm-thread-indentation-offset-of m) 0) n)))
	    (vm-thread-subtree msg)))
    (vm-thread-mark-for-summary-update (list msg))
    (vm-update-summary-and-mode-line)))

;; Dependency of threading information
;;
;; parent & children -> thread-list -> thread-indentation
;;                 |
;;                 |--> thread-subtree

;;;###autoload
(defun vm-build-threads (message-list)
  "For all messages in MESSAGE-LIST, build thread information in the
`vm-thread-obarray' and `vm-thread-subject-obarray'.  If MESSAGE-LIST
is nil, do it for all the messages in the folder.  USR, 2010-07-15"
  (let ((initializing (not (vectorp vm-thread-obarray)))
	(mp (or message-list vm-message-list))
	(n 0)
	;; Just for laughs, make the update interval vary.
	(modulus (+ (% (vm-abs (random)) 11) 40))
	;; no need to schedule reindents of reparented messages
	;; unless there were already messages present.
	(schedule-reindents message-list)
	m parent parent-sym id id-sym date refs old-parent-sym)
  (when initializing
    (setq vm-thread-obarray (make-vector 641 0)
	  vm-thread-subject-obarray (make-vector 641 0)))
    ;; Build threads using references
    (vm-build-reference-threads mp schedule-reindents initializing)
    ;; Build threads using subject
    (when vm-thread-using-subject
      (vm-build-subject-threads mp schedule-reindents initializing))
    ;; Calculate thread-subtrees for all the known message ID's
    (mapatoms
     (lambda (id-sym)
       (when (vm-th-message-of id-sym)
	 (vm-thread-subtree id-sym)))
     vm-thread-obarray)
    (when (> n modulus)
      (vm-inform 6 "Building threads... done"))))

(defun vm-build-reference-threads (mlist schedule-reindents initializing)
  "Build reference threads for all the messages in MLIST.  If threads are
already built, then just insert these messages into the threads
database.

If SCHEDULE-REINDENTS is non-nil, then ask for the summary lines of
all affected messages to be updated.

If INITIALIZING is non-nil, then assume that the threads database is
being initialized."
  (let ((n 0)
	(mp mlist)
	modulus total
	m parent parent-sym id id-sym date refs old-parent-sym)
    (setq total (* 2 (length mlist)))
    (setq modulus (max 10 (/ (length mlist) 50)))
    (while mp
      (setq m (car mp)
	    id (vm-su-message-id m)
	    id-sym (intern-soft id vm-thread-obarray))
      (if (member id vm-traced-message-ids)
	  (vm-thread-debug 'vm-build-reference-threads id m))
      (unless id-sym			; first occurrence now
	(setq id-sym (vm-th-new-thread-symbol m)))
      (if (vm-th-messages-of id-sym)	; registered already
	  (vm-th-add-message-to-symbol id-sym m)
	(vm-th-init-thread-symbol id-sym m))
      (when schedule-reindents
	(vm-thread-mark-for-summary-update (list m)))
      ;; Thread using the parent
      (setq parent (vm-parent m))
      (if (null parent)
	  ;; could be a duplicate copy of a message
	  (unless initializing
	    (vm-th-clear-subtree id-sym))
	(setq parent-sym (intern parent vm-thread-obarray))
	;; set the parent of m.
	;; if there was a parent already, update it consistently.
	(when (vm-th-safe-parent-p id-sym parent-sym)
	  (if (member (symbol-name id-sym) vm-traced-message-ids)
	      (vm-thread-debug 'vm-build-reference-threads-1 id-sym))	  
	  (cond ((null (vm-th-parent-of id-sym))
		 (unless initializing 
		   (vm-th-clear-cached-data id-sym parent-sym))
		 (vm-th-set-parent id-sym parent-sym))
		((eq (vm-th-parent-of id-sym) parent-sym)
		 ;; could be a duplicate copy of a message
		 (unless initializing
		   (vm-th-clear-subtree id-sym))
		 (when schedule-reindents
		   (vm-thread-mark-for-summary-update
		    (vm-th-messages-of parent-sym))))
		(t
		 (setq old-parent-sym (vm-th-parent-of id-sym))
		 (unless initializing 
		   (vm-th-clear-subtree old-parent-sym)	
		   (vm-th-clear-cached-data id-sym parent-sym))
		 (vm-th-delete-child old-parent-sym id-sym)
		 (vm-th-set-parent id-sym parent-sym)
		 (when schedule-reindents
		   (vm-thread-mark-for-summary-update
		    (vm-th-messages-of id-sym))
		   (if (vm-th-message-of old-parent-sym)
		       (vm-mark-for-summary-update
			(vm-th-message-of old-parent-sym))
		     (vm-thread-debug 'vm-build-reference-threads 
				      'old-parent-sym old-parent-sym)
		     ))))))
      (setq mp (cdr mp) n (1+ n))
      (if (zerop (% n modulus))
	  (vm-inform 7 "Building threads... %d%%" (* (/ (+ n 0.0) total) 100))))

    ;; use the References header to set parenting information
    ;; for ancestors of this message.  This does not override
    ;; a parent pointer for a message if it already exists.
    (setq mp mlist)
    (while mp
      (setq m (car mp)
	    id (vm-su-message-id m))
      (if (member id vm-traced-message-ids)
	  (vm-thread-debug 'vm-build-reference-threads-2 m))
      (if (cdr (setq refs (vm-references m)))
	  (let (parent-sym id-sym msgs msg-syms)
	    (setq parent-sym (intern (car refs) vm-thread-obarray)
		  refs (cdr refs))
	    (while refs
	      (setq id-sym (intern (car refs) vm-thread-obarray))
	      (when (and (null (vm-th-parent-of id-sym))
			 (vm-th-safe-parent-p id-sym parent-sym))
		(if (member (symbol-name id-sym) vm-traced-message-ids)
		    (vm-thread-debug 'vm-build-reference-threads-2 id-sym))
		(unless initializing 
		  (vm-th-clear-cached-data id-sym parent-sym))
		(vm-th-set-parent id-sym parent-sym)
		(if schedule-reindents
		    (vm-thread-mark-for-summary-update 
		     (vm-th-messages-of id-sym))))
	      (setq parent-sym id-sym
		    refs (cdr refs)))))
      (setq mp (cdr mp) n (1+ n))
      (if (zerop (% n modulus))
	  (vm-inform 7 "Building threads... %d%%" (* (/ (+ n 0.0) total) 100)))
      )))

(defun vm-th-clear-thread-lists (id-sym)
  "Clear the thread-list and thread-indentation fields of the
message with ID-SYM and all its descendants."
  (mapc (lambda (d)
	  (vm-set-thread-list-of d nil)
	  (vm-set-thread-indentation-of d nil))
	(vm-th-messages-of id-sym))
  (mapc 'vm-th-clear-thread-lists
	(vm-th-children-of id-sym)))

(defun vm-th-clear-subtree-of (id-sym)
  "Clear the thread-subtrees of the messages with ID-SYM, i.e.,
set them to nil.  They will get recalculated on demand."
  ;; (when (vm-th-message-of id-sym)
  ;;   (vm-set-thread-subtree-of (vm-th-message-of id-sym) nil))
  (mapc (lambda (m) 
  	  (vm-set-thread-subtree-of m nil))
  	(vm-th-messages-of id-sym))
  )

(defun vm-th-clear-subtree (id-sym)
  "Clear the thread subtrees of the messages with id-symbol ID-SYM and
all its ancestors, followed via the parent links."
  (let ((msg (vm-th-message-of id-sym))
	subject subject-sym)
    (vm-th-clear-subtree-of id-sym)
    (while (vm-th-parent-of id-sym)
      (setq id-sym (vm-th-parent-of id-sym))
      (vm-th-clear-subtree-of id-sym)
      (when (vm-th-message-of id-sym) 
	(setq msg (vm-th-message-of id-sym))))
    ;; msg is now the reference root of id-sym
    (when msg 
      (setq subject-sym (vm-ts-subject-symbol (vm-th-thread-symbol msg)))
      (when (boundp subject-sym)
	(setq id-sym (vm-ts-root-of subject-sym))
	(vm-th-clear-subtree-of id-sym)))))

(defun vm-th-safe-parent-p (id-sym parent-sym)
  "Check if it is safe to set the parent of ID-SYM to PARENT-SYM."
  ;; Check to make sure that ID-SYM is not an ancestor of PARENT-SYM
  (if (or (member (symbol-name id-sym) vm-traced-message-ids)
	  (member (symbol-name parent-sym) vm-traced-message-ids))
      (vm-thread-debug 'vm-thread-safe-parent-p id-sym parent-sym))
  (let ((ancestor parent-sym))
    (catch 'return
      (while ancestor
	(when (eq ancestor id-sym)
	  (throw 'return nil))
	(setq ancestor (vm-th-parent-of ancestor)))
      t)))

(defun vm-th-belongs-to-reference-thread (id-sym)
  "Check if ID-SYM is the symbol of a message in a reference thread
with other ancestors."
  (let ((parent (vm-th-parent-of id-sym)))
    (catch 'return
      (while parent
	(if (vm-th-messages-of parent)
	    (throw 'return t)
	  (setq parent (vm-th-parent-of parent))))
      nil)))

(defun vm-th-root (id-sym)
  "Return the reference-thread root message of ID-SYM; nil is returned
  in the special case ID-SYM doesn't have any messages or ancestors."
  (let ((parent (vm-th-parent-of id-sym))
	(root (vm-th-message-of id-sym)))
    (while parent
      (when (vm-th-messages-of parent)
	(setq root (vm-th-message-of parent)))
      (setq parent (vm-th-parent-of parent)))
    root))

(defun vm-build-subject-threads (mp schedule-reindents initializing)
  (let ((n 0)
	(modulus 10)
	m id id-sym date ref-root
	subject subject-sym)
    (while mp
      (setq m (car mp)
	    id (vm-su-message-id m)
	    id-sym (vm-th-thread-symbol m)
	    date (vm-so-sortable-datestring m)
	    ref-root (vm-th-root id-sym))
      (when (member id vm-traced-message-ids)
	(vm-thread-debug 'vm-build-subject-threads id m))
      ;; Use the reference root's subject rather than m's subject
      (setq subject (vm-so-sortable-subject ref-root)
	    subject-sym (vm-ts-subject-symbol (vm-th-thread-symbol ref-root)))
      (when (member subject vm-traced-message-subjects)
	(vm-thread-debug 'vm-build-subject-threads id m))
      ;; -------------- atomic block -------------------------------
      (let* ((inhibit-quit t))
	;; if this subject was never seen before create the
	;; information vector.
	(if (not (boundp subject-sym))
	    ;; new subject
	    (set subject-sym (vector id-sym date nil (list m)))
	  ;; this subject seen before 
	  (vm-ts-add-message subject-sym m)
	  (cond 
	   ;; duplicate copy of the ts-root
	   ((eq id-sym (vm-ts-root-of subject-sym))
	    (vm-th-clear-subtree (vm-ts-root-of subject-sym)))
	   ;; if older than the ts-root, make it the root
	   ((string< date (vm-ts-root-date-of subject-sym))
	    (let* ((i-sym (vm-ts-root-of subject-sym)))
	      (unless initializing
		(vm-ts-clear-cached-data i-sym subject-sym))
	      (unless (vm-th-belongs-to-reference-thread i-sym)
		;; strange.  why would i-sym ever be in a ref thread?
		(vm-ts-add-member subject-sym i-sym))
	      (vm-ts-set-root-of subject-sym id-sym)
	      (vm-ts-set-root-date-of subject-sym date)
	      ;; this loops _and_ recurses and I'm worried
	      ;; about it going into a spin someday.  So I
	      ;; unblock interrupts here.  It's not critical
	      ;; that it finish... the summary will just be out
	      ;; of sync.
	      (when schedule-reindents
		(let ((inhibit-quit nil))
		  ;; there might be need for vm-th-clear-subtree here
		  (vm-thread-mark-for-summary-update 
		   (vm-ts-messages-of subject-sym))))))
	   ;; newer than the ts-root
	   (t
	    (unless (vm-th-belongs-to-reference-thread id-sym)
	      (vm-th-clear-subtree (vm-ts-root-of subject-sym))
	      ;; no need to clear thread-lists; ts-root is unchanged
	      (vm-ts-add-member subject-sym id-sym))))))
      ;; -------------- end atomic block ----------------------------------
      (setq mp (cdr mp) n (1+ n))
      (when (zerop (% n modulus))
	(vm-inform 7 "Building threads... %d" n)))))

;; used by the thread sort code.
;;
;; vm-thread-list initializes the oldest-date property on
;; the message-id symbols.  Since this property is used as an
;; ordering key by the thread sort the oldest-date properties
;; must be computed before the sort begins, not during it.
;; Otherwise the sort won't be stable and there will be chaos.

;;;###autoload
(defun vm-build-thread-lists ()
  "Fill in the thread-list fields of the Soft data vector for all
messages in the folder.  Threads should have been built before this
function is called."
  (dolist (m vm-message-list)
    (vm-thread-list m))
  (if vm-thread-debug
      (vm-check-thread-integrity vm-message-list)))

;;;###autoload
(defun vm-thread-mark-for-summary-update (message-list)
  "Mark the messages in MESSAGE-LIST and all their descendants for
summary update.  This function does not depend on cached
thread-subtrees.                                USR, 2011-04-03" 
  (mapc (lambda (m)
	  ;; if thread-list is null then we've already marked this
	  ;; message, or it doesn't need marking.
	  (if (null (vm-thread-list-of m))
	      nil
	    (vm-mark-for-summary-update m t)
	    (vm-set-thread-list-of m nil)
	    (vm-set-thread-indentation-of m nil)
	    (vm-thread-mark-for-summary-update
	     (vm-th-child-messages-of (vm-thread-symbol m)))))
	message-list))

(defun vm-build-thread-list (message)
  "Returns the thread-list, i.e., the lineage of MESSAGE, as a list of
symbols interned in vm-thread-obarray."
  (let ((done nil)
	(loop-recovery-point nil)
	(date (vm-so-sortable-datestring message))
	m thread-list id-sym subject-sym loop-sym root-date youngest-date
	root ancestors)
    (setq m message)
    (with-current-buffer (vm-buffer-of m)
      ;; thread trees do not have loops any more, but better to be
      ;; safe than sorry.  USR, 2011-05-13
      (fillarray vm-thread-loop-obarray 0)
      (setq id-sym (vm-th-thread-symbol m)
	    thread-list (list id-sym))
      (when (member (symbol-name id-sym) vm-traced-message-ids)
	(vm-thread-debug 'vm-build-thread-list id-sym))
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
	(when (or (null root-date)
		  (string< date root-date))
	  (vm-th-set-oldest-date-of id-sym date))
	;; save the date of the youngest message in this thread
	(setq youngest-date (vm-th-youngest-date-of id-sym))
	(when (or (null root-date)
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
			  (vm-ts-subject-symbol (vm-th-thread-symbol m)))
		    (or (not (boundp subject-sym))
			(and (eq (vm-ts-root-of subject-sym) 
				 (vm-th-thread-symbol m)))))
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
		 (setq ancestors (remq id-sym (vm-thread-list root)))
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
(defun* vm-unthread-message-and-mirrors (message &key message-changing)
  "Removes MESSAGE and all its mirrored messages from their
current threads.  If optional argument MESSAGE-CHANGING is
non-nil, then forget information that might be different if the
message contents changed.

MESSAGE should be a real (non-virtual) message.

The full functionality of this function is not entirely clear.  
						USR, 2010-07-24"
  (save-current-buffer
    (mapc 
     (lambda (m)
       ;; Don't trust blindly.  The user could have killed some of
       ;; these buffers.
       (when (buffer-name (vm-buffer-of m))
	 (set-buffer (vm-buffer-of m))
	 (when (vectorp vm-thread-obarray)
	   (vm-unthread-message 
	    m :message-changing message-changing))))
     (cons message (vm-virtual-messages-of message)))))

;;;###autoload
(defun* vm-unthread-message (m &key message-changing)
  "Removes message M from its thread.  If optional argument
MESSAGE-CHANGING is non-nil, then forget information that might
be different if the message contents changed.  The message will be
reinserted into an appropriate thread later.       USR, 2011-03-17"
  ;; -------------- atomic block -------------------------------
  (let ((inhibit-quit t)
	date id-sym s-sym p-sym root-sym)
    ;; handles for the thread and thread-subject databases
    (setq id-sym (vm-th-thread-symbol m))
    (setq root-sym (vm-th-thread-symbol (vm-th-root id-sym)))
    (setq s-sym (vm-ts-subject-symbol root-sym))
    (if (member (symbol-name id-sym) vm-traced-message-ids)
	(vm-thread-debug 'vm-unthread-message id-sym))
    (if (member (symbol-name s-sym) vm-traced-message-subjects)
	(vm-thread-debug 'vm-unthread-message id-sym))
    ;; mark the subtree for summary update before we change it
    (vm-thread-mark-for-summary-update (list m))
    ;; discard cached thread properties of descendants and ancestors
    (vm-th-clear-cached-data id-sym id-sym)
    ;; remove the message from its erstwhile thread
    (when (boundp id-sym)
      ;; remove m from its thread node
      (vm-th-remove-message-from-symbol id-sym m)
      ;; reset the thread dates of m
      (setq date (vm-so-sortable-datestring m))
      (vm-th-set-youngest-date-of id-sym date)
      (vm-th-set-oldest-date-of id-sym date)
      ;; if message changed, remove it from the thread tree
      ;; not clear what is going on.  USR, 2010-07-24
      (when (and message-changing (null (vm-th-message-of id-sym)))
	(setq p-sym (vm-th-parent-of id-sym))
	(when p-sym 
	  (vm-th-delete-child p-sym id-sym))
	(vm-th-set-parent-of id-sym nil)))

    ;; remove the message from its erstwhile subject thread
    (when (boundp s-sym)
      (if (eq id-sym (vm-ts-root-of s-sym))
	  ;; (when message-changing
	  (cond
	   ;; duplicate copy present, so keep the root id-sym.
	   ;; FIXME the thread-subtree of the duplicate copy has to be
	   ;; cleared somehow.
	   ((vm-th-message-of id-sym)	
	      (vm-ts-set-messages-of
	       s-sym (remq m (vm-ts-messages-of s-sym))))
	   ;; subject thread becomes empty
	   ((null (remq m (vm-ts-messages-of s-sym)))
	      (makunbound s-sym))
	   (t
	    (let ((p (remq m (vm-ts-messages-of s-sym)))
		  oldest-msg oldest-date children)
	      (setq oldest-msg (vm-th-message-of (vm-th-thread-symbol (car p))))
	      (setq oldest-date (vm-so-sortable-datestring (car p)))
	      (setq p (cdr p))
	      (while p
		(when (and (string-lessp 
			    (vm-so-sortable-datestring (car p))
			    oldest-date))
		  (setq oldest-msg (vm-th-message-of 
				    (vm-th-thread-symbol (car p)))
			oldest-date (vm-so-sortable-datestring (car p))))
		(setq p (cdr p)))
	      (setq root-sym (vm-th-thread-symbol oldest-msg))
	      (vm-th-clear-cached-data root-sym root-sym)
	      (vm-ts-set-root-of s-sym root-sym)
	      (vm-ts-set-root-date-of s-sym oldest-date)
	      (setq children (remq root-sym (vm-ts-members-of s-sym)))
	      (vm-ts-set-members-of s-sym children)
	      (vm-ts-set-messages-of
	       s-sym (remq m (vm-ts-messages-of s-sym)))
	      ;; I'm not sure there aren't situations
	      ;; where this might loop forever.
	      (let ((inhibit-quit nil))
		(mapc (lambda (c-sym)
			(vm-thread-mark-for-summary-update 
			 (vm-th-messages-of c-sym)))
		      children)))))
	;; )
	(unless (vm-th-message-of id-sym)
	  (vm-ts-set-members-of 
	   s-sym (remq id-sym (vm-ts-members-of s-sym))))
	(vm-ts-set-messages-of 
	 s-sym (remq m (vm-ts-messages-of s-sym)))
	)))
  ;; -------------- end atomic block -------------------------------
  )

;; This function is still under development.  USR, 2011-04-04

;;;###autoload
(defun vm-attach-to-thread ()
  "Attach the current message as a child of the message last visited."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 0 (interactive-p))
  (vm-error-if-folder-read-only)
  (vm-build-threads-if-unbuilt)
  (unless vm-last-message-pointer
    (error "No last message visited"))
  (let ((new-parent (car vm-last-message-pointer))
	(p-sym (vm-thread-symbol (car vm-last-message-pointer)))
	(m (car vm-message-pointer))
	(m-sym (vm-thread-symbol (car vm-message-pointer))))
    ;; (vm-thread-mark-for-summary-update (list m))
    (vm-unthread-message m :message-changing t)
    (unless (vm-th-safe-parent-p m-sym p-sym)
      (error "Attaching to thread will create a cycle"))
    (vm-th-set-parent-of m-sym p-sym)
    (vm-th-add-child p-sym m-sym))
    (vm-inform 5 "Message attached to thread")
    (vm-update-summary-and-mode-line)
    )

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
(defalias 'vm-th-references 'vm-references)

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
(defalias 'vm-th-parent 'vm-parent)

;;;###autoload
(defun vm-thread-indentation (m)
  "Returns the cached thread-indentation of message M.  If the cache is
nil, calculates the thread-indentation and caches it.  It also applies
any thread-indentation-offset that has been defined for a subthread.
							USR, 2011-04-03"
  (+ (or (vm-thread-indentation-of m)
	 (let ((p (vm-thread-list m))
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
	   (if (and (eq (car p) (vm-thread-symbol m))
		    (not (eq (vm-th-message-of (car p)) m)))
	       ;; thread root is a duplicate of m
	       (vm-set-thread-indentation-of m n)
	     (vm-set-thread-indentation-of m (1- n)))
	   (vm-thread-indentation-of m)))
     (or (vm-thread-indentation-offset-of m)
	 0)
     ))

(defalias 'vm-th-thread-indentation 'vm-thread-indentation)

;;;###autoload
(defun vm-thread-list (m)
  "Returns the cached thread-list of message M.  If the cache is nil,
calculates the thread-list and caches it.  USR, 2010-03-13"
  (or (vm-thread-list-of m)
      (progn
	(vm-set-thread-list-of m (vm-build-thread-list m))
	;; reset the thread-subtrees, forcing them to be rebuilt
	;; (mapc 'vm-th-clear-subtree-of (vm-thread-list-of m))
	(vm-thread-list-of m))))
(defalias 'vm-th-thread-list 'vm-thread-list)

;;;###autoload
(defun vm-thread-root (m)
  "Returns the root message of M.  M can be either a message or
the interned symbol of a message.  If there are multiple messages with
the same root message ID, one of them is chosen arbitrarily.  Threads
should have been built for this function to work."
  (let (m-sym list id-sym)
    (cond ((symbolp m) 
	   (setq m-sym m)
	   (setq m (vm-th-message-of m-sym)))
	  (t
	   (setq m-sym (vm-thread-symbol m))))
    (if (and vm-debug (member (symbol-name m-sym) vm-traced-message-ids))
	(debug 'vm-thread-root m-sym))
    (catch 'return
      (unless m-sym
	(vm-thread-debug 'vm-thread-root m-sym)
	(throw 'return m))
      (setq list (vm-thread-list m))
      (while list
	(setq id-sym (car list))
	(when (vm-th-messages-of id-sym)
	  (throw 'return (vm-th-message-of id-sym)))
	(setq list (cdr list)))
      nil)))

;;;###autoload
(defun vm-thread-root-sym (m)
  "Returns interned symbol of the root message of M.  M can be
either a message or the interned symbol of M.  Threads should
have been built for this function to work.  

See also: `vm-thread-root'."
  (let (m-sym list id-sym)
    (cond ((symbolp m) 
	   (setq m-sym m)
	   (setq m (vm-th-message-of m-sym)))
	  (t
	   (setq m-sym (vm-thread-symbol m))))
    (if (and vm-debug (member (symbol-name m-sym) vm-traced-message-ids))
	(debug m-sym))
    (catch 'return
    (unless m-sym
      (vm-thread-debug 'vm-thread-root-sym m-sym)
      (throw 'return nil))
    (setq list (vm-thread-list m))
      (while list
	(setq id-sym (car list))
	(when (vm-th-messages-of id-sym)
	    (throw 'return id-sym))
	(setq list (cdr list)))
      nil)))

;;;###autoload
(defun vm-thread-root-p (m)
  "Returns t if message M is known to be a thread root, nil
otherwise.  No exceptions are thrown for errors."
  ;; Threads may not be turned on.  So, ignore errors.
  (condition-case err
      (and (eq m (vm-thread-root m))
	   (> (vm-thread-count m) 1))
    (vm-thread-error
     nil)))

;;;###autoload
(defun vm-thread-subtree (msg)
  "Returns the list of messages in the thread subtree of MSG.
MSG can be a message or the interned symbol of a message.
Threads should have been built for this function to work."
  (let (m-sym)
    (if (symbolp msg)
	(setq m-sym msg
	      msg (vm-th-message-of msg))
      (setq m-sym (vm-thread-symbol msg)))
    (unless m-sym
      (vm-thread-debug 'vm-thread-subtree m-sym)
      (signal 'vm-thread-error (list 'vm-thread-subtree)))
    (if (eq msg (vm-th-message-of m-sym))
	;; canonical message for this message ID
	(or (vm-thread-subtree-of msg)
	    ;; otherwise calcuate the thread-subtree
	    (let ((list (list m-sym))
		  (loop-obarray (make-vector 29 0))
		  subject-sym id-sym id
		  result)
	      (when (member (vm-su-message-id msg) vm-traced-message-ids)
		(with-current-buffer (vm-buffer-of msg)
		  (vm-thread-debug 'vm-thread-subtree (vm-su-message-id msg))))
	      (while list
		(setq id-sym (car list)
		      id (symbol-name id-sym))
		(when (and (vm-th-messages-of id-sym)
			   (not (memq (vm-th-message-of id-sym) result)))
		  (setq result (append result (vm-th-messages-of id-sym))))
		(when (null (intern-soft id loop-obarray))
		  (intern id loop-obarray)
		  (nconc list (copy-sequence (vm-th-children-of id-sym)))
		  (mapc
		   (lambda (m)
		     (setq subject-sym (vm-subject-symbol m))
		     (when (and (boundp subject-sym) 
				(eq id-sym (vm-ts-root-of subject-sym)))
		       (nconc list 
			      (copy-sequence (vm-ts-members-of subject-sym)))))
		   (vm-th-messages-of id-sym)))
		(setq list (cdr list)))
	      (when msg
		(vm-set-thread-subtree-of msg result))
	      result))
      ;; non-canonical message for this message ID
      (vm-set-thread-subtree-of msg (list msg))
      (list msg))))

;;;###autoload
(defun vm-thread-count (m)
  "Returns the number of messages in the thread-subtree of message M.
M can be a message or the interned symbol of M.  Threads should
have been built for this function to work."
  (length (vm-thread-subtree m)))

;;;###autoload
(defun vm-check-thread-integrity (&optional ml)
  "Check that all messages are members of their thread subtrees.
Conversely, all members of thread subtrees should actually belong
to the thread.  Used for testing purposes."
  (interactive)
  (vm-select-folder-buffer)
  (let ((errors-found nil))
  (when (vectorp vm-thread-obarray)
    (unless ml
      (with-current-buffer (or vm-mail-buffer (current-buffer))
	(setq ml vm-message-list)))
    ;; Check that all messages belong to their respective subtrees
    (mapc (lambda (m)
	    (let* ((root (vm-thread-root-sym m))
		   (tree (and root (vm-thread-subtree root))))
	      (if (vm-th-messages-of (vm-thread-symbol m))
		  (unless root
		    (vm-thread-debug 'message-with-no-root m)
		    (setq errors-found t))
		(vm-thread-debug 'message-lost m)
		(setq errors-found t))
	      (with-current-buffer (vm-buffer-of m)
		(unless (eq root 
			    (intern-soft (symbol-name root) vm-thread-obarray))
		  (vm-thread-debug 'interned-in-wrong-buffer root m)
		  (setq errors-found t)))
	      (when (and (vm-th-message-of root) (not (memq m tree)))
		(vm-thread-debug 'missing m))))
	  ml)
    ;; Check that all subtrees have correct messages
    (mapc (lambda (subroot)
	    (let* ((subtree (vm-thread-subtree subroot))
		   (buf (vm-buffer-of subroot)))
	      (mapc (lambda (m)
		      (unless (and (vm-thread-root m)
				   (eq (vm-thread-root m) 
				       (vm-thread-root subroot)))
			(vm-thread-debug 'spurious m)
			(setq errors-found t))
		      (unless (eq buf (vm-buffer-of m))
			(vm-thread-debug 'wrong-buffer m)
			(setq errors-found t)))
		    subtree)))
	  ml)
    ;; Recover from errors
    (when errors-found
      (vm-inform 0 (concat "Problem detected with the threads database; "
		       "try vm-fix-my-summary"))
      ;; (setq vm-thread-obarray 'bonk)
      ;; (setq vm-thread-subject-obarray 'bonk)
      ))))

;;; vm-thread.el ends here
