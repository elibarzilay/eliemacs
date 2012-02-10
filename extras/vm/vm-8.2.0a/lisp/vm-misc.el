;;; vm-misc.el --- Miscellaneous functions for VM
;;
;; This file is part of VM
;;
;; Copyright (C) 1989-2001 Kyle E. Jones
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

(provide 'vm-misc)

;; (eval-when-compile
;;   (require 'vm-misc))

;; vm-xemacs.el is a fake file to fool the Emacs 23 compiler
(declare-function find-coding-system "vm-xemacs" (coding-system-or-name))

;; Aliases for xemacs functions
(declare-function xemacs-abbreviate-file-name "vm-misc.el" 
		  (filename &optional hack-homedir))
(declare-function xemacs-insert-char "vm-misc.el"
		  (char &optional count ignored buffer))
;; Aliases for xemacs/fsfemacs functions with different arguments
(declare-function emacs-find-file-name-handler "vm-misc.el"
		  (filename &optional operation))
(declare-function emacs-focus-frame "vm-misc.el"
		  (&rest ignore))
(declare-function emacs-get-buffer-window "vm-misc.el"
		  (&optional buffer-or-name frame devices))

(declare-function vm-device-type "vm-misc.el"
		  (&optional device))
(declare-function vm-buffer-substring-no-properties "vm-misc.el"
		  (start end))
(declare-function substring-no-properties "vm-misc.el"
		  (string from &optional to))
(declare-function vm-extent-property "vm-misc.el" (overlay prop) t)
(declare-function vm-extent-object "vm-misc.el" (overlay) t)
(declare-function vm-set-extent-property "vm-misc.el" (overlay prop value) t)
(declare-function vm-set-extent-endpoints "vm-misc.el"
		  (overlay beg end &optional buffer) t)
(declare-function vm-make-extent "vm-misc.el"
		  (beg end &optional buffer front-advance rear-advance) t)
(declare-function vm-extent-end-position "vm-misc.el" (overlay) t)
(declare-function vm-extent-start-position "vm-misc.el" (overlay) t)
(declare-function vm-detach-extent "vm-misc.el" (overlay) t)
(declare-function vm-delete-extent "vm-misc.el" (overlay) t)
(declare-function vm-disable-extents "vm-misc.el" 
		  (&optional beg end name val) t)
(declare-function vm-extent-properties "vm-misc.el" (overlay) t)

(declare-function timezone-make-date-sortable "ext:timezone"
		  (date &optional local timezone))
(declare-function longlines-decode-region "ext:longlines"
		  (start end))
(declare-function longlines-wrap-region "ext:longlines"
		  (start end))
(declare-function vm-decode-mime-encoded-words "vm-mime" ())
(declare-function vm-decode-mime-encoded-words-in-string "vm-mime" (string))
(declare-function vm-su-subject "vm-summary" (message))


;; This file contains various low-level operations that address
;; incomaptibilities between Gnu and XEmacs.  Expect compiler warnings.

;; messages in the minibuffer

;; the chattiness levels are:
;; 0 - extremely quiet
;; 5 - medium
;; 7 - normal level
;; 10 - heavy debugging info

(defun vm-inform (level &rest args)
  (when (<= level vm-verbosity)
    (apply 'message args)))

(defun vm-warn (level sit-for &rest args)
  (when (<= level vm-verbosity)
    (apply 'message args)
    (sit-for sit-for)))

;; garbage-collector result
(defconst gc-fields '(:conses :syms :miscs 
			      :chars :vector 
			      :floats :intervals :strings))

(defsubst vm-garbage-collect ()
  (pp (vm-zip-lists gc-fields (garbage-collect))))

;; Make sure that interprogram-cut-function is defined
(unless (boundp 'interprogram-cut-function)
  (defvar interprogram-cut-function nil))

(defun vm-substring (string from &optional to)
  (let ((work-buffer nil))
    (set-buffer work-buffer)
    (unwind-protect
	(with-current-buffer work-buffer
	  (insert string)
	  (if (null to)
	      (setq to (length string))
	    (if (< to 0)
		(setq to (+ (length string) to))))
	  ;; string indices start at 0, buffers start at 1.
	  (setq from (1+ from)
		to (1+ to))
	  (if (> from (point-min))
	      (delete-region (point-min) from))
	  (if (< to (point-max))
	      (delete-region to (point-max)))
	  (buffer-string))
      (when work-buffer (kill-buffer work-buffer)))))

;; Taken from XEmacs as GNU Emacs is missing `replace-in-string' and defining
;; it may cause clashes with other packages defining it differently, in fact
;; we could also call the function `replace-regexp-in-string' as Roland
;; Winkler pointed out.
(defun vm-replace-in-string (str regexp newtext &optional literal)
  "Replace all matches in STR for REGEXP with NEWTEXT string,
 and returns the new string.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat `\\' in NEWTEXT as special:
  `\\&' in NEWTEXT means substitute original matched text.
  `\\N' means substitute what matched the Nth `\\(...\\)'.
       If Nth parens didn't match, substitute nothing.
  `\\\\' means insert one `\\'.
  `\\u' means upcase the next character.
  `\\l' means downcase the next character.
  `\\U' means begin upcasing all following characters.
  `\\L' means begin downcasing all following characters.
  `\\E' means terminate the effect of any `\\U' or `\\L'."
  (if (> (length str) 50)
      (let ((cfs case-fold-search))
	(with-temp-buffer
          (setq case-fold-search cfs)
	  (insert str)
	  (goto-char 1)
	  (while (re-search-forward regexp nil t)
	    (replace-match newtext t literal))
	  (buffer-string)))
    (let ((start 0) newstr)
      (while (string-match regexp str start)
        (setq newstr (replace-match newtext t literal str)
              start (+ (match-end 0) (- (length newstr) (length str)))
              str newstr))
      str)))

(defun vm-delete-non-matching-strings (regexp list &optional destructively)
  "Delete strings matching REGEXP from LIST.
Optional third arg non-nil means to destructively alter LIST, instead of
working on a copy.

The new version of the list, minus the deleted strings, is returned."
  (or destructively (setq list (copy-sequence list)))
  (let ((curr list) (prev nil))
    (while curr
      (if (string-match regexp (car curr))
	  (setq prev curr
		curr (cdr curr))
	(if (null prev)
	    (setq list (cdr list)
		  curr list)
	  (setcdr prev (cdr curr))
	  (setq curr (cdr curr)))))
    list ))

(defun vm-parse (string regexp &optional matchn matches)
  "Returns list of string by splitting STRING with REGEXP matches.
REGEXP must match one item and MATCHN can be used to select a match
group (default is 1).  MATCHES is the number of time the match is
applied (default until it does not match anymore).

This function is similar to a spring-split, but a bit more complex
and flexible."
  (or matchn (setq matchn 1))
  (let (list tem)
    (store-match-data nil)
    (while (and (not (eq matches 0))
		(not (eq (match-end 0) (length string)))
		(string-match regexp string (match-end 0)))
      (and (integerp matches) (setq matches (1- matches)))
      (if (not (consp matchn))
	  (setq list (cons (substring string (match-beginning matchn)
				      (match-end matchn)) list))
	(setq tem matchn)
	(while tem
	  (if (match-beginning (car tem))
	      (setq list (cons (substring string
					  (match-beginning (car tem))
					  (match-end (car tem))) list)
		    tem nil)
	    (setq tem (cdr tem))))))
   (if (and (integerp matches) (match-end 0)
	    (not (eq (match-end 0) (length string))))
       (setq list (cons (substring string (match-end 0) (length string))
			 list)))
   (nreverse list)))

(defun vm-parse-addresses (string)
  "Given a STRING containing email addresses extracted from a header
field, parse it and return a list of individual email addresses."
  (if (null string)
      ()
    (let ((work-buffer (vm-make-multibyte-work-buffer)))
      (with-current-buffer work-buffer
        (unwind-protect
            (let (list start s char)
              (insert string)
              (goto-char (point-min))
	      ;; Remove useless white space  TX
              (while (re-search-forward "[\t\f\n\r]\\{1,\\}" nil t)
                (replace-match " "))
              (goto-char (point-min))
              (skip-chars-forward " \t\f\n\r")
              (setq start (point))
              (while (not (eobp))
                (skip-chars-forward "^\"\\\\,(")
                (setq char (following-char))
                (cond ((= char ?\\)
                       (forward-char 1)
                       (if (not (eobp))
                           (forward-char 1)))
                      ((= char ?,)
                       (setq s (buffer-substring start (point)))
                       (if (or (null (string-match "^[ \t\f\n\r]+$" s))
                               (not (string= s ""))) 
                           (setq list (cons s list)))
                       (skip-chars-forward ", \t\f\n\r")
                       (setq start (point)))
                      ((= char ?\")
                       (re-search-forward "[^\\\\]\"" nil 0))
                      ((= char ?\()
                       (let ((parens 1))
                         (forward-char 1)
                         (while (and (not (eobp)) (not (zerop parens)))
                           (re-search-forward "[()]" nil 0)
                           (cond ((or (eobp)
                                      (= (char-after (- (point) 2)) ?\\)))
                                 ((= (preceding-char) ?\()
                                  (setq parens (1+ parens)))
                                 (t
                                  (setq parens (1- parens)))))))))
              (setq s (buffer-substring start (point)))
              (if (and (null (string-match "^[ \t\f\n\r]+$" s))
                       (not (string= s "")))
                  (setq list (cons s list)))
              (mapcar 'vm-fix-quoted-address (reverse list)))
          (and work-buffer (kill-buffer work-buffer)))))))

(defun vm-fix-quoted-address (a)
  "Sometimes there are qp-encoded addresses not quoted by \" and thus we
need to add quotes or leave them undecoded.             RWF"
  (let ((da (vm-decode-mime-encoded-words-in-string a)))
    (if (string= da a)
        a
      (if (or (string-match "^\\s-*\\([^\"']*,[^\"']*\\)\\b\\s-*\\(<.*\\)" da)
              (string-match "^\\s-*\"'\\([^\"']+\\)'\"\\(.*\\)" da))
          (concat "\"" (match-string 1 da) "\" " (match-string 2 da))
        da))))

(make-obsolete 'vmrf-fix-quoted-address 'vm-quoted-address "8.2.0")
          
(defun vm-parse-structured-header (string &optional sepchar keep-quotes)
  (if (null string)
      ()
    (let ((work-buffer (vm-make-work-buffer)))
      (buffer-disable-undo work-buffer)
      (with-current-buffer work-buffer
       (unwind-protect
	   (let ((list nil)
		 (nonspecials "^\"\\\\( \t\n\r\f")
		 start s char sp+sepchar)
	     (if sepchar
		 (setq nonspecials (concat nonspecials (list sepchar))
		       sp+sepchar (concat "\t\f\n\r " (list sepchar))))
	     (insert string)
	     (goto-char (point-min))
	     (skip-chars-forward "\t\f\n\r ")
	     (setq start (point))
	     (while (not (eobp))
	       (skip-chars-forward nonspecials)
	       (setq char (following-char))
	       (cond ((looking-at "[ \t\n\r\f]")
		      (delete-char 1))
		     ((= char ?\\)
		      (forward-char 1)
		      (if (not (eobp))
			  (forward-char 1)))
		     ((and sepchar (= char sepchar))
		      (setq s (buffer-substring start (point)))
		      (if (or (null (string-match "^[\t\f\n\r ]+$" s))
			      (not (string= s "")))
			  (setq list (cons s list)))
		      (skip-chars-forward sp+sepchar)
		      (setq start (point)))
		     ((looking-at " \t\n\r\f")
		      (skip-chars-forward " \t\n\r\f"))
		     ((= char ?\")
		      (let ((done nil))
			(if keep-quotes
			    (forward-char 1)
			  (delete-char 1))
			(while (not done)
			  (if (null (re-search-forward "[\\\\\"]" nil t))
			      (setq done t)
			    (setq char (char-after (1- (point))))
			    (cond ((char-equal char ?\\)
				   (delete-char -1)
				   (if (eobp)
				       (setq done t)
				     (forward-char 1)))
				  (t (if (not keep-quotes)
					 (delete-char -1))
				     (setq done t)))))))
		     ((= char ?\()
		      (let ((done nil)
			    (pos (point))
			    (parens 1))
			(forward-char 1)
			(while (not done)
			  (if (null (re-search-forward "[\\\\()]" nil t))
			      (setq done t)
			    (setq char (char-after (1- (point))))
			    (cond ((char-equal char ?\\)
				   (if (eobp)
				       (setq done t)
				     (forward-char 1)))
				  ((char-equal char ?\()
				   (setq parens (1+ parens)))
				  (t
				   (setq parens (1- parens)
					 done (zerop parens))))))
			(delete-region pos (point))))))
	     (setq s (buffer-substring start (point)))
	     (if (and (null (string-match "^[\t\f\n\r ]+$" s))
		      (not (string= s "")))
		 (setq list (cons s list)))
	     (nreverse list))
	(and work-buffer (kill-buffer work-buffer)))))))

(defvar buffer-file-type)

(defun vm-write-string (where string)
  (if (bufferp where)
      (vm-save-buffer-excursion
	(set-buffer where)
	(goto-char (point-max))
	(let ((buffer-read-only nil))
	  (insert string)))
    (let ((temp-buffer (generate-new-buffer "*vm-work*")))
      (unwind-protect
	  (with-current-buffer temp-buffer
	    (setq selective-display nil)
	    (insert string)
	    ;; correct for VM's uses of this function---
	    ;; writing out message separators
	    (setq buffer-file-type nil)
	    (write-region (point-min) (point-max) where t 'quiet))
	(and temp-buffer (kill-buffer temp-buffer))))))

(defun vm-check-for-killed-summary ()
  "If the current folder's summary buffer has been killed, reset
the vm-summary-buffer variable and all the summary markers in the
folder so that it remains a valid folder.  Take care of
vm-folders-summary-buffer in a similar way."
  (and (bufferp vm-summary-buffer) (null (buffer-name vm-summary-buffer))
       (let ((mp vm-message-list))
	 (setq vm-summary-buffer nil)
	 (while mp
	   (vm-set-su-start-of (car mp) nil)
	   (vm-set-su-end-of (car mp) nil)
	   (setq mp (cdr mp)))))
  (and (bufferp vm-folders-summary-buffer)
       (null (buffer-name vm-folders-summary-buffer))
       (setq vm-folders-summary-buffer nil)))

(defun vm-check-for-killed-presentation ()
  "If the current folder's Presentation buffer has been killed, reset
the vm-presentation-buffer variable."
  (and (bufferp vm-presentation-buffer-handle)
       (null (buffer-name vm-presentation-buffer-handle))
       (progn
	 (setq vm-presentation-buffer-handle nil
	       vm-presentation-buffer nil))))

;;;###autoload
(defun vm-check-for-killed-folder ()
  "If the current buffer's Folder buffer has been killed, reset the
vm-mail-buffer variable."
  (and (bufferp vm-mail-buffer) (null (buffer-name vm-mail-buffer))
       (setq vm-mail-buffer nil)))

(put 'folder-read-only 'error-conditions '(folder-read-only error))
(put 'folder-read-only 'error-message "Folder is read-only")

(defun vm-abs (n) (if (< n 0) (- n) n))

(defun vm-last (list) 
  "Return the last cons-cell of LIST."
  (while (cdr-safe list) (setq list (cdr list)))
  list)

(defun vm-last-elem (list) 
  "Return the last element of LIST."
  (while (cdr-safe list) (setq list (cdr list)))
  (car list))

(defun vm-vector-to-list (vector)
  (let ((i (1- (length vector)))
	list)
    (while (>= i 0)
      (setq list (cons (aref vector i) list))
      (vm-decrement i))
    list ))

(defun vm-extend-vector (vector length &optional fill)
  (let ((vlength (length vector)))
    (if (< vlength length)
	(apply 'vector (nconc (vm-vector-to-list vector)
			      (make-list (- length vlength) fill)))
      vector )))

(defun vm-obarray-to-string-list (blobarray)
  (let ((list nil))
    (mapatoms (function (lambda (s) (setq list (cons (symbol-name s) list))))
	      blobarray)
    list ))

(defun vm-zip-vectors (v1 v2)
  (if (= (length v1) (length v2))
      (let ((l1 (append v1 nil))
	    (l2 (append v2 nil)))
	(vconcat (vm-zip-lists l1 l2)))
    (error "Attempt to zip vectors of differing length: %s and %s" 
	   (length v1) (length v2))))

(defun vm-zip-lists (l1 l2)
  (cond ((or (null l1) (null l2))
	 (if (and (null l1) (null l2))
	     nil 
	   (error "Attempt to zip lists of differing length")))
	(t
	 (cons (car l1) (cons (car l2) (vm-zip-lists (cdr l1) (cdr l2)))))
	))

(defun vm-mapvector (proc vec)
  (let ((new-vec (make-vector (length vec) nil))
	(i 0)
	(n (length vec)))
    (while (< i n)
      (aset new-vec i (apply proc (aref vec i) nil))
      (setq i (1+ i)))
    new-vec))

(defun vm-mapcar (function &rest lists)
  "Apply function to all the curresponding elements of the remaining
argument lists.  The results are gathered into a list and returned.  

All the argument lists should be of the same length for this to be
well-behaved." 
  (let (arglist result)
    (while (car lists)
      (setq arglist (mapcar 'car lists))
      (setq result (cons (apply function arglist) result))
      (setq lists (mapcar 'cdr lists)))
    (nreverse result)))

(defun vm-mapc (proc &rest lists)
  "Apply PROC to all the corresponding elements of the remaining
argument lists.  Discard any results.

All the argument lists should be of the same length for this to be
well-behaved." 
  (let (arglist)
    (while (car lists)
      (setq arglist (mapcar 'car lists))
      (apply proc arglist)
      (setq lists (mapcar 'cdr lists)))))

(defun vm-delete (predicate list &optional retain)
  "Delete all elements satisfying PREDICATE from LIST and return
the resulting list.  If optional argument RETAIN is t, then
retain all elements that satisfy PREDICATE rather than deleting
them.  The original LIST is permanently modified."
  (let ((p list) 
	(retain (if retain 'not 'identity))
	prev)
    (while p
      (if (funcall retain (funcall predicate (car p)))
	  (if (null prev)
	      (setq list (cdr list) p list)
	    (setcdr prev (cdr p))
	    (setq p (cdr p)))
	(setq prev p p (cdr p))))
    list ))

(defun vm-elems (n list)
  "Select the first N elements of LIST and return them as a list."
  (let (res)
    (while (and list (> n 0))
      (setq res (cons (car list) res))
      (setq list (cdr list))
      (setq n (1- n)))
    (nreverse res)))

(defun vm-find (list pred)
  "Find the first element of LIST satisfying PRED and return the position"
  (let ((n 0))
    (while (and list (not (apply pred (car list) nil)))
      (setq list (cdr list))
      (setq n (1+ n)))
    (if list n nil)))

(defun vm-find-all (list pred)
  "Find all the elements of LIST satisfying PRED"
  (let ((n 0) (res nil))
    (while list 
      (when (apply pred (car list) nil)
	(setq res (cons (car list) res)))
      (setq list (cdr list))
      (setq n (1+ n)))
    (nreverse res)))

(defun vm-find2 (list1 list2 pred)
  "Find the first pair of elements of LIST1 and LIST2 satisfying
PRED and return the position"
  (let ((n 0))
    (while (and list1 list2 (not (apply pred (car list1) (car list2) nil)))
      (setq list1 (cdr list2)
	    list2 (cdr list2))
      (setq n (1+ n)))
    (if (and list1 list2) n nil)))

(defun vm-elems-of (list)
  "Return the set of elements of LIST as a list."
  (let ((res nil))
    (while list
      (unless (member (car list) res)
	(setq res (cons (car list) res)))
      (setq list (cdr list)))
    (nreverse res)))

(defun vm-for-all (list pred)
  (catch 'fail
    (progn
      (while list
	(if (apply pred (car list) nil)
	    (setq list (cdr list))
	  (throw 'fail nil)))
      t)))

(fset 'vm-device-type
      (cond (vm-xemacs-p 'device-type)
	    (vm-fsfemacs-p 'vm-fsfemacs-device-type)))

(defun vm-fsfemacs-device-type (&optional device)
  "An FSF Emacs emulation for XEmacs `device-type' function.  Returns
the type of the current screen device: one of 'x, 'gtk, 'w32, 'ns and
'pc.  The optional argument DEVICE is ignored."
  (if (eq window-system 'x)
      (if (featurep 'gtk) 'gtk)
    window-system))

(defun vm-generate-new-unibyte-buffer (name)
  (if vm-xemacs-p
      (generate-new-buffer name)
    (let* (;; (default-enable-multibyte-characters nil)
	   ;; don't need this because of set-buffer-multibyte below
	   (buffer (generate-new-buffer name)))
      (when (fboundp 'set-buffer-multibyte)
	(with-current-buffer buffer
	  (set-buffer-multibyte nil)))
      buffer)))

(defun vm-generate-new-multibyte-buffer (name)
  (if vm-xemacs-p
      (generate-new-buffer name)
    (let* (;; (default-enable-multibyte-characters t)
	   ;; don't need this because of set-buffer-multibyte below
	   (buffer (generate-new-buffer name)))
      (if (fboundp 'set-buffer-multibyte)
	  (with-current-buffer buffer
	    (set-buffer-multibyte t))
	;; This error checking only works on FSF
	(with-current-buffer buffer 
	  (unless enable-multibyte-characters
	    (error "VM internal error #1922: buffer is not multibyte"))))
      buffer)))

(defun vm-make-local-hook (hook)
  (if (fboundp 'make-local-hook)	; Emacs/XEmacs 21
      (make-local-hook hook)))

(fset 'xemacs-abbreviate-file-name 'abbreviate-file-name)

(defun vm-abbreviate-file-name (path)
  (if vm-xemacs-p
      (xemacs-abbreviate-file-name path t)
    (abbreviate-file-name path)))

(fset 'emacs-find-file-name-handler 'find-file-name-handler)
(defun vm-find-file-name-handler (filename operation)
  (if (fboundp 'find-file-name-handler)
      (condition-case ()
	  (emacs-find-file-name-handler filename operation)
	(wrong-number-of-arguments
	 (emacs-find-file-name-handler filename)))
    nil))

(fset 'emacs-focus-frame 'focus-frame)
(defun vm-select-frame-set-input-focus (frame)
  (if (fboundp 'select-frame-set-input-focus)
      ;; defined in FSF Emacs 22.1
      (select-frame-set-input-focus frame)
    (select-frame frame)
    (emacs-focus-frame frame)
    (raise-frame frame)))

(fset 'emacs-get-buffer-window 'get-buffer-window)
(defun vm-get-buffer-window (buffer &optional which-frames which-devices)
  (condition-case nil			; try XEmacs
      (or (emacs-get-buffer-window buffer which-frames which-devices)
	  (and vm-search-other-frames
	       (emacs-get-buffer-window buffer t t)))
    (wrong-number-of-arguments
     (condition-case nil		; try recent Gnu Emacs
	 (or (emacs-get-buffer-window buffer which-frames)
	     (and vm-search-other-frames
		  (emacs-get-buffer-window buffer t)))
       (wrong-number-of-arguments	; baseline old Emacs
	(emacs-get-buffer-window buffer))))))

(defun vm-get-visible-buffer-window (buffer &optional 
					    which-frames which-devices)
  (condition-case nil
      (or (emacs-get-buffer-window buffer which-frames which-devices)
	  (and vm-search-other-frames
	       (emacs-get-buffer-window buffer t which-devices)))
    (wrong-number-of-arguments
     (condition-case nil
	 (or (emacs-get-buffer-window buffer which-frames)
	     (and vm-search-other-frames
		  (get-buffer-window buffer 'visible)))
       (wrong-number-of-arguments
	(emacs-get-buffer-window buffer))))))

(defun vm-force-mode-line-update ()
  "Force a mode line update in all frames."
  (if (fboundp 'force-mode-line-update)
      (force-mode-line-update t)
    (with-current-buffer (other-buffer)
      (set-buffer-modified-p (buffer-modified-p)))))

(defun vm-delete-directory-file-names (list)
  (vm-delete 'file-directory-p list))

(defun vm-delete-backup-file-names (list)
  (vm-delete 'backup-file-name-p list))

(defun vm-delete-auto-save-file-names (list)
  (vm-delete 'auto-save-file-name-p list))

(defun vm-delete-index-file-names (list)
  (vm-delete 'vm-index-file-name-p list))

(defun vm-delete-directory-names (list)
  (vm-delete 'file-directory-p list))

(defun vm-index-file-name-p (file)
  (and (file-regular-p file)
       (stringp vm-index-file-suffix)
       (let ((str (concat (regexp-quote vm-index-file-suffix) "$")))
	 (string-match str file))
       t ))

(defun vm-delete-duplicates (list &optional all hack-addresses)
  "Delete duplicate equivalent strings from the list.
If ALL is t, then if there is more than one occurrence of a string in the list,
 then all occurrences of it are removed instead of just the subsequent ones.
If HACK-ADDRESSES is t, then the strings are considered to be mail addresses,
 and only the address part is compared (so that \"Name <foo>\" and \"foo\"
 would be considered to be equivalent.)"
  (let ((hashtable vm-delete-duplicates-obarray)
	(new-list nil)
	sym-string sym)
    (fillarray hashtable 0)
    (while list
      (setq sym-string
	    (if hack-addresses
		(nth 1 (funcall vm-chop-full-name-function (car list)))
	      (car list))
	    sym-string (or sym-string "-unparseable-garbage-")
	    sym (intern (if hack-addresses (downcase sym-string) sym-string)
			hashtable))
      (if (boundp sym)
	  (and all (setcar (symbol-value sym) nil))
	(setq new-list (cons (car list) new-list))
	(set sym new-list))
      (setq list (cdr list)))
    (delq nil (nreverse new-list))))

(defun vm-member-0 (thing list)
  (catch 'done
    (while list
      (and (equal (car list) thing)
	   (throw 'done list))
      (setq list (cdr list)))
    nil ))

(fset 'vm-member (symbol-function (if (fboundp 'member) 'member 'vm-member-0)))

(defun vm-delqual (ob list)
  (let ((prev nil)
	(curr list))
    (while curr
      (if (not (equal ob (car curr)))
	  (setq prev curr
		curr (cdr curr))
	(if (null prev)
	    (setq list (cdr list)
		  curr list)
	  (setq curr (cdr curr))
	  (setcdr prev curr))))
    list ))

(defun vm-copy-local-variables (buffer &rest variables)
  (let ((values (mapcar 'symbol-value variables)))
    (with-current-buffer buffer
      (vm-mapc 'set variables values))))

(put 'folder-empty 'error-conditions '(folder-empty error))
(put 'folder-empty 'error-message "Folder is empty")
(put 'unrecognized-folder-type 'error-conditions
     '(unrecognized-folder-type error))
(put 'unrecognized-folder-type 'error-message "Unrecognized folder type")

(defun vm-error-if-folder-empty ()
  (while (null vm-message-list)
    (if vm-folder-type
	(signal 'unrecognized-folder-type nil)
      (signal 'folder-empty nil))))

(defun vm-copy (object)
  "Make a copy of OBJECT, which could be a list, vector, string or marker."
  (cond ((consp object)
	 (let (return-value cons)
	   (setq return-value (cons (vm-copy (car object)) nil)
		 cons return-value
		 object (cdr object))
	   (while (consp object)
	     (setcdr cons (cons (vm-copy (car object)) nil))
	     (setq cons (cdr cons)
		   object (cdr object)))
	   (setcdr cons object)
	   return-value ))
	((vectorp object) (apply 'vector (mapcar 'vm-copy object)))
	((stringp object) (copy-sequence object))
	((markerp object) (copy-marker object))
	(t object)))

(defun vm-run-message-hook (message hook-variable)
  (with-current-buffer (vm-buffer-of message)
    (vm-save-restriction
      (widen)
      (save-excursion
	(narrow-to-region (vm-headers-of message) (vm-text-end-of message))
	(run-hooks hook-variable)))))

(defun vm-run-message-hook-with-args (message hook-variable &rest args)
  (with-current-buffer (vm-buffer-of message)
    (vm-save-restriction
      (widen)
      (save-excursion
	(narrow-to-region (vm-headers-of message) (vm-text-end-of message))
	(apply 'run-hook-with-args hook-variable args)))))

(defun vm-error-free-call (function &rest args)
  (condition-case nil
      (apply function args)
    (error nil)))

(put 'beginning-of-folder 'error-conditions '(beginning-of-folder error))
(put 'beginning-of-folder 'error-message "Beginning of folder")
(put 'end-of-folder 'error-conditions '(end-of-folder error))
(put 'end-of-folder 'error-message "End of folder")

(defun vm-trace (&rest args)
  (with-current-buffer (get-buffer-create "*vm-trace*")
    (apply 'insert args)))

(defun vm-timezone-make-date-sortable (string)
  (or (cdr (assq string vm-sortable-date-alist))
      (let ((vect (vm-parse-date string))
	    (date (vm-parse (current-time-string) " *\\([^ ]+\\)")))
	;; if specified date is incomplete fill in the holes
	;; with useful information, defaulting to the current
	;; date and timezone for everything except hh:mm:ss which
	;; defaults to midnight.
	(if (equal (aref vect 1) "")
	    (aset vect 1 (nth 2 date)))
	(if (equal (aref vect 2) "")
	    (aset vect 2 (nth 1 date)))
	(if (equal (aref vect 3) "")
	    (aset vect 3 (nth 4 date)))
	(if (equal (aref vect 4) "")
	    (aset vect 4 "00:00:00"))
	(if (equal (aref vect 5) "")
	    (aset vect 5 (vm-current-time-zone)))
	;; save this work so we won't have to do it again
	(setq vm-sortable-date-alist
	      (cons (cons string
			  (condition-case nil
			      (timezone-make-date-sortable
			       (format "%s %s %s %s %s"
				       (aref vect 1)
				       (aref vect 2)
				       (aref vect 3)
				       (aref vect 4)
				       (aref vect 5)))
			    (error "1970010100:00:00")))
		    vm-sortable-date-alist))
	;; return result
	(cdr (car vm-sortable-date-alist)))))

(defun vm-current-time-zone ()
  (or (condition-case nil
	  (let* ((zone (car (current-time-zone)))
		 (absmin (/ (vm-abs zone) 60)))
	    (format "%c%02d%02d" (if (< zone 0) ?- ?+)
		    (/ absmin 60) (% absmin 60)))
	(error nil))
      (let ((temp-buffer (vm-make-work-buffer)))
	(condition-case nil
	    (unwind-protect
		(with-current-buffer temp-buffer
		  (call-process "date" nil temp-buffer nil)
		  (nth 4 (vm-parse (vm-buffer-string-no-properties)
				   " *\\([^ ]+\\)")))
	      (and temp-buffer (kill-buffer temp-buffer)))
	  (error nil)))
      ""))

(defun vm-parse-date (date)
  (let ((weekday "")
	(monthday "")
	(month "")
	(year "")
	(hour "")
	(timezone "")
	(start nil)
	string
	(case-fold-search t))
    (if (string-match "sun\\|mon\\|tue\\|wed\\|thu\\|fri\\|sat" date)
	(setq weekday (substring date (match-beginning 0) (match-end 0))))
    (if (string-match "jan\\|feb\\|mar\\|apr\\|may\\|jun\\|jul\\|aug\\|sep\\|oct\\|nov\\|dec" date)
	(setq month (substring date (match-beginning 0) (match-end 0))))
    (if (string-match "[0-9]?[0-9]:[0-9][0-9]\\(:[0-9][0-9]\\)?" date)
	(setq hour (substring date (match-beginning 0) (match-end 0))))
    (cond ((string-match "[^a-z][+---][0-9][0-9][0-9][0-9]" date)
	   (setq timezone (substring date (1+ (match-beginning 0))
				     (match-end 0))))
	  ((or (string-match "e[ds]t\\|c[ds]t\\|p[ds]t\\|m[ds]t" date)
	       (string-match "ast\\|nst\\|met\\|eet\\|jst\\|bst\\|ut" date)
	       (string-match "gmt\\([+---][0-9]+\\)?" date))
	   (setq timezone (substring date (match-beginning 0) (match-end 0)))))
    (while (and (or (zerop (length monthday))
		    (zerop (length year)))
		(string-match "\\(^\\| \\)\\([0-9]+\\)\\($\\| \\)" date start))
      (setq string (substring date (match-beginning 2) (match-end 2))
	    start (match-end 0))
      (cond ((and (zerop (length monthday))
		  (<= (length string) 2))
	     (setq monthday string))
	    ((= (length string) 2)
	     (if (< (string-to-number string) 70)
		 (setq year (concat "20" string))
	       (setq year (concat "19" string))))
	    (t (setq year string))))
    
    (aset vm-parse-date-workspace 0 weekday)
    (aset vm-parse-date-workspace 1 monthday)
    (aset vm-parse-date-workspace 2 month)
    (aset vm-parse-date-workspace 3 year)
    (aset vm-parse-date-workspace 4 hour)
    (aset vm-parse-date-workspace 5 timezone)
    vm-parse-date-workspace))

(defun vm-should-generate-summary ()
  (cond ((eq vm-startup-with-summary t) t)
	((integerp vm-startup-with-summary)
	 (let ((n vm-startup-with-summary))
	   (cond ((< n 0) (null (nth (vm-abs n) vm-message-list)))
		 ((= n 0) nil)
		 (t (nth (1- n) vm-message-list)))))
	(vm-startup-with-summary t)
	(t nil)))

(defun vm-find-composition-buffer (&optional not-picky)
  (let ((b-list (buffer-list)) choice alternate)
    (save-excursion
     (while b-list
       (set-buffer (car b-list))
       (if (eq major-mode 'mail-mode)
	   (if (buffer-modified-p)
	       (setq choice (current-buffer)
		     b-list nil)
	     (and not-picky (null alternate)
		  (setq alternate (current-buffer)))
	     (setq b-list (cdr b-list)))
	 (setq b-list (cdr b-list))))
    (or choice alternate))))

(defun vm-get-file-buffer (file)
  "Like get-file-buffer, but also checks buffers against FILE's truename"
  (or (get-file-buffer file)
      (and (fboundp 'file-truename)
	   (get-file-buffer (file-truename file)))
      (and (fboundp 'find-buffer-visiting)
	   (find-buffer-visiting file))))

;; The following function is not working correctly on Gnu Emacs 23.
;; So we do it ourselves.
(defun vm-delete-auto-save-file-if-necessary ()
  (if vm-xemacs-p
      (delete-auto-save-file-if-necessary)
    (when (and buffer-auto-save-file-name delete-auto-save-files
	       (not (string= buffer-file-name buffer-auto-save-file-name))
	       (file-newer-than-file-p 
		buffer-auto-save-file-name buffer-file-name))
      (condition-case ()
	  (if (save-window-excursion
		(with-output-to-temp-buffer "*Directory*"
		  (buffer-disable-undo standard-output)
		  (save-excursion
		    (let ((switches dired-listing-switches)
			  (file buffer-file-name)
			  (save-file buffer-auto-save-file-name))
		      (if (file-symlink-p buffer-file-name)
			  (setq switches (concat switches "L")))
		      (set-buffer standard-output)
		      ;; Use insert-directory-safely, not insert-directory,
		      ;; because these files might not exist.  In particular,
		      ;; FILE might not exist if the auto-save file was for
		      ;; a buffer that didn't visit a file, such as "*mail*".
		      ;; The code in v20.x called `ls' directly, so we need
		      ;; to emulate what `ls' did in that case.
		      (insert-directory-safely save-file switches)
		      (insert-directory-safely file switches))))
		(yes-or-no-p 
		 (format "Delete auto save file %s? " 
			 buffer-auto-save-file-name)))
	      (delete-file buffer-auto-save-file-name))
	(file-error nil))
      (set-buffer-auto-saved))))

(defun vm-set-region-face (start end face)
  (let ((e (vm-make-extent start end)))
    (vm-set-extent-property e 'face face)))

(fset 'vm-xemacs-set-face-foreground (function set-face-foreground))
(fset 'vm-fsfemacs-set-face-foreground (function set-face-foreground))
(fset 'vm-xemacs-set-face-background (function set-face-background))
(fset 'vm-fsfemacs-set-face-background (function set-face-background))


(defun vm-default-buffer-substring-no-properties (beg end &optional buffer)
  (let ((s (if buffer
	       (with-current-buffer buffer
		 (buffer-substring beg end))
	     (buffer-substring beg end))))
    (set-text-properties 0 (length s) nil s)
    (copy-sequence s)))

(fset 'vm-buffer-substring-no-properties
  (cond ((fboundp 'buffer-substring-no-properties)
	 (function buffer-substring-no-properties))
	(vm-xemacs-p
	 (function buffer-substring))
	(t (function vm-default-buffer-substring-no-properties))))

(defun vm-buffer-string-no-properties ()
  (vm-buffer-substring-no-properties (point-min) (point-max)))

(fset 'vm-substring-no-properties
      (cond ((fboundp 'substring-no-properties)
	     (function substring-no-properties))
	    (t (function substring))))

(defun vm-insert-region-from-buffer (buffer &optional start end)
  (let ((target-buffer (current-buffer)))
    (set-buffer buffer)
    (save-restriction
      (widen)
      (or start (setq start (point-min)))
      (or end (setq end (point-max)))
      (set-buffer target-buffer)
      (insert-buffer-substring buffer start end)
      (set-buffer buffer))
    (set-buffer target-buffer)))

(if (not (fboundp 'vm-extent-property))
    (if vm-fsfemacs-p
	(fset 'vm-extent-property 'overlay-get)
      (fset 'vm-extent-property 'extent-property)))

(if (not (fboundp 'vm-extent-object))
    (if vm-fsfemacs-p
	(fset 'vm-extent-object 'overlay-buffer)
      (fset 'vm-extent-object 'extent-object)))

(if (not (fboundp 'vm-set-extent-property))
    (if vm-fsfemacs-p
	(fset 'vm-set-extent-property 'overlay-put)
      (fset 'vm-set-extent-property 'set-extent-property)))

(if (not (fboundp 'vm-set-extent-endpoints))
    (if vm-fsfemacs-p
	(fset 'vm-set-extent-endpoints 'move-overlay)
      (fset 'vm-set-extent-endpoints 'set-extent-endpoints)))

(if (not (fboundp 'vm-make-extent))
    (if vm-fsfemacs-p
	(fset 'vm-make-extent 'make-overlay)
      (fset 'vm-make-extent 'make-extent)))

(if (not (fboundp 'vm-extent-end-position))
    (if vm-fsfemacs-p
	(fset 'vm-extent-end-position 'overlay-end)
      (fset 'vm-extent-end-position 'extent-end-position)))

(if (not (fboundp 'vm-extent-start-position))
    (if vm-fsfemacs-p
	(fset 'vm-extent-start-position 'overlay-start)
      (fset 'vm-extent-start-position 'extent-start-position)))

(if (not (fboundp 'vm-detach-extent))
    (if vm-fsfemacs-p
	(fset 'vm-detach-extent 'delete-overlay)
      (fset 'vm-detach-extent 'detach-extent)))

(if (not (fboundp 'vm-delete-extent))
    (if vm-fsfemacs-p
	;; This doesn't actually destroy the overlay, but it is the
	;; best there is.
	(fset 'vm-delete-extent 'delete-overlay)
      (fset 'vm-delete-extent 'delete-extent)))

(if (not (fboundp 'vm-disable-extents))
    (if (and vm-fsfemacs-p (fboundp 'remove-overlays))
	(fset 'vm-disable-extents 'remove-overlays)
      ;; XEamcs doesn't need to disable extents because they don't
      ;; slow things down
      (fset 'vm-disable-extents (lambda (&optional beg end name val) nil))))

(if (not (fboundp 'vm-extent-properties))
    (if vm-fsfemacs-p
	(fset 'vm-extent-properties 'overlay-properties)
      (fset 'vm-extent-properties 'extent-properties)))

(defun vm-extent-at (pos &optional property)
  "Find an extent at POS in the current buffer having PROPERTY.
PROPERTY defaults nil, meaning any extent will do.

In XEmacs, the extent is the \"smallest\" extent at POS.  In FSF Emacs,
this may not be the case."
  (if (fboundp 'extent-at)
      (extent-at pos nil property)
    (let ((o-list (overlays-at pos))
	  (o nil))
      (if (null property)
	  (car o-list)
	(while o-list
	  (if (overlay-get (car o-list) property)
	      (setq o (car o-list)
		    o-list nil)
	    (setq o-list (cdr o-list))))
	o ))))

(defun vm-extent-list (beg end &optional property)
  "Returns a list of the extents that overlap the positions BEG to END.
If PROPERTY is given, then only the extents have PROPERTY are returned."
  (if (fboundp 'extent-list)
      (extent-list nil beg end nil property)
    (let ((o-list (overlays-in beg end)))
      (if property
	  (vm-delete (function (lambda (e)
				 (vm-extent-property e property)))
		     o-list t)
	o-list))))

(defun vm-copy-extent (e)
  (let ((props (vm-extent-properties e))
	(ee (vm-make-extent (vm-extent-start-position e)
			    (vm-extent-end-position e))))
    (while props
      (vm-set-extent-property ee (car props) (car (cdr props)))
      (setq props (cdr (cdr props))))))

(defun vm-make-tempfile (&optional filename-suffix proposed-filename)
  (let ((modes (default-file-modes))
	(file (vm-make-tempfile-name filename-suffix proposed-filename)))
    (unwind-protect
	(progn
	  (set-default-file-modes (vm-octal 600))
	  (vm-error-free-call 'delete-file file)
	  (write-region (point) (point) file nil 0))
      (set-default-file-modes modes))
    file ))

(defun vm-make-tempfile-name (&optional filename-suffix proposed-filename)
  (if (stringp proposed-filename)
      (setq proposed-filename (file-name-nondirectory proposed-filename)))
  (let (filename)
    (cond ((and (stringp proposed-filename)
		(not (file-exists-p
		      (setq filename (convert-standard-filename
				      (expand-file-name
				       proposed-filename
				       vm-temp-file-directory))))))
	   t )
	  ((stringp proposed-filename)
	   (let ((done nil))
	     (while (not done)
	       (setq filename (convert-standard-filename
			       (expand-file-name
				(format "%d-%s"
					vm-tempfile-counter
					proposed-filename)
				vm-temp-file-directory))
		     vm-tempfile-counter (1+ vm-tempfile-counter)
                     done (not (file-exists-p filename))))))
	  (t
	   (let ((done nil))
	     (while (not done)
	       (setq filename (convert-standard-filename
			       (expand-file-name
				(format "vm%d%d%s"
					vm-tempfile-counter
					(random 100000000)
					(or filename-suffix ""))
				vm-temp-file-directory))
		     vm-tempfile-counter (1+ vm-tempfile-counter)
		     done (not (file-exists-p filename)))))))
    filename ))

(defun vm-make-work-buffer (&optional name)
  "Create a unibyte buffer with NAME for VM to do its work in
encoding/decoding, conversions, subprocess communication etc."
  (let ((work-buffer (vm-generate-new-unibyte-buffer 
		      (or name "*vm-workbuf*"))))
    (buffer-disable-undo work-buffer)
;; probably not worth doing since no one sets buffer-offer-save
;; non-nil globally, do they?
;;    (with-current-buffer work-buffer
;;      (setq buffer-offer-save nil))
    work-buffer ))

(defun vm-make-multibyte-work-buffer (&optional name)
  (let ((work-buffer (vm-generate-new-multibyte-buffer 
		      (or name "*vm-workbuf*"))))
    (buffer-disable-undo work-buffer)
;; probably not worth doing since no one sets buffer-offer-save
;; non-nil globally, do they?
;;    (with-current-buffer work-buffer
;;      (setq buffer-offer-save nil))
    work-buffer ))

(fset 'xemacs-insert-char 'insert-char)
(defun vm-insert-char (char &optional count ignored buffer)
  (condition-case nil
      (progn
	(xemacs-insert-char char count ignored buffer)
	(fset 'vm-insert-char 'insert-char))
    (wrong-number-of-arguments
     (fset 'vm-insert-char 'vm-xemacs-compatible-insert-char)
     (vm-insert-char char count ignored buffer))))

(defun vm-xemacs-compatible-insert-char (char &optional count ignored buffer)
  (if (and buffer (eq buffer (current-buffer)))
      (insert-char char count)
    (with-current-buffer buffer
      (insert-char char count))))

(defun vm-symbol-lists-intersect-p (list1 list2)
  (catch 'done
    (while list1
      (and (memq (car list1) list2)
	   (throw 'done t))
      (setq list1 (cdr list1)))
    nil ))

(defun vm-folder-buffer-value (var)
  (if vm-mail-buffer
      (with-current-buffer 
	  vm-mail-buffer
	(symbol-value var))
    (symbol-value var)))

(defsubst vm-with-string-as-temp-buffer (string function)
  (let ((work-buffer (vm-make-multibyte-work-buffer)))
    (unwind-protect
	(with-current-buffer work-buffer
	  (insert string)
	  (funcall function)
	  (buffer-string))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-string-assoc (elt list)
  (let ((case-fold-search t)
	(found nil)
	(elt (regexp-quote elt)))
    (while (and list (not found))
      (if (and (equal 0 (string-match elt (car (car list))))
	       (= (match-end 0) (length (car (car list)))))
	  (setq found t)
	(setq list (cdr list))))
    (car list)))

(defun vm-nonneg-string (n)
  (if (< n 0)
      "?"
    (int-to-string n)))

(defun vm-string-member (elt list)
  (let ((case-fold-search t)
	(found nil)
	(elt (regexp-quote elt)))
    (while (and list (not found))
      (if (and (equal 0 (string-match elt (car list)))
	       (= (match-end 0) (length (car list))))
	  (setq found t)
	(setq list (cdr list))))
    list))

(defun vm-string-equal-ignore-case (str1 str2)
  (let ((case-fold-search t)
	(reg (regexp-quote str1)))
    (and (equal 0 (string-match reg str2))
	 (= (match-end 0) (length str2)))))

(defun vm-match-data ()
  (let ((n (1- (/ (length (match-data)) 2)))
        (list nil))
    (while (>= n 0)
      (setq list (cons (match-beginning n) 
                       (cons (match-end n) list))
            n (1- n)))
    list))

(defun vm-time-difference (t1 t2)
  (let (usecs secs 65536-secs carry)
    (setq usecs (- (nth 2 t1) (nth 2 t2)))
    (if (< usecs 0)
	(setq carry 1
	      usecs (+ usecs 1000000))
      (setq carry 0))
    (setq secs (- (nth 1 t1) (nth 1 t2) carry))
    (if (< secs 0)
	 (setq carry 1
	       secs (+ secs 65536))
      (setq carry 0))
    (setq 65536-secs (- (nth 0 t1) (nth 0 t2) carry))
    (+ (* 65536-secs 65536)
       secs
       (/ usecs (if (featurep 'lisp-float-type) 1e6 1000000)))))

(if (fboundp 'char-to-int)
    (fset 'vm-char-to-int 'char-to-int)
  (fset 'vm-char-to-int 'identity))

(cond ((fboundp 'charsets-in-region)
       (fset 'vm-charsets-in-region 'charsets-in-region))
      ((fboundp 'find-charset-region)
       (fset 'vm-charsets-in-region 'find-charset-region)))

;; Wrapper for coding-system-p:
;; The XEmacs function expects a coding-system object as its argument,
;; the GNU Emacs function expects a symbol.
;; In the non-MULE case, return nil (is this the right fallback?).
(defun vm-coding-system-p (name)
  (cond (vm-xemacs-mule-p
	 (coding-system-p (find-coding-system name)))
	(vm-fsfemacs-mule-p
	 (coding-system-p name))))

(cond ((fboundp 'coding-system-name)
       (fset 'vm-coding-system-name 'coding-system-name))
      (t
       (fset 'vm-coding-system-name 'identity)))

(if (fboundp 'coding-system-name)
    (defun vm-coding-system-name-no-eol (coding-system)
      (coding-system-name
       (coding-system-change-eol-conversion coding-system nil)))
  (defun vm-coding-system-name-no-eol (coding-system)
    (coding-system-change-eol-conversion coding-system nil)))

(defun vm-get-file-line-ending-coding-system (file)
  (if (not (or vm-fsfemacs-mule-p vm-xemacs-mule-p vm-xemacs-file-coding-p))
      nil
    (let ((coding-system-for-read  (vm-binary-coding-system))
	  (work-buffer (vm-make-work-buffer)))
      (unwind-protect
	  (with-current-buffer work-buffer
	    (condition-case nil
		(insert-file-contents file nil 0 4096)
	      (error nil))
	    (goto-char (point-min))
	    (cond ((re-search-forward "[^\r]\n" nil t)
		   (if vm-fsfemacs-mule-p 'raw-text-unix 'no-conversion-unix))
		  ((re-search-forward "\r[^\n]" nil t)
		   (if vm-fsfemacs-mule-p 'raw-text-mac 'no-conversion-mac))
		  ((search-forward "\r\n" nil t)
		   (if vm-fsfemacs-mule-p 'raw-text-dos 'no-conversion-dos))
		  (t (vm-line-ending-coding-system))))
	(and work-buffer (kill-buffer work-buffer))))))

(defun vm-new-folder-line-ending-coding-system ()
  (cond ((eq vm-default-new-folder-line-ending-type nil)
	 (vm-line-ending-coding-system))
	((eq vm-default-new-folder-line-ending-type 'lf)
	 (if vm-fsfemacs-mule-p 'raw-text-unix 'no-conversion-unix))
	((eq vm-default-new-folder-line-ending-type 'crlf)
	 (if vm-fsfemacs-mule-p 'raw-text-dos 'no-conversion-dos))
	((eq vm-default-new-folder-line-ending-type 'cr)
	 (if vm-fsfemacs-mule-p 'raw-text-mac 'no-conversion-mac))
	(t
	 (vm-line-ending-coding-system))))

(defun vm-collapse-whitespace ()
  (goto-char (point-min))
  (while (re-search-forward "[ \t\n]+" nil 0)
    (replace-match " " t t)))

(defvar vm-paragraph-prefix-regexp "^[ >]*"
  "A regexp used by `vm-forward-paragraph' to match paragraph prefixes.")

(defvar vm-empty-line-regexp "^[ \t>]*$"
  "A regexp used by `vm-forward-paragraph' to match paragraph prefixes.")

(defun vm-skip-empty-lines ()
  "Move forward as long as current line matches `vm-empty-line-regexp'."
  (while (and (not (eobp)) 
	      (looking-at vm-empty-line-regexp))
    (forward-line 1)))

(defun vm-forward-paragraph ()
  "Move forward to end of paragraph and do it also right for quoted text.
As a side-effect set `fill-prefix' to the paragraphs prefix.
Returns t if there was a line longer than `fill-column'."
  (let ((long-line)
	(line-no 1)
	len-fill-prefix)
    (forward-line 0)			; cover for bad fill-region fns
    (setq fill-prefix nil)
    (while (and 
	    ;; stop at end of buffer
	    (not (eobp)) 
	    ;; empty lines break paragraphs
	    (not (looking-at "^[ \t]*$"))
	    ;; do we see a prefix
	    (looking-at vm-paragraph-prefix-regexp)
	    (let ((m (match-string 0))
		  lenm)
	      (or (and (null fill-prefix)
		       ;; save prefix for next line
		       (setq fill-prefix m len-fill-prefix (length m)))
		  ;; is it still the same prefix?
		  (string= fill-prefix m)
		  ;; or is it just shorter by whitespace on the second line
		  (and 
		   (= line-no 2)
		   (< (setq lenm (length m)) len-fill-prefix)
		   (string-match "^[ \t]+$" (substring fill-prefix lenm))
		   ;; then save new shorter prefix
		   (setq fill-prefix m len-fill-prefix lenm)))))
      (end-of-line)
      (setq line-no (1+ line-no))
      (setq long-line (or long-line (> (current-column) fill-column)))
      (forward-line 1))
    long-line))

(defun vm-fill-paragraphs-containing-long-lines (width start end)
  "Fill paragraphs spanning more than WIDTH columns in region
START to END.  If WIDTH is 'window-width, the current width of
the Emacs window is used.  If vm-word-wrap-paragraphs is set
non-nil, then the longlines package is used to word-wrap long
lines without removing any existing line breaks.

In order to fill also quoted text you will need `filladapt.el' as the adaptive
filling of GNU Emacs does not work correctly here."
  (if (and vm-word-wrap-paragraphs (locate-library "longlines"))
      (vm-fill-paragraphs-by-longlines start end)
    (if (eq width 'window-width)
	(setq width (- (window-width (get-buffer-window (current-buffer))) 1)))
    (save-excursion
      (let ((buffer-read-only nil)
	    (fill-column vm-paragraph-fill-column)
	    (adaptive-fill-mode nil)
	    (abbrev-mode nil)
	    (fill-prefix nil)
	    ;; (use-hard-newlines t)
	    (filled 0)
	    (message (if (car vm-message-pointer)
			 (vm-su-subject (car vm-message-pointer))
		       (buffer-name)))
	    (needmsg (> (- end start) 12000)))
      
	(if needmsg
	    (vm-inform 5 "Filling message to column %d" fill-column))
      
	;; we need a marker for the end since this position might change 
	(or (markerp end) (setq end (vm-marker end)))
	(goto-char start)
      
	(while (< (point) end)
	  (setq start (point))
	  (vm-skip-empty-lines)
	  (when (and (< (point) end)	; if no newline at the end
		     (let ((fill-column width)) (vm-forward-paragraph)))
	    (fill-region start (point))
	    (setq filled (1+ filled))))
      
	;; Turning off these messages because they go by too fast and
	;; are not particularly enlightening.  USR, 2010-01-26
	;; (if (= filled 0)
	;;    (vm-inform 7 "Nothing to fill")
	;;  (vm-inform 7 "Filled %s paragraph%s"
	;;           (if (> filled 1) (format "%d" filled) "one")
	;;           (if (> filled 1) "s" "")))
	))))

(defun vm-fill-paragraphs-by-longlines (start end)
  "Uses longlines.el for filling the region."
  ;; prepare for longlines.el in XEmacs
  (require 'overlay)
  (require 'longlines)
  (defvar fill-nobreak-predicate nil)
  (defvar undo-in-progress nil)
  (defvar longlines-mode-hook nil)
  (defvar longlines-mode-on-hook nil)
  (defvar longlines-mode-off-hook nil)
  (unless (functionp 'replace-regexp-in-string)
    (defun replace-regexp-in-string (regexp rep string
                                            &optional fixedcase literal)
      (vm-replace-in-string string regexp rep literal)))
  (unless (functionp 'line-end-position)
    (defun line-end-position ()
      (save-excursion (end-of-line) (point))))
  (unless (functionp 'line-beginning-position)
    (defun line-beginning-position (&optional n)
      (save-excursion
        (if n (forward-line n))
        (beginning-of-line)
        (point)))
    (unless (functionp 'replace-regexp-in-string)
      (defun replace-regexp-in-string (regexp rep string
                                              &optional fixedcase literal)
        (vm-replace-in-string string regexp rep literal))))
  ;; now do the filling
  (let ((buffer-read-only nil)
        (fill-column 
	 (if (numberp vm-fill-paragraphs-containing-long-lines)
	     vm-fill-paragraphs-containing-long-lines
	   (- (window-width (get-buffer-window (current-buffer))) 1)))
	)
    (save-excursion
      (vm-save-restriction
       ;; longlines-wrap-region contains a (forward-line -1) which is causing
       ;; wrapping of headers which is wrong, so we restrict it here!
       (narrow-to-region start end)
       (longlines-decode-region start end) ; make linebreaks hard
       (longlines-wrap-region start end)  ; wrap, adding soft linebreaks
       (widen)))))


(defun vm-make-message-id ()
  (let (hostname
	(time (current-time)))
    (setq hostname (cond ((string-match "\\." (system-name))
			  (system-name))
			 ((and (stringp mail-host-address)
			       (string-match "\\." mail-host-address))
			  mail-host-address)
			 (t "gargle.gargle.HOWL")))
    (format "<%d.%d.%d.%d@%s>"
	    (car time) (nth 1 time) (nth 2 time)
	    (random 1000000)
	    hostname)))

(defun vm-keep-some-buffers (buffer ring-variable number-to-keep 
				    &optional rename-prefix)
  "Keep the BUFFER in the variable RING-VARIABLE, with NUMBER-TO-KEEP
being the maximum number of buffers kept.  If necessary, the
RING-VARIABLE is pruned.  If the optional argument string
RENAME-PREFIX is given BUFFER is renamed by adding the prefix at the
front before adding it to the RING-VARIABLE."
  (if (memq buffer (symbol-value ring-variable))
      (set ring-variable (delq buffer (symbol-value ring-variable)))
    (with-current-buffer buffer
      (rename-buffer (concat "saved " (buffer-name)) t)))
  (set ring-variable (cons buffer (symbol-value ring-variable)))
  (set ring-variable (vm-delete 'buffer-name
				(symbol-value ring-variable) t))
  (if (not (eq number-to-keep t))
      (let ((extras (nthcdr (or number-to-keep 0)
			    (symbol-value ring-variable))))
	(mapc (function
	       (lambda (b)
		 (when (and (buffer-name b)
			    (or (not (buffer-modified-p b))
				(not (with-current-buffer b
				       buffer-offer-save))))
		   (kill-buffer b))))
	      extras)
	(and (symbol-value ring-variable) extras
	     (setcdr (memq (car extras) (symbol-value ring-variable))
		     nil)))))

(defvar enable-multibyte-characters)
(defvar buffer-display-table)
(defun vm-fsfemacs-nonmule-display-8bit-chars ()
  (cond ((and vm-fsfemacs-p
	      (or (not vm-fsfemacs-mule-p)
		  (and (boundp 'enable-multibyte-characters)
		       (not enable-multibyte-characters))))
	 (let* (tab (i 160))
	   ;; We need the function make-display-table, but it is
	   ;; in disp-table.el, which overwrites the value of
	   ;; standard-display-table when it is loaded, which
	   ;; sucks.  So here we cruftily copy just enough goop
	   ;; out of disp-table.el so that a display table can be
	   ;; created, and thereby avoid loading disp-table.
	   (put 'display-table 'char-table-extra-slots 6)
	   (setq tab (make-char-table 'display-table nil))
	   (while (< i 256)
	     (aset tab i (vector i))
	     (setq i (1+ i)))
	   (setq buffer-display-table tab)))))

(defun vm-url-decode-string (string)
  (vm-with-string-as-temp-buffer string 'vm-url-decode-buffer))

(defun vm-url-decode-buffer ()
  (let ((case-fold-search t)
	(hex-digit-alist '((?0 .  0)  (?1 .  1)  (?2 .  2)  (?3 .  3)
			   (?4 .  4)  (?5 .  5)  (?6 .  6)  (?7 .  7)
			   (?8 .  8)  (?9 .  9)  (?A . 10)  (?B . 11)
			   (?C . 12)  (?D . 13)  (?E . 14)  (?F . 15)
			   (?a . 10)  (?b . 11)  (?c . 12)  (?d . 13)
			   (?e . 14)  (?f . 15)))
	char)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "%[0-9A-F][0-9A-F]" nil t)
	(insert-char (+ (* (cdr (assq (char-after (- (point) 2))
				      hex-digit-alist))
			   16)
			(cdr (assq (char-after (- (point) 1))
				   hex-digit-alist)))
		     1)
	(delete-region (- (point) 1) (- (point) 4))))))

(defun vm-process-kill-without-query (process &optional flag)
  (if (fboundp 'process-kill-without-query)
      (process-kill-without-query process flag)
    (set-process-query-on-exit-flag process flag)))

(defun vm-process-sentinel-kill-buffer (process what-happened)
  (kill-buffer (process-buffer process)))

(defun vm-fsfemacs-scroll-bar-width ()
  (or vm-fsfemacs-cached-scroll-bar-width
      (let (size)
	(setq size (frame-pixel-width))
	(scroll-bar-mode nil)
	(setq size (- size (frame-pixel-width)))
	(scroll-bar-mode nil)
	(setq vm-fsfemacs-cached-scroll-bar-width size))))

(defvar vm-disable-modes-ignore nil
  "List of modes ignored by `vm-disable-modes'.
Any mode causing an error while trying to disable it will be added to this
list.  It still will try to diable it, but no error messages are generated
anymore for it.")

(defun vm-disable-modes (&optional modes)
  "Disable the given minor modes.
If MODES is nil the take the modes from the variable 
`vm-disable-modes-before-encoding'."
  (let (m)
    (while modes
      (setq m (car modes) modes (cdr modes))
      (condition-case errmsg
          (if (functionp m)
              (funcall m -1))
	(error 
	 (when (not (member m vm-disable-modes-ignore))
	   (vm-inform 0 "Could not disable mode `%S': %S" m errmsg)
	   (setq vm-disable-modes-ignore (cons m vm-disable-modes-ignore)))
	 nil)))))

(defun vm-add-write-file-hook (vm-hook-fn)
  "Add a function to the hook called during write-file.

Emacs changed the name of write-file-hooks to write-file-functions as of 
Emacs 22.1. This function is used to supress compiler warnings."
  (if (boundp 'write-file-functions)
      (add-hook 'write-file-functions vm-hook-fn)
    (add-hook 'write-file-hooks vm-hook-fn)))

(defun vm-add-find-file-hook (vm-hook-fn)
  "Add a function to the hook called during find-file.

Emacs changed the name of the hook find-file-hooks to find-file-hook in
Emacs 22.1. This function used to supress compiler warnings."
  (if (boundp 'find-file-hook)
      (add-hook 'find-file-hook vm-hook-fn)
    (add-hook 'find-file-hooks vm-hook-fn)))

;;; vm-misc.el ends here
