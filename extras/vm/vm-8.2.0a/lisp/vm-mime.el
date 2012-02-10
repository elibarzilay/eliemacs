;;; vm-mime.el ---  MIME support functions
;;
;; This file is part of VM
;;
;; Copyright (C) 1997-2003 Kyle E. Jones
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

(provide 'vm-mime)

(eval-and-compile
  (require 'vm-misc))

(eval-when-compile
  (require 'vm-minibuf)
  (require 'vm-toolbar)
  (require 'vm-mouse)
  (require 'vm-summary)
  (require 'vm-folder)
  (require 'vm-menu)
  (require 'vm-crypto)
  (require 'vm-window)
  (require 'vm-page)
  (require 'vm-motion)
  (require 'vm-reply)
  (require 'vm-digest)
  (require 'vm-edit)
  )

;; vm-xemacs.el is a fake file to fool the Emacs 23 compiler
(declare-function get-itimer "vm-xemacs" (name))
(declare-function start-itimer "vm-xemacs"
		  (name function value &optional restart is-idle with-args
			&rest function-arguments))
(declare-function set-itimer-restart "vm-xemacs" (itimer restart))
(declare-function find-coding-system "vm-xemacs" (coding-system-or-name))
(declare-function latin-unity-representations-feasible-region 
		  "vm-xemacs" (start end))
(declare-function latin-unity-representations-present-region 
		  "vm-xemacs" (start end))
(declare-function latin-unity-massage-name "vm-xemacs" (a b))
(declare-function latin-unity-maybe-remap "vm-xemacs" 
		  (a1 a2 a3 a4 a5 a6))
(declare-function device-sound-enabled-p "vm-xemacs" (&optional device))
(declare-function device-bitplanes "vm-xemacs" (&optional device))
(declare-function font-height "vm-xemacs" (font &optional domain charset))
(declare-function make-glyph "vm-xemacs" (&optional spec-list type))
(declare-function set-glyph-baseline "vm-xemacs" 
		  (glyph spec &optional locale tag-set how-to-add))
(declare-function set-glyph-face "vm-xemacs" (glyph face))
(declare-function extent-list "vm-xemacs" 
		  (&optional buffer-or-string from to flags property value))
(declare-function extent-begin-glyph "vm-xemacs" (extent))
(declare-function set-extent-begin-glyph "vm-xemacs" 
		  (extent begin-glyph &optional layout))
(declare-function extent-live-p "vm-xemacs" (object))

(declare-function vm-mode "vm" (&optional read-only))

(defvar enable-multibyte-characters)

;; The following variables are defined in the code, depending on the
;; Emacs version being used.  They should not be initialized here.

(defvar vm-image-list)
(defvar vm-image-type)
(defvar vm-image-type-name)
(defvar vm-extent-list)
(defvar vm-overlay-list)


(defun vm-mime-error (&rest args)
  (signal 'vm-mime-error (list (apply 'format args)))
  (error "can't return from vm-mime-error"))

(if (fboundp 'define-error)
    (progn
      (define-error 'vm-image-too-small "Image too small")
      (define-error 'vm-mime-error "MIME error"))
  (put 'vm-image-too-small 'error-conditions '(vm-image-too-small error))
  (put 'vm-image-too-small 'error-message "Image too small")
  (put 'vm-mime-error 'error-conditions '(vm-mime-error error))
  (put 'vm-mime-error 'error-message "MIME error"))

;; A lot of the more complicated MIME character set processing is only
;; practical under MULE.
(eval-when-compile 
  (defvar latin-unity-ucs-list)
  (defvar latin-unity-character-sets)
  (defvar coding-system-list))

(defun vm-get-coding-system-priorities ()
  "Return the value of `vm-coding-system-priorities', or a reasonable
default for it if it's nil.  "
  (if vm-coding-system-priorities
      vm-coding-system-priorities
    (let ((res '(iso-8859-1 iso-8859-2 iso-8859-15 iso-8859-16 utf-8)))
      (dolist (list-item res)
	;; Assumes iso-8859-1 is always available, which is reasonable.
	(unless (vm-coding-system-p list-item)
	  (delq list-item res)))
      res)))

(defun vm-mime-charset-to-coding (charset)
  "Return the Emacs coding system corresonding to the given mime CHARSET."
  ;; We can depend on the fact that, in FSF Emacsen, coding systems
  ;; have aliases that correspond to MIME charset names.
  (let ((tmp nil))
    (cond (vm-fsfemacs-mule-p
	   (cond ((vm-coding-system-p (setq tmp (intern (downcase charset))))
		   tmp)
		  ((equal charset "us-ascii")
		   'raw-text)
		  ((equal charset "unknown")
		   'iso-8859-1)
		  (t 'undecided)))
	  (t
	   ;; What about the case where vm-m-m-c-t-c-a doesn't have an
	   ;; entry for the given charset? That shouldn't happen, if
	   ;; vm-mime-mule-coding-to-charset-alist and
	   ;; vm-mime-mule-charset-to-coding-alist have complete and
	   ;; matching entries. Admittedly this last is not a
	   ;; given. Should we make it so on startup? (By setting the
	   ;; key for any missing entries in
	   ;; vm-mime-mule-coding-to-charset-alist to being (format
	   ;; "%s" coding-system), if necessary.) RWF, 2005-03-25
	   (setq tmp (vm-string-assoc charset
				      vm-mime-mule-charset-to-coding-alist))
	   (if tmp (cadr tmp) nil))
	  )))
		  

(defun vm-get-mime-ucs-list ()
  "Return the value of `vm-mime-ucs-list', or a reasonable default for it if
it's nil.  This is used instead of `vm-mime-ucs-list' directly in order to
allow runtime checks for optional features like `mule-ucs' or
`latin-unity'.  "
  (if vm-mime-ucs-list
      vm-mime-ucs-list
    (if (featurep 'latin-unity)
	latin-unity-ucs-list
      (if (vm-coding-system-p 'utf-8)
	  '(utf-8 iso-2022-jp ctext escape-quoted)
	'(iso-2022-jp ctext escape-quoted)))))

(defun vm-update-mime-charset-maps ()
  "Check for the presence of certain Mule coding systems, and add
information about the corresponding MIME character sets to VM's
configuration.  "
  ;; Add some extra charsets that may not have been defined onto the end
  ;; of vm-mime-mule-charset-to-coding-alist.
  (mapc (lambda (x)
	  (and (vm-coding-system-p x)
	       ;; Not using vm-string-assoc because of some quoting
	       ;; weirdness it's doing. 
	       (if (not (assoc
			 (format "%s" x)
			 vm-mime-mule-charset-to-coding-alist))
		   (add-to-list 'vm-mime-mule-charset-to-coding-alist 
				(list (format "%s" x) x)))))
	'(utf-8 iso-8859-15 iso-8859-14 iso-8859-16
		alternativnyj iso-8859-6 iso-8859-7 koi8-c koi8-o koi8-ru koi8-t
		koi8-u macintosh windows-1250 windows-1251 windows-1252
		windows-1253 windows-1256))

  ;; And make sure that the map back from coding-systems is good for
  ;; those charsets.
  (mapc (lambda (x)
	  (or (assoc (car (cdr x)) vm-mime-mule-coding-to-charset-alist)
	      (add-to-list 'vm-mime-mule-coding-to-charset-alist
			   (list (car (cdr x)) (car x)))))
	vm-mime-mule-charset-to-coding-alist)
  ;; Whoops, doesn't get picked up for some reason. 
  (add-to-list 'vm-mime-mule-coding-to-charset-alist 
	       '(iso-8859-1 "iso-8859-1")))

(eval-when-compile
  (when vm-fsfemacs-p
    (defvar latin-unity-character-sets nil)))

(when vm-xemacs-mule-p
  (require 'vm-vars)
  (vm-update-mime-charset-maps)
  ;; If the user loads Mule-UCS, re-evaluate the MIME charset maps. 
  (unless (vm-coding-system-p 'utf-8)
    (eval-after-load "un-define" `(vm-update-mime-charset-maps)))
  ;; Ditto for latin-unity. 
  (unless (featurep 'latin-unity)
    (eval-after-load "latin-unity" `(vm-update-mime-charset-maps))))

;;----------------------------------------------------------------------------
;;; MIME layout structs (vm-mm)
;;----------------------------------------------------------------------------

(defconst vm-mime-layout-fields
  '[:type :qtype :encoding :id :description :disposition :qdisposition
	  :header-start :header-end :body-start :body-end
	  :parts :cache :message-symbol :display-error 
	  :layout-is-converted :unconverted-layout])

(defun vm-pp-mime-layout (layout)
  (pp (vm-zip-vectors vm-mime-layout-fields layout))
  nil)

(defun vm-make-layout (&rest plist)
  (vector
   (plist-get plist 'type)
   (plist-get plist 'qtype)
   (plist-get plist 'encoding)
   (plist-get plist 'id)
   (plist-get plist 'description)
   (plist-get plist 'disposition)
   (plist-get plist 'qdisposition)
   (plist-get plist 'header-start)
   (plist-get plist 'header-end)
   (plist-get plist 'body-start)
   (plist-get plist 'body-end)
   (plist-get plist 'parts)
   (plist-get plist 'cache)
   (plist-get plist 'message-symbol)
   (plist-get plist 'display-error)
   (plist-get plist 'layout-is-converted)
   (plist-get plist 'unconverted-layout)))

(defun vm-mime-copy-layout (from to)
  "Copy a MIME layout FROM to the layout TO.  The previous contents of
TO are overwritten.                                    USR, 2011-03-27"
  (let ((i (1- (length from))))
    (while (>= i 0)
      (aset to i (aref from i))
      (setq i (1- i)))))

(defun vm-mm-layout-type (e) (aref e 0))
(defun vm-mm-layout-qtype (e) (aref e 1))
(defun vm-mm-layout-encoding (e) (aref e 2))
(defun vm-mm-layout-id (e) (aref e 3))
(defun vm-mm-layout-description (e) (aref e 4))
(defun vm-mm-layout-disposition (e) (aref e 5))
(defun vm-mm-layout-qdisposition (e) (aref e 6))
(defun vm-mm-layout-header-start (e) (aref e 7))
(defun vm-mm-layout-header-end (e) (aref e 8))
(defun vm-mm-layout-body-start (e) (aref e 9))
(defun vm-mm-layout-body-end (e) (aref e 10))
(defun vm-mm-layout-parts (e) (aref e 11))
(defun vm-mm-layout-cache (e) (aref e 12))
(defun vm-mm-layout-message-symbol (e) (aref e 13))
(defun vm-mm-layout-message (e)
  (symbol-value (vm-mm-layout-message-symbol e)))
;; if display of MIME part fails, error string will be here.
(defun vm-mm-layout-display-error (e) (aref e 14))
(defun vm-mm-layout-is-converted (e) (aref e 15))
(defun vm-mm-layout-unconverted-layout (e) (aref e 16))

(defun vm-set-mm-layout-type (e type) (aset e 0 type))
(defun vm-set-mm-layout-qtype (e type) (aset e 1 type))
(defun vm-set-mm-layout-encoding (e encoding) (aset e 2 encoding))
(defun vm-set-mm-layout-id (e id) (aset e 3 id))
(defun vm-set-mm-layout-description (e des) (aset e 4 des))
(defun vm-set-mm-layout-disposition (e d) (aset e 5 d))
(defun vm-set-mm-layout-qdisposition (e d) (aset e 6 d))
(defun vm-set-mm-layout-header-start (e start) (aset e 7 start))
(defun vm-set-mm-layout-header-end (e start) (aset e 8 start))
(defun vm-set-mm-layout-body-start (e start) (aset e 9 start))
(defun vm-set-mm-layout-body-end (e end) (aset e 10 end))
(defun vm-set-mm-layout-parts (e parts) (aset e 11 parts))
(defun vm-set-mm-layout-cache (e c) (aset e 12 c))
(defun vm-set-mm-layout-message-symbol (e s) (aset e 13 s))
(defun vm-set-mm-layout-display-error (e c) (aset e 14 c))
(defun vm-set-mm-layout-is-converted (e c) (aset e 15 c))
(defun vm-set-mm-layout-unconverted-layout (e l) (aset e 16 l))

(defun vm-mime-type-with-params (type params)
  "Returns a string concatenating MIME TYPE (a string) and PARAMS (a
list of strings)."
  (if params
      (if vm-mime-avoid-folding-content-type
	  (concat type ";\n\t " (mapconcat 'identity params ";\n\t"))
	(concat type "; " (mapconcat 'identity params "; ")))
    type))

(defun vm-mime-make-message-symbol (m)
  (let ((s (make-symbol "<<m>>")))
    (set s m)
    s ))

(defun vm-mime-make-cache-symbol ()
  (let ((s (make-symbol "<<c>>")))
    (set s s)
    s ))

(defun vm-mm-layout (m)
  "Returns the mime layout of message M, either from the cache or by
freshly parsing the message contents."
  (or (vm-mime-layout-of m)
      (progn (vm-set-mime-layout-of m (vm-mime-parse-entity-safe m))
	     (vm-mime-layout-of m))))

(defun vm-mm-encoded-header (m)
  (or (vm-mime-encoded-header-flag-of m)
      (progn (setq m (vm-real-message-of m))
	     (vm-set-mime-encoded-header-flag-of
	      m
	      (save-excursion
		(set-buffer (vm-buffer-of m))
		(save-excursion
		  (save-restriction
		    (widen)
		    (goto-char (vm-headers-of m))
		    (let ((case-fold-search t))
		      (or (re-search-forward vm-mime-encoded-word-regexp
					     (vm-text-of m) t)
			  'none))))))
	     (vm-mime-encoded-header-flag-of m))))

;;----------------------------------------------------------------------------
;;; MIME encoding/decoding
;;----------------------------------------------------------------------------

(defun vm-mime-Q-decode-region (start end)
  (interactive "r")
  (let ((buffer-read-only nil))
    (subst-char-in-region start end ?_ (string-to-char " ") t)
    (vm-mime-qp-decode-region start end)))

(fset 'vm-mime-B-decode-region 'vm-mime-base64-decode-region)

(defun vm-mime-Q-encode-region (start end)
  (let ((buffer-read-only nil)
	(val))
    (setq val (vm-mime-qp-encode-region start end t)) ; may modify buffer
    (subst-char-in-region start (min end (point-max))
                          (string-to-char " ") ?_ t)
    val ))

(defun vm-mime-B-encode-region (start end)
  (vm-mime-base64-encode-region start end nil t))

(defun vm-mime-base64-decode-string (string)
  (vm-with-string-as-temp-buffer
   string
   (function
    (lambda () (vm-mime-base64-decode-region (point-min) (point-max))))))

(defun vm-mime-base64-encode-string (string)
  (vm-with-string-as-temp-buffer
   string
   (function
    (lambda () (vm-mime-base64-encode-region (point-min) (point-max)
					     nil t)))))

(defun vm-mime-crlf-to-lf-region (start end)
  (let ((buffer-read-only nil))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char start)
	(while (search-forward "\r\n" nil t)
	  (delete-char -2)
	  (insert "\n"))))))
      
(defun vm-mime-lf-to-crlf-region (start end)
  (let ((buffer-read-only nil))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char start)
	(while (search-forward "\n" nil t)
	  (delete-char -1)
	  (insert "\r\n"))))))
      
(defun vm-encode-coding-region (b-start b-end coding-system &rest foo)
  (let ((work-buffer nil)
	start end
	oldsize
	retval
	(b (current-buffer)))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (set-buffer work-buffer)
	  (insert-buffer-substring b b-start b-end)
	  (setq oldsize (buffer-size))
	  (setq retval (apply 'encode-coding-region (point-min) (point-max)
			      coding-system foo))
	  (setq start (point-min) end (point-max))
	  (setq retval (buffer-size))
	  (save-excursion
	    (set-buffer b)
	    (goto-char b-start)
	    (insert-buffer-substring work-buffer start end)
	    (delete-region (point) (+ (point) oldsize))
	    ;; Fixup the end point.  I have found no other way to
	    ;; let the calling function know where the region ends
	    ;; after encode-coding-region has scrambled the markers.
	    (and (markerp b-end)
		 (set-marker b-end (point)))
	    retval ))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-decode-coding-region (b-start b-end coding-system &rest foo)
  "This is a wrapper for decode-coding-region, having the same effect."
  (let ((work-buffer nil)
	start end
	oldsize
	retval
	(b (current-buffer)))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (setq oldsize (- b-end b-start))
	  (set-buffer work-buffer)
	  (insert-buffer-substring b b-start b-end)
	  (setq retval (apply 'decode-coding-region (point-min) (point-max)
			      coding-system foo))
	  (and vm-fsfemacs-p (set-buffer-multibyte t)) ; is this safe?
	  (setq start (point-min) end (point-max))
	  (save-excursion
	    (set-buffer b)
	    (goto-char b-start)
	    (delete-region (point) (+ (point) oldsize))
	    (insert-buffer-substring work-buffer start end)
	    ;; Fixup the end point.  I have found no other way to
	    ;; let the calling function know where the region ends
	    ;; after decode-coding-region has scrambled the markers.
	    (and (markerp b-end)
		 (set-marker b-end (point)))
	    retval ))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-mime-charset-decode-region (charset start end)
  (or (markerp end) (setq end (vm-marker end)))
  (cond ((or vm-xemacs-mule-p vm-fsfemacs-mule-p)
	 (if (or (and vm-xemacs-p (memq (vm-device-type) '(x gtk mswindows)))
		 vm-fsfemacs-p
		 (vm-mime-tty-can-display-mime-charset charset)
		 nil)
	     (let ((buffer-read-only nil)
		   (coding (vm-mime-charset-to-coding charset))
		   (opoint (point)))
	       ;; decode 8-bit indeterminate char to correct
	       ;; char in correct charset.
	       (vm-decode-coding-region start end coding)
	       (put-text-property start end 'vm-string t)
	       (put-text-property start end 'vm-charset charset)
	       (put-text-property start end 'vm-coding coding)
	       ;; In XEmacs 20.0 beta93 decode-coding-region moves point.
	       (goto-char opoint))))
	((not (vm-multiple-fonts-possible-p)) nil)
	((vm-mime-default-face-charset-p charset) nil)
	(t
	 (let ((font (cdr (vm-string-assoc
			   charset
			   vm-mime-charset-font-alist)))
	       (face (make-face (make-symbol "temp-face")))
	       (e (vm-make-extent start end)))
	   (put-text-property start end 'vm-string t)
	   (put-text-property start end 'vm-charset charset)
	   (if font
	       (condition-case data
		   (progn (set-face-font face font)
			  (if vm-fsfemacs-p
			      (put-text-property start end 'face face)
			    (vm-set-extent-property e 'duplicable t)
			    (vm-set-extent-property e 'face face)))
		 (error nil)))))))

(defun vm-mime-transfer-decode-region (layout start end)
  "Decode the body of a mime part given by LAYOUT at positions START
to END, and replace it by the decoded content.  The decoding carried
out includes base-64, quoted-printable, uuencode and CRLF conversion."
  (let ((case-fold-search t) (crlf nil))
    (if (or (vm-mime-types-match "text" (car (vm-mm-layout-type layout)))
	    (vm-mime-types-match "message" (car (vm-mm-layout-type layout))))
	(setq crlf t))
    (cond ((string-match "^base64$" (vm-mm-layout-encoding layout))
	   (vm-mime-base64-decode-region start end crlf))
	  ((string-match "^quoted-printable$"
			 (vm-mm-layout-encoding layout))
	   (vm-mime-qp-decode-region start end))
	  ((string-match "^x-uue$\\|^x-uuencode$"
			 (vm-mm-layout-encoding layout))
	   (vm-mime-uuencode-decode-region start end crlf)))))

(defun vm-mime-base64-decode-region (start end &optional crlf)
  (or (markerp end) (setq end (vm-marker end)))
  (and (> (- end start) 10000)
       (vm-emit-mime-decoding-message "Decoding base64..."))
  (let ((work-buffer nil)
	(done nil)
	(counter 0)
	(bits 0)
	(lim 0) inputpos
	(non-data-chars (concat "^=" vm-mime-base64-alphabet)))
    (unwind-protect
	(save-excursion
	  (cond
	   ((and (featurep 'base64)
		 (fboundp 'base64-decode-region)
		 ;; W3 reportedly has a Lisp version of this, and
		 ;; there's no point running it.
		 (subrp (symbol-function 'base64-decode-region))
		 ;; The FSF Emacs version of this is unforgiving
		 ;; of errors, which is not in the spirit of the
		 ;; MIME spec, so avoid using it.
		 (not vm-fsfemacs-p))
	    (condition-case data
		(base64-decode-region start end)
	      (error (vm-mime-error "%S" data)))
	    (and crlf (vm-mime-crlf-to-lf-region start end)))
	   (t
	    (setq work-buffer (vm-make-work-buffer))
	    (if vm-mime-base64-decoder-program
		(let* ((binary-process-output t) ; any text already has CRLFs
		       ;; use binary coding system in FSF Emacs/MULE
		       (coding-system-for-read (vm-binary-coding-system))
		       (coding-system-for-write (vm-binary-coding-system))
		       (status (apply 'vm-run-command-on-region
				      start end work-buffer
				      vm-mime-base64-decoder-program
				      vm-mime-base64-decoder-switches)))
		  (if (not (eq status t))
		      (vm-mime-error "%s" (cdr status))))
	      (goto-char start)
	      (skip-chars-forward non-data-chars end)
	      (while (not done)
		(setq inputpos (point))
		(cond
		 ((> (skip-chars-forward vm-mime-base64-alphabet end) 0)
		  (setq lim (point))
		  (while (< inputpos lim)
		    (setq bits (+ bits
				  (aref vm-mime-base64-alphabet-decoding-vector
					(char-after inputpos))))
		    (vm-increment counter)
		    (vm-increment inputpos)
		    (cond ((= counter 4)
			   (vm-insert-char (lsh bits -16) 1 nil work-buffer)
			   (vm-insert-char (logand (lsh bits -8) 255) 1 nil
					   work-buffer)
			   (vm-insert-char (logand bits 255) 1 nil work-buffer)
			   (setq bits 0 counter 0))
			  (t (setq bits (lsh bits 6)))))))
		(cond
		 ((= (point) end)
		  (if (not (zerop counter))
		      (vm-mime-error "at least %d bits missing at end of base64 encoding"
				     (* (- 4 counter) 6)))
		  (setq done t))
		 ((= (char-after (point)) 61) ; 61 is ASCII equals
		  (setq done t)
		  (cond ((= counter 1)
			 (vm-mime-error "at least 2 bits missing at end of base64 encoding"))
			((= counter 2)
			 (vm-insert-char (lsh bits -10) 1 nil work-buffer))
			((= counter 3)
			 (vm-insert-char (lsh bits -16) 1 nil work-buffer)
			 (vm-insert-char (logand (lsh bits -8) 255)
					 1 nil work-buffer))
			((= counter 0) t)))
		 (t (skip-chars-forward non-data-chars end)))))
	    (and crlf
		 (save-excursion
		   (set-buffer work-buffer)
		   (vm-mime-crlf-to-lf-region (point-min) (point-max))))
	    (goto-char start)
	    (insert-buffer-substring work-buffer)
	    (delete-region (point) end))))
      (and work-buffer (kill-buffer work-buffer))))
  (and (> (- end start) 10000)
       (vm-emit-mime-decoding-message "Decoding base64... done")))

(defun vm-mime-base64-encode-region (start end &optional crlf B-encoding)
  (or (markerp end) (setq end (vm-marker end)))
  (and (> (- end start) 200)
       (vm-inform 7 "Encoding base64..."))
  (let ((work-buffer nil)
	(buffer-undo-list t)
	(counter 0)
	(cols 0)
	(bits 0)
	(alphabet vm-mime-base64-alphabet)
	inputpos)
    (unwind-protect
	(save-excursion
	  (and crlf (vm-mime-lf-to-crlf-region start end))
	  (cond
	   ((and (featurep 'base64)
		 (fboundp 'base64-encode-region)
		 ;; W3 reportedly has a Lisp version of this, and
		 ;; there's no point running it.
		 (subrp (symbol-function 'base64-encode-region)))
	    (condition-case data
		(base64-encode-region start end B-encoding)
	      (wrong-number-of-arguments
	       ;; call with two args and then strip out the
	       ;; newlines if we're doing B encoding.
	       (condition-case data
		   (base64-encode-region start end)
		 (error (vm-mime-error "%S" data)))
	       (if B-encoding
		   (save-excursion
		     (goto-char start)
		     (while (search-forward "\n" end t)
		       (delete-char -1)))))
	      (error (vm-mime-error "%S" data))))
	   (t
	    (setq work-buffer (vm-make-work-buffer))
	    (if vm-mime-base64-encoder-program
		(let ((status (apply 'vm-run-command-on-region
				     start end work-buffer
				     vm-mime-base64-encoder-program
				     vm-mime-base64-encoder-switches)))
		  (if (not (eq status t))
		      (vm-mime-error "%s" (cdr status)))
		  (if B-encoding
		      (save-excursion
			(set-buffer work-buffer)
			;; if we're B encoding, strip out the line breaks
			(goto-char (point-min))
			(while (search-forward "\n" nil t)
			  (delete-char -1)))))
	      (setq inputpos start)
	      (while (< inputpos end)
		(setq bits (+ bits (char-after inputpos)))
		(vm-increment counter)
		(cond ((= counter 3)
		       (vm-insert-char (aref alphabet (lsh bits -18)) 1 nil
				       work-buffer)
		       (vm-insert-char (aref alphabet (logand (lsh bits -12) 63))
				       1 nil work-buffer)
		       (vm-insert-char (aref alphabet (logand (lsh bits -6) 63))
				       1 nil work-buffer)
		       (vm-insert-char (aref alphabet (logand bits 63)) 1 nil
				       work-buffer)
		       (setq cols (+ cols 4))
		       (cond ((= cols 72)
			      (setq cols 0)
			      (if (not B-encoding)
				  (vm-insert-char ?\n 1 nil work-buffer))))
		       (setq bits 0 counter 0))
		      (t (setq bits (lsh bits 8))))
		(vm-increment inputpos))
	      ;; write out any remaining bits with appropriate padding
	      (if (= counter 0)
		  nil
		(setq bits (lsh bits (- 16 (* 8 counter))))
		(vm-insert-char (aref alphabet (lsh bits -18)) 1 nil
				work-buffer)
		(vm-insert-char (aref alphabet (logand (lsh bits -12) 63))
				1 nil work-buffer)
		(if (= counter 1)
		    (vm-insert-char ?= 2 nil work-buffer)
		  (vm-insert-char (aref alphabet (logand (lsh bits -6) 63))
				  1 nil work-buffer)
		  (vm-insert-char ?= 1 nil work-buffer)))
	      (if (> cols 0)
		  (vm-insert-char ?\n 1 nil work-buffer)))
	    (or (markerp end) (setq end (vm-marker end)))
	    (goto-char start)
	    (insert-buffer-substring work-buffer)
	    (delete-region (point) end)))
	  (and (> (- end start) 200)
	       (vm-inform 7 "Encoding base64... done"))
	  (- end start))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-mime-qp-decode-region (start end)
  (and (> (- end start) 10000)
       (vm-emit-mime-decoding-message "Decoding quoted-printable..."))
  (let ((work-buffer nil)
	(buf (current-buffer))
	(case-fold-search nil)
	(hex-digit-alist '((?0 .  0)  (?1 .  1)  (?2 .  2)  (?3 .  3)
			   (?4 .  4)  (?5 .  5)  (?6 .  6)  (?7 .  7)
			   (?8 .  8)  (?9 .  9)  (?A . 10)  (?B . 11)
			   (?C . 12)  (?D . 13)  (?E . 14)  (?F . 15)
			   ;; some mailer uses lower-case hex
			   ;; digits despite this being forbidden
			   ;; by the MIME spec.
			   (?a . 10)  (?b . 11)  (?c . 12)  (?d . 13)
			   (?e . 14)  (?f . 15)))
	inputpos stop-point copy-point)
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (if vm-mime-qp-decoder-program
	      (let* ((binary-process-output t) ; any text already has CRLFs
		     ;; use binary coding system in FSF Emacs/MULE
		     (coding-system-for-read (vm-binary-coding-system))
		     (coding-system-for-write (vm-binary-coding-system))
		     (status (apply 'vm-run-command-on-region
				    start end work-buffer
				    vm-mime-qp-decoder-program
				    vm-mime-qp-decoder-switches)))
		(if (not (eq status t))
		    (vm-mime-error "%s" (cdr status))))
	    (goto-char start)
	    (setq inputpos start)
	    (while (< inputpos end)
	      (skip-chars-forward "^=\n" end)
	      (setq stop-point (point))
	      (cond ((looking-at "\n")
		     ;; spaces or tabs before a hard line break must be ignored
		     (skip-chars-backward " \t")
		     (setq copy-point (point))
		     (goto-char stop-point))
		    (t (setq copy-point stop-point)))
	      (save-excursion
		(set-buffer work-buffer)
		(insert-buffer-substring buf inputpos copy-point))
	      (cond ((= (point) end) t)
		    ((looking-at "\n")
		     (vm-insert-char ?\n 1 nil work-buffer)
		     (forward-char))
		    (t;; looking at =
		     (forward-char)
		     ;; a-f because some mailers use lower case hex
		     ;; digits despite them being forbidden by the
		     ;; MIME spec.
		     (cond ((looking-at "[0-9A-Fa-f][0-9A-Fa-f]")
			    (vm-insert-char (+ (* (cdr (assq (char-after (point))
							     hex-digit-alist))
						  16)
					       (cdr (assq (char-after
							   (1+ (point)))
							  hex-digit-alist)))
					    1 nil work-buffer)
			    (forward-char 2))
			   ((looking-at "\n") ; soft line break
			    (forward-char))
			   ((looking-at "\r")
			    ;; assume the user's goatloving
			    ;; delivery software didn't convert
			    ;; from Internet's CRLF newline
			    ;; convention to the local LF
			    ;; convention.
			    (forward-char))
			   ((looking-at "[ \t]")
			    ;; garbage added in transit
			    (skip-chars-forward " \t" end))
			   (t (vm-mime-error "something other than line break or hex digits after = in quoted-printable encoding")))))
	      (setq inputpos (point))))
	  (or (markerp end) (setq end (vm-marker end)))
	  (goto-char start)
	  (insert-buffer-substring work-buffer)
	  (delete-region (point) end))
      (and work-buffer (kill-buffer work-buffer))))
  (and (> (- end start) 10000)
       (vm-emit-mime-decoding-message "Decoding quoted-printable... done")))

(defun vm-mime-qp-encode-region (start end &optional Q-encoding quote-from)
  (and (> (- end start) 200)
       (vm-inform 7 "Encoding quoted-printable..."))
  (let ((work-buffer nil)
	(buf (current-buffer))
	(cols 0)
	(hex-digit-alist '((?0 .  0)  (?1 .  1)  (?2 .  2)  (?3 .  3)
			   (?4 .  4)  (?5 .  5)  (?6 .  6)  (?7 .  7)
			   (?8 .  8)  (?9 .  9)  (?A . 10)  (?B . 11)
			   (?C . 12)  (?D . 13)  (?E . 14)  (?F . 15)))
	char inputpos)

    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (if vm-mime-qp-encoder-program
	      (let* ((binary-process-output t) ; any text already has CRLFs
		     ;; use binary coding system in FSF Emacs/MULE
		     (coding-system-for-read (vm-binary-coding-system))
		     (coding-system-for-write (vm-binary-coding-system))
		     (status (apply 'vm-run-command-on-region
				    start end work-buffer
				    vm-mime-qp-encoder-program
				    vm-mime-qp-encoder-switches)))
		(if (not (eq status t))
		    (vm-mime-error "%s" (cdr status)))
		(if quote-from
		    (save-excursion
		      (set-buffer work-buffer)
		      (goto-char (point-min))
		      (while (re-search-forward "^From " nil t)
			(replace-match "=46rom " t t))))
		(if Q-encoding
		    (save-excursion
		      (set-buffer work-buffer)
		      ;; strip out the line breaks
		      (goto-char (point-min))
		      (while (search-forward "=\n" nil t)
			(delete-char -2))
		      ;; strip out the soft line breaks
		      (goto-char (point-min))
		      (while (search-forward "\n" nil t)
			(delete-char -1)))))
	    (setq inputpos start)
	    (while (< inputpos end)
	      (setq char (char-after inputpos))
	      (cond ((= char ?\n)
		     (vm-insert-char char 1 nil work-buffer)
		     (setq cols 0))
		    ((and (= char 32)
			  (not (= (1+ inputpos) end))
			  (not (= ?\n (char-after (1+ inputpos)))))
		     (vm-insert-char char 1 nil work-buffer)
		     (vm-increment cols))
		    ((or (< char 33) (> char 126)
			 ;; =
			 (= char 61)
			 ;; ?
			 (and Q-encoding (= char 63))
			 ;; _
			 (and Q-encoding (= char 95))
			 (and quote-from (= cols 0)
			      (let ((case-fold-search nil))
				(looking-at "From ")))
			 (and (= cols 0) (= char ?.)
			      (looking-at "\\.\\(\n\\|\\'\\)")))
		     (vm-insert-char ?= 1 nil work-buffer)
		     (vm-insert-char (car (rassq (lsh (logand char 255) -4)
						 hex-digit-alist))
				     1 nil work-buffer)
		     (vm-insert-char (car (rassq (logand char 15)
						 hex-digit-alist))
				     1 nil work-buffer)
		     (setq cols (+ cols 3)))
		    (t (vm-insert-char char 1 nil work-buffer)
		       (vm-increment cols)))
	      (cond ((> cols 70)
		     (setq cols 0)
		     (if Q-encoding
			 nil
		       (vm-insert-char ?= 1 nil work-buffer)
		       (vm-insert-char ?\n 1 nil work-buffer))))
	      (vm-increment inputpos)))
	  (or (markerp end) (setq end (vm-marker end)))
	  (goto-char start)
	  (insert-buffer-substring work-buffer)
	  (delete-region (point) end)
	  (and (> (- end start) 200)
	       (vm-inform 7 "Encoding quoted-printable... done"))
	  (- end start))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-mime-uuencode-decode-region (start end &optional crlf)
  (vm-emit-mime-decoding-message "Decoding uuencoded stuff...")
  (let ((work-buffer nil)
	(region-buffer (current-buffer))
	(case-fold-search nil)
	(tempfile (vm-make-tempfile-name)))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (set-buffer work-buffer)
	  (insert-buffer-substring region-buffer start end)
	  (goto-char (point-min))
	  (or (re-search-forward "^begin [0-7][0-7][0-7] " nil t)
	      (vm-mime-error "no begin line"))
	  (delete-region (point) (progn (forward-line 1) (point)))
	  (insert tempfile "\n")
	  (goto-char (point-max))
	  (beginning-of-line)
	  ;; Eudora reportedly doesn't terminate uuencoded multipart
	  ;; bodies with a line break. 21 June 1998.
	  ;; Actually it looks like Eudora doesn't understand the
	  ;; multipart newline boundary rule at all and can leave
	  ;; all types of attachments missing a line break.
	  (if (looking-at "^end\\'")
	      (progn
		(goto-char (point-max))
		(insert "\n")))
	  (if (stringp vm-mime-uuencode-decoder-program)
	      (let* ((binary-process-output t) ; any text already has CRLFs
		     ;; use binary coding system in FSF Emacs/MULE
		     (coding-system-for-read (vm-binary-coding-system))
		     (coding-system-for-write (vm-binary-coding-system))
		     (status (apply 'vm-run-command-on-region
				    (point-min) (point-max) nil
				    vm-mime-uuencode-decoder-program
				    vm-mime-uuencode-decoder-switches)))
		(if (not (eq status t))
		    (vm-mime-error "%s" (cdr status))))
	    (vm-mime-error "no uuencode decoder program defined"))
	  (delete-region (point-min) (point-max))
	  (insert-file-contents-literally tempfile)
	  (and crlf
	       (vm-mime-crlf-to-lf-region (point-min) (point-max)))
	  (set-buffer region-buffer)
	  (or (markerp end) (setq end (vm-marker end)))
	  (goto-char start)
	  (insert-buffer-substring work-buffer)
	  (delete-region (point) end))
      (and work-buffer (kill-buffer work-buffer))
      (vm-error-free-call 'delete-file tempfile)))
  (vm-emit-mime-decoding-message "Decoding uuencoded stuff... done"))

(defun vm-decode-mime-message-headers (&optional m)
  (vm-decode-mime-encoded-words 
   ;; the starting point with null m is (point) to match the
   ;; previous duplicated code here. Not sure whether it's
   ;; necessary. JCB, 2011-01-03
   (if m (vm-headers-of m) (point))
   (if m (vm-text-of m) (point-max))))

;; optional argument rstart and rend delimit the region in
;; which to decode
(defun vm-decode-mime-encoded-words (&optional rstart rend)
  (let ((case-fold-search t)
	(buffer-read-only nil)
	charset need-conversion encoding match-start match-end start end
	previous-end)
    (save-excursion
      (goto-char (or rstart (point-min)))
      (while (re-search-forward vm-mime-encoded-word-regexp rend t)
	(setq match-start (match-beginning 0)
	      match-end (match-end 0)
	      charset (buffer-substring (match-beginning 1) (match-end 1))
              need-conversion nil
	      encoding (buffer-substring (match-beginning 4) (match-end 4))
	      start (match-beginning 5)
	      end (vm-marker (match-end 5)))
	;; don't change anything if we can't display the
	;; character set properly.
	(if (and (not (vm-mime-charset-internally-displayable-p charset))
		 (not (setq need-conversion
			    (vm-mime-can-convert-charset charset))))
	    nil
	  ;; suppress whitespace between encoded words.
	  (and previous-end
	       (string-match "\\`[ \t\n]*\\'"
			     (buffer-substring previous-end match-start))
	       (setq match-start previous-end))
	  (delete-region end match-end)
	  (condition-case data
	      (cond ((string-match "B" encoding)
		     (vm-mime-base64-decode-region start end))
		    ((string-match "Q" encoding)
		     (vm-mime-Q-decode-region start end))
		    (t (vm-mime-error "unknown encoded word encoding, %s"
				      encoding)))
	    (vm-mime-error (apply 'message (cdr data))
			   (goto-char start)
			   (insert "**invalid encoded word**")
			   (delete-region (point) end)))
	  (and need-conversion
	       (setq charset (vm-mime-charset-convert-region
			      charset start end)))
	  (vm-mime-charset-decode-region charset start end)
	  (goto-char end)
	  (setq previous-end end)
	  (delete-region match-start start))))))

(defun vm-decode-mime-encoded-words-in-string (string)
  (if (and vm-display-using-mime
	   (let ((case-fold-search t))
	     (string-match vm-mime-encoded-word-regexp string)))
      (vm-with-string-as-temp-buffer string 'vm-decode-mime-encoded-words)
    string ))

(defun vm-reencode-mime-encoded-words ()
  "Reencode in mime the words in the current buffer that need
encoding.  The words that need encoding are expected to have
text-properties set with the appropriate characte set.  This would
have been done if the contents of the buffer are the result of a
previous mime decoding."
  (let ((charset nil)
	start coding pos q-encoding
	old-size
	(case-fold-search t)
	(done nil))
    (save-excursion
      (setq start (point-min))
      (while (not done)
	(setq charset (get-text-property start 'vm-charset))
	(setq pos (next-single-property-change start 'vm-charset))
	(or pos (setq pos (point-max) done t))
	(if charset
	    (progn
	      (if (setq coding (get-text-property start 'vm-coding))
		  (progn
		    (setq old-size (buffer-size))
		    (encode-coding-region start pos coding)
		    (setq pos (+ pos (- (buffer-size) old-size)))))
	      (setq pos
		    (+ start
		       (if (setq q-encoding
				 (string-match "^iso-8859-\\|^us-ascii"
					       charset))
			   (vm-mime-Q-encode-region start pos)
			 (vm-mime-B-encode-region start pos))))
	      (goto-char pos)
	      (insert "?=")
	      (setq pos (point))
	      (goto-char start)
	      (insert "=?" charset "?" (if q-encoding "Q" "B") "?")
	      (setq pos (+ pos (- (point) start)))))
	(setq start pos)))))

(defun vm-reencode-mime-encoded-words-in-string (string)
  "Reencode in mime the words in STRING that need
encoding.  The words that need encoding are expected to have
text-properties set with the appropriate characte set.  This would
have been done if the contents of the buffer are the result of a
previous mime decoding."
  (if (and vm-display-using-mime
	   (text-property-any 0 (length string) 'vm-string t string))
      (vm-with-string-as-temp-buffer string 'vm-reencode-mime-encoded-words)
    string ))

;;----------------------------------------------------------------------------
;;; MIME parsing
;;----------------------------------------------------------------------------

(fset 'vm-mime-parse-content-header 'vm-parse-structured-header)

(defun vm-mime-get-header-contents (header-name-regexp)
  (let ((contents nil)
	regexp)
    (setq regexp (concat "^\\(" header-name-regexp "\\)\\|\\(^$\\)"))
    (save-excursion
      (let ((case-fold-search t))
	(if (and (re-search-forward regexp nil t)
		 (match-beginning 1)
		 (progn (goto-char (match-beginning 0))
			(vm-match-header)))
	    (vm-matched-header-contents)
	  nil )))))

(defun* vm-mime-parse-entity (&optional m &key
					(default-type nil)
					(default-encoding nil)
					(passing-message-only nil))
  "Parse a MIME message M and return its mime-layout.
Optional arguments:
DEFAULT-TYPE is the type to use if no Content-Type is specified.
DEFAULT-ENCODING is the default character encoding if none is
  specified in the message.
PASSING-MESSAGE-ONLY is a boolean argument that says that VM is only
  passing through this message.  So, a full analysis is not required.
                                                     (USR, 2010-01-12)"
  (catch 'return-value
    (save-excursion
      (if (and m (not passing-message-only))
	  (progn
	    (setq m (vm-real-message-of m))
	    (set-buffer (vm-buffer-of m))))
      (let ((case-fold-search t) version type qtype encoding id description
	    disposition qdisposition boundary boundary-regexp start end
	    multipart-list pos-list c-t c-t-e done p returnval)
	(save-excursion
	  (save-restriction
	    (if (and m (not passing-message-only))
		(progn
		  (setq version (vm-get-header-contents m "MIME-Version:")
			version (car (vm-parse-structured-header version))
			type (vm-get-header-contents m "Content-Type:")
			version (if (or version
					vm-mime-require-mime-version-header)
				    version
				  (if type "1.0" nil))
			qtype (vm-parse-structured-header type ?\; t)
			type (vm-parse-structured-header type ?\;)
			encoding (vm-get-header-contents
				  m "Content-Transfer-Encoding:")
			version (if (or version
					vm-mime-require-mime-version-header)
				    version
				  (if encoding "1.0" nil))
			encoding (or encoding "7bit")
			encoding (or (car
				      (vm-parse-structured-header encoding))
				     "7bit")
			id (vm-get-header-contents m "Content-ID:")
			id (car (vm-parse-structured-header id))
			description (vm-get-header-contents
				     m "Content-Description:")
			description (and description
					 (if (string-match "^[ \t\n]*$"
							   description)
					     nil
					   description))
			disposition (vm-get-header-contents
				     m "Content-Disposition:")
			qdisposition (and disposition
					  (vm-parse-structured-header
					   disposition ?\; t))
			disposition (and disposition
					 (vm-parse-structured-header
					  disposition ?\;)))
		  (widen)
		  (narrow-to-region (vm-headers-of m) (vm-text-end-of m)))
	      (goto-char (point-min))
	      (setq type (vm-mime-get-header-contents "Content-Type:")
		    qtype (or (vm-parse-structured-header type ?\; t)
			      default-type)
		    type (or (vm-parse-structured-header type ?\;)
			     default-type)
		    encoding (or (vm-mime-get-header-contents
				  "Content-Transfer-Encoding:")
				 default-encoding)
		    encoding (or (car (vm-parse-structured-header encoding))
				 default-encoding)
		    id (vm-mime-get-header-contents "Content-ID:")
		    id (car (vm-parse-structured-header id))
		    description (vm-mime-get-header-contents
				 "Content-Description:")
		    description (and description (if (string-match "^[ \t\n]*$"
								   description)
						     nil
						   description))
		    disposition (vm-mime-get-header-contents
				 "Content-Disposition:")
		    qdisposition (and disposition
				      (vm-parse-structured-header
				       disposition ?\; t))
		    disposition (and disposition
				     (vm-parse-structured-header
				      disposition ?\;))))
	    (cond ((null m) t)
		  (passing-message-only t)
		  ((null version)
		   (throw 'return-value 'none))
		  ((or vm-mime-ignore-mime-version (string= version "1.0")) t)
		  (t (vm-mime-error "Unsupported MIME version: %s" version)))
	    ;; deal with known losers
	    ;; Content-Type: text
	    (cond ((and type (string-match "^text$" (car type)))
		   (setq type '("text/plain" "charset=us-ascii")
			 qtype '("text/plain" "charset=us-ascii"))))
	    (cond ((and m (not passing-message-only) (null type))
		   (throw 'return-value
			  (vm-make-layout
			   'type '("text/plain" "charset=us-ascii")
			   'qtype '("text/plain" "charset=us-ascii")
			   'encoding encoding
			   'id id
			   'description description
			   'disposition disposition
			   'qdisposition qdisposition
			   'header-start (vm-headers-of m)
			   'header-end (vm-marker (1- (vm-text-of m)))
			   'body-start (vm-text-of m)
			   'body-end (vm-text-end-of m)
			   'cache (vm-mime-make-cache-symbol)
			   'message-symbol (vm-mime-make-message-symbol m)
			   )))
		  ((null type)
		   (goto-char (point-min))
		   (or (re-search-forward "^\n\\|\n\\'" nil t)
		       (vm-mime-error "MIME part missing header/body separator line"))
		   (vm-make-layout
		    'type default-type
		    'qtype default-type
		    'encoding encoding
		    'id id
		    'description description
		    'disposition disposition
		    'qdisposition qdisposition
		    'header-start (vm-marker (point-min))
		    'header-body (vm-marker (1- (point)))
		    'body-start (vm-marker (point))
		    'body-end (vm-marker (point-max))
		    'cache (vm-mime-make-cache-symbol)
		    'message-symbol (vm-mime-make-message-symbol m)
		    ))
		  ((null (string-match "[^/ ]+/[^/ ]+" (car type)))
		   (vm-mime-error "Malformed MIME content type: %s"
				  (car type)))
		  ((and (string-match "^multipart/\\|^message/" (car type))
			(null (string-match "^\\(7bit\\|8bit\\|binary\\)$"
					    encoding))
			(if vm-mime-ignore-composite-type-opaque-transfer-encoding
			    (progn
			      ;; Some mailers declare an opaque
			      ;; encoding on a composite type even
			      ;; though it's only a subobject that
			      ;; uses that encoding.  Deal with it
			      ;; by assuming a proper transfer encoding.
			      (setq encoding "binary")
			      ;; return nil so and-clause will fail
			      nil )
			  t ))
		   (vm-mime-error "Opaque transfer encoding used with multipart or message type: %s, %s" (car type) encoding))
		  ((and (string-match "^message/partial$" (car type))
			(null (string-match "^7bit$" encoding)))
		   (vm-mime-error "Non-7BIT transfer encoding used with message/partial message: %s" encoding))
		  ((string-match "^multipart/digest" (car type))
		   (setq c-t '("message/rfc822")
			 c-t-e "7bit"))
		  ((string-match "^multipart/" (car type))
		   (setq c-t '("text/plain" "charset=us-ascii")
			 c-t-e "7bit")) ; below
		  ((string-match "^message/\\(rfc822\\|news\\|external-body\\)"
				 (car type))
		   (setq c-t '("text/plain" "charset=us-ascii")
			 c-t-e "7bit")
		   (goto-char (point-min))
		   (or (re-search-forward "^\n\\|\n\\'" nil t)
		       (vm-mime-error "MIME part missing header/body separator line"))
		   (throw 'return-value
			  (vm-make-layout
			   'type type
			   'qtype qtype
			   'encoding encoding
			   'id id
			   'description description
			   'disposition disposition
			   'qdisposition qdisposition
			   'header-start (vm-marker (point-min))
			   'header-end (vm-marker (1- (point)))
			   'body-start (vm-marker (point))
			   'body-end (vm-marker (point-max))
			   'parts (list
				   (save-restriction
				     (narrow-to-region (point) (point-max))
				     (vm-mime-parse-entity-safe 
				      m :default-type c-t 
				      :default-encoding c-t-e 
				      :passing-message-only t)))
			   'cache (vm-mime-make-cache-symbol)
			   'message-symbol (vm-mime-make-message-symbol m)
			   )))
		  (t
		   (goto-char (point-min))
		   (or (re-search-forward "^\n\\|\n\\'" nil t)
		       (vm-mime-error "MIME part missing header/body separator line"))
		   (throw 'return-value
			  (vm-make-layout
			   'type type
			   'qtype qtype
			   'encoding encoding
			   'id id
			   'description description
			   'disposition disposition
			   'qdisposition qdisposition
			   'header-start (vm-marker (point-min))
			   'header-end (vm-marker (1- (point)))
			   'body-start (vm-marker (point))
			   'body-end (vm-marker (point-max))
			   'cache (vm-mime-make-cache-symbol)
			   'message-symbol (vm-mime-make-message-symbol m)
			   ))))
	    (setq p (cdr type)
		  boundary nil)
	    (while p
	      (if (string-match "^boundary=" (car p))
		  (setq boundary (car (vm-parse (car p) "=\\(.+\\)"))
			p nil)
		(setq p (cdr p))))
	    (or boundary
		(vm-mime-error
		 "Boundary parameter missing in %s type specification"
		 (car type)))
	    ;; the \' in the regexp is to "be liberal" in the
	    ;; face of broken software that does not add a line
	    ;; break after the final boundary of a nested
	    ;; multipart entity.
	    (setq boundary-regexp
		  (concat "^--" (regexp-quote boundary)
			  "\\(--\\)?[ \t]*\\(\n\\|\\'\\)"))
	    (goto-char (point-min))
	    (setq start nil
		  multipart-list nil
		  done nil)
	    (while (and (not done) (re-search-forward boundary-regexp nil 0))
	      (if (null start)
		  (setq start (match-end 0))
		(and (match-beginning 1)
		     (setq done t))
		(setq pos-list (cons start
				     (cons (1- (match-beginning 0)) pos-list))
		      start (match-end 0))))
	    (if (and (not done)
		     (not vm-mime-ignore-missing-multipart-boundary))
		(vm-mime-error "final %s boundary missing" boundary)
	      (if (and start (not done))
		  (setq pos-list (cons start (cons (point) pos-list)))))
	    (setq pos-list (nreverse pos-list))
	    (while pos-list
	      (setq start (car pos-list)
		    end (car (cdr pos-list))
		    pos-list (cdr (cdr pos-list)))
	      (save-excursion
		(save-restriction
		  (narrow-to-region start end)
		  (setq multipart-list
			(cons (vm-mime-parse-entity-safe 
			       m :default-type c-t 
			       :default-encoding c-t-e 
			       :passing-message-only t)
			      multipart-list)))))
	    (goto-char (point-min))
	    (or (re-search-forward "^\n\\|\n\\'" nil t)
		(vm-mime-error "MIME part missing header/body separator line"))
	    (vm-make-layout
	     'type type
	     'qtype qtype
	     'encoding encoding
	     'id id
	     'description description
	     'disposition disposition
	     'qdisposition qdisposition
	     'header-start (vm-marker (point-min))
	     'header-end (vm-marker (1- (point)))
	     'body-start (vm-marker (point))
	     'body-end (vm-marker (point-max))
	     'parts (nreverse multipart-list)
	     'cache (vm-mime-make-cache-symbol)
	     'message-symbol (vm-mime-make-message-symbol m)
	     )))))))

(defun* vm-mime-parse-entity-safe (&optional m &key
					    (default-type nil)
					    (default-encoding nil)
					    (passing-message-only nil))
  "Like vm-mime-parse-entity, but recovers from any errors.
DEFAULT-TYPE, unless specified, is assumed to be text/plain.
DEFAULT-TRANSFER-ENCODING, unless specified, is assumed to be 7bit.
						(USR, 2010-01-12)"

  (or default-type (setq default-type '("text/plain" "charset=us-ascii")))
  (or default-encoding (setq default-encoding "7bit"))
  ;; don't let subpart parse errors make the whole parse fail.  use default
  ;; type if the parse fails.
  (condition-case error-data
      (vm-mime-parse-entity m :default-type default-type 
			    :default-encoding default-encoding 
			    :passing-message-only passing-message-only)
    (vm-mime-error
     (vm-inform 0 "%s" (car (cdr error-data)))
     ;; don't sleep, no one cares about MIME syntax errors
     ;;     (sleep-for 2)
     (let ((header (if (and m (not passing-message-only))
		       (vm-headers-of m)
		     (vm-marker (point-min))))
	   (text (if (and m (not passing-message-only))
		     (vm-text-of m)
		   (save-excursion
		     (re-search-forward "^\n\\|\n\\'"
					nil 0)
		     (vm-marker (point)))))
	   (text-end (if (and m (not passing-message-only))
			 (vm-text-end-of m)
		       (vm-marker (point-max)))))
     (vm-make-layout
      'type '("error/error")
      'qtype '("error/error")
      'encoding (vm-determine-proper-content-transfer-encoding text text-end)
      ;; cram the error message into the description slot
      'description (car (cdr error-data))
      ;; mark as an attachment to improve the chance that the user
      ;; will see the description.
      'disposition '("attachment")
      'qdisposition '("attachment")
      'header-start header
      'header-end (vm-marker (1- text))
      'body-start text
      'body-end text-end
      'cache (vm-mime-make-cache-symbol)
      'message-symbol (vm-mime-make-message-symbol m)
      )))))

;;----------------------------------------------------------------------------
;;; MIME layout operations
;;----------------------------------------------------------------------------

(defun vm-mime-get-xxx-parameter-internal (name param-list)
  "Return the parameter NAME from PARAM-LIST."
  (let ((match-end (1+ (length name)))
	(name-regexp (concat (regexp-quote name) "="))
	(case-fold-search t)
	(done nil))
    (while (and param-list (not done))
      (if (and (string-match name-regexp (car param-list))
	       (= (match-end 0) match-end))
	  (setq done t)
	(setq param-list (cdr param-list))))
    (and (car param-list)
	 (substring (car param-list) match-end))))

(defun vm-mime-get-xxx-parameter (name param-list)
  "Return the parameter NAME from PARAM-LIST.

If parameter value continuations was used, i.e. the parameter was split into
shorter pieces, rebuilt it from them."  
  (or (vm-mime-get-xxx-parameter-internal name param-list)
      (let ((n 0) content p)
        (while (setq p (vm-mime-get-xxx-parameter-internal
                        (format "%s*%d" name n)
                        param-list))
          (setq n (1+ n)
                content (concat content p)))
        content)))

(defun vm-mime-get-parameter (layout param)
  (let ((string (vm-mime-get-xxx-parameter 
		 param (cdr (vm-mm-layout-type layout)))))
    (if string (vm-decode-mime-encoded-words-in-string string))))

(defun vm-mime-get-disposition-parameter (layout param)
  (let ((string (vm-mime-get-xxx-parameter 
		 param (cdr (vm-mm-layout-disposition layout)))))
    (if string (vm-decode-mime-encoded-words-in-string string))))

(defun vm-mime-set-xxx-parameter (param value param-list)
  (let ((match-end (1+ (length param)))
	(param-regexp (concat (regexp-quote param) "="))
	(case-fold-search t)
	(done nil))
    (while (and param-list (not done))
      (if (and (string-match param-regexp (car param-list))
	       (= (match-end 0) match-end))
	  (setq done t)
	(setq param-list (cdr param-list))))
    (and (car param-list)
	 (setcar param-list (concat param "=" value)))))

(defun vm-mime-set-parameter (layout param value)
  (vm-mime-set-xxx-parameter param value (cdr (vm-mm-layout-type layout))))

(defun vm-mime-set-qparameter (layout param value)
  (setq value (concat "\"" value "\""))
  (vm-mime-set-xxx-parameter param value (cdr (vm-mm-layout-qtype layout))))

;;----------------------------------------------------------------------------
;;; Working with MIME layouts
;;----------------------------------------------------------------------------

(defun vm-mime-insert-mime-body (layout)
  "Insert in the current buffer the body of a mime part given by LAYOUT."
  (vm-insert-region-from-buffer 
   (marker-buffer (vm-mm-layout-body-start layout))
   (vm-mm-layout-body-start layout)
   (vm-mm-layout-body-end layout)))

(defun vm-mime-insert-mime-headers (layout)
  "Insert in the current buffer the headers of a mime part given by LAYOUT."
  (vm-insert-region-from-buffer
   (marker-buffer (vm-mm-layout-header-start layout))
   (vm-mm-layout-header-start layout)
   (vm-mm-layout-header-end layout)))

(defvar buffer-display-table)
(defvar standard-display-table)
(defvar buffer-file-type)
(defun vm-make-presentation-copy (m)
  "Create a copy of the message M in the Presentation Buffer.  If
the message is external then the copy is made from the external
source of the message."
  (let ((mail-buffer (current-buffer))
	pres-buf mm
	(real-m (vm-real-message-of m))
	(modified (buffer-modified-p)))
    (cond ((or (null vm-presentation-buffer-handle)
	       (null (buffer-name vm-presentation-buffer-handle)))
	   ;; Create a new Presentation buffer
	   (setq pres-buf (vm-generate-new-multibyte-buffer 
			   (concat (buffer-name) " Presentation")))
	   (save-excursion
	     (set-buffer pres-buf)
	     (if (fboundp 'buffer-disable-undo)
		 (buffer-disable-undo (current-buffer))
	       ;; obfuscation to make the v19 compiler not whine
	       ;; about obsolete functions.
	       (let ((x 'buffer-flush-undo))
		 (funcall x (current-buffer))))
	     (setq mode-name "VM Presentation"
		   major-mode 'vm-presentation-mode
		   vm-message-pointer (list nil)
		   vm-mail-buffer mail-buffer
		   mode-popup-menu (and vm-use-menus
					(vm-menu-support-possible-p)
					(vm-menu-mode-menu))
		   ;; Default to binary file type for DOS/NT.
		   buffer-file-type t
		   ;; Tell XEmacs/MULE not to mess with the text on writes.
		   buffer-read-only t
		   mode-line-format vm-mode-line-format)
	     ;; scroll in place messes with scroll-up and this loses
	     (defvar scroll-in-place)
	     (make-local-variable 'scroll-in-place)
	     (setq scroll-in-place nil)
	     (when (fboundp 'set-buffer-file-coding-system)
	       (set-buffer-file-coding-system (vm-binary-coding-system) t))
	     (vm-fsfemacs-nonmule-display-8bit-chars)
	     (if (and vm-mutable-frames vm-frame-per-folder
		      (vm-multiple-frames-possible-p))
		 (vm-set-hooks-for-frame-deletion))
	     (use-local-map vm-mode-map)
	     (vm-toolbar-install-or-uninstall-toolbar)
	     (when (vm-menu-support-possible-p)
	       (vm-menu-install-menus))
	     (run-hooks 'vm-presentation-mode-hook))
	   (setq vm-presentation-buffer-handle pres-buf)))
    (setq pres-buf vm-presentation-buffer-handle)
    (setq vm-presentation-buffer vm-presentation-buffer-handle)
    (setq vm-mime-decoded nil)
    ;; W3 or some other external mode might set some local colors
    ;; in this buffer; remove them before displaying a different
    ;; message here.
    (when (fboundp 'remove-specifier)
      (remove-specifier (face-foreground 'default) pres-buf)
      (remove-specifier (face-background 'default) pres-buf))
    (save-excursion
      (set-buffer (vm-buffer-of real-m))
      (save-restriction
	(widen)
	;; must reference this now so that headers will be in
	;; their final position before the message is copied.
	;; otherwise the vheader offset computed below will be
	;; wrong.
	(vm-vheaders-of real-m)
	(set-buffer pres-buf)
	;; do not keep undo information in presentation buffers 
	(setq buffer-undo-list t)
	(widen)
	(let ((buffer-read-only nil)
	      (inhibit-read-only t))
	  ;; We don't care about the buffer-modified-p flag of the
	  ;; Presentation buffer.  Only that of the folder matters.
	  ;; (setq modified (buffer-modified-p)) 
	  (unwind-protect
	      (progn
		(erase-buffer)
		(insert-buffer-substring (vm-buffer-of real-m)
					 (vm-start-of real-m)
					 (vm-end-of real-m)))
	    (vm-reset-buffer-modified-p modified pres-buf)))
	;; make a modifiable copy of the message struct
	(setq mm (copy-sequence m))
	;; also a modifiable copy of the location data
	;; other data will be shared with the Folder buffer
	(vm-set-location-data-of mm (vm-copy (vm-location-data-of m)))
	(set-marker (vm-start-of mm) (point-min))
	(set-marker (vm-headers-of mm) (+ (vm-start-of mm)
					  (- (vm-headers-of real-m)
					     (vm-start-of real-m))))
	(set-marker (vm-vheaders-of mm) (+ (vm-start-of mm)
					   (- (vm-vheaders-of real-m)
					      (vm-start-of real-m))))
	(set-marker (vm-text-of mm) (+ (vm-start-of mm)
				       (- (vm-text-of real-m)
					  (vm-start-of real-m))))
	(set-marker (vm-text-end-of mm) (+ (vm-start-of mm)
					   (- (vm-text-end-of real-m)
					      (vm-start-of real-m))))
	(set-marker (vm-end-of mm) (+ (vm-start-of mm)
				      (- (vm-end-of real-m)
					 (vm-start-of real-m))))

	;; fetch the real message now
	(goto-char (point-min))
	(cond ((and (vm-message-access-method-of mm)
		    (vm-body-to-be-retrieved-of mm))
	       ;; Remember that this does process I/O and
	       ;; accept-process-output, allowing concurrent threads
	       ;; to run!!!  USR, 2010-07-11
	       (condition-case err
		   (vm-fetch-message 
		    (list (vm-message-access-method-of mm)) mm)
		 (error
		  (vm-inform 0 "Cannot fetch message; %s" 
			   (error-message-string err)))))
	      ((re-search-forward "^X-VM-Storage: " (vm-text-of mm) t)
	       (vm-fetch-message (read (current-buffer)) mm)))
	;; This might be redundant.  Wasn't in revision 717.
	;; (vm-reset-buffer-modified-p modified (current-buffer)) 
	;; fixup the reference to the message
	(setcar vm-message-pointer mm)))))

;; This experimental code is now discarded.  USR, 2011-05-07

;; (defun vm-make-fetch-copy-if-necessary (m)
;;   "Create a copy of the message M in the Fetch Buffer if it is
;; not already present.  If it is an external message, the copy
;; is made from the external source of the message."
;;   (unless (and vm-fetch-buffer
;; 	       (eq (vm-real-message-sym-of m)
;; 		   (with-current-buffer vm-fetch-buffer
;; 		     (vm-real-message-sym-of (car vm-message-pointer)))))
;;     (vm-make-fetch-copy m)))


;; (defun vm-make-fetch-copy (m)
;;   "Create a copy of the message M in the Fetch Buffer.  If
;; it is an external message, the copy is made from the external
;; source of the message."
;;   (let ((mail-buffer (current-buffer))
;; 	fetch-buf mm
;; 	(real-m (vm-real-message-of m))
;; 	(modified (buffer-modified-p)))
;;     (cond ((or (null vm-fetch-buffer)
;; 	       (null (buffer-name vm-fetch-buffer)))
;; 	   (setq fetch-buf (vm-generate-new-multibyte-buffer 
;; 			    (concat (buffer-name) " Fetch")))
;; 	   (save-excursion
;; 	     (set-buffer fetch-buf)
;; 	     (if (fboundp 'buffer-disable-undo)
;; 		 (buffer-disable-undo (current-buffer))
;; 	       ;; obfuscation to make the v19 compiler not whine
;; 	       ;; about obsolete functions.
;; 	       (let ((x 'buffer-flush-undo))
;; 		 (funcall x (current-buffer))))
;; 	     (setq mode-name "VM Message"
;; 		   major-mode 'vm-message-mode
;; 		   vm-message-pointer (list nil)
;; 		   vm-mail-buffer mail-buffer
;; 		   mode-popup-menu (and vm-use-menus
;; 					(vm-menu-support-possible-p)
;; 					(vm-menu-mode-menu))
;; 		   ;; Default to binary file type for DOS/NT.
;; 		   buffer-file-type t
;; 		   ;; Tell XEmacs/MULE not to mess with the text on writes.
;; 		   buffer-read-only t
;; 		   mode-line-format vm-mode-line-format)
;; 	     ;; scroll in place messes with scroll-up and this loses
;; 	     (defvar scroll-in-place)
;; 	     (make-local-variable 'scroll-in-place)
;; 	     (setq scroll-in-place nil)
;; 	     (if (fboundp 'set-buffer-file-coding-system)
;; 		 (set-buffer-file-coding-system (vm-binary-coding-system) t))
;; 	     (vm-fsfemacs-nonmule-display-8bit-chars)
;; 	     (if (and vm-mutable-frames vm-frame-per-folder
;; 		      (vm-multiple-frames-possible-p))
;; 		 (vm-set-hooks-for-frame-deletion))
;; 	     (use-local-map vm-mode-map)
;; 	     (vm-toolbar-install-or-uninstall-toolbar)
;; 	     (when (vm-menu-support-possible-p)
;; 	       (vm-menu-install-menus))
;; 	     (run-hooks 'vm-message-mode-hook))
;; 	   (setq vm-fetch-buffer fetch-buf)))
;;     (setq fetch-buf vm-fetch-buffer)
;;     (setq vm-mime-decoded nil)
;;     ;; W3 or some other external mode might set some local colors
;;     ;; in this buffer; remove them before displaying a different
;;     ;; message here.
;;     (if (fboundp 'remove-specifier)
;; 	(progn
;; 	  (remove-specifier (face-foreground 'default) fetch-buf)
;; 	  (remove-specifier (face-background 'default) fetch-buf)))
;;     (save-excursion
;;       (set-buffer (vm-buffer-of real-m))
;;       (save-restriction
;; 	(widen)
;; 	;; must reference this now so that headers will be in
;; 	;; their final position before the message is copied.
;; 	;; otherwise the vheader offset computed below will be
;; 	;; wrong.
;; 	(vm-vheaders-of real-m)
;; 	(set-buffer fetch-buf)
;; 	;; do not keep undo information in message buffers 
;; 	(setq buffer-undo-list t)
;; 	(widen)
;; 	(let ((buffer-read-only nil)
;; 	      (inhibit-read-only t))
;; 	  ;; (setq modified (buffer-modified-p)) ; why this? USR, 2011-03-18
;; 	  (unwind-protect
;; 	      (progn
;; 		(erase-buffer)
;; 		(insert-buffer-substring (vm-buffer-of real-m)
;; 					 (vm-start-of real-m)
;; 					 (vm-end-of real-m)))
;; 	    (vm-restore-buffer-modified-p modified fetch-buf)))
;; 	(setq mm (copy-sequence m))
;; 	(vm-set-location-data-of mm (vm-copy (vm-location-data-of m)))
;; 	(vm-set-softdata-of mm (vm-copy (vm-softdata-of m)))
;; 	(vm-set-message-id-number-of mm 1)
;; 	(vm-set-buffer-of mm (current-buffer))
;; 	(set-marker (vm-start-of mm) (point-min))
;; 	(set-marker (vm-headers-of mm) (+ (vm-start-of mm)
;; 					  (- (vm-headers-of real-m)
;; 					     (vm-start-of real-m))))
;; 	(set-marker (vm-vheaders-of mm) (+ (vm-start-of mm)
;; 					   (- (vm-vheaders-of real-m)
;; 					      (vm-start-of real-m))))
;; 	(set-marker (vm-text-of mm) (+ (vm-start-of mm)
;; 				       (- (vm-text-of real-m)
;; 					  (vm-start-of real-m))))
;; 	(set-marker (vm-text-end-of mm) (+ (vm-start-of mm)
;; 					   (- (vm-text-end-of real-m)
;; 					      (vm-start-of real-m))))
;; 	(set-marker (vm-end-of mm) (+ (vm-start-of mm)
;; 				      (- (vm-end-of real-m)
;; 					 (vm-start-of real-m))))
;; 	(vm-set-mime-layout-of mm (vm-mime-parse-entity-safe))
;; 	;; fetch the real message now
;; 	(goto-char (point-min))
;; 	(cond ((and (vm-message-access-method-of mm)
;; 		    (vm-body-to-be-retrieved-of mm))
;; 	       ;; Remember that this does process I/O and
;; 	       ;; accept-process-output, and hence allow concurrent
;; 	       ;; threads to run!!!  USR, 2010-07-11  
;; 	       (condition-case err
;; 		   (vm-fetch-message 
;; 		    (list (vm-message-access-method-of mm)) mm)
;; 		 (error
;; 		  (vm-inform 0 "Cannot fetch; %s" (error-message-string err)))))
;; 	      ((re-search-forward "^X-VM-Storage: " (vm-text-of mm) t)
;; 	       (vm-fetch-message (read (current-buffer)) mm)))
;; 	(vm-reset-buffer-modified-p modified fetch-buf)
;; 	;; fixup the reference to the message
;; 	(setcar vm-message-pointer mm)))))

(defun vm-fetch-message (storage mm)
  "Fetch the real message based on the \"^X-VM-Storage:\" header.

This allows for storing only the headers required for the summary
and maybe a small preview of the message, or keywords for search,
etc.  Only when displaying it the actual message is fetched based
on the storage handler.

The information about the actual message is stored in the
\"^X-VM-Storage:\" header and should be a lisp list of the
following format.

    \(HANDLER ARGS...\)

HANDLER should correspond to a `vm-fetch-HANDLER-message'
function, e.g., the handler `file' corresponds to the function
`vm-fetch-file-message' which gets two arguments, the message
descriptor and the filename containing the message, and inserts the
message body from the file into the current buffer. 

For example, 'X-VM-Storage: (file \"message-11\")' will fetch 
the actual message from the file \"message-11\"."
  (goto-char (match-end 0))
  (save-excursion
    (set-buffer (marker-buffer (vm-text-of mm)))
    (let ((buffer-read-only nil)
	  (inhibit-read-only t)
	  (buffer-undo-list t)
	  (fetch-result nil))
      (goto-char (vm-text-of mm))
      (delete-region (point) (point-max))
      ;; Remember that this might do process I/O and accept-process-output,
      ;; allowing other threads to run!!!  USR, 2010-07-11 
      (vm-inform 6 "Fetching message from external source...")
      (setq fetch-result
	    (apply (intern (format "vm-fetch-%s-message" (car storage)))
		   mm (cdr storage)))
      (when fetch-result
	(vm-inform 6 "Fetching message from external source... done")
	;; delete the new headers
	(delete-region (vm-text-of mm)
		       (or (re-search-forward "\n\n" (point-max) t)
			   (point-max)))
	;; fix markers now
	(set-marker (vm-text-end-of mm) (point-max))	
	(set-marker (vm-end-of mm) (point-max))
	;; now care for the layout of the message, old layouts are
	;; invalid as the presentation buffer may have been used for
	;; other messages in the meantime and the marker got invalid
	;; by this.
	(vm-set-mime-layout-of mm (vm-mime-parse-entity-safe))
	))))
  
(defun vm-fetch-file-message (m filename)
  "Insert the message with message descriptor MM stored in the given FILENAME."
  (insert-file-contents filename nil nil nil t)
  t)

(fset 'vm-fetch-mode 'vm-mode)
(put 'vm-fetch-mode 'mode-class 'special)
(fset 'vm-presentation-mode 'vm-mode)
(put 'vm-presentation-mode 'mode-class 'special)

(defvar buffer-file-coding-system)

(defun vm-determine-proper-charset (beg end)
  "Work out what MIME character set to use for sending a message.

Uses `us-ascii' if the message is entirely ASCII compatible.  If MULE is not
available, and the message contains contains non-ASCII characters, consults
the variable `vm-mime-8bit-composition-charset' or uses `iso-8859-1.' if
that is nil.

Under MULE, `vm-coding-system-priorities' is searched, in order, for a coding
system that will encode all the characters in the message. If none is found,
consults the variable `vm-mime-8bit-composition-charset' or uses `iso-2022-jp',
which will preserve information for all the character sets of which Emacs is
aware - at the expense of being incompatible with the recipient's software, if
that recipient is outside of East Asia."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
	(if (or vm-xemacs-mule-p
		(and vm-fsfemacs-mule-p enable-multibyte-characters))
	    ;; Okay, we're on a MULE build.
	  (if (and vm-fsfemacs-mule-p
		   (fboundp 'check-coding-systems-region))
	      ;; check-coding-systems-region appeared in GNU Emacs 23.
	      (let* ((preapproved (vm-get-coding-system-priorities))
		     (ucs-list (vm-get-mime-ucs-list))
		     (cant-encode (check-coding-systems-region
				   (point-min) (point-max)
				   (cons 'us-ascii preapproved))))
		(if (not (assq 'us-ascii cant-encode))
		    ;; If there are only ASCII chars, we're done.
		    "us-ascii"
		  (while (and preapproved
			      (assq (car preapproved) cant-encode)
			      (not (memq (car preapproved) ucs-list)))
		    (setq preapproved (cdr preapproved)))
		  (if preapproved
		      (cadr (assq (car preapproved)
				  vm-mime-mule-coding-to-charset-alist))
		    ;; None of the entries in vm-coding-system-priorities
		    ;; can be used. This can only happen if no universal
		    ;; coding system is included. Fall back to utf-8.
		    "utf-8")))

	    (let ((charsets (delq 'ascii
				  (vm-charsets-in-region (point-min)
							 (point-max)))))
	      (cond
	       ;; No non-ASCII chars? Right, that makes it easy for us.
	       ((null charsets) "us-ascii")

	       ;; Check whether the buffer can be encoded using one of the
	       ;; vm-coding-system-priorities coding systems.
	       ((catch 'done

		  ;; We can't really do this intelligently unless latin-unity
		  ;; is available.
		  (if (featurep 'latin-unity)
		      (let ((csetzero charsets)
			    ;; Check what latin character sets are in the
			    ;; buffer.
			    (csets (latin-unity-representations-feasible-region
				    beg end))
			    (psets (latin-unity-representations-present-region
				    beg end))
			    (systems (vm-get-coding-system-priorities)))

			;; If one of the character sets is outside of latin
			;; unity's remit, check for a universal character
			;; set in vm-coding-system-priorities, and pass back
			;; the first one.
			;;
			;; Otherwise, there's no remapping that latin unity
			;; can do for us, and we should default to something
			;; iso-2022 based. (Since we're not defaulting to
			;; Unicode, at the moment.)

			(while csetzero
			  (if (not (memq 
				    (car csetzero) latin-unity-character-sets))
			      (let ((ucs-list (vm-get-mime-ucs-list))
				    (preapproved
				     (vm-get-coding-system-priorities)))
				(while preapproved
				  (if (memq (car preapproved) ucs-list)
				      (throw 'done 
					     (car (cdr (assq (car preapproved)
				      vm-mime-mule-coding-to-charset-alist)))))
				  (setq preapproved (cdr preapproved)))
				;; Nothing universal in the preapproved list.
				(throw 'done nil)))
			  (setq csetzero (cdr csetzero)))

			;; Okay, we're able to remap using latin-unity. Do so.
			(while systems
			  (let ((sys (latin-unity-massage-name (car systems)
					       'buffer-default)))
			    (when (latin-unity-maybe-remap (point-min) 
							   (point-max) sys 
							   csets psets t)
			      (throw 'done
				     (second (assq sys
				    vm-mime-mule-coding-to-charset-alist)))))
			  (setq systems (cdr systems)))
			(throw 'done nil))

		    ;; Right, latin-unity isn't available.  If there's only
		    ;; one non-ASCII character set in the region, and the
		    ;; corresponding coding system is on the preapproved
		    ;; list before the first universal character set, pass
		    ;; it back. Otherwise, if a universal character set is
		    ;; on the preapproved list, pass the first one of them
		    ;; back. Otherwise, pass back nil and use the
		    ;; "iso-2022-jp" entry below.

		    (let ((csetzero charsets)
			  (preapproved (vm-get-coding-system-priorities))
			  (ucs-list (vm-get-mime-ucs-list)))
		      (if (null (cdr csetzero))
			  (while preapproved
			    ;; If we encounter a universal character set on
			    ;; the preapproved list, pass it back.
			    (if (memq (car preapproved) ucs-list)
				(throw 'done
				       (second (assq (car preapproved)
				     vm-mime-mule-coding-to-charset-alist))))

			    ;; The preapproved entry isn't universal. Check if
			    ;; it's related to the single non-ASCII MULE
			    ;; charset in the buffer (that is, if the
			    ;; conceptually unordered MULE list of characters
			    ;; is based on a corresponding ISO character set,
			    ;; and thus the ordered ISO character set can
			    ;; encode all the characters in the MIME charset.)
			    ;;
			    ;; The string equivalence test is used because we
			    ;; don't have another mapping that is useful
			    ;; here. Nnngh.

			    (if (string=
				 (car (cdr (assoc (car csetzero)
				   vm-mime-mule-charset-to-charset-alist)))
				 (car (cdr (assoc (car preapproved)
				   vm-mime-mule-coding-to-charset-alist))))
				(throw 'done
				       (car (cdr (assoc (car csetzero)
				    vm-mime-mule-charset-to-charset-alist)))))
			    (setq preapproved (cdr preapproved)))

			;; Okay, there's more than one MULE character set in
			;; the buffer. Check for a universal entry in the
			;; preapproved list; if it exists pass it back,
			;; otherwise fall through to the iso-2022-jp below,
			;; because nothing on the preapproved list is
			;; appropriate.

			(while preapproved
			    ;; If we encounter a universal character set on
			    ;; the preapproved list, pass it back.
			    (when (memq (car preapproved) ucs-list)
			      (throw 'done
				     (second (assq (car preapproved)
				     vm-mime-mule-coding-to-charset-alist))))
			    (setq preapproved (cdr preapproved)))))
		    (throw 'done nil))))
	       ;; Couldn't do any magic with vm-coding-system-priorities. Pass
	       ;; back a Japanese iso-2022 MIME character set.
	       (t "iso-2022-jp")
	       ;; Undo the change made in revisin 493
	       ;; (t (or vm-mime-8bit-composition-charset "iso-2022-jp"))
	       ;;    -- 
	       )))
	  ;; If we're non-MULE and there are eight bit characters, use a
	  ;; sensible default.
	  (goto-char (point-min))
	  (if (re-search-forward "[^\000-\177]" nil t)
              (or vm-mime-8bit-composition-charset "iso-8859-1")
	  ;; We're non-MULE and there are purely 7bit characters in the
	  ;; region. Return vm-mime-7bit-c-c.
	  vm-mime-7bit-composition-charset)))))

(defun vm-determine-proper-content-transfer-encoding (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (catch 'done
	(goto-char (point-min))
	(and (re-search-forward "[\000\015]" nil t)
	     (throw 'done "binary"))

	(let ((toolong nil) bol)
	  (goto-char (point-min))
	  (setq bol (point))
	  (while (and (not (eobp)) (not toolong))
	    (forward-line)
	    (setq toolong (> (- (point) bol) 998)
		  bol (point)))
	  (and toolong (throw 'done "binary")))
	 
	(goto-char (point-min))
	(and (re-search-forward "[^\000-\177]" nil t)
	     (throw 'done "8bit"))

	"7bit"))))

;;----------------------------------------------------------------------------
;;; Predicates on MIME types and layouts
;;----------------------------------------------------------------------------

(defun vm-mime-types-match (type type/subtype)
  (let ((case-fold-search t))
    (cond ((null type/subtype)
           nil)
          ((string-match "/" type)
	   (if (and (string-match (regexp-quote type) type/subtype)
		    (equal 0 (match-beginning 0))
		    (equal (length type/subtype) (match-end 0)))
	       t
	     nil ))
	  ((and (string-match (regexp-quote type) type/subtype)
		(equal 0 (match-beginning 0))
		(equal (save-match-data
			 (string-match "/" type/subtype (match-end 0)))
		       (match-end 0)))))))

(defvar native-sound-only-on-console)

(defun vm-mime-text/html-handler ()
  (if (eq vm-mime-text/html-handler 'auto-select)
      (setq vm-mime-text/html-handler
            (cond ((locate-library "w3m")
                   'emacs-w3m)
                  ((locate-library "w3")
                   'w3)
                  ((executable-find "w3m")
                   'w3m)
                  ((executable-find "lynx")
                   'lynx)))
    vm-mime-text/html-handler))

(defun vm-mime-can-display-internal (layout &optional deep)
  (let ((type (car (vm-mm-layout-type layout))))
    (cond ((vm-mime-types-match "image/jpeg" type)
	   (and (vm-image-type-available-p 'jpeg) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/gif" type)
	   (and (vm-image-type-available-p 'gif) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/png" type)
	   (and (vm-image-type-available-p 'png) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/tiff" type)
	   (and (vm-image-type-available-p 'tiff) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/xpm" type)
	   (and (vm-image-type-available-p 'xpm) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/pbm" type)
	   (and (vm-image-type-available-p 'pbm) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/xbm" type)
	   (and (vm-image-type-available-p 'xbm) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "audio/basic" type)
	   (and vm-xemacs-p
		(or (featurep 'native-sound)
		    (featurep 'nas-sound))
		(or (device-sound-enabled-p)
		    (and (featurep 'native-sound)
			 (not native-sound-only-on-console)
			 (memq (vm-device-type) '(x gtk))))))
	  ((vm-mime-types-match "multipart" type) t)
	  ((vm-mime-types-match "message/external-body" type)
	   (or (not deep)
	       (vm-mime-can-display-internal
		(car (vm-mm-layout-parts layout)) t)))
	  ((vm-mime-types-match "message" type) t)
	  ((vm-mime-types-match "text/html" type)
	   ;; Allow vm-mime-text/html-handler to decide if text/html parts are displayable:
           (and (vm-mime-text/html-handler)
		(let ((charset (or (vm-mime-get-parameter layout "charset")
				   "us-ascii")))
		  (vm-mime-charset-internally-displayable-p charset))))
	  ((vm-mime-types-match "text" type)
	   (let ((charset (or (vm-mime-get-parameter layout "charset")
			      "us-ascii")))
	     (or (vm-mime-charset-internally-displayable-p charset)
		 (vm-mime-can-convert-charset charset))))
	  (t nil))))

(defun vm-mime-can-convert (type)
  "If given mime TYPE is convertible to some other type, return a
triple (source-type target-type command).  Otherwise, return nil."
  (or (vm-mime-can-convert-0 type vm-mime-type-converter-alist)
      (vm-mime-can-convert-0 type vm-mime-image-type-converter-alist)))

(defun vm-mime-can-convert-0 (type alist)
  (let (
	;; fake layout. make it the wrong length so an error will
	;; be signaled if vm-mime-can-display-internal ever asks
	;; for one of the other fields
	(fake-layout (make-vector 1 (list nil)))
	best second-best)
    (while (and alist (not best))
      (cond ((and (vm-mime-types-match (car (car alist)) type)
		  (not (vm-mime-types-match (nth 1 (car alist)) type)))
	     (cond ((and (not best)
			 (progn
			   (setcar (aref fake-layout 0) (nth 1 (car alist)))
			   (vm-mime-can-display-internal fake-layout)))
		    (setq best (car alist)))
		   ((and (not second-best)
			 (vm-mime-find-external-viewer (nth 1 (car alist))))
		    (setq second-best (car alist))))))
      (setq alist (cdr alist)))
    (or best second-best)))

(defun vm-mime-convert-undisplayable-layout (layout)
  (catch 'done
    (let ((ooo (vm-mime-can-convert (car (vm-mm-layout-type layout))))
	  ex work-buffer)
      (vm-inform 6 "Converting %s to %s..."
	       (car (vm-mm-layout-type layout))
	       (nth 1 ooo))
      (setq work-buffer (vm-make-work-buffer " *mime object*"))
      (vm-register-message-garbage 'kill-buffer work-buffer)
      (with-current-buffer work-buffer
	;; call-process-region calls write-region.
	;; don't let it do CR -> LF translation.
	(setq selective-display nil)
	(vm-mime-insert-mime-body layout)
	(vm-mime-transfer-decode-region layout (point-min) (point-max))
        ;; It is annoying to use cat for conversion of a mime type which
        ;; is just plain text.  Therefore we do not call it ...
        (setq ex 0)
        (if (= (length ooo) 2)
            (if (search-forward-regexp "\n\n" (point-max) t)
                (delete-region (point-min) (match-beginning 0)))
	  ;; it is arguable that if the type to be converted is text,
	  ;; we should convert from the object's native encoding to
	  ;; the default encoding. However, converting from text is
	  ;; likely to be rare, so we'll have that argument another
	  ;; time.  JCB, 2011-02-04
	  (let ((coding-system-for-write (vm-binary-coding-system))
		(coding-system-for-read (vm-binary-coding-system)))
	    (setq ex (call-process-region 
		      (point-min) (point-max) shell-file-name
		      t t nil shell-command-switch (nth 2 ooo)))))
	(unless (eq ex 0)
	  (switch-to-buffer work-buffer)
	  (vm-warn 0 5 
		   "Conversion from %s to %s failed (exit code %s)"
		   (car (vm-mm-layout-type layout)) (nth 1 ooo) ex)
	  (throw 'done nil))
	(goto-char (point-min))
	;; if the to-type is text, then we will assume that the conversion
	;; process outputs text in the default encoding.
	;; Really we ought to look at process-coding-system-alist etc,
	;; but I suspect that this is rarely used, and will become even
	;; less used as utf-8 becomes universal.  JCB, 2011-02-04
	;; But we will let detect-coding-region do as much work as it
	;; can.  USR, 2011-02-11
	(let* ((charset (vm-mime-find-charset-for-binary-buffer)))
	  (insert "Content-Type: " 
		  (vm-mime-type-with-params
		   (nth 1 ooo) 
		   (and (vm-mime-types-match "text" (nth 1 ooo))
			(list (concat "charset=" charset))))
		  "\n")
	  (insert "Content-Transfer-Encoding: binary\n\n")
	  (set-buffer-modified-p nil)
	  (vm-inform 6 "Converting %s to %s... done"
		   (car (vm-mm-layout-type layout))
		   (nth 1 ooo))
	  ;; irritatingly, we need to set the coding system here as well
	  (vm-make-layout
	   'type
	   (append (list (nth 1 ooo))
		   (append (cdr (vm-mm-layout-type layout))
			   (if (vm-mime-types-match "text" (nth 1 ooo))
			       (list (concat 
				      "charset=" charset)))))
	   'qtype
	   (append (list (nth 1 ooo)) (cdr (vm-mm-layout-type layout)))
	   'encoding "binary"
	   'id (vm-mm-layout-id layout)
	   'description (vm-mm-layout-description layout)
	   'disposition (vm-mm-layout-disposition layout)
	   'qdisposition (vm-mm-layout-qdisposition layout)
	   'header-start (vm-marker (point-min))
	   'header-end (vm-marker (1- (point)))
	   'body-start (vm-marker (point))
	   'body-end (vm-marker (point-max))
	   'parts nil
	   'cache (vm-mime-make-cache-symbol)
	   'message-symbol
	   (vm-mime-make-message-symbol (vm-mm-layout-message layout))
	   'display-error nil 
	   'layout-is-converted t ))))))

(defun vm-mime-find-charset-for-binary-buffer ()
  "Finds an appropriate MIME character set for the current buffer,
assuming that it is text."
  (let ((coding-systems (detect-coding-region (point-min) (point-max)))
	(coding-system nil) (n nil))
    ;; XEmacs returns a single coding-system sometimes
    (unless (listp coding-systems)
      (setq coding-systems (list coding-systems)))
    ;; Skip over the uninformative coding-systems
    (setq n
	  (vm-find coding-systems
		   (function 
		    (lambda (coding)
		      (and coding
			   (not (memq (vm-coding-system-name-no-eol coding)
				      '(raw-text no-conversion))))))))
    (when n
      (setq coding-system (nth n coding-systems)))
    ;; If no informative coding-system detected then use the default
    ;; buffer-file-coding-system 
    (when (or (null coding-system)
	      (eq (vm-coding-system-name-no-eol coding-system) 'undecided))
      (setq coding-system buffer-file-coding-system))
    (or (cadr (assq (vm-coding-system-name-no-eol coding-system)
		    vm-mime-mule-coding-to-charset-alist))
	"us-ascii")))
    

(defun vm-mime-can-convert-charset (charset)
  (vm-mime-can-convert-charset-0 charset vm-mime-charset-converter-alist))

(defun vm-mime-can-convert-charset-0 (charset alist)
  (let ((done nil))
    (while (and alist (not done))
      (cond ((and (vm-string-equal-ignore-case (car (car alist)) charset)
		  (vm-mime-charset-internally-displayable-p
		   (nth 1 (car alist))))
	     (setq done t))
	    (t (setq alist (cdr alist)))))
    (and alist (car alist))))

;; This function from VM 7.19 is not being used anywhere.  However,
;; see vm-mime-charset-convert-region for similar functionality.  
;; 						   USR, 2011-02-11
(defun vm-mime-convert-undisplayable-charset (layout)
  (let ((charset (vm-mime-get-parameter layout "charset"))
	ooo work-buffer)
    (setq ooo (vm-mime-can-convert-charset charset))
    (vm-inform 6 "Converting charset %s to %s..."
	     charset
	     (nth 1 ooo))
    (save-excursion
      (setq work-buffer (vm-make-work-buffer " *mime object*"))
      (vm-register-message-garbage 'kill-buffer work-buffer)
      (set-buffer work-buffer)
      ;; call-process-region calls write-region.
      ;; don't let it do CR -> LF translation.
      (setq selective-display nil)
      (vm-mime-insert-mime-body layout)
      (vm-mime-transfer-decode-region layout (point-min) (point-max))
      (call-process-region (point-min) (point-max) shell-file-name
			   t t nil shell-command-switch (nth 2 ooo))
      (setq layout
	    (vm-make-layout
	     'type (copy-sequence (vm-mm-layout-type layout))
	     'qtype (copy-sequence (vm-mm-layout-type layout))
	     'encoding "binary"
	     'id (vm-mm-layout-id layout)
	     'description (vm-mm-layout-description layout)
	     'disposition (vm-mm-layout-disposition layout)
	     'qdisposition (vm-mm-layout-qdisposition layout)
	     'header-start (vm-marker (point-min))
	     'header-body (vm-marker (1- (point)))
	     'body-start (vm-marker (point))
	     'body-end (vm-marker (point-max))
	     'cache (vm-mime-make-cache-symbol)
	     'message-symbol (vm-mime-make-message-symbol
			      (vm-mm-layout-message layout))
	     'layout-is-converted t
	     'onconverted-layout layout
	     ))
      (vm-mime-set-parameter layout "charset" (nth 1 ooo))
      (vm-mime-set-qparameter layout "charset" (nth 1 ooo))
      (goto-char (point-min))
      (let ((vm-mime-avoid-folding-content-type t)) ; maybe no need
	(insert-before-markers "Content-Type: " 
			       (vm-mime-type-with-params
				(car (vm-mm-layout-type layout))
				(cdr (vm-mm-layout-type layout)))
			       "\n"))
      (insert-before-markers "Content-Transfer-Encoding: binary\n\n")
      (set-buffer-modified-p nil)
      (vm-inform 6 "Converting charset %s to %s... done"
	       charset
	       (nth 1 ooo))
      layout)))

(defun vm-mime-charset-convert-region (charset b-start b-end)
  (let ((b (current-buffer))
	start end oldsize work-buffer ooo ex)
    (setq ooo (vm-mime-can-convert-charset charset))
    (setq work-buffer (vm-make-work-buffer " *mime object*"))
    (unwind-protect
	(with-current-buffer work-buffer
	  (setq oldsize (- b-end b-start))
	  (set-buffer work-buffer)
	  (insert-buffer-substring b b-start b-end)
	  ;; call-process-region calls write-region.
	  ;; don't let it do CR -> LF translation.
	  (setq selective-display nil)
	  (let ((coding-system-for-write (vm-binary-coding-system))
		(coding-system-for-read (vm-binary-coding-system)))
	    (setq ex (call-process-region 
		      (point-min) (point-max) shell-file-name
		      t t nil shell-command-switch (nth 2 ooo))))
	  (unless (eq ex 0)
	    (vm-warn 0 1 "Conversion from %s to %s signalled exit code %s"
		     (nth 0 ooo) (nth 1 ooo) ex))
	  ;; This cannot possibly safe.  USR, 2011-02-11
	  ;; (if vm-fsfemacs-mule-p 
	  ;;    (set-buffer-multibyte t))
	  (setq start (point-min) end (point-max))
	  (with-current-buffer b
	    (save-excursion
	      (goto-char b-start)
	      (insert-buffer-substring work-buffer start end)
	      (delete-region (point) (+ (point) oldsize))))
	  (nth 1 ooo))
      ;; unwind-protection
      (when work-buffer (kill-buffer work-buffer)))))

(defun* vm-mime-should-display-button (layout &key
					      (honor-content-disposition t))
  "Checks whether MIME object with LAYOUT should be displayed as
a button.  Optional keyword argument HONOR-CONTENT-DISPOSITION
says whether the Content-Disposition header of the MIME object
should be honored (default t).  The global setting of
`vm-mime-honor-content-disposition' also has this effect."
  ;; Karnaugh map analysis shows that
  ;; - all objects that are not auto-displayed should be buttons
  ;; - attachment disposition objects are displayed as buttons only if
  ;;   honor-content-disposition is non-nil
  (let ((type (car (vm-mm-layout-type layout)))
	(disposition (car (vm-mm-layout-disposition layout))))
    (setq disposition (and disposition (downcase disposition)))
    (setq honor-content-disposition 
	  (and honor-content-disposition vm-mime-honor-content-disposition))
    (cond ((vm-mime-types-match "multipart" type)
	   nil)
	  ((and disposition
		(or (eq honor-content-disposition t)
		    (and (eq honor-content-disposition 'internal-only)
			 (vm-mime-should-display-internal layout))))
	   (equal disposition "attachment"))
	  (t (not (vm-mime-should-auto-display layout))))))

(defun vm-mime-should-auto-display (layout)
  (let ((type (car (vm-mm-layout-type layout))))
    (and (or (eq vm-mime-auto-displayed-content-types t)
	     (vm-find (cons "multipart" vm-mime-auto-displayed-content-types)
		      (lambda (i) (vm-mime-types-match i type))))
	 (not (vm-find vm-mime-auto-displayed-content-type-exceptions
		       (lambda (i) (vm-mime-types-match i type)))))))

(defun vm-mime-should-display-internal (layout)
  (let ((type (car (vm-mm-layout-type layout))))
    (if (or (eq vm-mime-internal-content-types t)
	    (vm-find (cons "multipart" vm-mime-internal-content-types)
		     (lambda (i)
		       (vm-mime-types-match i type))))
	(not (vm-find vm-mime-internal-content-type-exceptions
		      (lambda (i)
			(vm-mime-types-match i type))))
      nil)))

(defun vm-mime-find-external-viewer (type)
  (catch 'done
    (let ((list vm-mime-external-content-type-exceptions)
	  (matched nil))
      (while list
	(if (vm-mime-types-match (car list) type)
	    (throw 'done nil)
	  (setq list (cdr list))))
      (setq list vm-mime-external-content-types-alist)
      (while (and list (not matched))
	(if (and (vm-mime-types-match (car (car list)) type)
		 (cdr (car list)))
	    (setq matched (cdr (car list)))
	  (setq list (cdr list))))
      matched )))
(fset 'vm-mime-can-display-external 'vm-mime-find-external-viewer)

(defun vm-mime-delete-button-maybe (extent)
  (let ((buffer-read-only))
    ;; if displayed MIME object should replace the button
    ;; remove the button now.
    (cond ((vm-extent-property extent 'vm-mime-disposable)
	   (delete-region (vm-extent-start-position extent)
			  (vm-extent-end-position extent))
	   (vm-detach-extent extent)))))

;;------------------------------------------------------------------------------
;;; MIME decoding
;;------------------------------------------------------------------------------


;;;###autoload
(defun vm-decode-mime-message (&optional state)
  "Decode the MIME objects in the current message.

The first time this command is run on a message, decoding is done.
The second time, buttons for all the objects are displayed instead.
The third time, the raw, undecoded data is displayed.

The optional argument STATE can specify which decode state to display:
'decoded, 'button or 'undecoded.

If decoding, the decoded objects might be displayed immediately, or
buttons might be displayed that you need to activate to view the
object.  See the documentation for the variables

    vm-mime-auto-displayed-content-types
    vm-mime-auto-displayed-content-type-exceptions
    vm-mime-internal-content-types
    vm-mime-internal-content-type-exceptions
    vm-mime-external-content-types-alist

to see how to control whether you see buttons or objects.

If the variable vm-mime-display-function is set, then its value
is called as a function with no arguments, and none of the
actions mentioned in the preceding paragraphs are taken.  At the
time of the call, the current buffer will be the presentation
buffer for the folder and a copy of the current message will be
in the buffer.  The function is expected to make the message
`MIME presentable' to the user in whatever manner it sees fit."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (interactive-p))
  (unless (or vm-display-using-mime vm-mime-display-function)
      (error "MIME display disabled, set vm-display-using-mime non-nil to enable."))
  (if vm-mime-display-function
      (progn
	(vm-make-presentation-copy (car vm-message-pointer))
	(set-buffer vm-presentation-buffer)
	(funcall vm-mime-display-function)
	;; We are done here
	)
    (when (null state)
      (cond ((null vm-mime-decoded)
	     (setq state 'decoded))
	    ((eq vm-mime-decoded 'decoded)
	     (setq state 'buttons))
	    ((eq vm-mime-decoded 'buttons)
	     (setq state 'undecoded))))
    (if vm-mime-decoded
	(cond ((eq state 'buttons)
	       (let ((vm-preview-lines nil)
		     (vm-auto-decode-mime-messages t)
		     (vm-mime-honor-content-disposition nil)
		     (vm-mime-auto-displayed-content-types '("multipart"))
		     (vm-mime-auto-displayed-content-type-exceptions nil))
		 (setq vm-mime-decoded nil)
		 (intern (buffer-name) vm-buffers-needing-display-update)
		 (save-excursion
		   (vm-present-current-message))
		 (setq vm-mime-decoded 'buttons)))
	      ((eq state 'undecoded)
	       (let ((vm-preview-lines nil)
		     (vm-auto-decode-mime-messages nil))
		 (intern (buffer-name) vm-buffers-needing-display-update)
		 (vm-present-current-message))))
      (let ((layout (vm-mm-layout (car vm-message-pointer)))
	    (m (car vm-message-pointer)))
	(vm-emit-mime-decoding-message "Decoding MIME message...")
	(when (stringp layout)
	  (error "Invalid MIME message: %s" layout))
	(when (vm-mime-plain-message-p m)
	  (error "Message needs no decoding."))
	(if (not vm-presentation-buffer)
	    ;; maybe user killed it - make a new one
	    (progn
	      (vm-make-presentation-copy (car vm-message-pointer))
	      (vm-expose-hidden-headers))
	  (set-buffer vm-presentation-buffer))
	;; Are we now in the Presentation buffer?  Why?  USR, 2010-05-08
	(when (and (interactive-p) (eq vm-system-state 'previewing))
	  (let ((vm-display-using-mime nil))
	    (vm-show-current-message)))
	(setq m (car vm-message-pointer))
	(vm-save-restriction
	 (widen)
	 (goto-char (vm-text-of m))
	 (let ((buffer-read-only nil)
	       (modified (buffer-modified-p)))
	   (unwind-protect
	       (save-excursion
		 (unless (eq (vm-mm-encoded-header m) 'none)
		   (vm-decode-mime-message-headers m))
		 (when (vectorp layout)
		   (vm-decode-mime-layout layout)
		   ;; Delete the original presentation copy
		   (delete-region (point) (point-max)))
		 (vm-energize-urls)
		 (vm-highlight-headers-maybe)
		 (vm-energize-headers-and-xfaces))
	     (set-buffer-modified-p modified))))
	(save-excursion (set-buffer vm-mail-buffer)
			(setq vm-mime-decoded 'decoded))
	(intern (buffer-name vm-mail-buffer) vm-buffers-needing-display-update)
	(vm-update-summary-and-mode-line)
	(vm-emit-mime-decoding-message "Decoding MIME message... done"))))
  (vm-display nil nil '(vm-decode-mime-message)
	      '(vm-decode-mime-message reading-message)))

(defun vm-mime-get-disposition-filename (layout)
  (let ((filename nil)
        (case-fold-search t))
    (setq filename (or (vm-mime-get-disposition-parameter layout "filename") 
                       (vm-mime-get-disposition-parameter layout "name")))
    (when (not filename)
      (setq filename (or (vm-mime-get-disposition-parameter layout "filename*") 
                         (vm-mime-get-disposition-parameter layout "name*")))
      ;; decode encoded filenames
      (when (and filename  
                 (string-match "^\\([^']+\\)'\\([^']*\\)'\\(.*%[0-9A-F][0-9A-F].*\\)$"
                               filename))
        ;; transform it to something we are already able to decode
        (let ((charset (match-string 1 filename))
              (f (match-string 3 filename)))
          (setq f (vm-replace-in-string f "%\\([0-9A-F][0-9A-F]\\)" "=\\1"))
          (setq filename (concat "=?" charset "?Q?" f "?="))
          (setq filename (vm-decode-mime-encoded-words-in-string filename)))))
    filename))

(defun vm-decode-mime-layout (layout &optional dont-honor-c-d)
  "Decode the MIME part in the current buffer using LAYOUT.  
If DONT-HONOR-C-D non-Nil, then don't honor the Content-Disposition
declarations in the attachments and make a decision independently.

LAYOUT can be a mime layout vector.  It can also be a button
extent in the current buffer, in which case the 'vm-mime-layout
property of the overlay will be extracted.  The button may be
deleted. 

Returns t if the display was successful.  Not clear what happens if it
is not successful.                                   USR, 2011-03-25"
  (let ((modified (buffer-modified-p))
	handler new-layout file type type2 type-no-subtype 
	(extent nil))
    (unless (vectorp layout)
      ;; handle a button extent
      (setq extent layout
	    layout (vm-extent-property extent 'vm-mime-layout))
      (goto-char (vm-extent-start-position extent))
      ;; if the button is for external-body, use the external-body
      (setq type (downcase (car (vm-mm-layout-type layout))))
      (when (vm-mime-types-match "message/external-body" type)
	(setq layout (car (vm-mm-layout-parts layout)))))
    (unwind-protect
	(progn
	  (setq type (downcase (car (vm-mm-layout-type layout)))
		type-no-subtype (car (vm-parse type "\\([^/]+\\)")))
	  (cond ((and vm-infer-mime-types
		      (or (and vm-infer-mime-types-for-text
			       (vm-mime-types-match "text/plain" type))
			  (vm-mime-types-match "application/octet-stream" type))
		      (setq file (vm-mime-get-disposition-filename layout))
		      (setq type2 (vm-mime-default-type-from-filename file))
		      (not (vm-mime-types-match type type2)))
		 (vm-set-mm-layout-type layout (list type2))
		 (vm-set-mm-layout-qtype layout
					 (list (concat "\"" type2 "\"")))
		 (setq type (downcase (car (vm-mm-layout-type layout)))
		       type-no-subtype (car (vm-parse type "\\([^/]+\\)")))))
	  
	  (cond ((and (vm-mime-should-display-button 
		       layout :honor-content-disposition (not dont-honor-c-d))
		      ;; original conditional-cases changed to fboundp
		      ;; checks.  USR, 2011-03-25
		      (or (fboundp 
			   (setq handler 
				 (intern (concat "vm-mime-display-button-"
						 type))))
			  (fboundp 
			   (setq handler
				 (intern (concat "vm-mime-display-button-"
						 type-no-subtype)))))
		      (funcall handler layout))
		 ;; if the handler returns t, we are done
		 )
		((and (vm-mime-should-display-internal layout)
		      (or (fboundp 
			   (setq handler 
				 (intern (concat "vm-mime-display-internal-"
						 type))))
			  (fboundp 
			   (setq handler
				 (intern (concat "vm-mime-display-internal-"
						 type-no-subtype)))))
		      (funcall handler layout))
		 ;; if the handler returns t, we are done
		 )
		((vm-mime-types-match "multipart" type)
		 (if (fboundp (setq handler
				    (intern (concat "vm-mime-display-internal-"
						    type))))
		     (funcall handler layout)
		   (vm-mime-display-internal-multipart/mixed layout))
		 )
		((and (vm-mime-find-external-viewer type)
		      (vm-mime-display-external-generic layout))
		 ;; external viewer worked.  the button should go away.
		 (when extent (vm-set-extent-property
			       extent 'vm-mime-disposable nil))
		 )
		((and (not (vm-mm-layout-is-converted layout))
		      (vm-mime-can-convert type)
		      (setq new-layout
			    (vm-mime-convert-undisplayable-layout layout)))
		 ;; conversion worked.  the button should go away.
		 (when extent
		   (vm-set-extent-property extent 'vm-mime-disposable t))
		 (vm-decode-mime-layout new-layout)
		 )
		(t 
		 (when extent (vm-mime-rewrite-failed-button
			       extent
			       (or (vm-mm-layout-display-error layout)
				   "no external viewer defined for type")))
		 (cond ((vm-mime-types-match "message/external-body" type)
			(if (null extent)
			    (vm-mime-display-button-xxxx layout t)
			  (setq extent nil)))
		       ((vm-mime-types-match "application/octet-stream" type)
			(vm-mime-display-internal-application/octet-stream
			 (or extent layout)))
		       ;; if everything else fails, do nothing
		       )
		 ))
	  (when extent (vm-mime-delete-button-maybe extent)))
      ;; unwind-protection
      (set-buffer-modified-p modified)))
  t )

(defun vm-mime-display-button-text (layout)
  (vm-mime-display-button-xxxx layout t))

(defun vm-mime-display-internal-text (layout)
  (vm-mime-display-internal-text/plain layout))

(defun vm-mime-cid-retrieve (url message)
  "Insert a content pointed by URL if it has the cid: scheme."
  (if (string-match "\\`cid:" url)
      (setq url (concat "<" (substring url (match-end 0)) ">"))
    (error "%S is not a cid url" url))
  (let ((part-list (vm-mm-layout-parts (vm-mm-layout message)))
        part)
    (while part-list
      (setq part (car part-list))
      (if (vm-mime-composite-type-p (car (vm-mm-layout-type part)))
          (setq part-list (nconc (copy-sequence (vm-mm-layout-parts part))
                                 (cdr part-list))))
      (setq part-list (cdr part-list))
      (if (not (equal url (vm-mm-layout-id part)))
          (setq part nil)
        (vm-mime-insert-mime-body part)
        (setq part-list nil)))
    (unless part
      (vm-inform 5 "No data for cid %S" url))
    part))

(defun vm-mime-display-internal-w3m-text/html (start end layout)
  (let ((charset (or (vm-mime-get-parameter layout "charset") "us-ascii")))
    (shell-command-on-region
     start (1- end)
     (format "w3m -dump -T text/html -I %s -O %s" charset charset)
     nil t)))
  
(defun vm-mime-display-internal-lynx-text/html (start end layout)
  (shell-command-on-region start (1- end)
;;                           "lynx -force_html /dev/stdin" 
			   "lynx -force_html -dump -pseudo_inlines -stdin"
			   nil t))

(defun vm-mime-display-internal-text/html (layout)
  "Dispatch handling of html to the actual html handler."
  ;; If the user has set the vm-mime-text/html-handler _variable_ to
  ;; 'auto-select, and it is left set that way in this function, we will get a
  ;; failure because there is no function called
  ;; "vm-mime-display-internal-auto-select-text/html". But, the
  ;; vm-mime-text/html-handler _function_ sets the corresponding _variable_
  ;; based upon a heuristic about available packages, so call it for its
  ;; side-effect now.  -- Brent Goodrick, 2008-12-08
  (vm-mime-text/html-handler)
  (if vm-mime-text/html-handler
      (condition-case error-data
	  (let ((buffer-read-only nil)
		(start (point))
		(charset (or (vm-mime-get-parameter layout "charset")
			     "us-ascii"))
		end buffer-size)
	    (vm-emit-mime-decoding-message
	     "Inlining text/html by %s..." vm-mime-text/html-handler)
	    (vm-mime-insert-mime-body layout)
	    (unless (bolp) (insert "\n"))
	    (setq end (point-marker))
	    (vm-mime-transfer-decode-region layout start end)
	    (vm-mime-charset-decode-region charset start end)
	    ;; block remote images by prefixing the link
	    (goto-char start)
	    (let ((case-fold-search t))
	      (while (re-search-forward vm-mime-text/html-blocker end t)
		(goto-char (match-end 0))
		(if (or t 
			(and vm-mime-text/html-blocker-exceptions
			     (looking-at vm-mime-text/html-blocker-exceptions))
			(looking-at "cid:"))
		    (progn
		      ;; TODO: write the image to a file and replace the link
		      )
		  (insert "blocked:"))))
	    ;; w3-region apparently deletes all the text in the
	    ;; region and then insert new text.  This makes the
	    ;; end == start.  The fix is to move the end marker
	    ;; forward with a placeholder character so that when
	    ;; w3-region delete all the text, end will still be
	    ;; ahead of the insertion point and so will be moved
	    ;; forward when the new text is inserted.  We'll
	    ;; delete the placeholder afterward.
	    (goto-char end)
	    (insert-before-markers "z")
	    ;; the view port (scrollbar) is sometimes messed up, try to avoid it
	    (save-window-excursion
	      ;; dispatch to actual handler
	      (funcall (intern (format "vm-mime-display-internal-%s-text/html"
				       vm-mime-text/html-handler))
		       start end layout))
	    ;; do clean up
	    (goto-char end)
	    (delete-char -1)
	    (vm-inform 6 "Inlining text/html by %s... done."
		     vm-mime-text/html-handler)
	    t)
	(error (vm-set-mm-layout-display-error
		layout
		(format "Inline text/html by %s display failed: %s"
			vm-mime-text/html-handler
			(error-message-string error-data)))
	       (vm-warn 0 2 "%s" (vm-mm-layout-display-error layout))
	       nil))
    ;; no handler
    (vm-warn 0 2 "No handler available for internal display of text/html")
    nil))
  

(defun vm-mime-display-internal-text/plain (layout &optional no-highlighting)
  "Display a text/plain mime part given by LAYOUT, carrying out
any necessary MIME-decoding, CRLF-conversion, charset-conversion
and word-wrapping/filling.  The original text is replaced by the
converted content.  Unless NO-HIGHLIGHTING is non-nil, the URL's
in the text are highlighted and energized."
  (let ((start (point)) end need-conversion
	(buffer-read-only nil)
	(charset (or (vm-mime-get-parameter layout "charset") "us-ascii")))
    (if (and (not (vm-mime-charset-internally-displayable-p charset))
	     (not (setq need-conversion (vm-mime-can-convert-charset charset))))
	(progn
	  (vm-set-mm-layout-display-error
	   layout (concat "Undisplayable charset: " charset))
	  (vm-warn 0 2 "%s" (vm-mm-layout-display-error layout))
	  nil)
      (vm-mime-insert-mime-body layout)
      (unless (bolp) (insert "\n"))
      (setq end (point-marker))
      (vm-mime-transfer-decode-region layout start end)
      (when need-conversion
	(setq charset (vm-mime-charset-convert-region charset start end)))
      (vm-mime-charset-decode-region charset start end)
      (unless no-highlighting (vm-energize-urls-in-message-region start end))
      (when (and (or vm-word-wrap-paragraphs
		     vm-fill-paragraphs-containing-long-lines)
		 (not no-highlighting))
	(vm-fill-paragraphs-containing-long-lines
	 vm-fill-paragraphs-containing-long-lines start end))
      (goto-char end)
      t )))

(defun vm-mime-display-internal-text/enriched (layout)
  (require 'enriched)
  (let ((start (point)) end
	(buffer-read-only nil)
	(enriched-verbose t)
	(charset (or (vm-mime-get-parameter layout "charset") "us-ascii")))
    (vm-emit-mime-decoding-message "Decoding text/enriched...")
    (vm-mime-insert-mime-body layout)
    (unless (bolp) (insert "\n"))
    (setq end (point-marker))
    (vm-mime-transfer-decode-region layout start end)
    (vm-mime-charset-decode-region charset start end)
    ;; enriched-decode expects a couple of headers at the top of
    ;; the region and will remove anything that looks like a
    ;; header.  Put a header section here for it to eat so it
    ;; won't eat message text instead.
    (goto-char start)
    (insert "Comment: You should not see this header\n\n")
    (condition-case errdata
	(enriched-decode start end)
      (error (vm-set-mm-layout-display-error
	      layout (format "enriched-decode signaled %s" errdata))
	     (vm-warn 0 2 "%s" (vm-mm-layout-display-error layout))
	     nil ))
    (vm-energize-urls-in-message-region start end)
    (goto-char end)
    (vm-emit-mime-decoding-message "Decoding text/enriched... done")
    t ))

(defun vm-mime-display-external-generic (layout)
  "Display mime object with LAYOUT in an external viewer, as
determined by `vm-mime-external-content-types-alist'."
  ;;  Optional argument FILE indicates that the content should be
  ;;  taken from it.
  (let ((program-list (copy-sequence
		       (vm-mime-find-external-viewer
			(car (vm-mm-layout-type layout)))))
	(buffer-read-only nil)
	start
	(coding-system-for-read (vm-binary-coding-system))
	(coding-system-for-write (vm-binary-coding-system))
	(append-file t)
	process	tempfile cache end suffix basename)
    (setq cache (get (vm-mm-layout-cache layout)
		     'vm-mime-display-external-generic)
	  process (nth 0 cache)
	  tempfile (nth 1 cache))
    (if (and (processp process) (eq (process-status process) 'run))
	t
      (cond ((or (null tempfile) (null (file-exists-p tempfile)))
	     (setq suffix (vm-mime-extract-filename-suffix layout)
		   suffix (or suffix
			      (vm-mime-find-filename-suffix-for-type layout)))
	     (setq basename (vm-mime-get-disposition-filename layout))
	     (setq tempfile (vm-make-tempfile suffix basename))
             (vm-register-message-garbage-files (list tempfile))
             (vm-mime-send-body-to-file layout nil tempfile t)))

      (if (symbolp (car program-list))
	  ;; use internal function if provided
	  (apply (car program-list)
		 (append (cdr program-list) (list tempfile)))

	;; quote file name for shell command only
	(or (cdr program-list)
	    (setq tempfile (shell-quote-argument tempfile)))
      
	;; expand % specs
	(let ((p program-list)
	      (vm-mf-attachment-file tempfile))
	  (while p
	    (if (string-match "\\([^%]\\|^\\)%f" (car p))
		(setq append-file nil))
	    (setcar p (vm-mime-sprintf (car p) layout))
	    (setq p (cdr p))))

	(vm-inform 6 "Launching %s..." (mapconcat 'identity program-list " "))
	(setq process
	      (if (cdr program-list)
		  (apply 'start-process
			 (format "view %25s"
				 (vm-mime-sprintf
				  (vm-mime-find-format-for-layout layout)
				  layout))
			 nil (if append-file
				 (append program-list (list tempfile))
			       program-list))
		(apply 'start-process
		       (format "view %25s"
			       (vm-mime-sprintf
				(vm-mime-find-format-for-layout layout)
				layout))
		       nil
		       (or shell-file-name "sh")
		       shell-command-switch
		       (if append-file
			   (list (concat (car program-list) " " tempfile))
			 program-list))))
	(vm-process-kill-without-query process t)
	(vm-inform 6 "Launching %s... done" (mapconcat 'identity
						   program-list
						   " "))
	(if vm-mime-delete-viewer-processes
	    (vm-register-message-garbage 'delete-process process))
	(put (vm-mm-layout-cache layout)
	     'vm-mime-display-external-generic
	     (list process tempfile)))))
  t )

(defun vm-mime-display-internal-application/octet-stream (layout)
  "Display a button for the MIME LAYOUT.  If a button extent is
given as the argument instead, then nothing is done.   USR, 2011-03-25"
  (if (vectorp layout)
      (let ((buffer-read-only nil)
	    (vm-mf-default-action "save to a file"))
	(vm-mime-insert-button
	 (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
	 (function
	  (lambda (layout)
	    (save-excursion
	      (vm-mime-save-application/octet-stream layout))))
	 layout nil)))
  t)

(defun vm-mime-save-application/octet-stream (layout)
  "Save an application/octet-stream object with LAYOUT to the
stated filename.  A button extent with a layout can also be given as
the argument.                                        USR, 2011-03-25"
  (unless (vectorp layout)
    (goto-char (vm-extent-start-position layout))
    (setq layout (vm-extent-property layout 'vm-mime-layout)))
  ;; support old "name" paramater for application/octet-stream
  ;; but don't override the "filename" parameter extracted from
  ;; Content-Disposition, if any.
  (let ((default-filename (vm-mime-get-disposition-filename layout))
	(file nil))
    (setq file (vm-mime-send-body-to-file layout default-filename))
    (when (and file vm-mime-delete-after-saving)
      (let ((vm-mime-confirm-delete nil))
	;; we don't care if the delete fails
	(condition-case nil
	    (vm-delete-mime-object (expand-file-name file))
	  (error nil)))))
  t )
(fset 'vm-mime-display-button-application/octet-stream
      'vm-mime-display-internal-application/octet-stream)

(defun vm-mime-display-button-application (layout)
  "Display button for an application type object described by LAYOUT."
  (vm-mime-display-button-xxxx layout nil))


(defun vm-mime-display-button-audio (layout)
  (vm-mime-display-button-xxxx layout nil))

(defun vm-mime-display-button-video (layout)
  (vm-mime-display-button-xxxx layout t))

(defun vm-mime-display-button-message (layout)
  (vm-mime-display-button-xxxx layout t))

(defun vm-mime-display-button-multipart (layout)
  (vm-mime-display-button-xxxx layout t))

(defun vm-mime-display-internal-multipart/mixed (layout)
  (let ((part-list (vm-mm-layout-parts layout)))
    (while part-list
      (let ((part (car part-list)))
        (vm-decode-mime-layout part)
        (setq part-list (cdr part-list))
	;; we always put separator because it is cleaner, and buttons
	;; may get expanded to documents in any case. USR, 2011-02-09
	(when part-list
	  (insert vm-mime-parts-display-separator))))
    t))


(defun vm-mime-display-internal-multipart/alternative (layout)
  (if (eq vm-mime-alternative-select-method 'all)
      (vm-mime-display-internal-multipart/mixed layout)
    (vm-mime-display-internal-show-multipart/alternative layout)))

(defun vm-mime-display-internal-show-multipart/alternative (layout)
  (let (best-layout)
    (cond ((eq vm-mime-alternative-select-method 'best)
	   (let ((done nil)
		 (best nil)
		 part-list type)
	     (setq part-list (vm-mm-layout-parts layout)
		   part-list (nreverse (copy-sequence part-list)))
	     (while (and part-list (not done))
	       (setq type (car (vm-mm-layout-type (car part-list))))
	       (if (or (vm-mime-can-display-internal (car part-list) t)
		       (vm-mime-find-external-viewer type))
		   (setq best (car part-list)
			 done t)
		 (setq part-list (cdr part-list))))
	     (setq best-layout (or best (car (vm-mm-layout-parts layout))))))
	  ((eq vm-mime-alternative-select-method 'best-internal)
	   (let ((done nil)
		 (best nil)
		 (second-best nil)
		 part-list type)
	     (setq part-list (vm-mm-layout-parts layout)
		   part-list (nreverse (copy-sequence part-list)))
	     (while (and part-list (not done))
	       (setq type (car (vm-mm-layout-type (car part-list))))
	       (cond ((and (vm-mime-can-display-internal (car part-list) t)
			   (vm-mime-should-display-internal (car part-list)))
		      (setq best (car part-list)
			    done t))
		     ((and (null second-best)
			   (vm-mime-find-external-viewer type))
		      (setq second-best (car part-list))))
	       (setq part-list (cdr part-list)))
	     (setq best-layout (or best second-best
				   (car (vm-mm-layout-parts layout))))))
	  ((and (consp vm-mime-alternative-select-method)
		(eq (car vm-mime-alternative-select-method)
		    'favorite-internal))
	   (let ((done nil)
		 (best nil)
		 (saved-part-list
		  (nreverse (copy-sequence (vm-mm-layout-parts layout))))
		 (favs (cdr vm-mime-alternative-select-method))
		 (second-best nil)
		 part-list type)
	     (while (and favs (not done))
	       (setq part-list saved-part-list)
	       (while (and part-list (not done))
		 (setq type (car (vm-mm-layout-type (car part-list))))
		 (cond ((or (vm-mime-can-display-internal (car part-list) t)
			    (vm-mime-find-external-viewer type))
			(if (vm-mime-types-match (car favs) type)
			    (setq best (car part-list)
				  done t)
			  (or second-best
			      (setq second-best (car part-list))))))
		 (setq part-list (cdr part-list)))
	       (setq favs (cdr favs)))
	     (setq best-layout (or best second-best
				   (car (vm-mm-layout-parts layout))))))
	  ((and (consp vm-mime-alternative-select-method)
		(eq (car vm-mime-alternative-select-method) 'favorite))
	   (let ((done nil)
		 (best nil)
		 (saved-part-list
		  (nreverse (copy-sequence (vm-mm-layout-parts layout))))
		 (favs (cdr vm-mime-alternative-select-method))
		 (second-best nil)
		 part-list type)
	     (while (and favs (not done))
	       (setq part-list saved-part-list)
	       (while (and part-list (not done))
		 (setq type (car (vm-mm-layout-type (car part-list))))
		 (cond ((and (vm-mime-can-display-internal (car part-list) t)
			     (vm-mime-should-display-internal (car part-list)))
			(if (vm-mime-types-match (car favs) type)
			    (setq best (car part-list)
				  done t)
			  (or second-best
			      (setq second-best (car part-list))))))
		 (setq part-list (cdr part-list)))
	       (setq favs (cdr favs)))
	     (setq best-layout (or best second-best
				   (car (vm-mm-layout-parts layout)))))))
    (when best-layout 
      (vm-decode-mime-layout best-layout))))

(defun vm-mime-display-internal-multipart/related (layout)
  "Decode multipart/related body parts.
This function decodes the ``start'' part (see RFC2387) only.  The
other parts will be decoded by the other VM functions through
emacs-w3m."
  (let* ((part-list (vm-mm-layout-parts layout))
	 (start-part (car part-list))
	 (start-id (vm-mime-get-parameter layout "start"))
	 layout)
    ;; Look for the start part.
    (if start-id
	(while part-list
	  (setq layout (car part-list))
	  (if (equal start-id (vm-mm-layout-id layout))
	      (setq start-part layout
		    part-list nil)
	    (setq part-list (cdr part-list)))))
    (if start-part (vm-decode-mime-layout start-part))))

(defun vm-mime-display-button-multipart/parallel (layout)
  (vm-mime-insert-button
   (concat
    ;; display the file name or disposition
    (let ((file (vm-mime-get-disposition-filename layout)))
      (if file (format " %s " file) ""))
    (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout) )
   (function
    (lambda (layout)
      (save-excursion
	(let ((vm-mime-auto-displayed-content-types t)
	      (vm-mime-auto-displayed-content-type-exceptions nil))
	  (vm-decode-mime-layout layout t)))))
   layout t))

(fset 'vm-mime-display-internal-multipart/parallel
      'vm-mime-display-internal-multipart/mixed)

(defun vm-mime-display-internal-multipart/digest (layout)
  (if (vectorp layout)
      (let ((buffer-read-only nil))
	(vm-mime-insert-button
	 (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
	 (function
	  (lambda (layout)
	    (save-excursion
	      (vm-mime-display-internal-multipart/digest layout))))
	 layout nil))
    (goto-char (vm-extent-start-position layout))
    (setq layout (vm-extent-property layout 'vm-mime-layout))
    (set-buffer (generate-new-buffer (format "digest from %s/%s"
					     (buffer-name vm-mail-buffer)
					     (vm-number-of
					      (car vm-message-pointer)))))
    (setq vm-folder-type vm-default-folder-type)
    (let ((ident-header nil))
      (if vm-digest-identifier-header-format
	  (setq ident-header (vm-summary-sprintf
			      vm-digest-identifier-header-format
			      (vm-mm-layout-message layout))))
      (vm-mime-burst-layout layout ident-header))
    (vm-save-buffer-excursion
     (vm-goto-new-folder-frame-maybe 'folder)
     (vm-mode)
     (if (vm-should-generate-summary)
	 (progn
	   (vm-goto-new-summary-frame-maybe)
	   (vm-summarize))))
    ;; temp buffer, don't offer to save it.
    (setq buffer-offer-save nil)
    (vm-display (or vm-presentation-buffer (current-buffer)) t
		(list this-command) '(vm-mode startup)))
  t )

(fset 'vm-mime-display-button-multipart/digest
      'vm-mime-display-internal-multipart/digest)

(defun vm-mime-display-button-message/rfc822 (layout)
  (let ((buffer-read-only nil))
    (vm-mime-insert-button
     (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
     (function
      (lambda (layout)
	(save-excursion
	  (vm-mime-display-internal-message/rfc822 layout))))
     layout nil)))

(fset 'vm-mime-display-button-message/news
      'vm-mime-display-button-message/rfc822)

(defun vm-mime-display-internal-message/rfc822 (layout)
  (if (vectorp layout)
      (let ((start (point))
	    (buffer-read-only nil))
	(vm-mime-insert-mime-headers (car (vm-mm-layout-parts layout)))
	(insert ?\n)
	(save-excursion
	  (goto-char start)
	  (vm-reorder-message-headers
	   nil :keep-list vm-visible-headers
	   :discard-regexp vm-invisible-header-regexp))
	(save-restriction
	  (narrow-to-region start (point))
	  (vm-decode-mime-encoded-words))
	(vm-mime-display-internal-multipart/mixed layout))
    (goto-char (vm-extent-start-position layout))
    (setq layout (vm-extent-property layout 'vm-mime-layout))
    (set-buffer (vm-generate-new-unibyte-buffer
		 (format "message from %s/%s"
			 (buffer-name vm-mail-buffer)
			 (vm-number-of
			  (car vm-message-pointer)))))
    (setq vm-folder-type vm-default-folder-type)
    (vm-mime-burst-layout layout nil)
    (set-buffer-modified-p nil)
    (vm-save-buffer-excursion
     (vm-goto-new-folder-frame-maybe 'folder)
     (vm-mode)
     (if (vm-should-generate-summary)
	 (progn
	   (vm-goto-new-summary-frame-maybe)
	   (vm-summarize))))
    ;; temp buffer, don't offer to save it.
    (setq buffer-offer-save nil)
    (vm-display (or vm-presentation-buffer (current-buffer)) t
		(list this-command) '(vm-mode startup)))
  t )
(fset 'vm-mime-display-internal-message/news
      'vm-mime-display-internal-message/rfc822)

(defun vm-mime-display-internal-message/delivery-status (layout)
  (vm-mime-display-internal-text/plain layout t))

(defun vm-mime-retrieve-external-body (layout)
  "Fetch an external body into the current buffer.
LAYOUT is the MIME layout struct for the message/external-body object."
  (let ((access-method (downcase (vm-mime-get-parameter layout "access-type")))
	(work-buffer (current-buffer)))
    (cond ((string= access-method "local-file")
	   (let ((name (vm-mime-get-parameter layout "name")))
	     (if (null name)
		 (vm-mime-error
		  "%s access type missing `name' parameter"
		  access-method))
	     (if (not (file-exists-p name))
		 (vm-mime-error "file %s does not exist" name))
	     (condition-case data
		 (insert-file-contents-literally name)
	       (error (signal 'vm-mime-error (cdr data))))))
	  ((and (string= access-method "url")
		vm-url-retrieval-methods)
	   (defvar w3-configuration-directory) ; for bytecompiler
	   (let ((url (vm-mime-get-parameter layout "url"))
		 ;; needed or url-retrieve will bitch
		 (w3-configuration-directory
		  (if (boundp 'w3-configuration-directory)
		      w3-configuration-directory
		    "~")))
	     (if (null url)
		 (vm-mime-error
		  "%s access type missing `url' parameter"
		  access-method))
	     (setq url (vm-with-string-as-temp-buffer
			url
			(function
			 (lambda ()
			   (goto-char (point-min))
			   (while (re-search-forward "[ \t\n]" nil t)
			     (delete-char -1))))))
	     (vm-mime-fetch-url-with-programs url work-buffer)))
	  ((and (or (string= access-method "ftp")
		    (string= access-method "anon-ftp"))
		(or (fboundp 'efs-file-handler-function)
		    (fboundp 'ange-ftp-hook-function)))
	   (let ((name (vm-mime-get-parameter layout "name"))
		 (directory (vm-mime-get-parameter layout "directory"))
		 (site (vm-mime-get-parameter layout "site"))
		 user)
	     (if (null name)
		 (vm-mime-error
		  "%s access type missing `name' parameter"
		  access-method))
	     (if (null site)
		 (vm-mime-error
		  "%s access type missing `site' parameter"
		  access-method))
	     (cond ((string= access-method "ftp")
		    (setq user (read-string
				(format "User name to access %s: "
					site)
				(user-login-name))))
		   (t (setq user "anonymous")))
	     (if (and (string= access-method "ftp")
		      vm-url-retrieval-methods
		      (vm-mime-fetch-url-with-programs
		       (if directory
			   (concat "ftp:////" site "/"
				   directory "/" name)
			 (concat "ftp:////" site "/" name))
		       work-buffer))
		 t
	       (cond (directory
		      (setq directory
			    (concat "/" user "@" site ":" directory))
		      (setq name (expand-file-name name directory)))
		     (t
		      (setq name (concat "/" user "@" site ":"
					 name))))
	       (condition-case data
		     (insert-file-contents-literally name)
		 (error (signal 'vm-mime-error
				(format "%s" (cdr data)))))))))))

(defun vm-mime-fetch-message/external-body (layout)
  "Fetch the external-body content described by LAYOUT and store
it in an internal buffer.  Update the LAYOUT so that it refers to the
fetched content."
  (let ((child-layout (car (vm-mm-layout-parts layout)))
	(access-method (downcase (vm-mime-get-parameter layout "access-type")))
	ob
	(work-buffer nil))
    (unwind-protect
	(cond
	 ((and (string= access-method "mail-server")
	       (vm-mm-layout-id child-layout)
	       (setq ob (vm-mime-find-leaf-content-id-in-layout-folder
			 layout (vm-mm-layout-id child-layout))))
	  (setq child-layout ob))
	 ((eq (marker-buffer (vm-mm-layout-header-start child-layout))
	      (marker-buffer (vm-mm-layout-body-start child-layout)))
	  ;; if the "body" is in the same buffer, that means that the
	  ;; external-body has not been retrieved yet
	  (setq work-buffer
		(vm-make-multibyte-work-buffer
		 (format "*%s mime object*"
			 (car (vm-mm-layout-type child-layout)))))
	  (condition-case data
	      (with-current-buffer work-buffer
		(if (fboundp 'set-buffer-file-coding-system)
		    (set-buffer-file-coding-system
		     (vm-binary-coding-system) t))
		(cond
		 ((or (string= access-method "ftp")
		      (string= access-method "anon-ftp")
		      (string= access-method "local-file")
		      (string= access-method "url"))
		  (vm-mime-retrieve-external-body layout))
		 ((string= access-method "mail-server")
		  (let ((server (vm-mime-get-parameter layout "server"))
			(subject (vm-mime-get-parameter layout "subject")))
		    (if (null server)
			(vm-mime-error
			 "%s access type missing `server' parameter"
			 access-method))
		    (if (not
			 (y-or-n-p
			  (format
			   "Send message to %s to retrieve external body? "
			   server)))
			(error "Aborted"))
		    (vm-mail-internal
		     (format "mail to MIME mail server %s" server)
		     server subject)
		    (mail-text)
		    (vm-mime-insert-mime-body child-layout)
		    (let ((vm-confirm-mail-send nil))
		      (vm-mail-send))
		    (vm-warn 0 2
			     (concat "Retrieval message sent.  "
				     "Retry viewing this object after "
				     "the response arrives."))))
		 (t
		  (vm-mime-error "unsupported access method: %s"
				 access-method))
		 )
		(when child-layout
		  (vm-set-mm-layout-body-end 
		   child-layout (vm-marker (point-max)))
		  (vm-set-mm-layout-body-start 
		   child-layout (vm-marker (point-min)))))
	    (vm-mime-error		; handler
	     (vm-set-mm-layout-display-error layout (cdr data))
	     (setq child-layout nil)))))
      ;; unwind-protections
      (when work-buffer
	(if child-layout		; refers to work-buffer
	    (vm-register-folder-garbage 'kill-buffer work-buffer)
	  (kill-buffer work-buffer))))))

(defun vm-mime-display-external-message/external-body (layout)
  "Display the external-body content described by LAYOUT."
  (vm-mime-fetch-message/external-body layout)
  (let ((child-layout (car (vm-mm-layout-parts layout))))
    (when child-layout 
      (vm-mime-display-external-generic child-layout))))

(defun vm-mime-display-internal-message/external-body (layout
						       &optional extent)
  "Display the external-body content described by LAYOUT.  The
optional argument EXTENT, if present, gives the extent of the MIME
button that this LAYOUT comes from."
  (vm-mime-fetch-message/external-body layout)
  (let ((child-layout (car (vm-mm-layout-parts layout))))
    (when child-layout 
      (vm-decode-mime-layout (or extent child-layout)))))

(defun vm-mime-display-button-message/external-body (layout)
  "Return a button usable for viewing message/external-body MIME parts."
  (let ((buffer-read-only nil)
	(tmplayout (copy-tree (car (vm-mm-layout-parts layout)) t))
	(filename "external: ")
	format)
    (when (vm-mime-get-parameter layout "name")
      (setq filename 
	    (concat filename
		    (file-name-nondirectory 
		     (vm-mime-get-parameter layout "name")))))
    (vm-mime-set-parameter tmplayout "name" filename)
    (vm-mime-set-xxx-parameter "filename" filename 
			       (vm-mm-layout-disposition tmplayout))
    (setq format (vm-mime-find-format-for-layout tmplayout))
    (vm-mime-insert-button
     (vm-replace-in-string
      (vm-mime-sprintf format tmplayout)	      
      "save to a file\\]"
      "display as text]")
     (function
      (lambda (extent)
	;; reuse the internal display code, but make sure that no new
	;; buttons should be created for the external-body content.
	(let ((layout (if vm-xemacs-p
                         (vm-extent-property extent 'vm-mime-layout)
                       (overlay-get extent 'vm-mime-layout)))
	      (vm-mime-auto-displayed-content-types t)
	      (vm-mime-auto-displayed-content-type-exceptions nil))
	  (vm-mime-display-internal-message/external-body 
	   layout extent))))
     layout
     nil)))


(defun vm-mime-fetch-url-with-programs (url buffer)
  (when
      (eq t (cond ((if (and (memq 'wget vm-url-retrieval-methods)
			    (condition-case data
				(vm-run-command-on-region 
				 (point) (point) buffer
				 vm-wget-program "-q" "-O" "-" url)
			      (error nil)))
		       t
		     (save-excursion
		       (set-buffer buffer)
		       (erase-buffer)
		       nil )))
		  ((if (and (memq 'w3m vm-url-retrieval-methods)
			    (condition-case data
				(vm-run-command-on-region 
				 (point) (point) buffer
				 vm-w3m-program "-dump_source" url)
			      (error nil)))
		       t
		     (save-excursion
		       (set-buffer buffer)
		       (erase-buffer)
		       nil )))
		  ((if (and (memq 'fetch vm-url-retrieval-methods)
			    (condition-case data
				(vm-run-command-on-region 
				 (point) (point) buffer
				 vm-fetch-program "-o" "-" url)
			      (error nil)))
		       t
		     (save-excursion
		       (set-buffer buffer)
		       (erase-buffer)
		       nil )))
		  ((if (and (memq 'curl vm-url-retrieval-methods)
			    (condition-case data
				(vm-run-command-on-region 
				 (point) (point) buffer
				 vm-curl-program url)
			      (error nil)))
		       t
		     (save-excursion
		       (set-buffer buffer)
		       (erase-buffer)
		       nil )))
		  ((if (and (memq 'lynx vm-url-retrieval-methods)
			    (condition-case data
				(vm-run-command-on-region 
				 (point) (point) buffer
				 vm-lynx-program "-source" url)
			      (error nil)))
		       t
		     (save-excursion
		       (set-buffer buffer)
		       (erase-buffer)
		       nil )))))
    (save-excursion
      (set-buffer buffer)
      (not (zerop (buffer-size))))))

(defun vm-mime-internalize-local-external-bodies (layout)
  "Given a LAYOUT representing a message/external-body object, convert
it to an internal object by retrieving the body.       USR, 2011-03-28"
  (cond ((vm-mime-types-match "message/external-body"
			      (car (vm-mm-layout-type layout)))
	 (when (string= (downcase
			 (vm-mime-get-parameter layout "access-type"))
			"local-file")
	   (let* ((child-layout 
		   (car (vm-mm-layout-parts layout)))
		  (work-buffer 
		   (vm-make-multibyte-work-buffer
		    (format "*%s mime object*"
			    (car (vm-mm-layout-type child-layout))))))
	     (unwind-protect
		 (let (oldsize)
		   (with-current-buffer work-buffer
		     (vm-mime-retrieve-external-body layout))
		   (goto-char (vm-mm-layout-body-start child-layout))
		   (setq oldsize (buffer-size))
		   (condition-case data
		       (insert-buffer-substring work-buffer)
		     (error (signal 'vm-mime-error (cdr data))))
		   ;; This is redundant because insertion moves point
		   ;; (goto-char (+ (point) (- (buffer-size) oldsize)))
		   (if (< (point) (vm-mm-layout-body-end child-layout))
		       (delete-region (point)
				      (vm-mm-layout-body-end child-layout))
		     (vm-set-mm-layout-body-end child-layout (point-marker)))
		   (delete-region (vm-mm-layout-header-start layout)
				  (vm-mm-layout-body-start layout))
		   (vm-mime-copy-layout child-layout layout)))
	     (when work-buffer (kill-buffer work-buffer)))))
	((vm-mime-composite-type-p (car (vm-mm-layout-type layout)))
	 (let ((p (vm-mm-layout-parts layout)))
	   (while p
	     (vm-mime-internalize-local-external-bodies (car p))
	     (setq p (cdr p)))))
	(t nil)))

(defun vm-mime-display-internal-message/partial (layout)
  (if (vectorp layout)
      (let ((buffer-read-only nil))
	(vm-mime-insert-button
	 (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
	 (function
	  (lambda (layout)
	    (save-excursion
	      (vm-mime-display-internal-message/partial layout))))
	 layout nil))
    (vm-inform 6 "Assembling message...")
    (let ((parts nil)
	  (missing nil)
	  (work-buffer nil)
	  extent id o number total m i prev part-header-pos
	  p-id p-number p-total p-list)
      (setq extent layout
	    layout (vm-extent-property extent 'vm-mime-layout)
	    id (vm-mime-get-parameter layout "id"))
      (if (null id)
	  (vm-mime-error
	   "message/partial message missing id parameter"))
      (save-excursion
	(set-buffer (marker-buffer (vm-mm-layout-body-start layout)))
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char (point-min))
	    (while (and (search-forward id nil t)
			(setq m (vm-message-at-point)))
	      (setq o (vm-mm-layout m))
	      (if (not (vectorp o))
		  nil
		(setq p-list (vm-mime-find-message/partials o id))
		(while p-list
		  (setq p-id (vm-mime-get-parameter (car p-list) "id"))
		  (setq p-total (vm-mime-get-parameter (car p-list) "total"))
		  (if (null p-total)
		      nil
		    (setq p-total (string-to-number p-total))
		    (when (< p-total 1)
		      (vm-mime-error 
		       "message/partial specified part total < 1, %d"
		       p-total))
		    (if total
			(unless (= total p-total)
			  (vm-mime-error 
			   (concat "message/partial specified total differs "
				   "between parts, (%d != %d)")
			   p-total total))
		      (setq total p-total)))
		  (setq p-number (vm-mime-get-parameter (car p-list) "number"))
		  (when (null p-number)
		    (vm-mime-error
		     "message/partial message missing number parameter"))
		  (setq p-number (string-to-number p-number))
		  (when (< p-number 1)
		    (vm-mime-error 
		     "message/partial part number < 1, %d" p-number))
		  (when (and total (> p-number total))
		    (vm-mime-error 
		     (concat "message/partial part number greater than "
			     " expected number of parts, (%d > %d)")
		     p-number total))
		  (setq parts (cons (list p-number (car p-list)) parts))
		  (setq p-list (cdr p-list))))
	      (goto-char (vm-mm-layout-body-end o))))))
      (when (null total)
	(vm-mime-error 
	 "total number of parts not specified in any message/partial part"))
      (setq parts (sort parts
			(function
			 (lambda (p q) (< (car p) (car q))))))
      (setq i 0)
      (setq p-list parts)
      (while p-list
	(cond ((< i (car (car p-list)))
	       (vm-increment i)
	       (cond ((not (= i (car (car p-list))))
		      (setq missing (cons i missing)))
		     (t (setq prev p-list
			      p-list (cdr p-list)))))
	      (t
	       ;; remove duplicate part
	       (setcdr prev (cdr p-list))
	       (setq p-list (cdr p-list)))))
      (while (< i total)
	(vm-increment i)
	(setq missing (cons i missing)))
      (if missing
	  (vm-mime-error 
	   "part%s %s%s missing"
	   (if (cdr missing) "s" "")
	   (mapconcat
	    (function identity)
	    (nreverse (mapcar 'int-to-string (or (cdr missing) missing)))
	    ", ")
	   (if (cdr missing) (concat " and " (car missing)) "")))
      (set-buffer (vm-generate-new-unibyte-buffer "assembled message"))
      (setq vm-folder-type vm-default-folder-type)
      (vm-mime-insert-mime-headers (car (cdr (car parts))))
      (goto-char (point-min))
      (vm-reorder-message-headers
       nil :keep-list nil
       :discard-regexp
"\\(Encrypted\\|Content-\\|MIME-Version\\|Message-ID\\|Subject\\|X-VM-\\|Status\\)")
      (goto-char (point-max))
      (setq part-header-pos (point))
      (while parts
	(vm-mime-insert-mime-body (car (cdr (car parts))))
	(setq parts (cdr parts)))
      (goto-char part-header-pos)
      (vm-reorder-message-headers
       nil 
       :keep-list '("Subject" "MIME-Version" "Content-" "Message-ID" "Encrypted")
       :discard-regexp nil)
      (vm-munge-message-separators vm-folder-type (point-min) (point-max))
      (goto-char (point-min))
      (insert (vm-leading-message-separator))
      (goto-char (point-max))
      (insert (vm-trailing-message-separator))
      (set-buffer-modified-p nil)
      (vm-inform 6 "Assembling message... done")
      (vm-save-buffer-excursion
       (vm-goto-new-folder-frame-maybe 'folder)
       (vm-mode)
       (if (vm-should-generate-summary)
	   (progn
	     (vm-goto-new-summary-frame-maybe)
	     (vm-summarize))))
      ;; temp buffer, don't offer to save it.
      (setq buffer-offer-save nil)
      (vm-display (or vm-presentation-buffer (current-buffer)) t
		  (list this-command) '(vm-mode startup)))
    t ))
(fset 'vm-mime-display-button-message/partial
      'vm-mime-display-internal-message/partial)

(defun vm-mime-display-internal-image-xxxx (layout image-type name)
  "Display the image object described by LAYOUT internally.
IMAGE-TYPE is its image type (png, jpeg etc.).  NAME is a string
describing the image type.                             USR, 2011-03-25"
  (cond
   (vm-xemacs-p
    (vm-mime-display-internal-image-xemacs-xxxx layout image-type name))
   ((and vm-fsfemacs-p (fboundp 'image-type-available-p))
    (vm-mime-display-internal-image-fsfemacs-xxxx layout image-type name))
   (t
    (vm-inform 0 "Unsupported Emacs version"))
   ))

(defun vm-mime-display-internal-image-xemacs-xxxx (layout image-type name)
  (if (and (vm-images-possible-here-p)
	   (vm-image-type-available-p image-type))
      (let ((start (point-marker)) end tempfile g e
	    (selective-display nil)
	    (incremental vm-mime-display-image-strips-incrementally)
	    do-strips
	    (keymap (make-sparse-keymap))
	    (buffer-read-only nil))
	(if (and (setq tempfile (get (vm-mm-layout-cache layout)
				     'vm-mime-display-internal-image-xxxx))
		 (file-readable-p tempfile))
	    nil
	  (vm-mime-insert-mime-body layout)
	  (setq end (point-marker))
	  (vm-mime-transfer-decode-region layout start end)
	  (setq tempfile (vm-make-tempfile))
	  (vm-register-folder-garbage-files (list tempfile))
	  ;; coding system for presentation buffer is binary so
	  ;; we don't need to set it here.
	  (write-region start end tempfile nil 0)
	  (put (vm-mm-layout-cache layout)
	       'vm-mime-display-internal-image-xxxx
	       tempfile)
	  (delete-region start end))
	(if (not (bolp))
	    (insert "\n"))
	(setq do-strips (and (stringp vm-imagemagick-convert-program)
			     vm-mime-use-image-strips))
	(cond (do-strips
	       (condition-case error-data
		   (let ((strips (vm-make-image-strips tempfile
						       (* 2 (font-height
							(face-font 'default)))
						       image-type
						       t incremental))
			 process image-list extent-list
			 start
			 (first t))
		     (define-key keymap 'button3 'vm-menu-popup-image-menu)
		     (setq process (car strips)
			   strips (cdr strips)
			   image-list strips)
		     (vm-register-message-garbage-files strips)
		     (setq start (point))
		     (while strips
		       (setq g (make-glyph
				(list
				 (cons nil
				       (vector 'string
					       ':data
					       (if (or first
						       (null (cdr strips)))
						   (progn
						     (setq first nil)
						     "+-----+")
						 "|image|"))))))
		       (insert " \n")
		       (setq e (vm-make-extent (- (point) 2) (1- (point))))
		       (vm-set-extent-property e 'begin-glyph g)
		       (vm-set-extent-property e 'start-open t)
		       (vm-set-extent-property e 'keymap keymap)
		       (setq extent-list (cons e extent-list))
		       (setq strips (cdr strips)))
		     (setq e (vm-make-extent start (point)))
		     (vm-set-extent-property e 'start-open t)
		     (vm-set-extent-property e 'vm-mime-layout layout)
		     (vm-set-extent-property e 'vm-mime-disposable t)
		     (vm-set-extent-property e 'keymap keymap)
		     (save-excursion
		       (set-buffer (process-buffer process))
		       (set (make-local-variable 'vm-image-list) image-list)
		       (set (make-local-variable 'vm-image-type) image-type)
		       (set (make-local-variable 'vm-image-type-name)
			    name)
		       (set (make-local-variable 'vm-extent-list)
			    (nreverse extent-list)))
		     (if incremental
			 (set-process-filter
			  process
			  'vm-process-filter-display-some-image-strips))
		     (set-process-sentinel
		      process
		      'vm-process-sentinel-display-image-strips))
		 (vm-image-too-small
		  (setq do-strips nil))
		 (error
		  (vm-inform 0 "Failed making image strips: %s" error-data)
		  ;; fallback to the non-strips way
		  (setq do-strips nil)))))
	(cond ((not do-strips)
	       (vm-inform 6 "Creating %s glyph..." name)
	       (setq g (make-glyph
			(list
			 (cons (list 'win)
			       (vector image-type ':file tempfile))
			 (cons (list 'win)
			       (vector 'string
				       ':data
				       (format "[Unknown/Bad %s image encoding]"
					       name)))
			 (cons nil
			       (vector 'string
				       ':data
				       (format "[%s image]\n" name))))))
	       (vm-inform 6 "")
	       ;; XEmacs 21.2 can pixel scroll images (sort of)
	       ;; if the entire image is above the baseline.
	       (set-glyph-baseline g 100)
	       (if (memq image-type '(xbm))
		   (set-glyph-face g 'vm-monochrome-image))
	       (insert " \n")
	       (define-key keymap 'button3 'vm-menu-popup-image-menu)
	       (setq e (vm-make-extent (- (point) 2) (1- (point))))
	       (vm-set-extent-property e 'keymap keymap)
	       (vm-set-extent-property e 'begin-glyph g)
	       (vm-set-extent-property e 'vm-mime-layout layout)
	       (vm-set-extent-property e 'vm-mime-disposable t)
	       (vm-set-extent-property e 'start-open t)))
	t )))

(defvar vm-menu-fsfemacs-image-menu)

(defun vm-mime-display-internal-image-fsfemacs-xxxx (layout image-type name)
  "Display the image object described by LAYOUT internally.
IMAGE-TYPE is its image type (png, jpeg etc.).  NAME is a string
describing the image type.                            USR, 2011-03-25"
  (if (and (vm-images-possible-here-p)
	   (vm-image-type-available-p image-type))
      (let (start end tempfile image work-buffer
	    (selective-display nil)
	    (incremental vm-mime-display-image-strips-incrementally)
	    do-strips
	    (buffer-read-only nil))
	(if (and (setq tempfile (get (vm-mm-layout-cache layout)
				     'vm-mime-display-internal-image-xxxx))
		 (file-readable-p tempfile))
	    nil
	  (unwind-protect
	      (progn
		(save-excursion
		  (setq work-buffer (vm-make-work-buffer))
		  (set-buffer work-buffer)
		  (setq start (point))
		  (vm-mime-insert-mime-body layout)
		  (setq end (point-marker))
		  (vm-mime-transfer-decode-region layout start end)
		  (setq tempfile (vm-make-tempfile))
		  (let ((coding-system-for-write (vm-binary-coding-system)))
		    (write-region start end tempfile nil 0))
		  (put (vm-mm-layout-cache layout)
		       'vm-mime-display-internal-image-xxxx
		       tempfile))
		(vm-register-folder-garbage-files (list tempfile)))
	    (and work-buffer (kill-buffer work-buffer))))
	(if (not (bolp))
	    (insert-char ?\n 1))
	(setq do-strips (and (stringp vm-imagemagick-convert-program)
			     vm-mime-use-image-strips))
	(cond (do-strips
	       (condition-case error-data
		   (let ((strips (vm-make-image-strips
				  tempfile
				  (* 2 (frame-char-height))
				  image-type t incremental))
			 (first t)
			 start o process image-list overlay-list)
		     (setq process (car strips)
			   strips (cdr strips)
			   image-list strips)
		     (if (null (process-buffer process))
			 (error "ImageMagick conversion failed"))
		     (vm-register-message-garbage-files strips)
		     (setq start (point))
		     (while strips
		       (if (or first (null (cdr strips)))
			   (progn
			     (setq first nil)
			     (insert "+-----+"))
			 (insert "|image|"))
		       (setq o (make-overlay (- (point) 7) (point)))
		       (overlay-put o 'evaporate t)
		       (setq overlay-list (cons o overlay-list))
		       (insert "\n")
		       (setq strips (cdr strips)))
		     (setq o (make-overlay start (point) nil t nil))
		     (overlay-put o 'vm-mime-layout layout)
		     (overlay-put o 'vm-mime-disposable t)
		     (if vm-use-menus
			 (overlay-put o 'vm-image vm-menu-fsfemacs-image-menu))
		     (save-excursion
		       (set-buffer (process-buffer process))
		       (set (make-local-variable 'vm-image-list) image-list)
		       (set (make-local-variable 'vm-image-type) image-type)
		       (set (make-local-variable 'vm-image-type-name)
			    name)
		       (set (make-local-variable 'vm-overlay-list)
			    (nreverse overlay-list)))
		     (if incremental
			 (set-process-filter
			  process
			  'vm-process-filter-display-some-image-strips))
		     (set-process-sentinel
		      process
		      'vm-process-sentinel-display-image-strips))
		 (vm-image-too-small
		  (setq do-strips nil))
		 (error
		  (vm-inform 0 "Failed making image strips: %s" error-data)
		  ;; fallback to the non-strips way
		  (setq do-strips nil)))))
	(cond ((not do-strips)
	       (setq image (list 'image ':type image-type ':file tempfile))
	       ;; insert one char so we can attach the image to it.
	       (insert "z")
	       (put-text-property (1- (point)) (point) 'display image)
	       (clear-image-cache t)
	       (let (o)
		 (setq o (make-overlay (- (point) 1) (point) nil t nil))
		 (overlay-put o 'evaporate t)
		 (overlay-put o 'vm-mime-layout layout)
		 (overlay-put o 'vm-mime-disposable t)
		 (if vm-use-menus
		     (overlay-put o 'vm-image vm-menu-fsfemacs-image-menu)))))
	t )
    ;; otherwise, image-type not available here
    nil ))

;; FSF Emacs 19 is not supported any more.  USR, 2011-02-23
;; (defun vm-mime-display-internal-image-fsfemacs-19-xxxx (layout image-type name)
;;   (if (and (vm-images-possible-here-p)
;; 	   (vm-image-type-available-p image-type))
;;       (catch 'done
;; 	(let ((selective-display nil)
;; 	      start end origfile workfile image work-buffer
;; 	      (hroll (if vm-fsfemacs-mule-p
;; 			 (+ (cdr (assq 'internal-border-width
;; 				       (frame-parameters)))
;; 			    (if (memq (cdr (assq 'vertical-scroll-bars
;; 						 (frame-parameters)))
;; 				      '(t left))
;; 				(vm-fsfemacs-scroll-bar-width)
;; 			      0))
;; 		       (cdr (assq 'internal-border-width
;; 				  (frame-parameters)))))
;; 	      (vroll (cdr (assq 'internal-border-width (frame-parameters))))
;; 	      (reverse (eq (cdr (assq 'background-mode (frame-parameters)))
;; 			   'dark))
;; 	      blob strips
;; 	      dims width height char-width char-height
;; 	      horiz-pad vert-pad trash-list
;; 	      (buffer-read-only nil))
;; 	  (if (and (setq blob (get (vm-mm-layout-cache layout)
;; 				   'vm-mime-display-internal-image-xxxx))
;; 		   (file-exists-p (car blob))
;; 		   (progn
;; 		     (setq origfile (car blob)
;; 			   workfile (nth 1 blob)
;; 			   width (nth 2 blob)
;; 			   height (nth 3 blob)
;; 			   char-width (nth 4 blob)
;; 			   char-height (nth 5 blob))
;; 		     (and (= char-width (frame-char-width))
;; 			  (= char-height (frame-char-height)))))
;; 	      (setq strips (nth 6 blob))
;; 	    (unwind-protect
;; 		(progn
;; 		  (save-excursion
;; 		    (setq work-buffer (vm-make-work-buffer))
;; 		    (set-buffer work-buffer)
;; 		    (if (and origfile (file-exists-p origfile))
;; 			(progn
;; 			  (insert-file-contents origfile)
;; 			  (setq start (point-min)
;; 				end (vm-marker (point-max))))
;; 		      (setq start (point))
;; 		      (vm-mime-insert-mime-body layout)
;; 		      (setq end (point-marker))
;; 		      (vm-mime-transfer-decode-region layout start end)
;; 		      (setq origfile (vm-make-tempfile))
;; 		      (setq trash-list (cons origfile trash-list))
;; 		      (let ((coding-system-for-write (vm-binary-coding-system)))
;; 			(write-region start end origfile nil 0)))
;; 		    (setq dims (condition-case error-data
;; 				   (vm-get-image-dimensions origfile)
;; 				 (error
;; 				  (vm-inform 0 "Failed getting image dimensions: %s"
;; 					   error-data)
;; 				  (throw 'done nil)))
;; 			  width (nth 0 dims)
;; 			  height (nth 1 dims)
;; 			  char-width (frame-char-width)
;; 			  char-height (frame-char-height)
;; 			  horiz-pad (if (< width char-width)
;; 					(- char-width width)
;; 				      (% width char-width))
;; 			  horiz-pad (if (zerop horiz-pad)
;; 					horiz-pad
;; 				      (- char-width horiz-pad))
;; 			  vert-pad (if (< height char-height)
;; 				       (- char-height height)
;; 				     (% height char-height))
;; 			  vert-pad (if (zerop vert-pad)
;; 				       vert-pad
;; 				     (- char-height vert-pad)))
;; 		    ;; crop one line from the bottom of the image
;; 		    ;; if vertical padding needed is odd so that
;; 		    ;; the image height plus the padding will be an
;; 		    ;; exact multiple of the char height.
;; 		    (if (not (zerop (% vert-pad 2)))
;; 			(setq height (1- height)
;; 			      vert-pad (1+ vert-pad)))
;; 		    (call-process-region start end
;; 					 vm-imagemagick-convert-program
;; 					 t t nil
;; 					 (if reverse "-negate" "-matte")
;; 					 "-crop"
;; 					 (format "%dx%d+0+0" width height)
;; 					 "-page"
;; 					 (format "%dx%d+0+0" width height)
;; 					 "-mattecolor" "white"
;; 					 "-frame"
;; 					 (format "%dx%d+0+0"
;; 						 (/ (1+ horiz-pad) 2)
;; 						 (/ vert-pad 2))
;; 					 "-"
;; 					 "-")
;; 		    (setq width (+ width (* 2 (/ (1+ horiz-pad) 2)))
;; 			  height (+ height (* 2 (/ vert-pad 2))))
;; 		    (if (null workfile)
;; 			(setq workfile (vm-make-tempfile)
;; 			      trash-list (cons workfile trash-list)))
;; 		    (let ((coding-system-for-write (vm-binary-coding-system)))
;; 		      (write-region (point-min) (point-max) workfile nil 0))
;; 		    (put (vm-mm-layout-cache layout)
;; 			 'vm-mime-display-internal-image-xxxx
;; 			 (list origfile workfile width height
;; 			       char-width char-height)))
;; 		  (when trash-list
;; 		       (vm-register-folder-garbage-files trash-list)))
;; 	      (and work-buffer (kill-buffer work-buffer))))
;; 	  (if (not (bolp))
;; 	      (insert-char ?\n 1))
;; 	  (condition-case error-data
;; 	      (let (o i-start start process image-list overlay-list)
;; 		(if (and strips (file-exists-p (car strips)))
;; 		    (setq image-list strips)
;; 		  (setq strips (vm-make-image-strips workfile char-height
;; 						     image-type t nil
;; 						     hroll vroll)
;; 			process (car strips)
;; 			strips (cdr strips)
;; 			image-list strips)
;; 		  (put (vm-mm-layout-cache layout)
;; 		       'vm-mime-display-internal-image-xxxx
;; 		       (list origfile workfile width height
;; 			     char-width char-height
;; 			     strips))
;; 		  (vm-register-message-garbage-files strips))
;; 		(setq i-start (point))
;; 		(while strips
;; 		  (setq start (point))
;; 		  (insert-char ?\  (/ width char-width))
;; 		  (put-text-property start (point) 'face 'vm-image-placeholder)
;; 		  (setq o (make-overlay start (point) nil t))
;; 		  (overlay-put o 'evaporate t)
;; 		  (setq overlay-list (cons o overlay-list))
;; 		  (insert "\n")
;; 		  (setq strips (cdr strips)))
;; 		(setq o (make-overlay i-start (point) nil t nil))
;; 		(overlay-put o 'vm-mime-layout layout)
;; 		(overlay-put o 'vm-mime-disposable t)
;; 		(if vm-use-menus
;; 		    (overlay-put o 'vm-image vm-menu-fsfemacs-image-menu))
;; 		(if process
;; 		    (save-excursion
;; 		      (set-buffer (process-buffer process))
;; 		      (set (make-local-variable 'vm-image-list) image-list)
;; 		      (set (make-local-variable 'vm-image-type) image-type)
;; 		      (set (make-local-variable 'vm-image-type-name)
;; 			   name)
;; 		      (set (make-local-variable 'vm-overlay-list)
;; 			   (nreverse overlay-list))
;; 		      ;; incremental strip display intentionally
;; 		      ;; omitted because it makes the Emacs 19
;; 		      ;; display completely repaint for each new
;; 		      ;; strip.
;; 		      (set-process-sentinel
;; 		       process
;; 		       'vm-process-sentinel-display-image-strips))
;; 		  (vm-display-image-strips-on-overlay-regions image-list
;; 							      (nreverse
;; 							       overlay-list)
;; 							      image-type)))
;; 	    (error
;; 	     (vm-inform 0 "Failed making image strips: %s" error-data)))
;; 	  t ))
;;     nil ))

(defun vm-get-image-dimensions (file)
  (let (work-buffer width height)
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (set-buffer work-buffer)
	  (call-process vm-imagemagick-identify-program nil t nil file)
	  (goto-char (point-min))
	  (or (search-forward " " nil t)
	      (error "no spaces in 'identify' output: %s"
		     (buffer-string)))
	  (if (not (re-search-forward "\\b\\([0-9]+\\)x\\([0-9]+\\)\\b" nil t))
	      (error "file dimensions missing from 'identify' output: %s"
		     (buffer-string)))
	  (setq width (string-to-number (match-string 1))
		height (string-to-number (match-string 2))))
      (and work-buffer (kill-buffer work-buffer)))
    (list width height)))

(defun vm-imagemagick-type-indicator-for (image-type)
  (cond ((eq image-type 'jpeg) "jpeg:")
	((eq image-type 'gif) "gif:")
	((eq image-type 'png) "png:")
	((eq image-type 'tiff) "tiff:")
	((eq image-type 'xpm) "xpm:")
	((eq image-type 'pbm) "pbm:")
	((eq image-type 'xbm) "xbm:")
	(t "")))

(defun vm-make-image-strips (file min-height image-type async incremental
				  &optional hroll vroll)
  (or hroll (setq hroll 0))
  (or vroll (setq vroll 0))
  (let ((process-connection-type nil)
	(i 0)
	(output-type (vm-imagemagick-type-indicator-for image-type))
	image-list dimensions width height starty newfile work-buffer
	quotient remainder adjustment process)
    (setq dimensions (vm-get-image-dimensions file)
	  width (car dimensions)
	  height (car (cdr dimensions)))
    (if (< height min-height)
	(signal 'vm-image-too-small nil))
    (setq quotient (/ height min-height)
	  remainder (% height min-height)
	  adjustment (/ remainder quotient)
	  remainder (% remainder quotient)
	  starty 0)
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (set-buffer work-buffer)
	  (goto-char (point-min))
	  (while (< starty height)
	    (setq newfile (vm-make-tempfile))
	    (if async
		(progn
		  ;; Problem - we have no way of knowing whether these
		  ;; calls succeed or not.  USR, 2011-02-23
		  (insert vm-imagemagick-convert-program
			  " -crop"
			  (format " %dx%d+0+%d"
				  width
				  (+ min-height adjustment
				     (if (zerop remainder) 0 1))
				  starty)
			  " -page"
			  (format " %dx%d+0+0"
				  width
				  (+ min-height adjustment
				     (if (zerop remainder) 0 1)))
			  (format " -roll +%d+%d" hroll vroll)
			  " \"" file "\" \"" output-type newfile "\"\n")
		  (when incremental
			(insert "echo XZXX" (int-to-string i) "XZXX\n"))
		  (setq i (1+ i)))
	      (call-process vm-imagemagick-convert-program nil nil nil
			    "-crop"
			    (format "%dx%d+0+%d"
				    width
				    (+ min-height adjustment
				       (if (zerop remainder) 0 1))
				    starty)
			    "-page"
			    (format "%dx%d+0+0"
				    width
				    (+ min-height adjustment
				       (if (zerop remainder) 0 1)))
			    "-roll"
			    (format "+%d+%d" hroll vroll)
			    file (concat output-type newfile)))
	    (setq image-list (cons newfile image-list)
		  starty (+ starty min-height adjustment
			    (if (zerop remainder) 0 1))
		  remainder (if (= 0 remainder) 0 (1- remainder))))
	  (when async
	    (goto-char (point-max))
	    (insert "exit\n")
	    (setq process
		  (start-process (format "image strip maker for %s" file)
				 (current-buffer)
				 shell-file-name))
	    (process-send-string process (buffer-string))
	    (setq work-buffer nil))
	  (if async
	      (cons process (nreverse image-list))
	    (nreverse image-list)))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-process-sentinel-display-image-strips (process what-happened)
  (save-excursion
    (set-buffer (process-buffer process))
    (cond ((and (boundp 'vm-extent-list)
		(boundp 'vm-image-list))
	   (let ((strips vm-image-list)
		 (extents vm-extent-list)
		 (image-type vm-image-type)
		 (type-name vm-image-type-name))
	     (vm-display-image-strips-on-extents strips extents image-type
						 type-name)))
	  ((and (boundp 'vm-overlay-list)
		(overlay-buffer (car vm-overlay-list))
		(boundp 'vm-image-list))
	   (let ((strips vm-image-list)
		 (overlays vm-overlay-list)
		 (image-type vm-image-type))
	     (vm-display-image-strips-on-overlay-regions strips overlays
							 image-type))))
    (kill-buffer (current-buffer))))

(defun vm-display-image-strips-on-extents (strips extents image-type type-name)
  (let (g)
    (while (and strips
		(file-exists-p (car strips))
		(extent-live-p (car extents))
		(vm-extent-object (car extents)))
      (setq g (make-glyph
	       (list
		(cons (list 'win)
		      (vector image-type ':file (car strips)))
		(cons (list 'win)
		      (vector
		       'string
		       ':data
		       (format "[Unknown/Bad %s image encoding]"
			       type-name)))
		(cons nil
		      (vector 'string
			      ':data
			      (format "[%s image]\n" type-name))))))
      (set-glyph-baseline g 50)
      (if (memq image-type '(xbm))
	  (set-glyph-face g 'vm-monochrome-image))
      (set-extent-begin-glyph (car extents) g)
      (setq strips (cdr strips)
	    extents (cdr extents)))))

(defun vm-display-image-strips-on-overlay-regions (strips overlays image-type)
  (let (prop value omodified)
    (save-excursion
      (set-buffer (overlay-buffer (car vm-overlay-list)))
      (setq omodified (buffer-modified-p))
      (save-restriction
	(widen)
	(unwind-protect
	    (let ((buffer-read-only nil))
	      (if (fboundp 'image-type-available-p)
		  (setq prop 'display)
		(setq prop 'face))
	      (while (and strips
			  (file-exists-p (car strips))
			  (overlay-end (car overlays)))
		(if (fboundp 'image-type-available-p)
		    (setq value (list 'image ':type image-type
				      ':file (car strips)
				      ':ascent 50))
		  (setq value (make-face (make-symbol "<vm-image-face>")))
		  (set-face-stipple value (car strips)))
		(put-text-property (overlay-start (car overlays))
				   (overlay-end (car overlays))
				   prop value)
		(setq strips (cdr strips)
		      overlays (cdr overlays))))
	  (set-buffer-modified-p omodified))))))

(defun vm-process-filter-display-some-image-strips (process output)
  (let (which-strips (i 0))
    (while (string-match "XZXX\\([0-9]+\\)XZXX" output i)
      (setq which-strips (cons (string-to-number (match-string 1 output))
			       which-strips)
	    i (match-end 0)))
    (save-excursion
      (set-buffer (process-buffer process))
      (cond ((and (boundp 'vm-extent-list)
		  (boundp 'vm-image-list))
	     (let ((strips vm-image-list)
		   (extents vm-extent-list)
		   (image-type vm-image-type)
		   (type-name vm-image-type-name))
	       (vm-display-some-image-strips-on-extents strips extents
							image-type
							type-name
							which-strips)))
	    ((and (boundp 'vm-overlay-list)
		  (overlay-buffer (car vm-overlay-list))
		  (boundp 'vm-image-list))
	     (let ((strips vm-image-list)
		   (overlays vm-overlay-list)
		   (image-type vm-image-type))
	       (vm-display-some-image-strips-on-overlay-regions
		strips overlays image-type which-strips)))))))

(defun vm-display-some-image-strips-on-extents
  (strips extents image-type type-name which-strips)
  (let (g sss eee)
    (while which-strips
      (setq sss (nthcdr (car which-strips) strips)
	    eee (nthcdr (car which-strips) extents))
      (cond ((and sss
		  (file-exists-p (car sss))
		  (extent-live-p (car eee))
		  (vm-extent-object (car eee)))
	     (setq g (make-glyph
		      (list
		       (cons (list 'win)
			     (vector image-type ':file (car sss)))
		       (cons (list 'win)
			     (vector
			      'string
			      ':data
			      (format "[Unknown/Bad %s image encoding]"
				      type-name)))
		       (cons nil
			     (vector 'string
				     ':data
				     (format "[%s image]\n" type-name))))))
	     (set-glyph-baseline g 50)
	     (if (memq image-type '(xbm))
		 (set-glyph-face g 'vm-monochrome-image))
	     (set-extent-begin-glyph (car eee) g)))
      (setq which-strips (cdr which-strips)))))

(defun vm-display-some-image-strips-on-overlay-regions
  (strips overlays image-type which-strips)
  (let (sss ooo prop value omodified)
    (save-excursion
      (set-buffer (overlay-buffer (car vm-overlay-list)))
      (setq omodified (buffer-modified-p))
      (save-restriction
	(widen)
	(unwind-protect
	    (let ((buffer-read-only nil))
	      (if (fboundp 'image-type-available-p)
		  (setq prop 'display)
		(setq prop 'face))
	      (while which-strips
		(setq sss (nthcdr (car which-strips) strips)
		      ooo (nthcdr (car which-strips) overlays))
		(cond ((and sss
			    (file-exists-p (car sss))
			    (overlay-end (car ooo)))
		       (if (fboundp 'image-type-available-p)
			   (setq value (list 'image ':type image-type
					     ':file (car sss)
					     ':ascent 50))
			 (setq value (make-face (make-symbol
						 "<vm-image-face>")))
			 (set-face-stipple value (car sss)))
		       (put-text-property (overlay-start (car ooo))
					  (overlay-end (car ooo))
					  prop value)))
		(setq which-strips (cdr which-strips))))
	  (set-buffer-modified-p omodified))))))

(defun vm-mime-display-internal-image/gif (layout)
  (vm-mime-display-internal-image-xxxx layout 'gif "GIF"))

(defun vm-mime-display-internal-image/jpeg (layout)
  (vm-mime-display-internal-image-xxxx layout 'jpeg "JPEG"))

(defun vm-mime-display-internal-image/png (layout)
  (vm-mime-display-internal-image-xxxx layout 'png "PNG"))

(defun vm-mime-display-internal-image/tiff (layout)
  (vm-mime-display-internal-image-xxxx layout 'tiff "TIFF"))

(defun vm-mime-display-internal-image/xpm (layout)
  (vm-mime-display-internal-image-xxxx layout 'xpm "XPM"))

(defun vm-mime-display-internal-image/pbm (layout)
  (vm-mime-display-internal-image-xxxx layout 'pbm "PBM"))

(defun vm-mime-display-internal-image/xbm (layout)
  (vm-mime-display-internal-image-xxxx layout 'xbm "XBM"))

(defun vm-mime-frob-image-xxxx (extent &rest convert-args)
  "Create and display a thumbnail (a PNG image) for the MIME
object described by EXTENT.  The thumbnail is stored in a file
whose identity is saved in the MIME layout cache of the object.

The remaining arguments CONVERT-ARGS are passed to the ImageMagick
convert program during the creation of the thumbnail image.  

The return value does not seem to be meaningful.     USR, 2011-03-25"
  (let* ((layout (vm-extent-property extent 'vm-mime-layout))
	 (blob (get (vm-mm-layout-cache layout)
		    'vm-mime-display-internal-image-xxxx))
         (saved-type (vm-mm-layout-type layout))
         success tempfile
	 (work-buffer nil))
    ;; Emacs 19 uses a different layout cache than XEmacs or Emacs 21+.
    ;; The cache blob is a list in that case.
    (if (consp blob)
	(setq tempfile (car blob))
      (setq tempfile blob))
    (setq work-buffer (vm-make-work-buffer))
    (unwind-protect
	(with-current-buffer work-buffer
	  (set-buffer-file-coding-system (vm-binary-coding-system))
          ;; convert just the first page "[0]" and enforce PNG output by "png:"
	  (let ((coding-system-for-read (vm-binary-coding-system)))
	    (setq success
		  (eq 0 (apply 'call-process vm-imagemagick-convert-program
			       tempfile t nil
			       (append convert-args (list "-[0]" "png:-"))))))
	  (when success
	    (write-region (point-min) (point-max) tempfile nil 0)
	    (when (consp blob)
	      (setcar (nthcdr 5 blob) 0))
	    (put (vm-mm-layout-cache layout) 'vm-image-modified t)))
      ;; unwind-protection
      (when work-buffer (kill-buffer work-buffer)))
    (unwind-protect
	(when success
	  ;; the output is always PNG now, so fix it for displaying, but restore
	  ;; it for the layout afterwards
	  (vm-set-mm-layout-type layout '("image/png"))
	  (vm-mark-image-tempfile-as-message-garbage-once layout tempfile)
	  (vm-mime-display-internal-generic extent))
      (vm-set-mm-layout-type layout saved-type))))

(defun vm-mark-image-tempfile-as-message-garbage-once (layout tempfile)
  (if (get (vm-mm-layout-cache layout) 'vm-message-garbage)
      nil
    (vm-register-message-garbage-files (list tempfile))
    (put (vm-mm-layout-cache layout) 'vm-message-garbage t)))

(defun vm-mime-rotate-image-left (extent)
  (vm-mime-frob-image-xxxx extent "-rotate" "-90"))

(defun vm-mime-rotate-image-right (extent)
  (vm-mime-frob-image-xxxx extent "-rotate" "90"))

(defun vm-mime-mirror-image (extent)
  (vm-mime-frob-image-xxxx extent "-flop"))

(defun vm-mime-brighten-image (extent)
  (vm-mime-frob-image-xxxx extent "-modulate" "115"))

(defun vm-mime-dim-image (extent)
  (vm-mime-frob-image-xxxx extent "-modulate" "85"))

(defun vm-mime-monochrome-image (extent)
  (vm-mime-frob-image-xxxx extent "-monochrome"))

(defun vm-mime-revert-image (extent)
  (let* ((layout (vm-extent-property extent 'vm-mime-layout))
	 (blob (get (vm-mm-layout-cache layout)
		    'vm-mime-display-internal-image-xxxx))
	 tempfile)
    ;; Emacs 19 uses a different layout cache than XEmacs or Emacs 21+.
    ;; The cache blob is a list in that case.
    (if (consp blob)
	(setq tempfile (car blob))
      (setq tempfile blob))
    (and (stringp tempfile)
	 (vm-error-free-call 'delete-file tempfile))
    (put (vm-mm-layout-cache layout) 'vm-image-modified nil)
    (vm-mime-display-generic extent)))

(defun vm-mime-larger-image (extent)
  (let* ((layout (vm-extent-property extent 'vm-mime-layout))
	 (blob (get (vm-mm-layout-cache layout)
		    'vm-mime-display-internal-image-xxxx))
	 dims tempfile)
    ;; Emacs 19 uses a different layout cache than XEmacs or Emacs 21+.
    ;; The cache blob is a list in that case.
    (if (consp blob)
	(setq tempfile (car blob))
      (setq tempfile blob))
    (setq dims (vm-get-image-dimensions tempfile))
    (vm-mime-frob-image-xxxx extent
			     "-scale"
			     (concat (int-to-string (* 2 (car dims)))
				     "x"
				     (int-to-string (* 2 (nth 1 dims)))))))

(defun vm-mime-smaller-image (extent)
  (let* ((layout (vm-extent-property extent 'vm-mime-layout))
	 (blob (get (vm-mm-layout-cache layout)
		    'vm-mime-display-internal-image-xxxx))
	 dims tempfile)
    ;; Emacs 19 uses a different layout cache than XEmacs or Emacs 21+.
    ;; The cache blob is a list in that case.
    (if (consp blob)
	(setq tempfile (car blob))
      (setq tempfile blob))
    (setq dims (vm-get-image-dimensions tempfile))
    (vm-mime-frob-image-xxxx extent
			     "-scale"
			     (concat (int-to-string (/ (car dims) 2))
				     "x"
				     (int-to-string (/ (nth 1 dims) 2))))))

(defcustom vm-mime-thumbnail-max-geometry "80x80"
  "If thumbnails should be displayed as part of MIME buttons, then set
this variable to a string describing the geometry, e.g., \"80x80\".
Otherwise, set it to nil.                              USR, 2011-03-25"
  :group 'vm-mime
  :type '(choice string
		 (const :tag "Disable thumbnails." nil)))

(defun vm-mime-display-button-image (layout)
  "Displays a button for the MIME LAYOUT and includes a thumbnail
image when possible."
  (if (and vm-imagemagick-convert-program
	   vm-mime-thumbnail-max-geometry
	   (vm-images-possible-here-p))
      ;; create a thumbnail and display it
      (let (tempfile start end thumb-extent glyph)
	;; fake an extent to display the image as thumb
	(setq start (point))
	(insert " ")
	(setq thumb-extent (vm-make-extent start (point)))
	(vm-set-extent-property thumb-extent 'vm-mime-layout layout)
	(vm-set-extent-property thumb-extent 'vm-mime-disposable nil)
	(vm-set-extent-property thumb-extent 'start-open t)
	;; write out the image data 
	(with-current-buffer (vm-make-work-buffer)
	  (vm-mime-insert-mime-body layout)
	  (vm-mime-transfer-decode-region layout (point-min) (point-max))
	  (setq tempfile (vm-make-tempfile))
	  (let ((coding-system-for-write (vm-binary-coding-system)))
	    (write-region (point-min) (point-max) tempfile nil 0))
	  (kill-buffer (current-buffer)))
	;; store the temp filename
	(put (vm-mm-layout-cache layout)
	     'vm-mime-display-internal-image-xxxx
	     tempfile)
	(vm-register-folder-garbage-files (list tempfile))
	;; display a thumbnail over the fake extent
	(let ((vm-mime-internal-content-types '("image"))
	      (vm-mime-internal-content-type-exceptions nil)
	      (vm-mime-use-image-strips nil))
	  (vm-mime-frob-image-xxxx thumb-extent
				   "-thumbnail" 
				   vm-mime-thumbnail-max-geometry))
	;; extract image data, don't need the image itself!
	;; if the display was not successful, glyph will be nil
	(setq glyph (if vm-xemacs-p
			(let ((e1 (vm-extent-at start))
			      (e2 (vm-extent-at (1+ start))))
			  (or (and e1 (extent-begin-glyph e1))
			      (and e2 (extent-begin-glyph e2))))
		      (get-text-property start 'display)))
	(delete-region start (point))
	;; insert the button and replace the image 
	(setq start (point))
	(vm-mime-display-button-xxxx layout t)
	(when glyph
	  (if vm-xemacs-p
	      (set-extent-begin-glyph (vm-extent-at start) glyph)
	    (put-text-property start (1+ start) 'display glyph)))
	;; remove the cached thumb so that full sized image will be shown
	(put (vm-mm-layout-cache layout)
	     'vm-mime-display-internal-image-xxxx
	     nil)
	t)
    ;; just display the normal button
    (vm-mime-display-button-xxxx layout t)))

(defun vm-mime-display-button-application/pdf (layout)
  (vm-mime-display-button-image layout))

(defun vm-mime-display-internal-audio/basic (layout)
  (if (and vm-xemacs-p
	   (or (featurep 'native-sound)
	       (featurep 'nas-sound))
	   (or (device-sound-enabled-p)
	       (and (featurep 'native-sound)
		    (not native-sound-only-on-console)
		    (memq (vm-device-type) '(x gtk)))))
      (let ((start (point-marker)) end tempfile
	    (selective-display nil)
	    (buffer-read-only nil))
	(if (setq tempfile (get (vm-mm-layout-cache layout)
				'vm-mime-display-internal-audio/basic))
	    nil
	  (vm-mime-insert-mime-body layout)
	  (setq end (point-marker))
	  (vm-mime-transfer-decode-region layout start end)
	  (setq tempfile (vm-make-tempfile))
	  (vm-register-folder-garbage-files (list tempfile))
	  ;; coding system for presentation buffer is binary, so
	  ;; we don't need to set it here.
	  (write-region start end tempfile nil 0)
	  (put (vm-mm-layout-cache layout)
	       'vm-mime-display-internal-audio/basic
	       tempfile)
	  (delete-region start end))
	(start-itimer "audioplayer"
		      (list 'lambda nil (list 'play-sound-file tempfile))
		      1)
	t )
    nil ))

(defun vm-mime-display-generic (layout)
  "Display the mime object described by LAYOUT, irrespective of
whether it is meant to be to be displayed automatically."
  (save-excursion
    (let ((vm-mime-auto-displayed-content-types t)
	  (vm-mime-auto-displayed-content-type-exceptions nil))
      (vm-decode-mime-layout layout t))))

(defun vm-mime-display-internal-generic (layout)
  "Display the mime object described by LAYOUT internally,
irrespective of whether it is meant to be to be displayed
automatically.  No external viewers are tried.     USR, 2011-03-25"
  (save-excursion
    (let ((vm-mime-auto-displayed-content-types t)
	  (vm-mime-auto-displayed-content-type-exceptions nil)
	  (vm-mime-external-content-types-alist nil))
      (vm-decode-mime-layout layout t))))

(defun vm-mime-display-button-xxxx (layout disposable)
  "Display a button for the mime object described by LAYOUT.  If
DISPOSABLE is true, then the button will be removed when it is
expanded to display the mime object."
  (vm-mime-insert-button
   (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
   (function vm-mime-display-generic)
   layout disposable))

;;----------------------------------------------------------------------------
;;; MIME buttons
;;
;; vm-find-layout-extent-at-point: () -> extent
;; vm-mime-run-display-funciton-at-point: (layout -> 'a) -> 'a
;; vm-mime-reader-map-save-file: () -> file
;; vm-mime-reader-map-save-message: () -> file
;; vm-mime-reader-map-pipe-to-command: () -> void
;; vm-mime-reader-map-pipe-to-command-discard-output: () -> void
;; vm-mime-reader-map-pipe-to-printer: () -> void
;; vm-mime-reader-map-display-using-external-viewer: () -> void
;; vm-mime-reader-map-display-using-default: () -> void
;; vm-mime-reader-map-display-object-as-type: () -> void
;; vm-mime-reader-map-attach-to-composition: () -> void
;;----------------------------------------------------------------------------

(defun vm-find-layout-extent-at-point ()
  "Return the MIME layout of the MIME button at point."
  (vm-extent-at (point) 'vm-mime-layout))

;;;###autoload
(defun vm-mime-run-display-function-at-point (&optional function)
  "Run the 'vm-mime-function for the MIME button at point.
If optional argument FUNCTION is given, run it instead.
					          USR, 2011-03-07"
  (interactive)
  (if (and (memq major-mode '(vm-mode vm-virtual-mode))
	   (vm-body-to-be-retrieved-of (car vm-message-pointer)))
      (error "Message must be loaded to view attachments" ))

  ;; save excursion to keep point from moving.  its motion would
  ;; drag window point along, to a place arbitrarily far from
  ;; where it was when the user triggered the button.
  (save-excursion
    (let ((extent (vm-find-layout-extent-at-point))
	  retval )
      (and extent
	   (funcall 
	    (or function (vm-extent-property extent 'vm-mime-function))
	    extent)))))

;;;###autoload
(defun vm-mime-reader-map-save-file ()
  "Write the MIME object at point to a file."
  (interactive)
  ;; make sure point doesn't move, we need it to stay on the tag
  ;; if the user wants to delete after saving.
  (let (file)
    (save-excursion
      (setq file (vm-mime-run-display-function-at-point
		  'vm-mime-send-body-to-file)))
    (when (and file vm-mime-delete-after-saving)
      (let ((extent (vm-find-layout-extent-at-point)))
	(vm-mime-delete-body-after-saving extent file)))
    file ))

;;;###autoload
(defun vm-mime-reader-map-save-message ()
  "Save the MIME object at point to a folder."
  (interactive)
  ;; make sure point doesn't move, we need it to stay on the tag
  ;; if the user wants to delete after saving.
  (let (folder)
    (save-excursion
      (setq folder (vm-mime-run-display-function-at-point
		    'vm-mime-send-body-to-folder)))
    (when (and folder vm-mime-delete-after-saving)
      (let ((extent (vm-find-layout-extent-at-point)))
	(vm-mime-delete-body-after-saving extent folder)))
    folder ))

;;;###autoload
(defun vm-mime-reader-map-pipe-to-command ()
  "Pipe the MIME object at point to a shell command."
  (interactive)
  (vm-mime-run-display-function-at-point
   'vm-mime-pipe-body-to-queried-command))

;;;###autoload
(defun vm-mime-reader-map-pipe-to-command-discard-output ()
  "Pipe the MIME object at point to a shell command."
  (interactive)
  (vm-mime-run-display-function-at-point
   'vm-mime-pipe-body-to-queried-command-discard-output))

;;;###autoload
(defun vm-mime-reader-map-pipe-to-printer ()
  "Print the MIME object at point."
  (interactive)
  (vm-mime-run-display-function-at-point 
   'vm-mime-send-body-to-printer))

;;;###autoload
(defun vm-mime-reader-map-display-using-external-viewer ()
  "Display the MIME object at point with an external viewer."
  (interactive)
  (vm-mime-run-display-function-at-point
   'vm-mime-display-body-using-external-viewer))

;;;###autoload
(defun vm-mime-reader-map-display-using-default ()
  "Display the MIME object at point using the `default' face."
  (interactive)
  (vm-mime-run-display-function-at-point 
   'vm-mime-display-body-as-text))

;;;###autoload
(defun vm-mime-reader-map-display-object-as-type ()
  "Display the MIME object at point as some other type."
  (interactive)
  (vm-mime-run-display-function-at-point 
   'vm-mime-display-object-as-type))

;;;###autoload
(defun vm-mime-reader-map-convert-then-display ()
  "Convert the MIME object at point to text and display it."
  (interactive)
  (vm-mime-run-display-function-at-point 
   'vm-mime-convert-body-then-display))

;;;###autoload
(defun vm-mime-reader-map-attach-to-composition ()
  "Attach the MIME object at point to a message being composed.  The
buffer for message composition is queried from the minibufer."
  (interactive)
  (vm-mime-run-display-function-at-point
   'vm-mime-attach-body-to-composition))

;;----------------------------------------------------------------------------
;;; MIME-related commands
;;
;; vm-mime-action-on-all-attachments :
;;	(int, ((message, layout, type, filename) -> void),
;;	 &optional type list, message list, bool) 
;;	-> void
;;----------------------------------------------------------------------------

;;;###autoload
(defun vm-mime-action-on-all-attachments 
  (count action &optional types exceptions mlist quiet)
  "On the next COUNT messages or marked messages, call the
function ACTION on all \"attachments\".  For the purpose of this
function, an \"attachment\" is a mime part part which has
\"attachment\" as its disposition, or simply has an associated
filename, or has a type that matches a regexp in TYPES but
doesn't match one in EXCEPTIONS.

If QUIET is true no messages are generated.

ACTION will get called with four arguments: MSG LAYOUT TYPE FILENAME." 
  (unless mlist
    (or count (setq count 1))
    (vm-check-for-killed-folder)
    (vm-select-folder-buffer-and-validate 1 nil))

  (let ((mlist (or mlist (vm-select-operable-messages
			  count (interactive-p) "Action on"))))
    (vm-retrieve-operable-messages count mlist)
    (save-excursion
      (while mlist
        (let (m parts layout filename type disposition o)
          (setq o (vm-mm-layout (car mlist)))
          (when (stringp o)
            (setq o 'none)
            (backtrace)
            (vm-inform 0 "There is a bug, please report it with *backtrace*"))
          (unless (eq o 'none)
            (setq type (car (vm-mm-layout-type o)))
            
            (cond ((or (vm-mime-types-match "multipart/alternative" type)
                       (vm-mime-types-match "multipart/mixed" type)
                       (vm-mime-types-match "multipart/report" type)
                       (vm-mime-types-match "message/rfc822" type)
                       )
                   (setq parts (copy-sequence (vm-mm-layout-parts o))))
                  (t (setq parts (list o))))
            
            (while parts
              (while (vm-mime-composite-type-p
		      (car (vm-mm-layout-type (car parts))))
		(setq parts 
		      (nconc (copy-sequence (vm-mm-layout-parts (car parts)))
			     (cdr parts))))
              
              (setq layout (car parts)
                    type (car (vm-mm-layout-type layout))
                    disposition (car (vm-mm-layout-disposition layout))
                    filename (vm-mime-get-disposition-filename layout) )
              
              (cond ((or filename
                         (and disposition (string= disposition "attachment"))
                         (and (not (vm-mime-types-match 
				    "message/external-body" type))
                              types
                              (vm-mime-is-type-valid type types exceptions)))
                     (when (not quiet)
                       (vm-inform 8
			"Action on part type=%s filename=%s disposition=%s"
			type filename disposition))
                     (funcall action (car mlist) layout type filename))
                    ((not quiet)
                     (vm-inform 8
		      "No action on part type=%s filename=%s disposition=%s"
		      type filename disposition)))
              (setq parts (cdr parts)))))
        (setq mlist (cdr mlist))))))

(defun vm-mime-is-type-valid (type types-alist type-exceptions)
  (catch 'done
    (let ((list type-exceptions)
          (matched nil))
      (while list
        (if (vm-mime-types-match (car list) type)
            (throw 'done nil)
          (setq list (cdr list))))
      (setq list types-alist)
      (while (and list (not matched))
        (if (vm-mime-types-match (car list) type)
            (setq matched t)
          (setq list (cdr list))))
      matched )))

;;;###autoload
(defun vm-delete-all-attachments (&optional count)
  "Delete all attachments from the next COUNT messages or marked
messages.  For the purpose of this function, an \"attachment\" is
a mime part part which has \"attachment\" as its disposition or
simply has an associated filename.  Any mime types that match
`vm-mime-deleteable-types' but not `vm-mime-deleteable-type-exceptions'
are also included."
  (interactive "p")
  (vm-check-for-killed-summary)
  (if (interactive-p) (vm-follow-summary-cursor))
  
  (let ((n 0))
    (vm-mime-action-on-all-attachments
     count
     (lambda (msg layout type file)
       (vm-inform 7 "Deleting `%s%s" type (if file (format " (%s)" file) ""))
       (vm-mime-discard-layout-contents layout)
       (setq n (+ 1 n)))
     vm-mime-deleteable-types
     vm-mime-deleteable-type-exceptions)
    (when (interactive-p)
      (vm-discard-cached-data count)
      (let ((vm-preview-lines nil))
	(vm-present-current-message)))
    (if (> n 0)
	(vm-inform 5 "%d attachment%s deleted" n (if (= n 1) "" "s"))
      (vm-inform 5 "No attachments deleted")))
  (vm-update-summary-and-mode-line))

;; (define-obsolete-function-alias 'vm-mime-delete-all-attachments
;;   'vm-delete-all-attachments "8.2.0")
(defalias 'vm-mime-delete-all-attachments
  'vm-delete-all-attachments)
(make-obsolete 'vm-mime-delete-all-attachments
	       'vm-delete-all-attachments "8.2.0")

;;;###autoload
(defun vm-save-all-attachments (&optional count
					  directory
					  no-delete-after-saving)
  "Save all attachments in the next COUNT messages or marked
messages.  For the purpose of this function, an \"attachment\" is
a mime part part which has \"attachment\" as its disposition or
simply has an associated filename.  Any mime types that match
`vm-mime-saveable-types' but not `vm-mime-saveable-type-exceptions'
are also included.

The attachments are saved to the specified DIRECTORY.  The
variables `vm-all-attachments-directory' or
`vm-mime-attachment-save-directory' can be used to set the
default location.  When directory does not exist it will be
created."
  (interactive
   (list current-prefix-arg
         (vm-read-file-name
          "Attachment directory: "
          (or vm-mime-all-attachments-directory
              vm-mime-attachment-save-directory
              default-directory)
          (or vm-mime-all-attachments-directory
              vm-mime-attachment-save-directory
              default-directory)
          nil nil
          vm-mime-save-all-attachments-history)))

  (vm-check-for-killed-summary)
  (if (interactive-p) (vm-follow-summary-cursor))
 
  (let ((n 0))
    (vm-mime-action-on-all-attachments
     count
     ;; the action to be performed BEGIN
     (lambda (msg layout type file)
       (let ((directory (if (functionp directory)
                            (funcall directory msg)
                          directory)))
         (setq file 
	       (if file
		   (expand-file-name (file-name-nondirectory file) directory)
		 (vm-read-file-name
		  (format "Save %s to file: " type)
		  (or directory
		      vm-mime-all-attachments-directory
		      vm-mime-attachment-save-directory)
		  (or directory
		      vm-mime-all-attachments-directory
		      vm-mime-attachment-save-directory)
		  nil nil
		  vm-mime-save-all-attachments-history)
		 ))
         
         (if (and file (file-exists-p file))
             (if (y-or-n-p (format "Overwrite `%s'? " file))
                 (delete-file file)
               (setq file nil)))
         
         (when file
           (vm-inform 5 "Saving %s" (if file (format " (%s)" file) ""))
           (make-directory (file-name-directory file) t)
           (vm-mime-send-body-to-file layout file file)
           (if vm-mime-delete-after-saving
               (let ((vm-mime-confirm-delete nil))
                 (vm-mime-discard-layout-contents 
		  layout (expand-file-name file))))
           (setq n (+ 1 n)))))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; the action to be performed END
     ;; attachment filters 
     vm-mime-saveable-types
     vm-mime-saveable-type-exceptions)

    (when (interactive-p)
      (vm-discard-cached-data count)
      (let ((vm-preview-lines nil))
	(vm-present-current-message)))
    
    (if (> n 0)
        (vm-inform 5 "%d attachment%s saved" n (if (= n 1) "" "s"))
      (vm-inform 5 "No attachments saved"))))

;; (define-obsolete-function-alias 'vm-mime-save-all-attachments
;;   'vm-save-all-attachments "8.2.0")
(defalias 'vm-mime-save-all-attachments
  'vm-save-all-attachments)
(make-obsolete 'vm-mime-save-all-attachments
  'vm-save-all-attachments "8.2.0")

(defun vm-save-attachments (&optional count
				      no-delete-after-saving)
  "Save all attachments in the next COUNT messages or marked
messages.  For the purpose of this function, an \"attachment\" is
a mime part part which has \"attachment\" as its disposition or
simply has an associated filename.  Any mime types that match
`vm-mime-saveable-types' but not `vm-mime-saveable-type-exceptions'
are also included.

The attachments are saved in file names input from the
minibuffer.  (This is the main difference from
`vm-save-all-attachments'.) 

The variables `vm-all-attachments-directory' or
`vm-mime-attachment-save-directory' can be used to set the
default location.  When directory does not exist it will be
confirmed before creating a new directory."
  (interactive "p")

  (vm-check-for-killed-summary)
  (if (interactive-p) (vm-follow-summary-cursor))
 
  (let ((n 0)
	(directory nil))
    (vm-mime-action-on-all-attachments
     count
     ;; the action to be performed BEGIN
     (lambda (msg layout type file-name)
       (let ((file (vm-read-file-name
		    (if file-name			; prompt
			(format "Save (default %s): " file-name)
		      (format "Save %s: " type))
		    (file-name-as-directory		; directory
		     (or directory		      
			 vm-mime-attachment-save-directory
			 vm-mime-all-attachments-directory))
		    (and file-name			; default-filename
			 (concat
			  (file-name-as-directory 	      
			   (or directory		      
			       vm-mime-attachment-save-directory
			       vm-mime-all-attachments-directory))
			  (or file-name "")))
		    nil nil			      ; mustmatch initial
		    vm-mime-save-all-attachments-history ; predicate
		    )))
	 (setq directory (file-name-directory file))
         (when (file-exists-p file)
	   (if (y-or-n-p (format "Overwrite `%s'? " file))
	       nil 		; (delete-file file)
	     (setq file nil)))
	 (unless (file-exists-p directory)
	   (if (y-or-n-p 
		(format "Directory %s does not exist; create it?" directory))
	       (make-directory directory t)
	     (setq file nil)))
         (when file
           (vm-inform 5 "Saving %s" (if file (format " (%s)" file) ""))
           (vm-mime-send-body-to-file layout file file)
           (if vm-mime-delete-after-saving
               (let ((vm-mime-confirm-delete nil))
                 (vm-mime-discard-layout-contents 
		  layout (expand-file-name file))))
           (setq n (+ 1 n)))))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; the action to be performed END
     ;; attachment filters 
     vm-mime-saveable-types
     vm-mime-saveable-type-exceptions)

    (when (interactive-p)
      (vm-discard-cached-data count)
      (vm-present-current-message))
    
    (if (> n 0)
        (vm-inform 5 "%d attachment%s saved" n (if (= n 1) "" "s"))
      (vm-inform 5 "No attachments saved"))))
;; for the karking compiler
(defvar vm-menu-mime-dispose-menu)

(defun vm-mime-set-image-stamp-for-type (e type)
  "Set an image stamp for MIME button extent E as appropriate for
TYPE.                                                 USR, 2011-03-25"
  (cond
   (vm-xemacs-p
    (vm-mime-xemacs-set-image-stamp-for-type e type))
   (vm-fsfemacs-p
    (vm-mime-fsfemacs-set-image-stamp-for-type e type))))

(defconst vm-mime-type-images
  '(("text" "text.xpm")
    ("image" "image.xpm")
    ("audio" "audio.xpm")
    ("video" "video.xpm")
    ("message" "message.xpm")
    ("application" "application.xpm")
    ("multipart" "multipart.xpm")))

(defun vm-mime-xemacs-set-image-stamp-for-type (e type)
  "Set an image stamp for MIME button extent E as appropriate for
TYPE.                                                  USR, 2011-03-25"
  (if (and (vm-images-possible-here-p)
	   (vm-image-type-available-p 'xpm)
	   (> (device-bitplanes) 7))
      (let ((dir (vm-image-directory))
	    (tuples vm-mime-type-images)
	    glyph file sym p)
	(setq file (catch 'done
		     (while tuples
		       (if (vm-mime-types-match (car (car tuples)) type)
			   (throw 'done (car tuples))
			 (setq tuples (cdr tuples))))
		     nil)
	      file (and file (nth 1 file))
	      sym (and file (intern file vm-image-obarray))
	      glyph (and sym (boundp sym) (symbol-value sym))
	      glyph (or glyph
			(and file
			     (make-glyph
			      (list
			       (vector 'xpm ':file
				       (expand-file-name file dir))
			       [nothing])))))
	(and sym (not (boundp sym)) (set sym glyph))
	(and glyph (set-extent-begin-glyph e glyph)))))

(defun vm-mime-fsfemacs-set-image-stamp-for-type (e type)
  "Set an image stamp for MIME button extent E as appropriate for
TYPE.

This is done by extending the extent with one character position at
the front and placing the image there as the display text property.
                                                         USR, 2011-03-25"
  (if (and (vm-images-possible-here-p)
	   (vm-image-type-available-p 'xpm))
      (let ((dir (vm-image-directory))
        (tuples vm-mime-type-images)
             file)
	(setq file (catch 'done
		     (while tuples
		       (if (vm-mime-types-match (car (car tuples)) type)
			   (throw 'done (car tuples))
			 (setq tuples (cdr tuples))))
		     nil)
	      file (and file (nth 1 file))
	      file (and file (expand-file-name file dir)))
	(if file
	    (save-excursion
	      (let ((buffer-read-only nil))
		(set-buffer (overlay-buffer e))
		(goto-char (overlay-start e))
		(insert "x")
		(move-overlay e (1- (point)) (overlay-end e))
		(put-text-property (1- (point)) (point) 'display
				   (list 'image
					 ':ascent 80
					 ':color-symbols
					   (list
					    (cons "background"
						  (cdr (assq
							'background-color
							(frame-parameters)))))
					 ':type 'xpm
					 ':file file))))))))

(defun vm-mime-insert-button (caption action layout disposable)
  "Display a button for a mime object, using CAPTION as the label (a
string) and ACTION as the default action (a function).  The mime object
is described by LAYOUT.  If DISPOSABLE is true, then the button will
be removed when it is expanded to display the mime object."
  (let ((start (point))	e
	(keymap vm-mime-reader-map)
	(buffer-read-only nil))
    (if (fboundp 'set-keymap-parents)
	(if (current-local-map)
	    (set-keymap-parents keymap (list (current-local-map))))
      (setq keymap (append keymap (current-local-map))))
    (if (not (bolp))
	(insert "\n"))
    (insert caption "\n")
    ;; we must use the same interface that the vm-extent functions
    ;; use.  if they use overlays, then we call make-overlay.
    (if vm-fsfemacs-p
	;; we MUST have the five arg make-overlay.  overlays must
	;; advance when text is inserted at their start position or
	;; inline text and graphics will seep into the button
	;; overlay and then be removed when the button is removed.
	(setq e (vm-make-extent start (point) nil t nil))
      (setq e (vm-make-extent start (point)))
      (vm-set-extent-property e 'start-open t)
      (vm-set-extent-property e 'end-open t))
    (vm-mime-set-image-stamp-for-type e (car (vm-mm-layout-type layout)))
    (when vm-fsfemacs-p
      (vm-set-extent-property e 'local-map keymap))
    (when vm-xemacs-p
      (vm-set-extent-property e 'highlight t)
      (vm-set-extent-property e 'keymap keymap)
      (vm-set-extent-property e 'balloon-help 'vm-mouse-3-help))
    ;; for all
    (vm-set-extent-property e 'vm-button t)
    (vm-set-extent-property e 'vm-mime-disposable disposable)
    (vm-set-extent-property e 'face vm-mime-button-face)
    (vm-set-extent-property e 'mouse-face vm-mime-button-mouse-face)
    (vm-set-extent-property e 'vm-mime-layout layout)
    (vm-set-extent-property e 'vm-mime-function action)
    ;; for vm-continue-postponed-message
    (when vm-xemacs-p
      (vm-set-extent-property e 'duplicable t))
    (when vm-fsfemacs-p
      (put-text-property (overlay-start e)
			 (overlay-end e)
			 'vm-mime-layout layout))
    ;; return t as decoding worked
    t))

(defun vm-mime-rewrite-failed-button (button error-string)
  (let* ((buffer-read-only nil)
	 (start (point)))
    (goto-char (vm-extent-start-position button))
    (insert (format "DISPLAY FAILED -- %s\n" error-string))
    (vm-set-extent-endpoints button start (vm-extent-end-position button))
    (delete-region (point) (vm-extent-end-position button))))


;;---------------------------------------------------------------------------
;;; MIME button operations
;;
;; vm-mime-send-body-to-file: (extent-or-layout 
;;		               &optional filename filename bool) -> filename
;; vm-mime-send-body-to-folder: (extent-or-layout 
;;		                 &optional filename) -> filename
;; vm-mime-delete-body-after-saving: (extent) -> void
;; vm-mime-pipe-body-to-queried-command: (extent &optional bool) -> bool
;; vm-mime-pipe-body-to-queried-command-discard-output: (extent) -> bool
;; vm-mime-send-body-to-printer: (extent) -> bool
;; vm-mime-display-body-as-text: (extent) -> ?
;; vm-mime-display-object-as-type: (extent) -> ?
;; vm-mime-display-body-using-external-viewer: (extent) -> ?
;; vm-mime-convert-body-then-display: (extent) -> ?
;; vm-mime-attach-body-to-composition: (extent) -> ?
;;---------------------------------------------------------------------------
 
;; From: Eric E. Dors
;; Date: 1999/04/01
;; Newsgroups: gnu.emacs.vm.info
;; example filter-alist variable
(defvar vm-mime-write-file-filter-alist
  '(("application/mac-binhex40" . "hexbin -s "))
  "*A list of filter used when writing attachements to files."
  )
 
;; function to parse vm-mime-write-file-filter-alist
(defun vm-mime-find-write-filter (type)
  (let ((e-alist vm-mime-write-file-filter-alist)
	(matched nil))
    (while (and e-alist (not matched))
      (if (and (vm-mime-types-match (car (car e-alist)) type)
	       (cdr (car e-alist)))
	  (setq matched (cdr (car e-alist)))
	(setq e-alist (cdr e-alist))))
    matched))

(defun vm-mime-delete-body-after-saving (layout file)
  (unless (vectorp layout)
    (setq layout (vm-extent-property layout 'vm-mime-layout)))
  (unless (vm-mime-types-match "message/external-body"
			       (car (vm-mm-layout-type layout)))
    (let ((vm-mime-confirm-delete nil))
      ;; we don't care if the delete fails
      (condition-case nil
	  (vm-delete-mime-object (expand-file-name file))
	(error nil)))))

(defun vm-mime-send-body-to-file (layout &optional default-filename file
                                         overwrite)
  (unless (vectorp layout)
    (setq layout (vm-extent-property layout 'vm-mime-layout)))
  (when (vm-mime-types-match "message/external-body"
			     (car (vm-mm-layout-type layout)))
    (vm-mime-fetch-message/external-body layout)
    (setq layout (car (vm-mm-layout-parts layout))))
  (unless default-filename
    (setq default-filename (vm-mime-get-disposition-filename layout)))
  (when default-filename
    (setq default-filename (file-name-nondirectory default-filename)))
  (let (;; evade the XEmacs dialog box, yeccch.
	(use-dialog-box nil)
	(dir vm-mime-attachment-save-directory)
	(done nil))
    (unless file
      (while (not done)
	(setq file
	      (read-file-name
	       (if default-filename
		   (format "Write MIME body to file (default %s): "
			   default-filename)
		 "Write MIME body to file: ")
	       dir default-filename)
	      file (expand-file-name file dir))
	(if (not (file-directory-p file))
	    (setq done t)
	  (unless default-filename
	    (error "%s is a directory" file))
	  (setq file (expand-file-name default-filename file)
		done t))))
    (let ((work-buffer (vm-make-work-buffer))
	  (coding-system-for-read (vm-binary-coding-system)))
      (unwind-protect
	  (with-current-buffer work-buffer
	    (setq selective-display nil)
	    ;; Tell DOS/Windows NT whether the file is binary
	    (setq buffer-file-type (not (vm-mime-text-type-layout-p layout)))
	    ;; Tell XEmacs/MULE not to mess with the bits unless
	    ;; this is a text type.
	    (if (fboundp 'set-buffer-file-coding-system)
		(if (vm-mime-text-type-layout-p layout)
		    (set-buffer-file-coding-system
		     (vm-line-ending-coding-system) nil)
		  (set-buffer-file-coding-system (vm-binary-coding-system) t)))
	    (vm-mime-insert-mime-body layout)
	    (vm-mime-transfer-decode-region layout (point-min) (point-max))
            (unless (or overwrite (not (file-exists-p file)))
              (or (y-or-n-p "File exists, overwrite? ")
                  (error "Aborted")))
	    ;; Bind the jka-compr-compression-info-list to nil so
	    ;; that jka-compr won't compress already compressed
	    ;; data.  This is a crock, but as usual I'm getting
	    ;; the bug reports for somebody else's bad code.
	    (let ((jka-compr-compression-info-list nil)
		  (command (vm-mime-find-write-filter
			    (car (vm-mm-layout-type layout)))))
	      (if command (shell-command-on-region (point-min) (point-max)
						   (concat command " > " file))
		(write-region (point-min) (point-max) file nil nil)))
	    
	    file )
	(when work-buffer (kill-buffer work-buffer))))))

(defun vm-mime-send-body-to-folder (layout &optional default-filename)
  (unless (vectorp layout)
    (setq layout (vm-extent-property layout 'vm-mime-layout)))
  (when (vm-mime-types-match "message/external-body"
			     (car (vm-mm-layout-type layout)))
    (vm-mime-fetch-message/external-body layout)
    (setq layout (car (vm-mm-layout-parts layout))))
  (let ((type (car (vm-mm-layout-type layout)))
	file)
    (if (not (or (vm-mime-types-match type "message/rfc822")
		 (vm-mime-types-match type "message/news")))
	(vm-mime-send-body-to-file layout default-filename)
      (let ((work-buffer (vm-make-work-buffer))
	    (coding-system-for-read (vm-binary-coding-system))
	    (coding-system-for-write (vm-binary-coding-system)))
	(unwind-protect
	    (with-current-buffer work-buffer
	      (setq selective-display nil)
	      ;; Tell DOS/Windows NT whether the file is binary
	      (setq buffer-file-type t)
	      ;; Tell XEmacs/MULE not to mess with the bits unless
	      ;; this is a text type.
	      (if (fboundp 'set-buffer-file-coding-system)
		  (set-buffer-file-coding-system
		   (vm-line-ending-coding-system) nil))
	      (vm-mime-insert-mime-body layout)
	      (vm-mime-transfer-decode-region layout (point-min) (point-max))
	      (goto-char (point-min))
	      (insert (vm-leading-message-separator 'mmdf))
	      (goto-char (point-max))
	      (insert (vm-trailing-message-separator 'mmdf))
	      (set-buffer-modified-p nil)
	      (vm-mode t)
	      (let ((vm-check-folder-types t)
		    (vm-convert-folder-types t))
		(setq file (call-interactively 'vm-save-message)))
	      (vm-quit-no-change)
	      file )
	  (when work-buffer (kill-buffer work-buffer)))))))

(defun vm-mime-pipe-body-to-command (command layout &optional discard-output)
  (unless (vectorp layout)
    (setq layout (vm-extent-property layout 'vm-mime-layout)))
  (when (vm-mime-types-match "message/external-body"
			     (car (vm-mm-layout-type layout)))
    (vm-mime-fetch-message/external-body layout)
    (setq layout (car (vm-mm-layout-parts layout))))
  (let ((output-buffer (if discard-output
			   0
			 (get-buffer-create "*Shell Command Output*"))))
    (when (bufferp output-buffer)
      (with-current-buffer output-buffer
	(erase-buffer)))
    (let ((work-buffer (vm-make-work-buffer)))
      (unwind-protect
	  (with-current-buffer work-buffer
	    ;; call-process-region calls write-region.
	    ;; don't let it do CR -> LF translation.
	    (setq selective-display nil)
	    (vm-mime-insert-mime-body layout)
	    (vm-mime-transfer-decode-region layout (point-min) (point-max))
	    (let ((pop-up-windows (and pop-up-windows
				       (eq vm-mutable-windows t)))
		  (process-coding-system-alist
		   (if (vm-mime-text-type-layout-p layout)
		       nil
		     (list (cons "." (vm-binary-coding-system)))))
		  ;; Tell DOS/Windows NT whether the input is binary
		  (binary-process-input
		   (not
		    (vm-mime-text-type-layout-p layout))))
	      (call-process-region (point-min) (point-max)
				   (or shell-file-name "sh")
				   nil output-buffer nil
				   shell-command-switch command)))
	(when work-buffer (kill-buffer work-buffer))))
    (when (bufferp output-buffer)
      (if (not (zerop (with-current-buffer output-buffer (buffer-size))))
	  (vm-display output-buffer t (list this-command)
		      '(vm-pipe-message-to-command))
	(vm-display nil nil (list this-command)
		    '(vm-pipe-message-to-command))))
    t ))

(defun vm-mime-pipe-body-to-queried-command (button &optional discard-output)
  (let ((command (read-string "Pipe object to command: ")))
    (vm-mime-pipe-body-to-command command button discard-output)))

(defun vm-mime-pipe-body-to-queried-command-discard-output (button)
  (vm-mime-pipe-body-to-queried-command button t))

(defun vm-mime-send-body-to-printer (button)
  (vm-mime-pipe-body-to-command (mapconcat (function identity)
					   (nconc (list vm-print-command)
						  vm-print-command-switches)
					   " ")
				button))

(defun vm-mime-display-body-as-text (button)
  (let ((vm-mime-auto-displayed-content-types '("text/plain"))
	(vm-mime-auto-displayed-content-type-exceptions nil)
	(layout (copy-sequence (vm-extent-property button 'vm-mime-layout))))
    (vm-set-extent-property button 'vm-mime-disposable t)
    (vm-set-extent-property button 'vm-mime-layout layout)
    ;; not universally correct, but close enough.
    (vm-set-mm-layout-type layout '("text/plain" "charset=us-ascii"))
    (goto-char (vm-extent-start-position button))
    (vm-decode-mime-layout button t)))

(defun vm-mime-display-object-as-type (button)
  (let ((vm-mime-auto-displayed-content-types t)
	(vm-mime-auto-displayed-content-type-exceptions nil)
	(old-layout (vm-extent-property button 'vm-mime-layout))
	layout
	(type (read-string "View as MIME type: ")))
    (setq layout (copy-sequence old-layout))
    (vm-set-extent-property button 'vm-mime-layout layout)
    ;; not universally correct, but close enough.
    (setcar (vm-mm-layout-type layout) type)
    (goto-char (vm-extent-start-position button))
    (vm-decode-mime-layout button t)))

(defun vm-mime-display-body-using-external-viewer (button)
  (let ((layout (vm-extent-property button 'vm-mime-layout))
	(vm-mime-external-content-type-exceptions nil))
    (when (vm-mime-types-match "message/external-body"
			       (car (vm-mm-layout-type layout)))
      (vm-mime-fetch-message/external-body layout)
      (setq layout (car (vm-mm-layout-parts layout))))
    (if (vm-mime-find-external-viewer (car (vm-mm-layout-type layout)))
	(vm-mime-display-external-generic layout)
      (error "No viewer defined for type %s"
	     (car (vm-mm-layout-type layout))))))

(defun vm-mime-convert-body-then-display (button)
  (let ((layout (vm-extent-property button 'vm-mime-layout)))
    (when (vm-mime-types-match "message/external-body"
			       (car (vm-mm-layout-type layout)))
      (vm-mime-fetch-message/external-body layout)
      (setq layout (car (vm-mm-layout-parts layout))))
    (setq layout (vm-mime-convert-undisplayable-layout layout))
    (if (null layout)
	nil
      (vm-set-extent-property button 'vm-mime-disposable t)
      (vm-set-extent-property button 'vm-mime-layout layout)
      (goto-char (vm-extent-start-position button))
      (vm-decode-mime-layout button t))))


(defun vm-mime-attach-body-to-composition (button)
  (let ((layout (vm-extent-property button 'vm-mime-layout))
	(vm-mime-external-content-type-exceptions nil))
    (goto-char (vm-extent-start-position button))
    (when (vm-mime-types-match "message/external-body"
			       (car (vm-mm-layout-type layout)))
      (vm-mime-fetch-message/external-body layout)
      (setq layout (car (vm-mm-layout-parts layout))))
    (vm-attach-object-to-composition layout)))

(defun vm-mime-get-button-layout ()
  "Return the MIME layout of the MIME button at point.   USR, 2011-03-07"
  (vm-mime-run-display-function-at-point
   (function
    (lambda (extent)
      (vm-extent-property extent 'vm-mime-layout)))))

(defun vm-mime-scrub-description (string)
  (let ((work-buffer nil))
      (save-excursion
       (unwind-protect
	   (progn
	     (setq work-buffer (vm-make-work-buffer))
	     (set-buffer work-buffer)
	     (insert string)
	     (while (re-search-forward "[ \t\n]+" nil t)
	       (replace-match " "))
	     (buffer-string))
	 (and work-buffer (kill-buffer work-buffer))))))

;; unused
;;(defun vm-mime-layout-description (layout)
;;  (let ((type (car (vm-mm-layout-type layout)))
;;	description name)
;;    (setq description
;;	  (if (vm-mm-layout-description layout)
;;	      (vm-mime-scrub-description (vm-mm-layout-description layout))))
;;    (concat
;;     (if description description "")
;;     (if description ", " "")
;;     (cond ((vm-mime-types-match "multipart/digest" type)
;;	    (let ((n (length (vm-mm-layout-parts layout))))
;;	      (format "digest (%d message%s)" n (if (= n 1) "" "s"))))
;;	   ((vm-mime-types-match "multipart/alternative" type)
;;	    "multipart alternative")
;;	   ((vm-mime-types-match "multipart" type)
;;	    (let ((n (length (vm-mm-layout-parts layout))))
;;	      (format "multipart message (%d part%s)" n (if (= n 1) "" "s"))))
;;	   ((vm-mime-types-match "text/plain" type)
;;	    (format "plain text%s"
;;		    (let ((charset (vm-mime-get-parameter layout "charset")))
;;		      (if charset
;;			  (concat ", " charset)
;;			""))))
;;	   ((vm-mime-types-match "text/enriched" type)
;;	    "enriched text")
;;	   ((vm-mime-types-match "text/html" type)
;;	    "HTML")
;;	   ((vm-mime-types-match "image/gif" type)
;;	    "GIF image")
;;	   ((vm-mime-types-match "image/jpeg" type)
;;	    "JPEG image")
;;	   ((and (vm-mime-types-match "application/octet-stream" type)
;;		 (setq name (vm-mime-get-parameter layout "name"))
;;		 (save-match-data (not (string-match "^[ \t]*$" name))))
;;	    name)
;;	   (t type)))))

(defun vm-mime-layout-contains-type (layout type)
  (if (vm-mime-types-match type (car (vm-mm-layout-type layout)))
      layout
    (let ((p (vm-mm-layout-parts layout))
	  (result nil)
	  (done nil))
      (while (and p (not done))
	(if (setq result (vm-mime-layout-contains-type (car p) type))
	    (setq done t)
	  (setq p (cdr p))))
      result )))

;; breadth first traversal
(defun vm-mime-find-digests-in-layout (layout)
  (let ((layout-list (list layout))
	layout-type
	(result nil))
    (while layout-list
      (setq layout-type (car (vm-mm-layout-type (car layout-list))))
      (cond ((string-match "^multipart/digest\\|message/\\(rfc822\\|news\\)"
			   layout-type)
	     (setq result (nconc result (list (car layout-list)))))
	    ((vm-mime-composite-type-p layout-type)
	     (setq layout-list (nconc layout-list
				      (copy-sequence
				       (vm-mm-layout-parts
					(car layout-list)))))))
      (setq layout-list (cdr layout-list)))
    result ))
  
(defun vm-mime-plain-message-p (m)
  "A message M is considered plain if
   - it does not have encoded headers, and
   - - it does not have a MIME layout, or
   - - it has a text/plain component as its first element with ASCII
   - -   character set and unibyte encoding (7bit, 8bit or binary).
Returns non-NIL value M is a plain message."
  (save-match-data
    (let ((o (vm-mm-layout m))
	  (case-fold-search t))
      (and (eq (vm-mm-encoded-header m) 'none)
	   (or (not (vectorp o))
	       (and (vm-mime-types-match "text/plain"
					 (car (vm-mm-layout-type o)))
		    (string-match "^us-ascii$"
				  (or (vm-mime-get-parameter o "charset")
				      "us-ascii"))
		    (string-match "^\\(7bit\\|8bit\\|binary\\)$"
				  (vm-mm-layout-encoding o))))))))

(defun vm-mime-text-type-p (type)
  (let ((case-fold-search t))
    (or (string-match "^text/" type) (string-match "^message/" type))))

(defun vm-mime-text-type-layout-p (layout)
  (or (vm-mime-types-match "text" (car (vm-mm-layout-type layout)))
      (vm-mime-types-match "message" (car (vm-mm-layout-type layout)))))


(defun vm-mime-tty-can-display-mime-charset (name)
  "Can the current TTY correctly display the given MIME character set?"
  (and (fboundp 'console-tty-output-coding-system)
       ;; Is this check too paranoid?
       (vm-coding-system-p (console-tty-output-coding-system))
       (fboundp 'coding-system-get)
       (let
	   ;; Nnngh, latin-unity-base-name isn't doing the right thing for
	   ;; me with MULE-UCS and UTF-8 as the terminal coding system. Of
	   ;; course, it's not evident that it _can_ do the right thing.
	   ;;
	   ;; The intention is that ourtermcs is the version of the
	   ;; coding-system without line-ending information attached to its
	   ;; end.
	   ((ourtermcs (vm-coding-system-name
                        (or (car 
                             (coding-system-get
                              (console-tty-output-coding-system)
                              'alias-coding-systems))
                            (coding-system-base
                             (console-tty-output-coding-system))))))
	 (or (eq ourtermcs (vm-mime-charset-to-coding name))
	     ;; The vm-mime-mule-charset-to-coding-alist check is to make
	     ;; sure it does the right thing with a nonsense MIME character
	     ;; set name.
	     (and (memq ourtermcs (vm-get-mime-ucs-list))
		  (vm-mime-charset-to-coding name) 
		  t)
	     (vm-mime-default-face-charset-p name)))))

(defun vm-mime-charset-internally-displayable-p (name)
  "Can the given MIME charset be displayed within emacs by VM?"
  (cond ((and vm-xemacs-mule-p (memq (vm-device-type) '(x gtk mswindows)))
	 (or (vm-mime-charset-to-coding name)
	     (vm-mime-default-face-charset-p name)))

	;; vm-mime-tty-can-display-mime-charset (called below) fails
	;; for GNU Emacs. So keep things simple, since there's no harm
	;; if replacement characters are displayed.
	(vm-fsfemacs-mule-p)
	((vm-multiple-fonts-possible-p)
	 (or (vm-mime-default-face-charset-p name)
	     (vm-string-assoc name vm-mime-charset-font-alist)))

	;; If the terminal-coding-system variable is set to something that
	;; can encode all the characters of the given MIME character set,
	;; then we can display any message in the given MIME character set
	;; internally.

	((vm-mime-tty-can-display-mime-charset name))
	(t
	 (vm-mime-default-face-charset-p name))))

(defun vm-mime-default-face-charset-p (charset)
  (and (or (eq vm-mime-default-face-charsets t)
	   (and (consp vm-mime-default-face-charsets)
		(vm-string-member charset vm-mime-default-face-charsets)))
       (not (vm-string-member charset
			      vm-mime-default-face-charset-exceptions))))


(defun vm-mime-find-message/partials (layout id)
  (let ((list nil)
	(type (vm-mm-layout-type layout)))
    (cond ((vm-mime-composite-type-p (car (vm-mm-layout-type layout)))
	   (let ((parts (vm-mm-layout-parts layout)) o)
	     (while parts
	       (setq o (vm-mime-find-message/partials (car parts) id))
	       (if o
		   (setq list (nconc o list)))
	       (setq parts (cdr parts)))))
	  ((vm-mime-types-match "message/partial" (car type))
	   (if (equal (vm-mime-get-parameter layout "id") id)
	       (setq list (cons layout list)))))
    list ))

(defun vm-mime-find-leaf-content-id-in-layout-folder (layout id)
  (save-excursion
    (save-restriction
      (let (m (o nil))
	(set-buffer (vm-buffer-of
		     (vm-real-message-of
		      (vm-mm-layout-message layout))))
	(widen)
	(goto-char (point-min))
	(while (and (search-forward id nil t)
		    (setq m (vm-message-at-point)))
	  (setq o (vm-mm-layout m))
	  (if (not (vectorp o))
	      nil
	    (setq o (vm-mime-find-leaf-content-id o id))
	    (if (null o)
		nil
	      ;; if we found it, end the search loop
	      (goto-char (point-max)))))
	o ))))

(defun vm-mime-find-leaf-content-id (layout id)
  (let ((list nil)
	(type (vm-mm-layout-type layout)))
    (catch 'done
      (cond ((vm-mime-composite-type-p (car (vm-mm-layout-type layout)))
	     (let ((parts (vm-mm-layout-parts layout)) o)
	       (while parts
		 (setq o (vm-mime-find-leaf-content-id (car parts) id))
		 (if o
		     (throw 'done o))
		 (setq parts (cdr parts)))))
	    (t
	     (if (equal (vm-mm-layout-id layout) id)
		 (throw 'done layout)))))))

(defun vm-message-at-point ()
  (let ((mp vm-message-list)
	(point (point))
	(done nil))
    (while (and mp (not done))
      (if (and (>= point (vm-start-of (car mp)))
	       (<= point (vm-end-of (car mp))))
	  (setq done t)
	(setq mp (cdr mp))))
    (car mp)))

(defun vm-mime-make-multipart-boundary ()
  (let ((boundary (make-string 10 ?a))
	(i 0))
    (random t)
    (while (< i (length boundary))
      (aset boundary i (aref vm-mime-base64-alphabet
			     (% (vm-abs (lsh (random) -8))
				(length vm-mime-base64-alphabet))))
      (vm-increment i))
    boundary ))

(defun vm-mime-extract-filename-suffix (layout)
  (let ((filename (vm-mime-get-disposition-filename layout))
	(suffix nil) i)
    (if (and filename (string-match "\\.[^.]+$" filename))
	(setq suffix (substring filename (match-beginning 0) (match-end 0))))
    suffix ))

(defun vm-mime-find-filename-suffix-for-type (layout)
  (let ((type (car (vm-mm-layout-type layout)))
	suffix
	(alist vm-mime-attachment-auto-suffix-alist))
    (while alist
      (if (vm-mime-types-match (car (car alist)) type)
	  (setq suffix (cdr (car alist))
		alist nil)
	(setq alist (cdr alist))))
    suffix ))

;;;###autoload


(defun vm-attach-file (file type &optional charset description
			    no-suggested-filename)
  "Attach a file to a VM composition buffer to be sent along with the message.
The file is not inserted into the buffer and MIME encoded until
you execute `vm-mail-send' or `vm-mail-send-and-exit'.  A visible tag
indicating the existence of the attachment is placed in the
composition buffer.  You can move the attachment around or remove
it entirely with normal text editing commands.  If you remove the
attachment tag, the attachment will not be sent.

First argument, FILE, is the name of the file to attach.  Second
argument, TYPE, is the MIME Content-Type of the file.  Optional
third argument CHARSET is the character set of the attached
document.  This argument is only used for text types, and it is
ignored for other types.  Optional fourth argument DESCRIPTION
should be a one line description of the file.  Nil means include
no description.  Optional fifth argument NO-SUGGESTED-FILENAME non-nil
means that VM should not add a filename to the Content-Disposition
header created for the object.

When called interactively all arguments are read from the
minibuffer.

This command is for attaching files that do not have a MIME
header section at the top.  For files with MIME headers, you
should use `vm-attach-mime-file' to attach such a file.  VM
will extract the content type information from the headers in
this case and not prompt you for it in the minibuffer."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command)
         (completion-ignored-extensions nil)
	 (charset nil)
	 description file default-type type)
     (unless vm-send-using-mime
	 (error (concat "MIME attachments disabled, "
			"set vm-send-using-mime non-nil to enable.")))
     (setq file (vm-read-file-name "Attach file: "
                                   vm-mime-attachment-source-directory
                                   nil t)
	   default-type (or (vm-mime-default-type-from-filename file)
			    "application/octet-stream")
	   type (completing-read
		 (format "Content type (default %s): "
			 default-type)
		 vm-mime-type-completion-alist)
	   type (if (> (length type) 0) type default-type))
     (when (vm-mime-types-match "text" type)
       (setq charset (completing-read "Character set (default US-ASCII): "
				      vm-mime-charset-completion-alist)
	     charset (if (> (length charset) 0) charset)))
     (setq description (read-string "One line description: "))
     (when (string-match "^[ \t]*$" description)
       (setq description nil))
     (list file type charset description nil)))
  (unless vm-send-using-mime
    (error (concat "MIME attachments disabled, "
		   "set vm-send-using-mime non-nil to enable.")))
  (when (file-directory-p file)
    (error "%s is a directory, cannot attach" file))
  (unless (file-exists-p file)
    (error "No such file: %s" file))
  (unless (file-readable-p file)
    (error "You don't have permission to read %s" file))
  (when charset 
    (setq charset (list (concat "charset=" charset))))
  (when description 
    (setq description (vm-mime-scrub-description description)))
  (vm-attach-object file :type type :params charset 
			 :description description :mimed nil))
(defalias 'vm-mime-attach-file 'vm-attach-file)

;;;###autoload
(defun vm-attach-mime-file (file type)
  "Attach a MIME encoded file to a VM composition buffer to be sent
along with the message.

The file is not inserted into the buffer until you execute
`vm-mail-send' or `vm-mail-send-and-exit'.  A visible tag indicating
the existence of the attachment is placed in the composition
buffer.  You can move the attachment around or remove it entirely
with normal text editing commands.  If you remove the attachment
tag, the attachment will not be sent.

The first argument, FILE, is the name of the file to attach.
When called interactively the FILE argument is read from the
minibuffer.

The second argument, TYPE, is the MIME Content-Type of the object.

This command is for attaching files that have a MIME
header section at the top.  For files without MIME headers, you
should use `vm-attach-file' to attach the file."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command)
	 file type default-type)
     (unless vm-send-using-mime
       (error (concat "MIME attachments disabled, "
		      "set vm-send-using-mime non-nil to enable.")))
     (setq file (vm-read-file-name "Attach file: "
                                   vm-mime-attachment-source-directory
                                   nil t)
	   default-type (or (vm-mime-default-type-from-filename file)
			    "application/octet-stream")
	   type (completing-read
		 (format "Content type (default %s): "
			 default-type)
		 vm-mime-type-completion-alist)
	   type (if (> (length type) 0) type default-type))
     (list file type)))
  (unless vm-send-using-mime
    (error (concat "MIME attachments disabled, "
		   "set vm-send-using-mime non-nil to enable.")))
  (when (file-directory-p file)
    (error "%s is a directory, cannot attach" file))
  (unless (file-exists-p file)
    (error "No such file: %s" file))
  (unless (file-readable-p file)
    (error "You don't have permission to read %s" file))
  (vm-attach-object file :type type :params nil 
			 :description nil :mimed t))
(defalias 'vm-mime-attach-mime-file 'vm-attach-mime-file)

;;;###autoload
(defun vm-attach-buffer (buffer type &optional charset description)
  "Attach a buffer to a VM composition buffer to be sent along with
the message.

The buffer contents are not inserted into the composition
buffer and MIME encoded until you execute `vm-mail-send' or
`vm-mail-send-and-exit'.  A visible tag indicating the existence
of the attachment is placed in the composition buffer.  You
can move the attachment around or remove it entirely with
normal text editing commands.  If you remove the attachment
tag, the attachment will not be sent.

First argument, BUFFER, is the buffer or name of the buffer to
attach.  Second argument, TYPE, is the MIME Content-Type of the
file.  Optional third argument CHARSET is the character set of
the attached document.  This argument is only used for text
types, and it is ignored for other types.  Optional fourth
argument DESCRIPTION should be a one line description of the
file.  Nil means include no description.

When called interactively all arguments are read from the
minibuffer.

This command is for attaching files that do not have a MIME
header section at the top.  For files with MIME headers, you
should use `vm-attach-mime-file' to attach such a file.  VM
will extract the content type information from the headers in
this case and not prompt you for it in the minibuffer."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command)
	 (charset nil)
	 description file default-type type buffer buffer-name)
     (unless vm-send-using-mime
       (error (concat "MIME attachments disabled, "
		      "set vm-send-using-mime non-nil to enable.")))
     (setq buffer-name (read-buffer "Attach buffer: " nil t)
	   default-type (or (vm-mime-default-type-from-filename buffer-name)
			    "application/octet-stream")
	   type (completing-read
		 (format "Content type (default %s): "
			 default-type)
		 vm-mime-type-completion-alist)
	   type (if (> (length type) 0) type default-type))
     (when (vm-mime-types-match "text" type)
       (setq charset (completing-read "Character set (default US-ASCII): "
				      vm-mime-charset-completion-alist)
	     charset (if (> (length charset) 0) charset)))
     (setq description (read-string "One line description: "))
     (when (string-match "^[ \t]*$" description)
       (setq description nil))
     (list buffer-name type charset description)))
  (unless (setq buffer (get-buffer buffer))
    (error "Buffer %s does not exist." buffer))
  (unless vm-send-using-mime
    (error (concat "MIME attachments disabled, "
		   "set vm-send-using-mime non-nil to enable.")))
  (when charset 
    (setq charset (list (concat "charset=" charset))))
  (when description 
    (setq description (vm-mime-scrub-description description)))
  (vm-attach-object buffer :type type :params charset
			 :description description :mimed nil))
(defalias 'vm-mime-attach-buffer 'vm-attach-buffer)


;;;###autoload
(defun vm-attach-message (message &optional description)
  "Attach a message from a VM folder to the current VM
composition.

The message is not inserted into the buffer and MIME encoded until
you execute `vm-mail-send' or `vm-mail-send-and-exit'.  A visible tag
indicating the existence of the attachment is placed in the
composition buffer.  You can move the attachment around or remove
it entirely with normal text editing commands.  If you remove the
attachment tag, the attachment will not be sent.

First argument, MESSAGE, is either a VM message struct or a list
of message structs.  When called interactively a message number is read
from the minibuffer.  The message will come from the parent
folder of this composition.  If the composition has no parent,
the name of a folder will be read from the minibuffer before the
message number is read.

If this command is invoked with a prefix argument, the name of a
folder is read and that folder is used instead of the parent
folder of the composition.

If this command is invoked on marked message (via
`vm-next-command-uses-marks') the marked messages in the selected
folder will be attached as a MIME message digest.    If
applied to collapsed threads in summary and thread operations are
enabled via `vm-enable-thread-operations' then all messages in the
thread are attached.

Optional second argument DESCRIPTION is a one-line description of
the message being attached.  This is also read from the
minibuffer if the command is run interactively."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command)
	 (result 0)
	 mlist mp default prompt description folder)
     (unless (eq major-mode 'mail-mode)
       (error "Command must be used in a VM Mail mode buffer."))
     (unless vm-send-using-mime
       (error (concat "MIME attachments disabled, "
		      "set vm-send-using-mime non-nil to enable.")))
     (when current-prefix-arg
       (setq vm-mail-buffer (vm-read-folder-name)
	     vm-mail-buffer (if (string= vm-mail-buffer "") nil
			      (setq current-prefix-arg nil)
			      (get-buffer vm-mail-buffer))))
     (cond ((or current-prefix-arg (null vm-mail-buffer)
		(not (buffer-live-p vm-mail-buffer)))
	    (let ((dir (if vm-folder-directory
			   (expand-file-name vm-folder-directory)
			 default-directory))
		  file)
	      (let ((last-command last-command)
		    (this-command this-command))
		(setq file (read-file-name "Attach message from folder: "
					   dir nil t)))
	      (let ((coding-system-for-read (vm-binary-coding-system)))
		(setq folder (find-file-noselect file)))
	      (with-current-buffer folder
		(vm-mode)
		(setq mlist (vm-select-operable-messages 1 t "Attach")))))
	   (t
	    (setq folder vm-mail-buffer)
	    (with-current-buffer folder
	      (setq mlist (vm-select-operable-messages 1 t "Attach")))))
     (when (null mlist)
       (with-current-buffer folder
	 (setq default (and vm-message-pointer
			    (vm-number-of (car vm-message-pointer)))
	       prompt (if default
			  (format "Attach message number: (default %s) "
				  default)
			"Attach message number: "))
	 (while (zerop result)
	   (setq result (read-string prompt))
	   (and (string= result "") default (setq result default))
	   (setq result (string-to-number result)))
	 (when (null (setq mp (nthcdr (1- result) vm-message-list)))
	   (error "No such message."))))
     (setq description (read-string "Description: "))
     (when (string-match "^[ \t]*$" description)
       (setq description nil))
     (list (or mlist (car mp)) description)))

  (unless vm-send-using-mime
    (error (concat "MIME attachments disabled, "
		   "set vm-send-using-mime non-nil to enable.")))
  (if (not (consp message))
      (let* ((work-buffer (vm-generate-new-unibyte-buffer "*attached message*"))
	     (m (vm-real-message-of message))
	     (folder (vm-buffer-of m)))
	(with-current-buffer work-buffer
	  (vm-insert-region-from-buffer folder (vm-headers-of m)
					(vm-text-end-of m))
	  (goto-char (point-min))
	  (vm-reorder-message-headers
	   nil :keep-list nil
	   :discard-regexp vm-internal-unforwarded-header-regexp))
	(and description (setq description
			       (vm-mime-scrub-description description)))
	(vm-attach-object work-buffer 
			       :type "message/rfc822" :params nil 
			       :disposition '("inline")
			       :description description)
	(make-local-variable 'vm-forward-list)
	(setq vm-system-state 'forwarding
	      vm-forward-list (list message))
	(add-hook 'kill-buffer-hook
		  `(lambda ()
		     (if (eq (current-buffer) ,(current-buffer))
		  	 (kill-buffer ,work-buffer)))
		  ))
    (let ((work-buffer (vm-generate-new-unibyte-buffer "*attached messages*"))
	  boundary)
      (with-current-buffer work-buffer
	(setq boundary (vm-mime-encapsulate-messages
			message :keep-list vm-mime-digest-headers
			:discard-regexp vm-mime-digest-discard-header-regexp
			:always-use-digest t))
	(goto-char (point-min))
	(insert "MIME-Version: 1.0\n")
	(insert "Content-Type: "
		(vm-mime-type-with-params 
		 "multipart/digest"
		 (list (concat "boundary=\"" boundary "\"")))
		"\n")
	(insert "Content-Transfer-Encoding: "
		(vm-determine-proper-content-transfer-encoding
		 (point)
		 (point-max))
		"\n\n"))
      (when description 
	(setq description (vm-mime-scrub-description description)))
      (vm-attach-object work-buffer :type "multipart/digest"
			     :params (list (concat "boundary=\"" 
						   boundary "\"")) 
			     :disposition '("inline")
			     :description nil :mimed t)
      (make-local-variable 'vm-forward-list)
      (setq vm-system-state 'forwarding
	    vm-forward-list (copy-sequence message))
      (add-hook 'kill-buffer-hook
		`(lambda ()
		   (if (eq (current-buffer) ,(current-buffer))
		       (kill-buffer ,work-buffer)))))))
(defalias 'vm-mime-attach-message 'vm-attach-message)


;;;###autoload
(defun vm-attach-message-to-composition (composition &optional description)
  "Attach the current message from the current VM folder to a VM
composition.

The message is not inserted into the buffer and MIME encoded until
you execute `vm-mail-send' or `vm-mail-send-and-exit'.  A visible tag
indicating the existence of the attachment is placed in the
composition buffer.  You can move the attachment around or remove
it entirely with normal text editing commands.  If you remove the
attachment tag, the attachment will not be sent.

First argument COMPOSITION is the buffer into which the object
will be inserted.  When this function is called interactively
COMPOSITION's name will be read from the minibuffer.

If this command is invoked on marked message (via
`vm-next-command-uses-marks') the marked messages in the selected
folder will be attached as a MIME message digest.    If
applied to collapsed threads in summary and thread operations are
enabled via `vm-enable-thread-operations' then all messages in the
thread are attached.

Optional second argument DESCRIPTION is a one-line description of
the message being attached.  This is also read from the
minibuffer if the command is run interactively."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command)
	 description)
     (vm-select-folder-buffer-and-validate 1 t)
     (unless (memq major-mode '(vm-mode vm-virtual-mode))
       (error "Command must be used in a VM buffer."))
     (unless vm-send-using-mime
       (error (concat "MIME attachments disabled, "
		      "set vm-send-using-mime non-nil to enable.")))
     (list
      (read-buffer "Attach object to buffer: "
		   (vm-find-composition-buffer) t)
      (progn (setq description (read-string "Description: "))
	     (when (string-match "^[ \t]*$" description)
	       (setq description nil))
	     description))))

  (unless vm-send-using-mime
    (error (concat "MIME attachments disabled, "
		   "set vm-send-using-mime non-nil to enable.")))
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)

  (let* ((work-buffer (vm-generate-new-unibyte-buffer "*attached message*"))
	 (m (vm-real-message-of (vm-current-message)))
	 (folder (vm-buffer-of m))
	 w)
    (with-current-buffer work-buffer
      (vm-insert-region-from-buffer folder (vm-headers-of m)
				    (vm-text-end-of m))
      (goto-char (point-min))
      (vm-reorder-message-headers
       nil :keep-list nil
       :discard-regexp vm-internal-unforwarded-header-regexp)
      (when description 
	(setq description
	      (vm-mime-scrub-description description))))
    (with-current-buffer composition
      (vm-attach-object work-buffer 
			     :type "message/rfc822" :params nil 
			     :disposition '("inline")
			     :description description)
      (make-local-variable 'vm-forward-list)
      (setq vm-system-state 'forwarding
	    vm-forward-list (list m))
      ;; move window point forward so that if this command
      ;; is used consecutively, the insertions will be in
      ;; the correct order in the composition buffer.
      (setq w (vm-get-buffer-window composition))
      (and w (set-window-point w (point)))
      (add-hook 'kill-buffer-hook
		`(lambda ()
		   (if (eq (current-buffer) ,(current-buffer))
		       (kill-buffer ,work-buffer)))))))
(defalias 'vm-mime-attach-message-to-composition
  'vm-attach-message-to-composition)
		      
;;;###autoload
(defun vm-attach-object-to-composition (layout &optional composition)
  "Attach the mime object described by LAYOUT to a VM composition buffer.

The object is not inserted into the buffer and MIME encoded until
you execute `vm-mail-send' or `vm-mail-send-and-exit'.  A visible tag
indicating the existence of the object is placed in the
composition buffer.  You can move the object around or remove
it entirely with normal text editing commands.  If you remove the
object tag, the object will not be sent.

The optional argument COMPOSITION is the buffer into which the object
will be inserted.  When this function is called interactively
COMPOSITION's name will be read from the minibuffer."
  (unless composition
    (setq composition (read-buffer "Attach object to buffer: "
				   (vm-find-composition-buffer) t)))
  (unless vm-send-using-mime
    (error (concat "MIME attachments disabled, "
		   "set vm-send-using-mime non-nil to enable.")))
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)

  (let ((work-buffer (vm-make-work-buffer)) 
	buf start w)
    (unwind-protect
	(with-current-buffer work-buffer
	  (vm-mime-insert-mime-headers layout)
	  (insert "\n")
	  (setq start (point))
	  (vm-mime-insert-mime-body layout)
	  (vm-mime-transfer-decode-region layout start (point-max))
	  (goto-char (point-min))
	  (vm-reorder-message-headers 
	   nil :keep-list nil :discard-regexp "Content-Transfer-Encoding:")
	  (insert "Content-Transfer-Encoding: binary\n")
	  (set-buffer composition)
	  ;; FIXME need to copy the disposition from the original
	  (vm-attach-object work-buffer 
			    :type (car (vm-mm-layout-type layout)) 
			    :params (cdr (vm-mm-layout-type layout))
			    :description (vm-mm-layout-description 
					  layout)
			    :mimed t)
	  ;; move window point forward so that if this command
	  ;; is used consecutively, the insertions will be in
	  ;; the correct order in the composition buffer.
	  (setq w (vm-get-buffer-window composition))
	  (and w (set-window-point w (point)))
	  (setq buf work-buffer
		work-buffer nil)	; schedule to be killed later
	  (add-hook 'kill-buffer-hook
		    `(lambda ()
		       (if (eq (current-buffer) ,(current-buffer))
			   (kill-buffer ,buf))))
	  )
      ;; unwind-protection
      (when work-buffer (kill-buffer work-buffer)))))
(defalias 'vm-mime-attach-object-to-composition
  'vm-attach-object-to-composition)
(defalias 'vm-mime-attach-object-from-message 
  'vm-attach-object-to-composition)
(make-obsolete 'vm-mime-attach-object-from-message
	       'vm-attach-object-to-composition "8.2.0")


(defun* vm-attach-object (object &key type params description 
				      (mimed nil)
				      (disposition '("unspecified"))
				      (no-suggested-filename nil))
  "Attach a MIME OBJECT to the mail composition in the current
buffer.  The OBJECT could be:
  - the full path name of a file
  - a buffer, or
  - a list with the elements: buffer, start position, end position,
    disposition and optional file name.
TYPE, PARAMS and DESCRIPTION and DISPOSITION are the standard MIME
properties. 
MIMED says whether the OBJECT already has MIME headers.
Optional argument NO-SUGGESTED-FILENAME is a boolean indicating that
there is no file name for this object.             USR, 2011-03-07"
  (unless (eq major-mode 'mail-mode)
    (error "VM internal error: vm-attach-object not in Mail mode buffer."))
  (when (vm-mail-mode-get-header-contents "MIME-Version")
    (error "Can't attach MIME object to already encoded MIME buffer."))
  (let (start end e tag-string file-name
	(fb (list vm-mime-forward-local-external-bodies)))
    (cond ((and (stringp object) (not mimed))
	   (if (or (vm-mime-types-match "application" type)
		   (vm-mime-types-match "model" type))
	       (setq disposition (list "attachment"))
	     (setq disposition (list "inline")))
	   (unless no-suggested-filename
	     (setq file-name (file-name-nondirectory object))
	     ;; why fuse things together?  USR, 2011-03-17
;; 	     (setq type 
;; 		   (concat type "; name=\"" file-name "\""))
	     (setq params
		   (list (concat "name=\"" file-name "\"")))
	     (setq disposition 
		   (nconc disposition
			  (list (concat "filename=\"" file-name "\""))))))
	  ((listp object) 
	   (setq file-name (nth 4 object))
	   (setq disposition (nth 3 object)))
	  (t
	   (setq file-name 
		 (or (vm-mime-get-xxx-parameter "name" params)
		     (vm-mime-get-xxx-parameter "filename" params)))))
    (when (< (point) (save-excursion (mail-text) (point)))
      (mail-text))
    (setq start (point))
    (setq tag-string (format "[ATTACHMENT %s, %s]" 
			     (or file-name description "") 
			     (or type "MIME file")))
;;     (if (listp object)
;; 	(setq tag-string (format "[ATTACHMENT %s, %s]" 
;; 				 (or (nth 4 object) "") type))
;;       (setq tag-string (format "[ATTACHMENT %s, %s]" object
;; 			     (or type "MIME file"))))
    (insert tag-string "\n")
    (setq end (1- (point)))


    (cond (vm-fsfemacs-p
	   (put-text-property start end 'front-sticky nil)
	   (put-text-property start end 'rear-nonsticky t)
	   ;; can't be intangible because menu clicking at a position
	   ;; needs to set point inside the tag so that a command can
	   ;; access the text properties there.
	   ;; (put-text-property start end 'intangible object) 
	   (put-text-property start end 'face vm-attachment-button-face)
	   (put-text-property start end 'font-lock-face 
			      vm-attachment-button-face)
	   (put-text-property start end 'mouse-face 
			      vm-attachment-button-mouse-face)
	   (put-text-property start end 'vm-mime-forward-local-refs fb)
	   (put-text-property start end 'vm-mime-type type)
	   (put-text-property start end 'vm-mime-object object)
	   (put-text-property start end 'vm-mime-parameters params)
	   (put-text-property start end 'vm-mime-description description)
	   (put-text-property start end 'vm-mime-disposition disposition)
	   (put-text-property start end 'vm-mime-encoding nil)
	   (put-text-property start end 'vm-mime-encoded mimed)
	   ;; (put-text-property start end 'duplicable t)
	   )
	  (vm-xemacs-p
	   (setq e (vm-make-extent start end))
	   (vm-mime-set-image-stamp-for-type e (or type "text/plain"))
	   (vm-set-extent-property e 'start-open t)
	   (vm-set-extent-property e 'face vm-mime-button-face)
	   (vm-set-extent-property e 'mouse-face vm-mime-button-mouse-face)
	   (vm-set-extent-property e 'duplicable t)
	   (let ((keymap (make-sparse-keymap)))
	     (when vm-popup-menu-on-mouse-3
	       (define-key keymap 'button3
		 'vm-menu-popup-attachment-menu))
             (define-key keymap [return] 'vm-mime-change-content-disposition)
	     (vm-set-extent-property e 'keymap keymap)
	     (vm-set-extent-property e 'balloon-help 'vm-mouse-3-help))
	   (vm-set-extent-property e 'vm-mime-forward-local-refs fb)
	   (vm-set-extent-property e 'vm-mime-type type)
	   (vm-set-extent-property e 'vm-mime-object object)
	   (vm-set-extent-property e 'vm-mime-parameters params)
	   (vm-set-extent-property e 'vm-mime-description description)
	   (vm-set-extent-property e 'vm-mime-disposition disposition)
	   (vm-set-extent-property e 'vm-mime-encoding nil)
	   (vm-set-extent-property e 'vm-mime-encoded mimed)))))
(defalias 'vm-mime-attach-object 'vm-attach-object)

(defun vm-mime-attachment-forward-local-refs-at-point ()
  (cond (vm-fsfemacs-p
	 (let ((fb (get-text-property (point) 'vm-mime-forward-local-refs)))
	   (car fb) ))
	(vm-xemacs-p
	 (let* ((e (vm-extent-at (point) 'vm-mime-type))
		(fb (vm-extent-property e 'vm-mime-forward-local-refs)))
	   (car fb) ))))

(defun vm-mime-set-attachment-forward-local-refs-at-point (val)
  (cond (vm-fsfemacs-p
	 (let ((fb (get-text-property (point) 'vm-mime-forward-local-refs)))
	   (setcar fb val) ))
	(vm-xemacs-p
	 (let* ((e (vm-extent-at (point) 'vm-mime-type))
		(fb (vm-extent-property e 'vm-mime-forward-local-refs)))
	   (setcar fb val) ))))

(defun vm-mime-delete-attachment-button ()
  (cond (vm-fsfemacs-p
         ;; TODO
         )
	(vm-xemacs-p
	 (let ((e (vm-extent-at (point) 'vm-mime-type)))
           (delete-region (vm-extent-start-position e)
                          (vm-extent-end-position e))))))

(defun vm-mime-delete-attachment-button-keep-infos ()
  (cond (vm-fsfemacs-p
         ;; TODO
         )
	(vm-xemacs-p
	 (let ((e (vm-extent-at (point) 'vm-mime-type)))
           (save-excursion
             (goto-char (1+ (vm-extent-start-position e)))
             (insert " --- DELETED ")
             (goto-char (vm-extent-end-position e))
             (insert " ---")
             (vm-delete-extent e))))))

;;;###autoload
(defun vm-mime-change-content-disposition ()
  (interactive)
  (vm-mime-set-attachment-disposition-at-point
   (intern
    (completing-read "Disposition-type: "
                     '(("unspecified") ("inline") ("attachment"))
                     nil
                     t))))

(defun vm-mime-attachment-disposition-at-point ()
  (cond (vm-fsfemacs-p
	 (let ((disp (get-text-property (point) 'vm-mime-disposition)))
	   (intern (car disp))))
	(vm-xemacs-p
	 (let* ((e (vm-extent-at (point) 'vm-mime-disposition))
		(disp (vm-extent-property e 'vm-mime-disposition)))
	   (intern (car disp))))))

(defun vm-mime-set-attachment-disposition-at-point (sym)
  (cond (vm-fsfemacs-p
	 (let ((disp (get-text-property (point) 'vm-mime-disposition)))
	   (setcar disp (symbol-name sym))))
	(vm-xemacs-p
	 (let* ((e (vm-extent-at (point) 'vm-mime-disposition))
		(disp (vm-extent-property e 'vm-mime-disposition)))
	   (setcar disp (symbol-name sym))))))


(defun vm-mime-attachment-encoding-at-point ()
  (cond (vm-fsfemacs-p
	 (get-text-property (point) 'vm-mime-encoding))
	(vm-xemacs-p
	 (let ((e (vm-extent-at (point) 'vm-mime-encoding)))
           (if e (vm-extent-property e 'vm-mime-encoding))))))

(defun vm-mime-set-attachment-encoding-at-point (sym)
  (cond (vm-fsfemacs-p
	 ;; (set-text-property (point) 'vm-mime-encoding sym)
	 (put-text-property (point) (point) 'vm-mime-encoding sym)
	 )
	(vm-xemacs-p
	 (let ((e (vm-extent-at (point) 'vm-mime-disposition)))
           (vm-set-extent-property e 'vm-mime-encoding sym)))))

(defun vm-disallow-overlay-endpoint-insertion 
  (overlay after start end &optional old-size)
  "Hook function called before and after text is inserted at the
endpoint of an OVERLAY.  AFTER is true if the call is being made after
insertion.  Otherwise, it is being made before insertion.  START and
END denote the range of the text inserted.  Optional argument
OLD-SIZE is ignored.

This hook does nothing when called before insertion.  When it is
called after insertion, it moves the overlay so that the inserted is
excluded from the overlay."
  (when after
    (cond ((= start (overlay-start overlay))
	   (move-overlay overlay end (overlay-end overlay)))
	  ((= start (overlay-end overlay))
	   (move-overlay overlay (overlay-start overlay) start)))))

(defun vm-mime-attachment-button-extents (start end &optional prop)
  "Return the extents of all attachment buttons in the region.  Optional
argument PROP can specify an extent property, in which case only those
extents that have the property are returned.

In GNU Emacs version of this function, attachment buttons are expected
to be denoted by text-properties rather than extents.  \"Fake\"
extents are created for the purpose of this function.  USR, 2011-03-27"
  (let ((e-list  (if vm-xemacs-p
		     (vm-extent-list start end prop)
		   (vm-mime-fake-attachment-overlays start end prop))))
    (sort e-list (function
		  (lambda (e1 e2)
		    (< (vm-extent-end-position e1)
		       (vm-extent-end-position e2)))))))

(defun vm-mime-fake-attachment-overlays (start end &optional prop)
  "For all attachment buttons in the region, i.e., pieces of text
with the given text property PROP, create \"fake\" attachment
overlays with the 'vm-mime-object property.  The list of these
overlays is returned.

This function is only used with GNU Emacs, not XEmacs.  USR, 2011-02-19"
  ;; This round about method is being used because in GNU Emacs,
  ;; only text properties are preserved under killing and yanking.
  ;; So, text properties are normally used for attachment buttons and
  ;; converted to overlays just before MIME encoding.  USR, 2011-02-19
  (when (null prop) (setq prop 'vm-mime-object))
  (let ((o-list nil)
	(done nil)
	(pos start)
	object props o)
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(while (not done)
	  (setq object (get-text-property pos prop))
	  (setq pos (next-single-property-change pos prop))
	  (unless pos 
	    (setq pos (point-max) 
		  done t))
	  (when object
	    (setq o (make-overlay start pos nil t nil))
	    ;; (overlay-put o 'insert-in-front-hooks
	    ;; 		 '(vm-disallow-overlay-endpoint-insertion))
	    ;; (overlay-put o 'insert-behind-hooks
	    ;; 		 '(vm-disallow-overlay-endpoint-insertion))
	    (setq props (text-properties-at start))
	    (unless (eq prop 'vm-mime-object)
	      (setq props (append (list 'vm-mime-object t) props)))
	    (while props
	      (overlay-put o (car props) (cadr props))
	      (setq props (cddr props)))
	    (setq o-list (cons o o-list)))
	  (setq start pos))
	o-list ))))

(defun vm-mime-default-type-from-filename (file)
  (let ((alist vm-mime-attachment-auto-type-alist)
	(case-fold-search t)
	(done nil))
    (while (and alist (not done))
      (if (string-match (car (car alist)) file)
	  (setq done t)
	(setq alist (cdr alist))))
    (and alist (cdr (car alist)))))

(defun vm-remove-mail-mode-header-separator ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat "^" mail-header-separator "$") nil t)
	(progn
	  (delete-region (match-beginning 0) (match-end 0))
	   t )
      nil )))

(defun vm-add-mail-mode-header-separator ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^$" nil t)
	(replace-match mail-header-separator t t))))

(defun vm-mime-transfer-encode-region (encoding beg end crlf)
  "Encode region between BEG and END using transfer ENCODING (base64,
quoted-printable or binary).  CRLF says whether carriage returns
should be included (?)                               USR, 2011-03-27"
  (let ((case-fold-search t)
	(armor-from (and vm-mime-composition-armor-from-lines
			 (let ((case-fold-search nil))
			   (save-excursion
			     (goto-char beg)
			     (re-search-forward "^From " nil t)))))
	(armor-dot (let ((case-fold-search nil))
		     (save-excursion
		       (goto-char beg)
		       (re-search-forward "^\\.\n" nil t)))))
    (cond ((string-match "^binary$" encoding)
	   (vm-mime-base64-encode-region beg end crlf)
	   (setq encoding "base64"))
	  ((and (not armor-from) (not armor-dot)
		(string-match "^7bit$" encoding)) t)
	  ((string-match "^base64$" encoding) t)
	  ((string-match "^quoted-printable$" encoding) t)
	  ((eq vm-mime-8bit-text-transfer-encoding 'quoted-printable)
	   (vm-mime-qp-encode-region beg end nil armor-from)
	   (setq encoding "quoted-printable"))
	  ((eq vm-mime-8bit-text-transfer-encoding 'base64)
	   (vm-mime-base64-encode-region beg end crlf)
	   (setq encoding "base64"))
	  ((or armor-from armor-dot)
	   (vm-mime-qp-encode-region beg end nil armor-from)
	   (setq encoding "quoted-printable")))
    (downcase encoding) ))

(defun vm-mime-transfer-encode-layout (layout)
  "Encode a MIME object described by LAYOUT in transfer encoding (base64,
quoted-printable or binary).                            USR, 2011-03-27"
  (let ((list (vm-mm-layout-parts layout))
	(type (car (vm-mm-layout-type layout)))
	(encoding "7bit")
	(vm-mime-8bit-text-transfer-encoding
	 vm-mime-8bit-text-transfer-encoding))
  (cond ((vm-mime-composite-type-p type)
	 ;; MIME messages of type "message" and
	 ;; "multipart" are required to have a non-opaque
	 ;; content transfer encoding.  This means that
	 ;; if the user only wants to send out 7bit data,
	 ;; then any subpart that contains 8bit data must
	 ;; have an opaque (qp or base64) 8->7bit
	 ;; conversion performed on it so that the
	 ;; enclosing entity can use a non-opaque
	 ;; encoding.
	 ;;
	 ;; message/partial requires a "7bit" encoding so
	 ;; force 8->7 conversion in that case.
	 (cond ((memq vm-mime-8bit-text-transfer-encoding
		      '(quoted-printable base64))
		t)
	       ((vm-mime-types-match "message/partial" type)
		(setq vm-mime-8bit-text-transfer-encoding
		      'quoted-printable)))
	 (while list
	   (if (equal (vm-mime-transfer-encode-layout (car list)) "8bit")
	       (setq encoding "8bit"))
	   (setq list (cdr list))))
	(t
	 (when (and (vm-mime-types-match "message/partial" type)
		    (not (memq vm-mime-8bit-text-transfer-encoding
			       '(quoted-printable base64))))
	   (setq vm-mime-8bit-text-transfer-encoding 'quoted-printable))
	 (setq encoding
	       (vm-mime-transfer-encode-region (vm-mm-layout-encoding layout)
					       (vm-mm-layout-body-start layout)
					       (vm-mm-layout-body-end layout)
					       (vm-mime-text-type-layout-p
						layout)))))
  ;; seems redundant because an encoding can never be equal to a type.
  ;; but it wasn't meant to be encoding becuase it woundn't be a list.
  ;; who knows that is supposed to be?  USR, 2011-03-27
  (unless (equal encoding (downcase (car (vm-mm-layout-type layout))))
      (save-excursion
	(save-restriction
	  (goto-char (vm-mm-layout-header-start layout))
	  (narrow-to-region (point) (vm-mm-layout-header-end layout))
	  (vm-reorder-message-headers 
	   nil :keep-list nil :discard-regexp "Content-Transfer-Encoding:")
	  (if (not (equal encoding "7bit"))
	      (insert "CONTENT-TRANSFER-ENCODING: " encoding "\n"))
	  encoding )))))

(defun vm-mime-text-description (start end)
  (save-excursion
    (goto-char start)
    (if (looking-at "[ \t\n]*-- \n")
	".signature"
      (if (re-search-forward "^-- \n" nil t)
	  "message body and .signature"
	"message body text"))))
;; tried this but random text in the object tag does't look right.
;;      (skip-chars-forward " \t\n")
;;      (let ((description (buffer-substring (point) (min (+ (point) 20) end)))
;;	    (ellipsis (< (+ (point) 20) end))
;;	    (i nil))
;;	(while (setq i (string-match "[\t\r\n]" description i))
;;	  (aset description i " "))
;;	(cond ((= 0 (length description)) nil)
;;	      (ellipsis (concat description "..."))
;;	      (t description))))))

;;;###autoload
(defun vm-delete-mime-object (&optional saved-file)
  "Delete the contents of the MIME object at point.
The MIME object is replaced by a text/plain object that briefly
describes what was deleted."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer-and-validate 1 (interactive-p))
  (vm-error-if-folder-read-only)
  (when (and (vm-virtual-message-p (car vm-message-pointer))
	     (null (vm-virtual-messages-of (car vm-message-pointer))))
    (error "Can't edit unmirrored virtual messages."))
  (when vm-presentation-buffer
    (set-buffer vm-presentation-buffer))
  (let (layout label)
    (let ((e (vm-extent-at (point) 'vm-mime-layout)))
      (if (null e)
	  (error "No MIME button found at point.")
	(setq layout (vm-extent-property e 'vm-mime-layout))
	(when (and (vm-mm-layout-message layout)
		   (eq layout (vm-mime-layout-of
			       (vm-mm-layout-message layout))))
	  (error (concat "Can't delete the only MIME object; "
			 "use vm-delete-message instead.")))
	(when vm-mime-confirm-delete
	  (unless (y-or-n-p (vm-mime-sprintf "Delete %t? " layout))
	    (error "Aborted")))
	(let ((inhibit-read-only t)
	      opos
	      (buffer-read-only nil))
	  (save-excursion
	    (vm-save-restriction
	     (goto-char (vm-extent-start-position e))
	     (setq opos (point))
	     (setq label (vm-mime-sprintf 
			  vm-mime-deleted-object-label layout))
	     (insert label)
	     (delete-region (point) (vm-extent-end-position e))
	     (vm-set-extent-endpoints e opos (point)))))
	(vm-mime-discard-layout-contents layout saved-file)))
    (when (interactive-p)
      ;; make the change visible and place the cursor behind the removed object
      (vm-discard-cached-data)
      (when vm-presentation-buffer
        (set-buffer vm-presentation-buffer)
        (re-search-forward (regexp-quote label) (point-max) t)))))

(defun vm-mime-discard-layout-contents (layout &optional file)
  (save-excursion
    (let ((inhibit-read-only t)
	  (buffer-read-only nil)
	  (m (vm-mm-layout-message layout))
	  newid new-layout)
      (if (null m)
	  (error "Message body not loaded"))
      (set-buffer (vm-buffer-of m))
      (vm-save-restriction
	(widen)
	(if (vm-mm-layout-is-converted layout)
	    (setq layout (vm-mm-layout-unconverted-layout layout)))
	(goto-char (vm-mm-layout-header-start layout))
	(cond ((null file)
	       (insert "Content-Type: text/plain; charset=us-ascii\n\n")
	       (vm-set-mm-layout-body-start layout (point-marker))
	       (insert (vm-mime-sprintf vm-mime-deleted-object-label layout)))
	      (t
	       (insert "Content-Type: message/external-body; access-type=local-file; name=\"" file "\"\n")
	       (insert "Content-Transfer-Encoding: 7bit\n\n")
	       (insert "Content-Type: " 
		       (vm-mime-type-with-params
			(car (vm-mm-layout-qtype layout))
			(cdr (vm-mm-layout-qtype layout)))
		       "\n")
	       (if (vm-mm-layout-qdisposition layout)
		   (let ((p (vm-mm-layout-qdisposition layout)))
		     (insert "Content-Disposition: "
			     (mapconcat 'identity p "; ")
			     "\n")))
	       (if (vm-mm-layout-id layout)
		   (insert "Content-ID: " (vm-mm-layout-id layout) "\n")
		 (setq newid (vm-make-message-id))
		 (insert "Content-ID: " newid "\n"))
	       (insert "Content-Transfer-Encoding: binary\n\n")
	       (insert "[Deleted " (vm-mime-sprintf "%d]\n" layout))
	       (insert "[Saved to " file " on " (system-name) "]\n")))
	(delete-region (point) (vm-mm-layout-body-end layout))
	(vm-set-edited-flag-of m t)
	(vm-set-byte-count-of m nil)
	(vm-set-line-count-of m nil)
	(vm-set-stuff-flag-of m t)
	;; For the dreaded From_-with-Content-Length folders recompute
	;; the message length and make a new Content-Length header.
	(if (eq (vm-message-type-of m) 'From_-with-Content-Length)
	    (let (length)
	      (goto-char (vm-headers-of m))
	      ;; first delete all copies of Content-Length
	      (while (and (re-search-forward vm-content-length-search-regexp
					     (vm-text-of m) t)
			  (null (match-beginning 1))
			  (progn (goto-char (match-beginning 0))
				 (vm-match-header vm-content-length-header)))
		(delete-region (vm-matched-header-start)
			       (vm-matched-header-end)))
	      ;; now compute the message body length
	      (setq length (- (vm-end-of m) (vm-text-of m)))
	      ;; insert the header
	      (goto-char (vm-headers-of m))
	      (insert vm-content-length-header " "
		      (int-to-string length) "\n")))
	;; make sure we get the summary updated.  The 'edited'
	;; flag might already be set and therefore trying to set
	;; it again might not have triggered an update.  We need
	;; the update because the message size has changed.
	(vm-mark-for-summary-update (vm-mm-layout-message layout))
	(cond (file
	       (save-restriction
		 (narrow-to-region (vm-mm-layout-header-start layout)
				   (vm-mm-layout-body-end layout))
		 (setq new-layout (vm-mime-parse-entity-safe))
		 (vm-set-mm-layout-message-symbol
		  new-layout (vm-mm-layout-message-symbol layout))
		 (vm-mime-copy-layout new-layout layout)))
	      (t
	       (vm-set-mm-layout-type layout '("text/plain"))
	       (vm-set-mm-layout-qtype layout '("text/plain"))
	       (vm-set-mm-layout-encoding layout "7bit")
	       (vm-set-mm-layout-id layout nil)
	       (vm-set-mm-layout-description
		layout
		(vm-mime-sprintf "Deleted %d" layout))
	       (vm-set-mm-layout-disposition layout nil)
	       (vm-set-mm-layout-qdisposition layout nil)
	       (vm-set-mm-layout-parts layout nil)
	       (vm-set-mm-layout-display-error layout nil)))))))

(defun vm-mime-encode-words (&optional encoding)
  (goto-char (point-min))

  ;; find right encoding 
  (setq encoding (or encoding vm-mime-encode-headers-type))
  (save-excursion
    (when (stringp encoding)
      (setq encoding 
            (if (re-search-forward encoding (point-max) t)
                'B
              'Q))))
  ;; now encode the words 
  (let ((case-fold-search nil)
        start end charset coding)
    (while (re-search-forward vm-mime-encode-headers-words-regexp (point-max) t)
      (setq start (match-beginning 1)
            end   (vm-marker (match-end 0))
            charset (or (vm-determine-proper-charset start end)
                        vm-mime-8bit-composition-charset)
            coding (vm-mime-charset-to-coding charset))
      ;; encode coding system body
      (when (and  coding (not (eq coding 'no-conversion)))
        (if vm-xemacs-p
	    (vm-encode-coding-region start end coding)
	  ;; using vm-encode-coding-region causes wrong encoding in GNU Emacs
	  (encode-coding-region start end coding)))
      ;; encode 
      (if (eq encoding 'Q)
	  (vm-mime-Q-encode-region start end)
        (vm-mime-base64-encode-region  start end))
      ;; insert start and end markers 
      (goto-char start)
      (insert "=?" charset "?" (format "%s" encoding) "?")
      (setq start (point))
      (goto-char end)
      (insert "?=")
      ;; goto end for next round
      (goto-char end))))

;;;###autoload
(defun vm-mime-encode-words-in-string (string &optional encoding)
  (and string
       (vm-with-string-as-temp-buffer 
	(vm-substring-no-properties string 0)
	'vm-mime-encode-words)))

(defun vm-mime-encode-headers ()
  "Encodes the headers of a message.

Only the words containing a non 7bit ASCII char are encoded, but not the whole
header as this will cause trouble for the recipients and authors headers.

Whitespace between encoded words is trimmed during decoding and thus those
should be encoded together."
  (interactive)
  (save-excursion 
    (let ((headers (concat "^\\(" vm-mime-encode-headers-regexp "\\):"))
          (case-fold-search nil)
          (encoding vm-mime-encode-headers-type)
          body-start
          start end)
      (goto-char (point-min))
      (search-forward (concat "\n" mail-header-separator "\n"))
      (setq body-start (vm-marker (match-beginning 0)))
      (goto-char (point-min))
      
      (while (let ((case-fold-search t))
	       (re-search-forward headers body-start t))
        (goto-char (match-end 0))
        (setq start (point))
        (when (not (looking-at "\\s-"))
          (insert " ")
          (backward-char 1))
        (save-excursion
          (setq end (or (and (re-search-forward "^[^ \t:]+:" body-start t)
                             (match-beginning 0))
                        body-start)))
        (vm-save-restriction
         (narrow-to-region start end)
         (vm-mime-encode-words))
        (goto-char end)))))

;;;###autoload
(defun vm-mime-encode-composition ()
 "MIME encode the current mail composition buffer.

This function chooses the MIME character set(s) to use, and transforms the
message content from the Emacs-internal encoding to the corresponding
octets in that MIME character set.

It then applies some transfer encoding to the message. For details of the
transfer encodings available, see the documentation for
`vm-mime-8bit-text-transfer-encoding.'

Finally, it creates the headers that are necessary to identify the message
as one that uses MIME.

Under MULE, it explicitly sets `buffer-file-coding-system' to a binary
 (no-transformation) coding system, to avoid further transformation of the
message content when it's passed to the MTA (that is, the mail transfer
agent; under Unix, normally sendmail.)

Attachment tags added to the buffer with `vm-attach-file' are expanded
and the approriate content-type and boundary markup information is added."

  (interactive)

  (vm-mail-mode-show-headers)

  (vm-disable-modes vm-disable-modes-before-encoding)

  (vm-mime-encode-headers)

  (if vm-mail-reorder-message-headers
      (vm-reorder-message-headers 
       nil :keep-list vm-mail-header-order :discard-regexp 'none))
  
  (buffer-enable-undo)
  (let ((unwind-needed t)
	(mybuffer (current-buffer)))
    (unwind-protect
	(progn
	  (vm-mime-encode-composition-internal)
	  (setq unwind-needed nil))
      (and unwind-needed (consp buffer-undo-list)
	   (eq mybuffer (current-buffer))
	   (setq buffer-undo-list (primitive-undo 1 buffer-undo-list))))))

(defvar enriched-mode)

;; This function was originally XEmacs-specific.  It has now been
;; generalized to both XEmacs and GNU Emacs.  USR, 2011-03-27

(defun vm-mime-encode-composition-internal ()
  "MIME encode the message composition in the current buffer."
  (save-restriction
    (widen)
    (unless (eq major-mode 'mail-mode)
      (error "Command must be used in a VM Mail mode buffer."))
    (when (vm-mail-mode-get-header-contents "MIME-Version:")
      (error "Message is already MIME encoded."))
    (let ((8bit nil)
	  (multipart t)		        ; start off asuming multipart
	  (boundary-positions nil)	; position markers for the parts
	  text-result			; results from text encodings
	  forward-local-refs already-mimed layout e e-list boundary
	  type encoding charset params description disposition object
	  opoint-min encoded-attachment)
      (when vm-xemacs-p
	;;Make sure we don't double encode UTF-8 (for example) text.
	(setq buffer-file-coding-system (vm-binary-coding-system)))
      (goto-char (mail-text-start))
      (setq e-list (vm-mime-attachment-button-extents 
		    (point) (point-max) 'vm-mime-object))
      ;; We have a multipart message unless there's just one
      ;; attachment and no other readable text in the buffer.
      (when (and (= (length e-list) 1)
		 (looking-at "[ \t\n]*")
		 (= (match-end 0)
		    (vm-extent-start-position (car e-list)))
		 (save-excursion
		   (goto-char (vm-extent-end-position (car e-list)))
		   (looking-at "[ \t\n]*\\'")))
	(setq multipart nil))
      ;; 1. Insert the text parts and attachments
      (if (null e-list)
	  ;; no attachments
	  (vm-mime-encode-text-part (point) (point-max) t)
	;; attachments to be handled
	(while e-list
	  (setq e (car e-list))
	  ;; 1a. Insert the text part
	  (if (or (not multipart)
		  (save-excursion
		    (eq (vm-extent-start-position e)
			(re-search-forward 
			 "[ \t\n]*" (vm-extent-start-position e) t))))
	      ;; found an attachment
	      (delete-region (point) (vm-extent-start-position e))
	    ;; found text
	    (setq text-result 
		  (vm-mime-encode-text-part
		   (point) (vm-extent-start-position e) nil))
	    (setq boundary-positions 
		  (cons (car text-result) boundary-positions))
	    (setq 8bit (or 8bit (equal (cdr text-result) "8bit"))))

	  ;; 1b. Prepare for the object
	  (goto-char (vm-extent-start-position e))
	  (narrow-to-region (point) (point))
	  (setq object (vm-extent-property e 'vm-mime-object))

	  ;; 1c. Insert the object
	  (cond ((bufferp object)
		 (vm-mime-insert-buffer-substring 
		  object (vm-extent-property e 'vm-mime-type)))
		;; insert attachment from another folder
		((listp object)
		 (save-restriction
		   (with-current-buffer (nth 0 object)
		     (widen))
		   (setq boundary-positions 
			 (cons (point-marker) boundary-positions))
		   (insert-buffer-substring 
		    (nth 0 object) (nth 1 object) (nth 2 object))
		   (setq encoded-attachment t)))
		;; insert file
		((stringp object)
		 (vm-mime-insert-file-contents 
		  object (vm-extent-property e 'vm-mime-type))))

	  ;; 1d. Gather information about the object from the extent.
	  (if (setq already-mimed (vm-extent-property e 'vm-mime-encoded))
	      (setq layout 
		    (vm-mime-parse-entity
		     nil :default-type (list "text/plain" "charset=us-ascii")
		     :default-encoding "7bit")
		    type (or (vm-extent-property e 'vm-mime-type)
			     (car (vm-mm-layout-type layout)))
		    params (or (vm-extent-property e 'vm-mime-parameters)
			       (cdr (vm-mm-layout-qtype layout)))
		    forward-local-refs
		        (car (vm-extent-property e 'vm-mime-forward-local-refs))
		    description (vm-extent-property e 'vm-mime-description)
		    disposition
		    (if (not (equal
			      (car (vm-extent-property e 'vm-mime-disposition))
			      "unspecified"))
			(vm-extent-property e 'vm-mime-disposition)
		      (vm-mm-layout-qdisposition layout)))
	    (setq type (vm-extent-property e 'vm-mime-type)
		  params (vm-extent-property e 'vm-mime-parameters)
		  forward-local-refs
		      (car (vm-extent-property e 'vm-mime-forward-local-refs))
		  description (vm-extent-property e 'vm-mime-description)
		  disposition
		  (if (not (equal
			    (car (vm-extent-property e 'vm-mime-disposition))
			    "unspecified"))
		      (vm-extent-property e 'vm-mime-disposition)
		    nil)))
	  ;; 1e. Encode the object if necessary
	  (cond ((vm-mime-types-match "text" type)
		 (setq encoding
		       (or (vm-extent-property e 'vm-mime-encoding)
			   (vm-determine-proper-content-transfer-encoding
			    (if already-mimed
				(vm-mm-layout-body-start layout)
			      (point-min))
			    (point-max)))
		       encoding (vm-mime-transfer-encode-region
				 encoding
				 (if already-mimed
				     (vm-mm-layout-body-start layout)
				   (point-min))
				 (point-max)
				 t))
		 (setq 8bit (or 8bit (equal encoding "8bit"))))

		((vm-mime-composite-type-p type)
		 (setq opoint-min (point-min))
		 (unless already-mimed
		   (goto-char (point-min))
		   (insert "Content-Type: " type "\n")
		   ;; vm-mime-transfer-encode-layout will replace
		   ;; this if the transfer encoding changes.
		   (insert "Content-Transfer-Encoding: 7bit\n\n")
		   (setq layout 
			 (vm-mime-parse-entity
			  nil 
			  :default-type (list "text/plain" "charset=us-ascii")
			  :default-encoding "7bit"))
		   (setq already-mimed t))
		 (when (and layout (not forward-local-refs))
		   (vm-mime-internalize-local-external-bodies layout)
		   ; update the cached data for the new layout
		   (setq type (car (vm-mm-layout-type layout))
			 params (cdr (vm-mm-layout-qtype layout))
			 disposition (vm-mm-layout-qdisposition layout)))
		 (setq encoding (vm-mime-transfer-encode-layout layout))
		 (setq 8bit (or 8bit (equal encoding "8bit")))
		 (goto-char (point-max))
		 (widen)
		 (narrow-to-region opoint-min (point)))

		((not encoded-attachment)
		 (when (and layout (not forward-local-refs))
		   (vm-mime-internalize-local-external-bodies layout)
		   ; update the cached data that might now be stale
		   ; but retain the disposition if nothing new
		   (setq type (car (vm-mm-layout-type layout))
			 params (cdr (vm-mm-layout-qtype layout))
			 disposition (or (vm-mm-layout-qdisposition layout)
					 disposition)))
		 (if already-mimed
		     (setq encoding (vm-mime-transfer-encode-layout layout))
		   (vm-mime-base64-encode-region (point-min) (point-max))
		   (setq encoding "base64"))))

	  ;; 1f. Add the required MIME headers
	  (unless (or (not multipart) encoded-attachment)
	    (goto-char (point-min))
	    (setq boundary-positions (cons (point-marker) boundary-positions))
	    (when already-mimed
	      ;; trim headers
	      (vm-reorder-message-headers 
	       nil :keep-list '("Content-ID:") :discard-regexp nil)
	      ;; remove header/text separator
	      (goto-char (1- (vm-mm-layout-body-start layout)))
	      (when (looking-at "\n")
		(delete-char 1)))
	    (insert "Content-Type: " 
		    (vm-mime-type-with-params type params)
		    "\n")
	    (when description
	      (insert "Content-Description: " description "\n"))
	    (when disposition
	      (insert "Content-Disposition: "
		      (vm-mime-type-with-params 
		       (car disposition) (cdr disposition))
		      "\n"))
	    (insert "Content-Transfer-Encoding: " encoding "\n\n"))
	  (goto-char (point-max))
	  (widen)

	  ;; 1g. Delete the original attachment button
	  (save-excursion
	    (goto-char (vm-extent-start-position e))
	    (vm-assert (looking-at "\\[ATTACHMENT")))
	  (delete-region (vm-extent-start-position e)
			 (vm-extent-end-position e))
	  (vm-detach-extent e)
	  (when (looking-at "\n") (delete-char 1))
	  (setq e-list (cdr e-list)))

	;; 2. Handle the remaining chunk of text after the last
	;; extent, if any.
	(if (and multipart (not (looking-at "[ \t\n]*\\'")))
	    (progn
	      (setq text-result 
		    (vm-mime-encode-text-part (point) (point-max) nil))
	      (setq boundary-positions 
		    (cons (car text-result) boundary-positions))
	      (setq 8bit (or 8bit (equal (cdr text-result) "8bit")))
	      (goto-char (point-max)))
	  (delete-region (point) (point-max)))

	;; 3. Create and insert boundary lines
	(when multipart 
	  (setq boundary (vm-mime-make-multipart-boundary))
	  (mail-text)
	  (while (re-search-forward 
		  (concat "^--" (regexp-quote boundary) "\\(--\\)?$")
		  nil t)
	    (setq boundary (vm-mime-make-multipart-boundary))
	    (mail-text))
	  (goto-char (point-max))
	  (insert "\n--" boundary "--\n")
	  (while boundary-positions
	    (goto-char (car boundary-positions))
	    (insert "\n--" boundary "\n")
	    (setq boundary-positions (cdr boundary-positions))))

	;; 4. Add MIME headers to the message
	(when (and (not multipart) already-mimed)
	  (goto-char (vm-mm-layout-header-start layout))
	  ;; trim headers
	  (vm-reorder-message-headers
	   nil :keep-list '("Content-ID:") :discard-regexp nil)
	  ;; remove header/text separator
	  (goto-char (vm-mm-layout-header-end layout))
	  (when (looking-at "\n") (delete-char 1))
	  ;; copy remainder to enclosing entity's header section
	  (goto-char (point-max))
	  (when multipart
	    (insert-buffer-substring (current-buffer)
				     (vm-mm-layout-header-start layout)
				     (vm-mm-layout-body-start layout)))
	  (delete-region (vm-mm-layout-header-start layout)
			 (vm-mm-layout-body-start layout)))
	(goto-char (point-min))
	(vm-remove-mail-mode-header-separator)
	(vm-reorder-message-headers
	 nil :keep-list nil 
	 :discard-regexp
	 "\\(Content-Type:\\|MIME-Version:\\|Content-Transfer-Encoding\\)")
	(vm-add-mail-mode-header-separator)
	(insert "MIME-Version: 1.0\n")
	(if multipart
	    (progn
	      (insert "Content-Type: "
		      (vm-mime-type-with-params 
		       "multipart/mixed" 
		       (list (format "boundary=\"%s\"" boundary)))
		      "\n")
	      (insert "Content-Transfer-Encoding: "
		      (if 8bit "8bit" "7bit") "\n"))
	  (insert "Content-Type: " (vm-mime-type-with-params type params) "\n")
	  (when disposition
	    (insert "Content-Disposition: " 
		    (vm-mime-type-with-params 
		     (car disposition) (cdr disposition))
		    "\n"))
	  (when description
	    (insert "Content-Description: " description "\n"))
	  (insert "Content-Transfer-Encoding: " encoding "\n"))))))

(defun vm-mime-encode-text-part (beg end whole-message)
  "Encode the text from BEG to END in a composition buffer
as MIME part and add appropriate MIME headers.  If WHOLE-MESSAGE is
true, then encode it as the entire message.

Returns a pair consisting of a marker pointing to the start of the
encoded MIME part and the transfer-encoding used.  But if
WHOLE-MESSAGE is true then nil is returned."
  (let ((enriched (and (boundp 'enriched-mode) enriched-mode))
	type encoding charset params description marker)
    (narrow-to-region beg end)
    ;; support enriched-mode for text/enriched composition
    (when enriched
      (let ((enriched-initial-annotation ""))
	(if vm-fsfemacs-p
	    (save-excursion
	      ;; insert/delete trick needed to avoid
	      ;; enriched-mode tags from seeping into the
	      ;; attachment overlays.  I really wish
	      ;; front-advance / rear-advance overlay
	      ;; endpoint properties actually worked.
	      (goto-char (point-max))
	      (insert-before-markers "\n")
	      (enriched-encode (point-min) (1- (point)))
	      (goto-char (point-max))
	      (delete-char -1))
	  (enriched-encode (point-min) (point-max)))))
            
    (setq charset (vm-determine-proper-charset (point-min) (point-max)))
    (when (vm-emacs-mule-p)
      (let ((coding-system
	     (vm-mime-charset-to-coding charset)))
	(unless coding-system
	  (error "Can't find a coding system for charset %s" charset))
	(encode-coding-region (point-min) (point-max) 
	     ;; What about the case where vm-m-m-c-t-c-a doesn't have an
	     ;; entry for the given charset? That shouldn't happen, if
	     ;; vm-mime-mule-coding-to-charset-alist and
	     ;; vm-mime-mule-charset-to-coding-alist have complete and
	     ;; matching entries. Admittedly this last is not a
	     ;; given. Should we make it so on startup? (By setting the
	     ;; key for any missing entries in
	     ;; vm-mime-mule-coding-to-charset-alist to being (format "%s"
	     ;; coding-system), if necessary.)        RWF, 2005-03-25
			      coding-system)))

    ;; not clear why this is needed.  USR, 2011-03-27
    (when vm-xemacs-p
      (when whole-message (enriched-mode -1)))
    (setq encoding (vm-determine-proper-content-transfer-encoding
		    (point-min) (point-max))
	  encoding (vm-mime-transfer-encode-region 
		    encoding (point-min) (point-max) t)
	  description (vm-mime-text-description 
		       (point-min) (point-max)))
    (if whole-message
	(progn
	  (widen)
	  (vm-remove-mail-mode-header-separator)
	  (goto-char (point-min))
	  (vm-reorder-message-headers
	   nil :keep-list nil 
	   :discard-regexp
	   "\\(Content-Type:\\|Content-Transfer-Encoding\\|MIME-Version:\\)")
	  (insert "MIME-Version: 1.0\n")
	  (if enriched
	      (insert "Content-Type: text/enriched; charset=" charset "\n")
	    (insert "Content-Type: text/plain; charset=" charset "\n"))
	  (insert "Content-Transfer-Encoding: " encoding "\n")
	  (vm-add-mail-mode-header-separator)
	  nil)

      (setq marker (point-marker))
      (if enriched
	  (insert "Content-Type: text/enriched; charset=" charset "\n")
	(insert "Content-Type: text/plain; charset=" charset "\n"))
      (when description
	(insert "Content-Description: " description "\n"))
      (insert "Content-Transfer-Encoding: " encoding "\n\n")
      (widen)
      (cons marker encoding))))


;; This function is now defunct.   Use vm-mime-encode-composition.
;; USR, 2011-03-27
(defun vm-mime-fsfemacs-encode-composition ()
  "MIME encode the message composition in the current buffer."
  (save-restriction
    (widen)
    (unless (eq major-mode 'mail-mode)
      (error "Command must be used in a VM Mail mode buffer."))
    (when (vm-mail-mode-get-header-contents "MIME-Version:")
      (error "Message is already MIME encoded."))
    (let ((8bit nil)
	  (just-one nil)
	  (boundary-positions nil)	; markers for the start of parts
	  marker
	  forward-local-refs already-mimed layout e e-list boundary
	  type encoding charset params description disposition object
	  opoint-min postponed-attachment)
      (goto-char (mail-text-start))
      (setq e-list (vm-mime-attachment-button-extents 
		    (point) (point-max) 'vm-mime-object))
      ;; If there's just one attachment and no other readable
      ;; text in the buffer then make the message type just be
      ;; the attachment type rather than sending a multipart
      ;; message with one attachment
      (setq just-one (and (= (length e-list) 1)
			  (looking-at "[ \t\n]*")
			  (= (match-end 0)
			     (vm-extent-start-position (car e-list)))
			  (save-excursion
			    (goto-char (vm-extent-end-position (car e-list)))
			    (looking-at "[ \t\n]*\\'"))))
      (if (null e-list)
	  ;; no attachments
	  (vm-mime-encode-text-part (point) (point-max) t)
	;; attachments to be handled
	(while e-list
	  (setq e (car e-list))
	  (if (or just-one
		  (save-excursion
		    (eq (vm-extent-start-position e)
			(re-search-forward 
			 "[ \t\n]*" (vm-extent-start-position e) t))))
	      ;; found an attachment
	      (delete-region (point) (vm-extent-start-position e))
	    ;; found text
	    (setq marker (vm-mime-encode-text-part
			  (point) (vm-extent-start-position e) nil))
	    (setq boundary-positions (cons marker boundary-positions)))
	  (goto-char (vm-extent-start-position e))
	  (narrow-to-region (point) (point))
	  (setq object (vm-extent-property e 'vm-mime-object))

	  ;; insert the object
	  (cond ((bufferp object)
		 (vm-mime-insert-buffer-substring 
		  object (vm-extent-property e 'vm-mime-type)))
		;; insert attachment from another folder
		((listp object)
		 (save-restriction
		   (with-current-buffer (nth 0 object)
		     (widen))
		   (setq boundary-positions 
			 (cons (point-marker) boundary-positions))
		   (insert-buffer-substring 
		    (nth 0 object) (nth 1 object) (nth 2 object))
		   (setq postponed-attachment t)))
		;; insert file
		((stringp object)
		 (vm-mime-insert-file-contents 
		  object (vm-extent-property e 'vm-mime-type))))
	  ;; gather information about the object from the extent.
	  (if (setq already-mimed (vm-extent-property e 'vm-mime-encoded))
	      (setq layout 
		    (vm-mime-parse-entity
		     nil :default-type (list "text/plain" "charset=us-ascii")
		     :default-encoding "7bit")
		    type (or (vm-extent-property e 'vm-mime-type)
			     (car (vm-mm-layout-type layout)))
		    params (or (vm-extent-property e 'vm-mime-parameters)
			       (cdr (vm-mm-layout-qtype layout)))
		    forward-local-refs
		        (car (vm-extent-property e 'vm-mime-forward-local-refs))
		    description (vm-extent-property e 'vm-mime-description)
		    disposition
		    (if (not (equal
			      (car (vm-extent-property e 'vm-mime-disposition))
			      "unspecified"))
			(vm-extent-property e 'vm-mime-disposition)
		      (vm-mm-layout-qdisposition layout)))
	    (setq type (vm-extent-property e 'vm-mime-type)
		  params (vm-extent-property e 'vm-mime-parameters)
		  forward-local-refs
		      (car (vm-extent-property e 'vm-mime-forward-local-refs))
		  description (vm-extent-property e 'vm-mime-description)
		  disposition
		  (if (not (equal
			    (car (vm-extent-property e 'vm-mime-disposition))
			    "unspecified"))
		      (vm-extent-property e 'vm-mime-disposition)
		    nil)))
	  (cond ((vm-mime-types-match "text" type)
		 (setq encoding
		       (or (vm-extent-property e 'vm-mime-encoding)
			   (vm-determine-proper-content-transfer-encoding
			    (if already-mimed
				(vm-mm-layout-body-start layout)
			      (point-min))
			    (point-max)))
		       encoding (vm-mime-transfer-encode-region
				 encoding
				 (if already-mimed
				     (vm-mm-layout-body-start layout)
				   (point-min))
				 (point-max)
				 t))
		 (setq 8bit (or 8bit (equal encoding "8bit"))))
		((vm-mime-composite-type-p type)
		 (setq opoint-min (point-min))
		 (unless already-mimed
		   (goto-char (point-min))
		   (insert "Content-Type: " type "\n")
		   ;; vm-mime-transfer-encode-layout will replace
		   ;; this if the transfer encoding changes.
		   (insert "Content-Transfer-Encoding: 7bit\n\n")
		   (setq layout 
			 (vm-mime-parse-entity
			  nil 
			  :default-type (list "text/plain" "charset=us-ascii")
			  :default-encoding "7bit"))
		   (setq already-mimed t))
		 (when (and layout (not forward-local-refs))
		   (vm-mime-internalize-local-external-bodies layout)
		   ; update the cached data that might now be stale
		   (setq type (car (vm-mm-layout-type layout))
			 params (cdr (vm-mm-layout-qtype layout))
			 disposition (vm-mm-layout-qdisposition layout)))
		 (setq encoding (vm-mime-transfer-encode-layout layout))
		 (setq 8bit (or 8bit (equal encoding "8bit")))
		 (goto-char (point-max))
		 (widen)
		 (narrow-to-region opoint-min (point)))
		((not postponed-attachment)
		 (when (and layout (not forward-local-refs))
		   (vm-mime-internalize-local-external-bodies layout)
		   ; update the cached data that might now be stale
		   (setq type (car (vm-mm-layout-type layout))
			 params (cdr (vm-mm-layout-qtype layout))
			 disposition (vm-mm-layout-qdisposition layout)))
		 (if already-mimed
		     (setq encoding (vm-mime-transfer-encode-layout layout))
		   (vm-mime-base64-encode-region (point-min) (point-max))
		   (setq encoding "base64"))))
	  (unless (or just-one postponed-attachment)
	    (goto-char (point-min))
	    (setq boundary-positions (cons (point-marker) boundary-positions))
	    (when already-mimed
	      ;; trim headers - why remove perfectly good headers?  USR
	      (vm-reorder-message-headers 
	       nil :keep-list '("Content-ID:") :discard-regexp nil)
	      ;; remove header/text separator
	      (goto-char (1- (vm-mm-layout-body-start layout)))
	      (when (looking-at "\n")
		(delete-char 1)))
	    (insert "Content-Type: " 
		    (vm-mime-type-with-params type params)
		    "\n")
	    (when description
	      (insert "Content-Description: " description "\n"))
	    (when disposition
	      (insert "Content-Disposition: " (car disposition))
	      (when (cdr disposition)
		(insert ";\n\t" (mapconcat 'identity
					   (cdr disposition)
					   ";\n\t")))
	      (insert "\n"))
	    (insert "Content-Transfer-Encoding: " encoding "\n\n"))
	  (goto-char (point-max))
	  (widen)
	  (save-excursion
	    (goto-char (vm-extent-start-position e))
	    (vm-assert (looking-at "\\[ATTACHMENT")))
	  (delete-region (vm-extent-start-position e)
			 (vm-extent-end-position e))
	  (vm-detach-extent e)
	  (if (looking-at "\n")
	      (delete-char 1))
	  (setq e-list (cdr e-list)))
	;; handle the remaining chunk of text after the last
	;; extent, if any.
	(if (or just-one (looking-at "[ \t\n]*\\'"))
	    (delete-region (point) (point-max))
	  (setq marker (vm-mime-encode-text-part (point) (point-max) nil))
	  (setq boundary-positions (cons marker boundary-positions))
	  ;; FIXME is this needed?
	  ;; (setq 8bit (or 8bit (equal encoding "8bit")))
	  (goto-char (point-max)))
	(setq boundary (vm-mime-make-multipart-boundary))
	(mail-text)
	(while (re-search-forward (concat "^--"
					  (regexp-quote boundary)
					  "\\(--\\)?$")
				  nil t)
	  (setq boundary (vm-mime-make-multipart-boundary))
	  (mail-text))
	(goto-char (point-max))
	(or just-one (insert "\n--" boundary "--\n"))
	(while boundary-positions
	  (goto-char (car boundary-positions))
	  (insert "\n--" boundary "\n")
	  (setq boundary-positions (cdr boundary-positions)))
	(when (and just-one already-mimed)
	  (goto-char (vm-mm-layout-header-start layout))
	  ;; trim headers
	  (vm-reorder-message-headers
	   nil :keep-list '("Content-ID:") :discard-regexp nil)
	  ;; remove header/text separator
	  (goto-char (vm-mm-layout-header-end layout))
	  (if (looking-at "\n")
	      (delete-char 1))
	  ;; copy remainder to enclosing entity's header section
	  (goto-char (point-max))
	  (unless just-one
	    (insert-buffer-substring (current-buffer)
				     (vm-mm-layout-header-start layout)
				     (vm-mm-layout-body-start layout)))
	  (delete-region (vm-mm-layout-header-start layout)
			 (vm-mm-layout-body-start layout)))
	(goto-char (point-min))
	(vm-remove-mail-mode-header-separator)
	(vm-reorder-message-headers
	 nil :keep-list nil 
	 :discard-regexp
	 "\\(Content-Type:\\|MIME-Version:\\|Content-Transfer-Encoding\\)")
	(vm-add-mail-mode-header-separator)
	(insert "MIME-Version: 1.0\n")
	(if just-one
	    (insert "Content-Type: " 
		    (vm-mime-type-with-params type params)
		    "\n")
	  (insert "Content-Type: "
		  (vm-mime-type-with-params 
		   "multipart/mixed"
		   (list (concat "boundary=\"" boundary "\"")))
		  "\n"))
	(when (and just-one description)
	    (insert "Content-Description: " description "\n"))
	(when (and just-one disposition)
	  (insert "Content-Disposition: " 
		  (vm-mime-type-with-params (car disposition) (cdr disposition))
		  "\n"))
	(if just-one
	    (insert "Content-Transfer-Encoding: " encoding "\n")
	  (if 8bit
	      (insert "Content-Transfer-Encoding: 8bit\n")
	    (insert "Content-Transfer-Encoding: 7bit\n")))))))
(make-obsolete 'vm-mime-fsfemacs-encode-composition
	       'vm-mime-encode-composition-internal "8.2.0")

(defun vm-mime-fsfemacs-encode-text-part (beg end whole-message)
  "Encode the text from BEG to END in a composition buffer
as MIME part and add appropriate MIME headers.  If WHOLE-MESSAGE is
true, then encode it as the entire message.

Returns marker pointing to the start of the encoded MIME part."
  (let ((enriched (and (boundp 'enriched-mode) enriched-mode))
	type encoding charset params description marker)
    (narrow-to-region beg end)
    ;; support enriched-mode for text/enriched composition
    (when enriched
      (let ((enriched-initial-annotation ""))
	(save-excursion
	  ;; insert/delete trick needed to avoid
	  ;; enriched-mode tags from seeping into the
	  ;; attachment overlays.  I really wish
	  ;; front-advance / rear-advance overlay
	  ;; endpoint properties actually worked.
	  (goto-char (point-max))
	  (insert-before-markers "\n")
	  (enriched-encode (point-min) (1- (point)))
	  (goto-char (point-max))
	  (delete-char -1))))

    (setq charset (vm-determine-proper-charset (point-min) (point-max)))
    (when vm-fsfemacs-mule-p
      (let ((coding-system
	     (vm-mime-charset-to-coding charset)))
	(unless coding-system
	  (error "Can't find a coding system for charset %s" charset))
	(encode-coding-region (point-min) (point-max) coding-system)))

    (setq encoding (vm-determine-proper-content-transfer-encoding
		    (point-min) (point-max))
	  encoding (vm-mime-transfer-encode-region 
		    encoding (point-min) (point-max) t)
	  description (vm-mime-text-description 
		       (point-min) (point-max)))
    (if whole-message
	(progn
	  (widen)
	  (vm-remove-mail-mode-header-separator)
	  (goto-char (point-min))
	  (vm-reorder-message-headers
	   nil :keep-list nil 
	   :discard-regexp
	   "\\(Content-Type:\\|Content-Transfer-Encoding\\|MIME-Version:\\)")
	  (insert "MIME-Version: 1.0\n")
	  (if enriched
	      (insert "Content-Type: text/enriched; charset=" charset "\n")
	    (insert "Content-Type: text/plain; charset=" charset "\n"))
	  (insert "Content-Transfer-Encoding: " encoding "\n")
	  (vm-add-mail-mode-header-separator))

      (setq marker (point-marker))
      (if enriched
	  (insert "Content-Type: text/enriched; charset=" charset "\n")
	(insert "Content-Type: text/plain; charset=" charset "\n"))
      (when description
	(insert "Content-Description: " description "\n"))
      (insert "Content-Transfer-Encoding: " encoding "\n\n")
      (widen)
      marker)))
(make-obsolete 'vm-mime-fsfemacs-encode-text-part
	       'vm-mime-encode-text-part "8.2.0")


(defun vm-mime-fragment-composition (size)
  (save-restriction
    (widen)
    (vm-inform 5 "Fragmenting message...")
    (let ((buffers nil)
	  (total-markers nil)
	  (id (vm-mime-make-multipart-boundary))
	  (n 1)
	  b header-start header-end master-buffer start end)
      (vm-remove-mail-mode-header-separator)
      ;; message/partial must have "7bit" content transfer
      ;; encoding, so force everything to be encoded for
      ;; 7bit transmission.
      (let ((vm-mime-8bit-text-transfer-encoding
	     (if (eq vm-mime-8bit-text-transfer-encoding '8bit)
		 'quoted-printable
	       vm-mime-8bit-text-transfer-encoding)))
	(vm-mime-transfer-encode-layout
	 (vm-mime-parse-entity
	  nil 
	  :default-type (list "text/plain" "charset=us-ascii")
	  :default-encoding "7bit")))
      (goto-char (point-min))
      (setq header-start (point))
      (search-forward "\n\n")
      (setq header-end (1- (point)))
      (setq master-buffer (current-buffer))
      (goto-char (point-min))
      (setq start (point))
      (while (not (eobp))
	(condition-case nil
	    (progn
	      (forward-char (max (- size 150) 2000))
	      (beginning-of-line))
	  (end-of-buffer nil))
	(setq end (point))
	(setq b (generate-new-buffer (concat (buffer-name) " part "
					     (int-to-string n))))
	(setq buffers (cons b buffers))
	(set-buffer b)
	(make-local-variable 'vm-send-using-mime)
	(setq vm-send-using-mime nil)
	(insert-buffer-substring master-buffer header-start header-end)
	(goto-char (point-min))
	(vm-reorder-message-headers 
	 nil :keep-list nil
	 :discard-regedp
         "\\(Content-Type:\\|MIME-Version:\\|Content-Transfer-Encoding\\)")
	(insert "MIME-Version: 1.0\n")
	(insert (format
		 (if vm-mime-avoid-folding-content-type
		     "Content-Type: message/partial; id=%s; number=%d"
		   "Content-Type: message/partial;\n\tid=%s;\n\tnumber=%d")
		 id n))
	(if vm-mime-avoid-folding-content-type
	    (insert (format "; total=%d" n))
	  (insert (format ";\n\ttotal=%d" n)))
	(setq total-markers (cons (point) total-markers))
	(insert "\nContent-Transfer-Encoding: 7bit\n")
	(goto-char (point-max))
	(insert mail-header-separator "\n")
	(insert-buffer-substring master-buffer start end)
	(vm-increment n)
	(set-buffer master-buffer)
	(setq start (point)))
      (vm-decrement n)
      (vm-add-mail-mode-header-separator)
      (let ((bufs buffers))
	(while bufs
	  (set-buffer (car bufs))
	  (goto-char (car total-markers))
	  (prin1 n (current-buffer))
	  (setq bufs (cdr bufs)
		total-markers (cdr total-markers)))
	(set-buffer master-buffer))
      (vm-inform 5 "Fragmenting message... done")
      (nreverse buffers))))

;; moved to vm-reply.el, not MIME-specific.
(fset 'vm-mime-preview-composition 'vm-preview-composition)

(defun vm-mime-composite-type-p (type)
  "Check if TYPE is a MIME type that might have subparts."
  (or (vm-mime-types-match "message/rfc822" type)
      (vm-mime-types-match "message/news" type)
      (vm-mime-types-match "multipart" type)))

;; Unused currrently.
;;
;;(defun vm-mime-map-atomic-layouts (function list)
;;  (while list
;;    (if (vm-mime-composite-type-p (car (vm-mm-layout-type (car list))))
;;	(vm-mime-map-atomic-layouts function (vm-mm-layout-parts (car list)))
;;      (funcall function (car list)))
;;    (setq list (cdr list))))

(defun vm-mime-sprintf (format layout)
  ;; compile the format into an eval'able s-expression
  ;; if it hasn't been compiled already.
  (let ((match (assoc format vm-mime-compiled-format-alist)))
    (if (null match)
	(progn
	  (vm-mime-compile-format format)
	  (setq match (assoc format vm-mime-compiled-format-alist))))
    ;; The local variable name `vm-mime-layout' is mandatory here for
    ;; the format s-expression to work.
    (let ((vm-mime-layout layout))
      (eval (cdr match)))))

(defun vm-mime-compile-format (format)
  (let ((return-value (vm-mime-compile-format-1 format 0)))
    (setq vm-mime-compiled-format-alist
	  (cons (cons format (nth 1 return-value))
		vm-mime-compiled-format-alist))))

(defun vm-mime-compile-format-1 (format start-index)
  (or start-index (setq start-index 0))
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
		"%\\(-\\)?\\([0-9]+\\)?\\(\\.\\(-?[0-9]+\\)\\)?\\([()acdefknNstTx%]\\)"
		format last-match-end))
	(setq conv-spec (aref format (match-beginning 5)))
	(setq new-match-end (match-end 0))
	(if (memq conv-spec '(?\( ?a ?c ?d ?e ?f ?k ?n ?N ?s ?t ?T ?x))
	    (progn
	      (cond ((= conv-spec ?\()
		     (save-match-data
		       (let ((retval (vm-mime-compile-format-1 format
							       (match-end 5))))
			 (setq sexp (cons (nth 1 retval) sexp)
			       new-match-end (car retval)))))
		    ((= conv-spec ?a)
		     (setq sexp (cons (list 'vm-mf-default-action
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?c)
		     (setq sexp (cons (list 'vm-mf-text-charset
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?d)
		     (setq sexp (cons (list 'vm-mf-content-description
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?e)
		     (setq sexp (cons (list 'vm-mf-content-transfer-encoding
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?f)
		     (setq sexp (cons (list 'vm-mf-attachment-file
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?k)
		     (setq sexp (cons (list 'vm-mf-event-for-default-action
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?n)
		     (setq sexp (cons (list 'vm-mf-parts-count
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?N)
		     (setq sexp (cons (list 'vm-mf-partial-number
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?s)
		     (setq sexp (cons (list 'vm-mf-parts-count-pluralizer
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?t)
		     (setq sexp (cons (list 'vm-mf-content-type
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?T)
		     (setq sexp (cons (list 'vm-mf-partial-total
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?x)
		     (setq sexp (cons (list 'vm-mf-external-body-content-type
					    'vm-mime-layout) sexp))))
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
      (unless done
	(setq sexp-fmt
	      (cons (substring format last-match-end (length format))
		    sexp-fmt)
	      done t))
      (setq sexp-fmt (apply 'concat (nreverse sexp-fmt)))
      (if sexp
	  (setq sexp (cons 'format (cons sexp-fmt (nreverse sexp))))
	(setq sexp sexp-fmt)))
    (list last-match-end sexp)))

(defun vm-mime-find-format-for-layout (layout)
  (let ((p vm-mime-button-format-alist)
	(type (car (vm-mm-layout-type layout))))
    (catch 'done
      (cond ((vm-mime-types-match "error/error" type)
	     (throw 'done "%d"))
	    ((vm-mime-types-match "text/x-vm-deleted" type)
	     (throw 'done "%d")))
      (while p
	(if (vm-mime-types-match (car (car p)) type)
	    (throw 'done (cdr (car p)))
	  (setq p (cdr p))))
      "%-25.25t [%k to %a]" )))

(defun vm-mf-content-type (layout)
  (car (vm-mm-layout-type layout)))

(defun vm-mf-external-body-content-type (layout)
  (car (vm-mm-layout-type (car (vm-mm-layout-parts layout)))))

(defun vm-mf-content-transfer-encoding (layout)
  (vm-mm-layout-encoding layout))

(defun vm-mf-content-description (layout)
  (or (vm-mm-layout-description layout)
      (let ((p vm-mime-type-description-alist)
	    (type (car (vm-mm-layout-type layout))))
	(catch 'done
	  (while p
	    (if (vm-mime-types-match (car (car p)) type)
		(throw 'done (cdr (car p)))
	      (setq p (cdr p))))
	  nil ))
      (vm-mf-content-type layout)))

(defun vm-mf-text-charset (layout)
  (or (vm-mime-get-parameter layout "charset")
      "us-ascii"))

(defun vm-mf-parts-count (layout)
  (int-to-string (length (vm-mm-layout-parts layout))))

(defun vm-mf-parts-count-pluralizer (layout)
  (if (= 1 (length (vm-mm-layout-parts layout))) "" "s"))

(defun vm-mf-partial-number (layout)
  (or (vm-mime-get-parameter layout "number")
      "?"))

(defun vm-mf-partial-total (layout)
  (or (vm-mime-get-parameter layout "total")
      "?"))

(defun vm-mf-attachment-file (layout)
  (or vm-mf-attachment-file ;; for %f expansion in external viewer arg lists
      (vm-mime-get-disposition-filename layout)
      (vm-mime-get-parameter layout "name")
      "<no suggested filename>"))

(defun vm-mf-event-for-default-action (layout)
  (if (vm-mouse-support-possible-here-p)
      "Click mouse-2"
    "Press RETURN"))

(defun vm-mf-default-action (layout)
  (if (eq vm-mime-alternative-select-method 'all)
      (concat (vm-mf-default-action-orig layout) " alternative")
    (vm-mf-default-action-orig layout)))

(defun vm-mf-default-action-orig (layout)
  (or vm-mf-default-action
      (let (cons)
        (cond ((or (vm-mime-can-display-internal layout)
		   (vm-mime-find-external-viewer
		    (car (vm-mm-layout-type layout))))
	       (let ((p vm-mime-default-action-string-alist)
		     (type (car (vm-mm-layout-type layout))))
		 (catch 'done
		   (while p
		     (if (vm-mime-types-match (car (car p)) type)
			 (throw 'done (cdr (car p)))
		       (setq p (cdr p))))
		   nil )))
	      ((setq cons (vm-mime-can-convert
			   (car (vm-mm-layout-type layout))))
	       (format "display as %s" (nth 1 cons)))
	      (t "save to a file")))
      ;; should not be reached
      "burn in the raging fires of hell forever"))

(defun vm-mime-map-layout-parts (m function &optional layout path)
  "Apply FUNCTION to each part of the message M.
This function will call itself recursively with the currently processed LAYOUT
and the PATH to it.  PATH is a list of parent layouts where the root is at the
end of the path."
  (unless layout
    (setq layout (vm-mm-layout m)))
  (when (vectorp layout)
    (funcall function m layout path)
    (let ((parts (copy-sequence (vm-mm-layout-parts layout))))
      (while parts
        (vm-mime-map-layout-parts m function (car parts) (cons layout path))
        (setq parts (cdr parts))))))

(defun vm-list-mime-part-structure (&optional verbose)
  "List mime part structure of the current message."
  (interactive "P")
  (vm-check-for-killed-summary)
  (if (interactive-p) (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 0 (interactive-p))
  (let ((m (car vm-message-pointer)))
    (switch-to-buffer "*VM mime part layout*")
    (erase-buffer)
    ;; (setq truncate-lines t)
    (insert (format "%s\n" (vm-decode-mime-encoded-words-in-string
                            (vm-su-subject m))))
    (vm-mime-map-layout-parts
     m
     (lambda (m layout path)
       (if verbose
           (insert (format "%s%S\n" (make-string (length path) ? ) layout))
         (insert (format "%s%S%s%s%s\n" (make-string (length path) ? )
                         (vm-mm-layout-type layout)
                         (let ((id (vm-mm-layout-id layout)))
                           (if id (format " id=%S" id) ""))
                         (let ((desc (vm-mm-layout-description layout)))
                           (if desc (format " desc=%S" desc) ""))
                         (let ((dispo (vm-mm-layout-disposition layout)))
                           (if dispo (format " %S" dispo) "")))))))))
(defalias 'vm-mime-list-part-structure
  'vm-list-mime-part-structure)

;;;###autoload
(defun vm-nuke-alternative-text/html-internal (m)
  "Delete all text/html parts of multipart/alternative parts of message M.
Returns the number of deleted parts.  text/html parts are only deleted iff
the first sub part of a multipart/alternative is a text/plain part."
  (let ((deleted-count 0)
        prev-type this-type parent-types
        nuke-html)
    (vm-mime-map-layout-parts
     m
     (lambda (m layout path)
       (setq this-type (car (vm-mm-layout-type layout))
             parent-types (mapcar (lambda (layout)
                                    (car (vm-mm-layout-type layout)))
                                  path))
       (when (and nuke-html
                  (member "multipart/alternative" parent-types)
                  (vm-mime-types-match "text/html" this-type))
         (save-excursion
           (set-buffer (vm-buffer-of m))
           (let ((inhibit-read-only t)
                 (buffer-read-only nil))
             (vm-save-restriction
              (widen)
              (if (vm-mm-layout-is-converted layout)
                  (setq layout (vm-mm-layout-unconverted-layout layout)))
              (goto-char (vm-mm-layout-header-start layout))
              (forward-line -1)
              (delete-region (point) (vm-mm-layout-body-end layout))
              (vm-set-edited-flag-of m t)
              (vm-set-byte-count-of m nil)
              (vm-set-line-count-of m nil)
              (vm-set-stuff-flag-of m t)
              (vm-mark-for-summary-update m)))
           (setq deleted-count (1+ deleted-count))))
       (if (and (vm-mime-types-match "multipart/alternative" prev-type)
                (vm-mime-types-match "text/plain" this-type))
           (setq nuke-html t))
       (setq prev-type this-type)))
    deleted-count))

;;;###autoload
(defun vm-nuke-alternative-text/html (&optional count mlist)
  "Removes the text/html part of all multipart/alternative message parts.

This is a destructive operation and cannot be undone!"
  (interactive "p")
  (when (interactive-p)
    (vm-follow-summary-cursor))
  (vm-select-folder-buffer-and-validate 0 (interactive-p))
  (let ((mlist (or mlist 
		   (vm-select-operable-messages
		    count (interactive-p) "Nuke html of"))))
    (vm-retrieve-operable-messages count mlist)
    (save-excursion
      (while mlist
        (let ((count (vm-nuke-alternative-text/html-internal (car mlist))))
          (when (interactive-p)
            (if (= count 0)
                (vm-inform 5 "No text/html parts found.")
              (vm-inform 5 "%d text/html part%s deleted."
                       count (if (> count 1) "s" ""))))
          (setq mlist (cdr mlist))))))
  (when (interactive-p)
    (vm-discard-cached-data count)
    (vm-present-current-message)))
(defalias 'vm-mime-nuke-alternative-text/html
  'vm-nuke-alterantive-text/html)
(make-obsolete 'vm-mime-nuke-alternative-text/html
	       'vm-nuke-alternative-text/html "8.2.0")

;;-----------------------------------------------------------------------------
;; The following functions are taken from vm-pine.el
;; Copyright (C) Robert Widhopf-Fenk

;;;###autoload
(defun vm-mime-convert-to-attachment-buttons ()
  "Replace all mime buttons in the current buffer by attachment buttons."
  (interactive)
  (cond (vm-xemacs-p
         (let ((e-list (vm-extent-list 
			(point-min) (point-max) 'vm-mime-layout)))
           (setq e-list
                 (sort e-list
                       (function (lambda (e1 e2)
                                   (< (vm-extent-end-position e1)
                                      (vm-extent-end-position e2))))))
           ;; Then replace the buttons, because doing it at once will result in
           ;; problems since the new buttons are from the same extent.
           (while e-list
             (vm-mime-replace-by-attachment-button (car e-list))
             (setq e-list (cdr e-list)))))
        (vm-fsfemacs-p
         (let ((e-list (vm-mime-attachment-button-extents
			(point-min) (point-max) 'vm-mime-layout)))
           (while e-list
             (vm-mime-replace-by-attachment-button (car e-list))
             (setq e-list (cdr e-list)))
	   (goto-char (point-max))))
        (t
         (error "don't know how to MIME encode composition for %s"
                (emacs-version)))))

;; This function is now unused.  USR, 2011-02-14
;; (defun vm-mime-re-fake-attachment-overlays (start end)
;;   "For all MIME buttons in the region, create \"fake\" attachment
;; overlays, which are then used during MIME encoding of the
;; composition.  This function is similar to
;; `vm-mime-fake-attachment-overlays' and used only with FSF Emacs.
;; 						  USR, 2011-02-14"
;;   (let ((o-list nil)
;; 	(done nil)
;; 	(pos start)
;; 	object props o)
;;     (save-excursion
;;       (save-restriction
;; 	(narrow-to-region start end)
;; 	(while (not done)
;; 	  (setq object (get-text-property pos 'vm-mime-layout))
;; 	  (setq pos (next-single-property-change pos 'vm-mime-layout))
;; 	  (unless pos 
;; 	    (setq pos (point-max) 
;; 		  done t))
;; 	  (when object
;; 	    (setq o (make-overlay start pos))
;; 	    (overlay-put o 'insert-in-front-hooks
;; 			 '(vm-disallow-overlay-endpoint-insertion))
;; 	    (overlay-put o 'insert-behind-hooks
;; 			 '(vm-disallow-overlay-endpoint-insertion))
;; 	    (setq props (append (list 'vm-mime-object t)
;; 				(text-properties-at start)))
;; 	    (while props
;; 	      (overlay-put o (car props) (cadr props))
;; 	      (setq props (cddr props)))
;; 	    (setq o-list (cons o o-list)))
;; 	  (setq start pos))
;; 	o-list ))))

(defun vm-mime-replace-by-attachment-button (x)
  "Replace the MIME button specified by X by an attachment button."
  (save-excursion
    (let* ((layout (vm-extent-property x 'vm-mime-layout))
	   (xstart (vm-extent-start-position x))
	   (xend   (vm-extent-end-position x))
	   (start  (vm-mm-layout-header-start layout))
	   (end    (vm-mm-layout-body-end   layout))
	   (buf    (marker-buffer start))
	   (desc   (or (vm-mm-layout-description layout)
		       "message body text"))
	   (disp   (or (vm-mm-layout-disposition layout)
		       '("inline")))
	   (file   (vm-mime-get-disposition-parameter layout "filename"))
	   (filename nil)
	   (type   (vm-mm-layout-type layout)))

      ;; special case of message/external-body
      (when (and type
		 (string= (car type) "message/external-body")
		 (string= (cadr type) "access-type=local-file"))
	(save-excursion
	  (setq filename (substring (caddr type) 5))
	  (vm-select-folder-buffer)
	  (save-restriction
	    (let ((start (vm-mm-layout-body-start layout))
		  (end   (vm-mm-layout-body-end layout)))
	      (set-buffer (marker-buffer (vm-mm-layout-body-start layout)))
	      (widen)
	      (goto-char start)
	      (if (not (re-search-forward
			"Content-Type: \"?\\([^ ;\" \n\t]+\\)\"?;?"
			end t))
		  (error "No `Content-Type' header found in: %s"
			 (buffer-substring start end))
		(setq type (list (match-string 1))))))))
        
      ;; insert an attached-object-button
      (goto-char xstart)
      (cond (filename
	     (vm-attach-file filename (car type)))
	    (file
	     (vm-attach-object (list buf start end disp file) 
				    :type (car type) :params nil 
				    :description desc :mimed t))
	    (t
	     (vm-attach-object (list buf start end disp)
				    :type (car type) :params nil 
				    :description desc :mimed t)))
      ;; delete the mime-button
      (delete-region (vm-extent-start-position x) (vm-extent-end-position x))
      (vm-detach-extent x))))


;; This code was originally part of
;; vm-mime-xemacs/fsfemacs-encode-composition functions.

(defun vm-mime-insert-file-contents (file type)
  "Safely insert the contents of FILE of TYPE into the current
buffer." 
  (if vm-xemacs-p
      (let ((coding-system-for-read
	     (if (vm-mime-text-type-p type)
		 (vm-line-ending-coding-system)
	       (vm-binary-coding-system)))
	    ;; keep no undos 
	    (buffer-undo-list t)
	    ;; no transformations!
	    (format-alist nil)
	    ;; no decompression!
	    (jka-compr-compression-info-list nil)
	    ;; don't let buffer-file-coding-system be changed
	    ;; by insert-file-contents.  The
	    ;; value we bind to it to here isn't important.
	    (buffer-file-coding-system (vm-binary-coding-system)))
	(insert-file-contents file))
    ;; as of FSF Emacs 19.34, even with the hooks
    ;; we've attached to the attachment overlays,
    ;; text STILL can be inserted into them when
    ;; font-lock is enabled.  Explaining why is
    ;; beyond the scope of this comment and I
    ;; don't know the answer anyway.  This
    ;; insertion dance works to prevent it.
    (insert-before-markers " ")
    (forward-char -1)
    (let ((coding-system-for-read
	   (if (vm-mime-text-type-p type)
	       (vm-line-ending-coding-system)
	     (vm-binary-coding-system)))
	  ;; keep no undos 
	  (buffer-undo-list t)
	  ;; no transformations!
	  (format-alist nil)
	  ;; no decompression!
	  (jka-compr-compression-info-list nil)
	  ;; don't let buffer-file-coding-system be
	  ;; changed by insert-file-contents.  The
	  ;; value we bind to it to here isn't
	  ;; important.
	  (buffer-file-coding-system (vm-binary-coding-system))
	  ;; For NTEmacs 19: need to do this to make
	  ;; sure CRs aren't eaten.
	  (file-name-buffer-file-type-alist '(("." . t))))
      (condition-case data
	  (insert-file-contents file)
	(error
	 ;; font-lock could signal this error in FSF
	 ;; Emacs versions prior to 21.0.  Catch it
	 ;; and ignore it.
	 (if (equal data '(error "Invalid search bound (wrong side of point)"))
	     nil
	   (signal (car data) (cdr data)))))
      (goto-char (point-max))
      (delete-char -1))))

(defun vm-mime-insert-buffer-substring (buffer type)
  "Safe insert the contents of BUFFER of TYPE into the current buffer."
  (if vm-xemacs-p
      (insert-buffer-substring buffer)
    ;; Under Emacs 20.7 inserting a unibyte buffer
    ;; contents that contain 8-bit characters into a
    ;; multibyte buffer causes the inserted data to be
    ;; corrupted with the dreaded \201 corruption.  So
    ;; we write the data out to disk and let the file
    ;; be inserted, which gets aoround the problem.
    (let ((tempfile (vm-make-tempfile)))
      ;; make note to delete the tempfile after insertion
      (with-current-buffer buffer
	(let ((buffer-file-coding-system
	       (vm-binary-coding-system)))
	  (write-region (point-min) (point-max) tempfile nil 0)))
      (unwind-protect
	  (vm-mime-insert-file-contents 
	   tempfile type)
	(vm-error-free-call 'delete-file tempfile)))))


;;; vm-mime.el ends here
