;;; vm-summary-faces.el --- faces support for VM summary buffers
;;
;; This file is part of VM
;; 
;; Copyright (C) 2001 Robert Fenk
;; Copyright (C) 2010 Uday S Reddy
;;
;; Author:      Robert Fenk
;; Status:      Tested with XEmacs 21.4.15 & VM 7.18
;; Keywords:    VM 
;; X-URL:       http://www.robf.de/Hacking/elisp

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

;;; Commentary:
;;
;;  to use this add the following line to your ~/.vm file
;;
;;  (add-hook 'vm-summary-mode-hook 'vm-summary-faces-mode)
;;
;;; Code

(provide 'vm-summary-faces)

(eval-when-compile
  (require 'vm-misc))

(eval-and-compile
  (require 'vm-summary)
  (require 'vm-virtual))

;; (eval-and-compile
;;   (if vm-xemacs-p (require 'overlay)))

(declare-function vm-extent-property "vm-misc.el" (overlay prop) t)
(declare-function vm-set-extent-property "vm-misc.el" (overlay prop value) t)


(eval-and-compile
  (if (fboundp 'mapcar-extents)
      (defun vm-summary-faces-list-extents () (mapcar-extents 'identity))
    (defun vm-summary-faces-list-extents ()
      (let ((o (overlay-lists))) (nconc (car o) (cdr o))))))

(defvar vm-summary-faces-hide nil
  "Last face hidden by `vm-summary-faces-hide'.")

;;;###autoload
(defun vm-summary-faces-hide (&optional prop)
  "Toggle visibility of a particular vm-summary-face.  By
default, the deleted face is toggled (with the effect that all
deleted messages will be hidden or unhidden).  

With a prefix argument, the property name identifying the face is
queried interactively.  The property is a keyword such as edited,
collapsed or outgoing which has an associated face such as
vm-summary-edited.  See `vm-summary-faces-alist' for a list
of available face names."
  (interactive "P")
  (if (and (listp prop) (numberp (car prop)))
      (setq prop (completing-read "Face name: "
                                  (mapcar (lambda (f)
                                            (list (format "%s" (cadr f))))
                                          vm-summary-faces-alist)
                                  nil t "vm-summary-deleted")))
  (setq prop (or prop vm-summary-faces-hide "vm-summary-deleted"))
  (vm-select-folder-buffer-and-validate 0 (interactive-p))
  (vm-summarize)
  (set-buffer vm-summary-buffer)
  (let ((extents (vm-summary-faces-list-extents))
	(hidden-face (intern prop))
        x faces)
    (while extents
      (setq x (car extents)) 
      (setq faces (vm-extent-property x 'face))
      (unless (listp faces)
	(setq faces (list faces)))
      (when (memq hidden-face faces)
        (vm-set-extent-property 
	 x 'invisible (not (vm-extent-property x 'invisible))))
      (setq extents (cdr extents)))))

;;;###autoload
(defun vm-summary-faces-add (msg)
  "Add a face to a summary entry according to `vm-summary-faces-alist'."
  (let ((faces vm-summary-faces-alist)
        (x (or (vm-su-summary-mouse-track-overlay-of msg)
               (vm-extent-at (vm-su-start-of msg))
               (vm-extent-at (vm-su-end-of msg)))))
    (while faces
      (when (apply 'vm-vs-or msg (list (caar faces)))
	(cond ((vm-collapsed-root-p msg)
	       (vm-set-extent-property 
		x 'face (list (cadar faces) 'vm-summary-collapsed)))
	      ((vm-expanded-root-p msg)
	       (vm-set-extent-property
		x 'face (list (cadar faces) 'vm-summary-expanded)))
	      (t
	       (vm-set-extent-property
		x 'face (list (cadar faces)))))
        (setq faces nil))
      (setq faces (cdr faces)))))

(defun vm-summary-faces-destroy ()
  "Removes the face from all summary entries."
  (let ((extents (vm-summary-faces-list-extents))
        x)
    (while extents
      (setq x (car extents))
      (vm-set-extent-property x 'face nil)
      (setq extents (cdr extents)))))

;;;###autoload
(defun vm-summary-faces-mode (&optional arg)
  "Toggle `vm-summary-faces-mode'.  Optional argument ARG should be 0
or 1, indicating whether the summary faces should be off or on.

When it is on, the VM summary buffers are decorated with faces, i.e.,
fonts and colors, for easy recogniton of the message status."
  (interactive "P")
  (if (null arg)
      (setq vm-summary-enable-faces (not vm-summary-enable-faces))
    (if (> (prefix-numeric-value arg) 0)
        (setq vm-summary-enable-faces t)
      (setq vm-summary-enable-faces nil)))

  (when (interactive-p)
    (vm-inform 1 "VM summary faces mode is %s"
             (if vm-summary-enable-faces "on" "off")))
  
  (if (memq major-mode '(vm-mode vm-virtual-mode vm-summary-mode
                                 vm-presentation-mode))
      (save-excursion
        (vm-select-folder-buffer-and-validate 0 (interactive-p))
        (vm-summarize)
        (set-buffer vm-summary-buffer)
        (if vm-summary-enable-faces
	    (progn
	      (mapc 'vm-summary-faces-add vm-message-list)
	      (if vm-summary-overlay
		  (vm-set-extent-property vm-summary-overlay 'face
					  'vm-summary-selected)))
          (vm-summary-faces-destroy)
          (if vm-summary-overlay
              (vm-set-extent-property vm-summary-overlay 'face
                                      vm-summary-highlight-face))))))

;; No need for advice because the code has been integrated into 
;; VM.  USR, 2010-08-01 

;; (defadvice vm-mouse-set-mouse-track-highlight 
;;	(after vm-summary-faces activate)
;;   (when (and vm-summary-enable-faces
;;              (eq major-mode 'vm-summary-mode)
;;              (boundp 'm)
;;              m)
;;     ;; FIXME there is a warning about a free variable here, sorry!
;;     (vm-summary-faces-add m)))

(defun vm-summary-faces-fix-pointer ()
  (if vm-summary-overlay
      (vm-set-extent-property vm-summary-overlay 'face
			      (if vm-summary-enable-faces
				  'vm-summary-selected
				vm-summary-highlight-face))))

(add-hook 'vm-summary-pointer-update-hook 'vm-summary-faces-fix-pointer)

