;;; vm-reply.el --- Mailing, forwarding, and replying commands
;;
;; This file is part of VM
;;
;; Copyright (C) 2011 Uday S. Reddy
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

;;; Commentary:
;; This file provides functions that can be used in a Dired buffer to
;; send files to VM.
;;; Interface:
;; Interactive commands:
;;
;; vm-dired-attach-file: (buffer) -> unit
;; vm-dired-do-attach-files: (buffer) -> unit
;;
;;; Code:

(provide 'vm-dired)

(require 'dired) 

(eval-when-compile
  (require 'vm-misc)
  (require 'vm-minibuf)
  (require 'vm-menu)
  (require 'vm-folder)
  (require 'vm-summary)
  (require 'vm-window)
  (require 'vm-page)
  (require 'vm-motion)
  (require 'vm-mime)
  (require 'vm-digest)
  (require 'vm-undo)
  )

(eval-and-compile
  (require 'dired))

(declare-function vm-dired-file-name-at-point "vm-dired.el" ())

(cond ((fboundp 'dired-file-name-at-point) ; Emacs 23 dired
       (fset 'vm-dired-file-name-at-point 'dired-file-name-at-point))
      ((fboundp 'dired-filename-at-point) ; Emacs 22 dired-x
       (fset 'vm-dired-file-name-at-point 'dired-filename-at-point))
      (t
       (error "vm-dired not supported in Emacs version %s" emacs-version)))

;;;###autoload
(defun vm-dired-attach-file (composition)
  "Attach the file at point in the dired buffer to a VM composition
buffer as a mime attachment. 

The file is not inserted into the buffer and MIME encoded until
you execute `vm-mail-send' or `vm-mail-send-and-exit'.  A visible tag
indicating the existence of the object is placed in the
composition buffer.  You can move the object around or remove
it entirely with normal text editing commands.  If you remove the
object tag, the object will not be sent.

First argument COMPOSITION is the buffer into which the object
will be inserted.  When this function is called interactively
COMPOSITION's name will be read from the minibuffer."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command))
     (list (read-buffer "Attach file to buffer: "
			(vm-find-composition-buffer) t))))
  (unless vm-send-using-mime
    (error (concat "MIME attachments disabled, "
		   "set vm-send-using-mime non-nil to enable.")))
  (let ((file (vm-dired-file-name-at-point))
	type)
    (when (and file (file-regular-p file))
      (setq type (or (vm-mime-default-type-from-filename file)
		     "application/octet-stream"))
      (with-current-buffer composition
	(vm-attach-file file type)))))

;;;###autoload
(defun vm-dired-do-attach-files (composition)
  "Attach all marked files in the dired buffer to a VM composition
buffer as mime attachments. 

The files are not inserted into the buffer and MIME encoded until
you execute `vm-mail-send' or `vm-mail-send-and-exit'.  For each
file, a visible tag indicating the existence of the object is
placed in the composition buffer.  You can move the objects around
or remove them entirely with normal text editing commands.  If you
remove an object tag, the object will not be sent.

First argument COMPOSITION is the buffer into which the objects
will be inserted.  When this function is called interactively
COMPOSITION's name will be read from the minibuffer."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command))
     (list (read-buffer "Attach object to buffer: "
			(vm-find-composition-buffer) t))))
  (unless vm-send-using-mime
    (error (concat "MIME attachments disabled, "
		   "set vm-send-using-mime non-nil to enable.")))
  (dired-map-over-marks
   (let ((file (dired-get-filename))
	 type)
     (setq type (or (vm-mime-default-type-from-filename file)
		    "application/octet-stream"))
     (with-current-buffer composition
       (vm-attach-file file type)))
   nil))

;;; vm-dired.el ends here
