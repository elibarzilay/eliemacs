;;; scroll-in-place.el --- a simpler scroll-in-place replacement

;;; Copyright (C) 2009 Eli Barzilay <eli@barzilay.org>
;;;               2011 Nick Alcock <nix@esperi.org.uk>

;; Author: Eli Barzilay <eli@barzilay.org>
;; Created: 2011-03-02
;; Keywords: local

;; This file is not part of Emacs.

;;; Commentary:

;; This file defines an improved scroll-in-place, with most of the vast
;; complexity of the original given to Emacs instead.

;;; Requirements:

(eval-when-compile (require 'cl))

;;; Code:

;;;###autoload
(defvar scroll-in-place t
  "*Enable in-place scrolling.

When this variable is true (i.e., non-`nil'), the standard Emacs
vertical scrolling commands `scroll-down', `sccroll-up',
`scroll-other-window-down', and `scroll-other-window' will
attempt to keep point at its current position in the
window (window line and column).  In other words, point stays
\"in place\" within the window.

When this variable is `nil', or `scroll-preserve-screen-position'
is `nil', the standard Emacs vertical scrolling commands behave
as usual.  The \"in place\" equivalents, however, are still
available as separate commands.

This variable may be made buffer-local in order to disable (or
enable) \"in place\" scrolling in particular buffers.")

(defvar SIP-last-scroll-arg nil
  "The last prefix argument scroll was invoked with.")

(defvar SIP-scroll-column nil
  "The column we were moved to as a consequence of scrolling.")

;; remember the last command and its group, so we can identify repeated uses
;; of groups even when this is invoked from other commands
(defvar SIP-last-scroll-command+group nil
  "The last command used to scroll, and the group of commands it was in.")

(defvar SIP-scroll-posns nil
  "A (buffer-local) list of remembered positions.

Holds (UPWARD-POSNS DOWNWARD-POSNS) with a list of positions
upward from the current position, and downward from it.  See
`SIP-get-scroll-posn' for the format of a position.")
(make-variable-buffer-local 'SIP-scroll-posns)

;; Remember original scrolling commands.
(dolist (std '(scroll-up scroll-down
               scroll-other-window scroll-other-window-down))
  (let ((saved (intern (format "SIP-orig-%s" std))))
    (unless (fboundp saved) (fset saved (symbol-function std)))))

(defun SIP-get-scroll-posn ()
  "Get the current scroll position, a list of values.

Currently contains the cursor position and the window row/column,
but may change to include more (or different) information."
  (list (point) (window-start) (window-hscroll)))

(defun SIP-set-scroll-posn (posn)
  "Set the scroll position.

POSN is in the format of `SIP-get-scroll-posn'."
  (goto-char (nth 0 posn))
  (set-window-start nil (nth 1 posn) t)
  (set-window-hscroll nil (nth 2 posn)))

(defun SIP-set-visual-column ()
  "Figure out which column to place the cursor on during scrolling."
  ;; same as the code at the top of `line-move-visual'
  (let ((posn (posn-at-point)))
    (setq SIP-scroll-column
          (if (eq (nth 1 posn) 'right-fringe) ; overflow-newline-into-fringe
            (- (window-width) 1)
            (let ((x (car (posn-x-y posn))))
              (and x (truncate (/ (float x) (frame-char-width)))))))))

(defun SIP-goto-visual-column ()
  "Go to the column suggested by the `SIP-scroll-column'."
  (when SIP-scroll-column (vertical-motion (cons SIP-scroll-column 0))))

(defun SIP-do-scroll (arg isdown group)
  "Scroll, endeavouring to keep the cursor in the same place on the screen.

ARG is the number of lines to scroll; ISDOWN is t if this is a downward scroll;
GROUP designates a group of interrelated scrolling commands that should
cancel each other out."
  (let* ((direction (if isdown -1 +1))
         (repeated
          ;; this makes it possible for things to work fine even when called
          ;; through some other command
          (prog1 (and (eq (car SIP-last-scroll-command+group) last-command)
                      (eq (cdr SIP-last-scroll-command+group) group)
                      (memq current-prefix-arg '(nil -)))
            (setq SIP-last-scroll-command+group (cons this-command group))))
         (arg (if repeated
                SIP-last-scroll-arg
                (progn (SIP-set-visual-column)
                       (setq SIP-last-scroll-arg arg))))
         (direction (if (or (eq arg '-) (< (prefix-numeric-value arg) 0))
                      (- direction) direction))
         (direction (if (> direction 0) 'down 'up))
         ;; these hold the referencing cons cell (so it can be modified)
         past-box future-box
         (curpos (SIP-get-scroll-posn)))
    (unless (and repeated SIP-scroll-posns)
      (setq SIP-scroll-posns (list '() '())))
    ;; pull the right boxes
    (if (eq direction 'up)
      (setq future-box SIP-scroll-posns past-box   (cdr SIP-scroll-posns))
      (setq past-box   SIP-scroll-posns future-box (cdr SIP-scroll-posns)))
    ;; remember where we are now, unless it's in the same place as last time
    (unless (and (consp (car past-box)) (equal curpos (caar past-box)))
      (setcar past-box (cons curpos (car past-box))))
    ;; dump a future position that is the same as where we are (might happen
    ;; when we get to the buffer edges) see if there's a future position we
    ;; should go to (at most one, but still use `while' for safety)
    (while (and (consp (car future-box)) (equal curpos (caar future-box)))
      (setcar future-box (cdar future-box)))
    (cond
      ;; see if there's a future position we should go to
      ((consp (car future-box))
       (let ((posn (caar future-box)))
         (setcar future-box (cdar future-box))
         (SIP-set-scroll-posn posn)))
      ;; we're at the edge so there is nothing to do
      ((if (eq direction 'up) (bobp) (eobp))
       nil)
      ;; otherwise try do the needed scroll if the edge is not visible...
      ((or (pos-visible-in-window-p
            (if (eq direction 'up) (point-min) (point-max)))
           (condition-case nil
               (progn
                 (funcall (if isdown 'SIP-orig-scroll-down 'SIP-orig-scroll-up)
                          arg)
                 (SIP-goto-visual-column)
                 ;; if we went down and now we see the bottom (and it we know
                 ;; it wasn't visible before), then make it be the bottom
                 (when (and (eq direction 'down)
                            (pos-visible-in-window-p (point-max)))
                   (save-excursion (goto-char (point-max)) (recenter -1)))
                 nil)
             ((beginning-of-buffer end-of-buffer) t)))
       ;; ...but if the edge is visible (or scrolling failed), move instead
       (if (integerp arg)
         (let ((SIP-line-movement-without-dings t)
               ;; set a goal column, and make sure we do a visual movement
               (temporary-goal-column (float SIP-scroll-column))
               (line-move-visual t)
               ;; and fake a second call to use it
               (this-command 'previous-line))
           (with-no-warnings
             (if (eq direction 'up)
               (previous-line (abs arg))
               (next-line (abs arg))))
           (SIP-goto-visual-column))
         (goto-char (if (eq direction 'up) (point-min) (point-max))))))))

(defmacro defun-SIP-up/down (name-pat inter keep docstr)
  "A macro to generate up/down scrolling commands.

NAME-PAT is the name of the group of up/down scrolling commands being
defined, with the up/down portion replaced with `XX'.
INTER is the interactive specification of the scrolling command.
KEEP is T if the cursor should not be moved, only the screen.
DOCSTR is the function's docstring, with `XX' replaced appropriately."
  (let ((mk (lambda (downp)
              (let* ((u/d    (if downp "down" "up"))
                     (name   (intern (replace-regexp-in-string
                                      "XX" u/d (symbol-name name-pat) t)))
                     (docstr (replace-regexp-in-string "XX" u/d docstr t))
                     (doit `(SIP-do-scroll arg ,downp ',name-pat))
                     (doit (if keep
                             `(let ((p (point)))
                                ,doit
                                ;; go back only if possible
                                (when (and (< p (window-end nil t))
                                           (<= (window-start) p))
                                  (goto-char p)))
                             doit)))
                `((defun ,name (&optional arg)
                    ,docstr (interactive ,inter) ,doit)
                  (put ',name 'CUA 'move)
                  (put ',name 'scroll-command t) ; for v24
                  (put ',name 'isearch-scroll t) ; for v23
                  )))))
    `(progn ,@(funcall mk nil) ,@(funcall mk t))))

(defun-SIP-up/down SIP-scroll-XX "^P" nil
  "Wrapper for `scroll-XX' that does a scroll-in-place.
Also:
- when reaching the edge, move the cursor instead of beeping,
- consecutive uses with no prefix use the first prefix in the sequence.")

(defun-SIP-up/down SIP-scroll-XX-1 "^p" nil
  "Like `SIP-scroll-XX' with a default of one line.")

(defun-SIP-up/down SIP-scroll-XX-1-stay "^p" t
  "Like `SIP-scroll-XX-1' but stay in the same place.")

;; This only works properly in conjunction with
;; `scroll-preserve-screen-position'.

(setq scroll-preserve-screen-position 'always)

;; Replace the standard Emacs commands: preserve their docstrings.
(dolist (dir '(up down))
  (dolist (other '(nil t))
    (let* ((std  (intern (if other
                           (format "scroll-other-window%s"
                                   (if (eq dir 'up) "" "-down"))
                           (format "scroll-%s" dir))))
           (replace (intern (format "SIP-scroll-%s" dir)))
           (orig    (intern (format "SIP-orig-%s"   std)))
           (old-doc (documentation std)))
      (fset std `(lambda (&optional arg)
                   (if (and scroll-in-place scroll-preserve-screen-position)
                     ,(if other
                        `(save-selected-window
                           (select-window (other-window-for-scrolling))
                           (,replace arg))
                        `(,replace arg))
                     ;; Forcibly break any sequence of scrolling commands
                     ;; which may have been in force before the binding,
                     ;; in case scroll-in-place is turned on again
                     (setq SIP-scroll-posns nil
                           SIP-last-scroll-command+group nil
                           SIP-last-scroll-arg nil
                           SIP-scroll-column nil)
                     (,orig arg))))
      (put std 'function-documentation old-doc))))

(provide 'scroll-in-place)
