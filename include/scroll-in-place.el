;;; scroll-in-place.el --- a simpler scroll-in-place replacement

;;; Copyright (C) 2009-2011 Eli Barzilay <eli@barzilay.org>
;;;               2011 Nick Alcock <nix@esperi.org.uk>

;; Author: Eli Barzilay <eli@barzilay.org>
;; Created: 2011-03-02
;; Keywords: local

;; This file is not part of Emacs.

;;; Commentary:

;; This file defines an improved scroll-in-place, with most of the vast
;; complexity of the original given to Emacs instead.  The entry point
;; functions that are defined here are `SIP-scroll-up' and `SIP-scroll-down',
;; which are then used to replace the built-in `scroll-up' and `scroll-down'.
;; This does nothing by itself, since the two functions simply call the
;; built-in ones -- but if `scroll-preserve-screen-position' is set to
;; `in-place' then the extra functionality kicks in.  In other words, loading
;; this file makes it possible to customize scrolling by an `in-place' setting.
;; Also binds new scrolling functions: `scroll-up-1' and `scroll-down-1'
;; default to one-line scrolling, and `scroll-up-1-stay' and
;; `scroll-down-1-stay' do the same but keep the cursor in the same position
;; (if possible).  (In addition to `scroll-preserve-screen-position', the code
;; also checks that `scroll-in-place' is non-nil -- this is not really needed,
;; but there are some uses of the scroll commands that are broken by this
;; (gnus, vm) and set `scroll-in-place' to nil -- so we check it to respect
;; those settings.)
;;
;; This whole thing can easily be adapted to become part of Emacs, for example,
;; added to the `scroll-up-command' and `scroll-down-command'.
;;
;; The original idea for this comes from `scroll-in-place' by Eric Eide, who
;; took it from `scroll-fix' by Joe Wells.  But the code has no relation to
;; those, mostly because the hard bit (keeping the point in place) is now done
;; by Emacs.  The relevant parts of the comments that this file implements are
;; below (edited: removed comments about things that are not done, possibly
;; because they're part of Emacs):
;;
;;;; + Because the improved scrolling commands keep point at its original
;;;;   window position, these scrolling commands are "reversible."  The
;;;;   `scroll-up' command undoes the effect of the immediately previous
;;;;   `scroll-down' command (if any) and vice versa.  In other words, if you
;;;;   scroll up and then immediately scroll back down, the window config-
;;;;   uration is restored to its exact original state.  This allows you to
;;;;   browse through a buffer more easily, as you can always get back to the
;;;;   original configuration.
;;;;
;;;;   Note, however, that the improved scrolling commands are guaranteed to be
;;;;   reversible only if there are no intervening non-scrolling commands.
;;;;   Also, if you give a prefix argument to a scrolling command (in order to
;;;;   specify the number of lines to scroll by), previous scrolling commands
;;;;   may no longer be reversible.
;;;;
;;;;   You might find it useful to think of the scrolling commands as forming
;;;;   "chains."  A scrolling command either starts or continues a chain.  By
;;;;   issuing a non-scrolling command or by changing the number of lines to be
;;;;   scrolled, you break the chain.  Scrolling commands are guaranteed to be
;;;;   reversible only within the current chain.  Hopefully that's clear
;;;;   enough.
;;;;
;;;; + When a scrolling command is given a prefix argument (which specifies the
;;;;   number of lines to scroll by), then that argument becomes the default
;;;;   scrolling distance for all immediately subsequent scrolling commands.
;;;;   This means that you can easily set the scrolling distance for a chain
;;;;   of scrolling commands.  Note that a new prefix argument or any non-
;;;;   scrolling command breaks the chain (as described above), and any further
;;;;   scrolling commands will use the usual defaults (or the prefix argument
;;;;   you specify at that time, of course).
;;;;
;;;; + If the scrolling commands cannot keep point at its initial window
;;;;   position (because a buffer boundary is on screen and the window can't be
;;;;   scrolled as far as necessary to keep point at the right place), point is
;;;;   allowed to temporarily stray from its initial window position.  That is,
;;;;   point moves the correct number of window lines, even if it means that it
;;;;   has to stray from its desired window position.  This straying is undone
;;;;   when (and if) the scrolling action is reversed.

;;; Code:

(defvar SIP-last-scroll-arg nil
  "The last prefix argument scroll was invoked with.")

(defvar SIP-scroll-column nil
  "The column we were moved to as a consequence of scrolling.")

;; remember the last buffer state counter to break scroll sequences when the
;; buffer is modified (eg, in shells)
(defvar SIP-last-bufchars nil "The last buffer state counter used to scroll.")

(defvar SIP-scroll-posns nil
  "A (buffer-local) list of remembered positions.

Holds (UPWARD-POSNS DOWNWARD-POSNS) with a list of positions upward from the
current position, and downward from it.  See `SIP-get-scroll-posn' for the
format of a position.")
(make-variable-buffer-local 'SIP-scroll-posns)

;; Remember original scrolling commands.
(dolist (std '(scroll-up scroll-down))
  (let ((saved (intern (format "SIP-orig-%s" std))))
    (unless (fboundp saved) (fset saved (symbol-function std)))))

(defun SIP-get-scroll-posn ()
  "Get the current scroll position, a list of values.

Currently contains the cursor position and the window row/column, but may
change to include more (or different) information."
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

(defun SIP-do-scroll-internal (arg isdown group orig)
  ;; This is the body of `SIP-do-scroll', which is dealing with the question of
  ;; whether to do an in-place scrolling or not, based on
  ;; `scroll-preserve-screen-position'.
  ;; associate commands -> groups, so it work when called from other commands:
  (when (symbolp this-command) (put this-command 'SIP-group group))
  ;; Is the below needed?
  ;; (setq this-command orig) ; try this to deal with the C-down C-pgdn problem
  (let* ((bufchars (buffer-chars-modified-tick))
         (repeated (and (symbolp last-command)
                        (equal group (get last-command 'SIP-group))
                        (equal SIP-last-bufchars bufchars)
                        (or (equal arg SIP-last-scroll-arg)
                            (not (integerp arg)))))
         (arg (cond ((not repeated)
                     (SIP-set-visual-column)
                     (setq SIP-last-scroll-arg arg))
                    (t (when (eq arg '-) (setq isdown (not isdown)))
                       SIP-last-scroll-arg)))
         ;; isdown means pgdn (so => scroll up)
         (isdown (if (or (eq arg '-) (< (prefix-numeric-value arg) 0))
                   (not isdown) isdown))
         ;; these hold the referencing cons cell (so it can be modified)
         past-box future-box
         (curpos (SIP-get-scroll-posn)))
    (setq SIP-last-bufchars bufchars)
    (unless (and repeated SIP-scroll-posns)
      (setq SIP-scroll-posns (list '() '())))
    ;; pull the right boxes
    (if isdown
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
      ((if isdown (bobp) (eobp)) nil)
      ;; otherwise try to do the needed scroll if the edge is not visible...
      ((or (pos-visible-in-window-p (if isdown (point-min) (point-max)))
           (condition-case nil
               (progn (funcall orig arg)
                      (SIP-goto-visual-column)
                      (when (and (not isdown)
                                 (pos-visible-in-window-p (point-max)))
                        (save-excursion (goto-char (point-max)) (recenter -1)))
                 nil)
             ((beginning-of-buffer end-of-buffer) t)))
       ;; ...but if the edge is visible (or scrolling failed), move instead
       (if (integerp arg)
         (let (;; set a goal column, and make sure we do a visual movement
               (temporary-goal-column (float SIP-scroll-column))
               (line-move-visual t)
               ;; and fake a second call to use it
               (this-command 'previous-line))
           (with-no-warnings
             (if isdown (previous-line (abs arg)) (next-line (abs arg))))
           (SIP-goto-visual-column))
         (goto-char (if isdown (point-min) (point-max))))))))

(defvar scroll-in-place t
  "If this is nil, `scroll-in-place' functionality is disabled.
The main way for controlling `scroll-in-place' is via
`scroll-preserve-screen-position', but this variable is also used for older
packages that expect to disable changing scrolling when it is nil.")

(defun SIP-do-scroll (arg isdown group)
  "Scroll, endeavouring to keep the cursor in the same place on the screen.

Keeps the cursor position only if `scroll-preserve-screen-position' is bound
to `in-place', otherwise does a plain scroll (eg, using `scroll-up').
`scroll-in-place' is also checked, since there are some packages that use that
and expect to avoid different scroll behavior.

ARG is the number of lines to scroll; ISDOWN is t if this is a downward scroll;
GROUP designates a group of interrelated scrolling commands that should
cancel each other out."
  (let ((orig (if isdown 'SIP-orig-scroll-down 'SIP-orig-scroll-up)))
    (if (and (eq scroll-preserve-screen-position 'in-place)
             scroll-in-place)
      (SIP-do-scroll-internal arg isdown group orig)
      (progn ;; forcibly break any sequence of scrolling commands
             (setq SIP-last-bufchars nil)
             (funcall orig arg)))))

(defmacro defun-SIP-up/down (name-pat inter other docstr)
  "A macro to generate up/down scrolling commands.

NAME-PAT is the name of the group of up/down scrolling commands being
defined, with the up/down portion replaced with `XX'.
INTER is the interactive specification of the scrolling command.
OTHER is T if the scrolling should be performed on the \"other\" window.
DOCSTR is the function's docstring, with `XX' replaced appropriately."
  (let ((mk (lambda (downp)
              (let* ((u/d    (if downp "down" "up"))
                     (name   (intern (replace-regexp-in-string
                                      "XX" u/d (symbol-name name-pat) t)))
                     (docstr (replace-regexp-in-string "XX" u/d docstr t))
                     (doit `(SIP-do-scroll arg ',downp ',name-pat))
                     (doit (if other
                             `(save-selected-window
                                (select-window (other-window-for-scrolling))
                                ,doit)
                             doit)))
                `((defun ,name (&optional arg)
                    ,docstr (interactive ,inter) ,doit)
                  (put ',name 'scroll-command t))))))
    `(progn ,@(funcall mk nil) ,@(funcall mk t))))

(defun-SIP-up/down SIP-scroll-XX "^P" nil
  "Wrapper for `scroll-XX' that does a scroll-in-place.
Also:
- when reaching the edge, move the cursor instead of beeping,
- consecutive uses with no prefix use the first prefix in the sequence.")

(defun-SIP-up/down SIP-scroll-other-window-XX "^P" t
  "Like `scroll-XX', but for the other window.
\(See `other-window-for-scrolling' for details.)")

(defun-SIP-up/down scroll-XX-1 "^p" nil
  "Like `scroll-XX' with a default of one line.")

;; These should not be scroll-XX commands, since then things get messed
;; up when the built-in scrolling functions try to restore the position,
;; resulting in jittery cursor jumps.
(defun scroll-up-1-stay (arg)
  "Like `scroll-up-1' but stay in the same position."
  (interactive "^p")
  (let ((p (point)) (scroll-preserve-screen-position nil))
    (scroll-up-1 arg)
    (when (pos-visible-in-window-p p) (goto-char p))))
(defun scroll-down-1-stay (arg)
  "Like `scroll-down-1' but stay in the same position."
  (interactive "^p")
  (let ((p (point)) (scroll-preserve-screen-position nil))
    (scroll-down-1 arg)
    (when (pos-visible-in-window-p p) (goto-char p))))
;; still use these properties, so it can be used in isearch
(put 'scroll-up-1-stay 'scroll-command t)
(put 'scroll-down-1-stay 'scroll-command t)

;; Replace the standard Emacs commands: preserve their docstrings.
(dolist (dir '(up down))
  (dolist (other '(nil t))
    (let* ((std (intern (if other
                          (format "scroll-other-window%s"
                                  (if (eq dir 'up) "" "-down"))
                          (format "scroll-%s" dir))))
           (replace (intern (format "SIP-scroll%s-%s"
                                    (if other "-other-window" "")
                                    dir)))
           (old-doc (documentation std)))
      (fset std (symbol-function replace))
      (put std 'function-documentation old-doc))))

(provide 'scroll-in-place)
