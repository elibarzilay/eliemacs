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

;;; Code:

;; remember last argument used, and the column we started with
(defvar eli-last-scroll-arg nil)
(defvar eli-scroll-column nil)
;; remember the last command and its group, so we can identify repeated uses
;; of groups even when this is invoked from other commands
(defvar last-scroll-command-and-group nil)

;; remembered positions: (list upward-positions downward-positions)
(defvar eli-scroll-posns nil)
(make-variable-buffer-local 'eli-scroll-posns)

(defun eli-get-scroll-position ()
  (list (point) (window-start) (window-hscroll)))
(defun eli-set-scroll-position (posn)
  (goto-char (nth 0 posn))
  (set-window-start nil (nth 1 posn) t)
  (set-window-hscroll nil (nth 2 posn)))

(defun eli-set-visual-column ()
  ;; same as the code at the top of `line-move-visual'
  (let ((posn (posn-at-point)))
    (setq eli-scroll-column
          (if (eq (nth 1 posn) 'right-fringe) ; overflow-newline-into-fringe
            (- (window-width) 1)
            (let ((x (car (posn-x-y posn))))
              (and x (truncate (/ (float x) (frame-char-width)))))))))
(defun eli-goto-visual-column ()
  (when eli-scroll-column (vertical-motion (cons eli-scroll-column 0))))

(defun eli-do-scroll (arg isdown group)
  (let* ((direction (if isdown -1 +1))
         (repeated
          ;; see the above comment: this makes it possible for things to work
          ;; fine even when called through some other command
          (prog1 (and (eq (car last-scroll-command-and-group) last-command)
                      (eq (cdr last-scroll-command-and-group) group)
                      (memq current-prefix-arg '(nil -)))
            (setq last-scroll-command-and-group (cons this-command group))))
         (arg (if repeated
                eli-last-scroll-arg
                (progn (eli-set-visual-column)
                       (setq eli-last-scroll-arg arg))))
         (direction (if (or (eq arg '-) (< (prefix-numeric-value arg) 0))
                      (- direction) direction))
         (direction (if (> direction 0) 'down 'up))
         ;; these hold the referencing cons cell (so it can be modified)
         past-box future-box
         (curpos (eli-get-scroll-position)))
    (unless (and repeated eli-scroll-posns)
      (setq eli-scroll-posns (list '() '())))
    ;; pull the right boxes
    (if (eq direction 'up)
      (setq future-box eli-scroll-posns past-box   (cdr eli-scroll-posns))
      (setq past-box   eli-scroll-posns future-box (cdr eli-scroll-posns)))
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
         (eli-set-scroll-position posn)))
      ;; we're at the edge so there is nothing to do
      ((if (eq direction 'up) (bobp) (eobp))
       nil)
      ;; otherwise try do the needed scroll if the edge is not visible...
      ((or (pos-visible-in-window-p
            (if (eq direction 'up) (point-min) (point-max)))
           (condition-case nil
               (progn
                 (funcall (if isdown 'scroll-down 'scroll-up) arg)
                 (eli-goto-visual-column)
                 ;; if we went down and now we see the bottom (and it we know
                 ;; it wasn't visible before), then make it be the bottom
                 (when (and (eq direction 'down)
                            (pos-visible-in-window-p (point-max)))
                   (save-excursion (goto-char (point-max)) (recenter -1)))
                 nil)
             ((beginning-of-buffer end-of-buffer) t)))
       ;; ...but if the edge is visible (or scrolling failed), move instead
       (if (integerp arg)
         (let ((eli-line-movement-without-dings t)
               ;; set a goal column, and make sure we do a visual movement
               (temporary-goal-column (float eli-scroll-column))
               (line-move-visual t)
               ;; and fake a second call to use it
               (this-command 'previous-line))
           (if (eq direction 'up)
             (eli-previous-line (abs arg))
             (eli-next-line (abs arg)))
           (eli-goto-visual-column))
         (goto-char (if (eq direction 'up) (point-min) (point-max))))))))

(defmacro defun-eli-up/down (name-pat inter keep docstr)
  (let ((mk (lambda (u/d downp)
              (let* ((name   (intern (replace-regexp-in-string
                                      "XX" u/d (symbol-name name-pat) t)))
                     (docstr (replace-regexp-in-string "XX" u/d docstr t))
                     (doit `(eli-do-scroll arg ,downp ',name-pat))
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
                  (put ',name 'isearch-scroll t))))))
    `(progn ,@(funcall mk "up" nil) ,@(funcall mk "down" t))))

(defun-eli-up/down eli-scroll-XX "P" nil
  "Wrapper for `scroll-XX' that does a scroll-in-place.
Also:
- when reaching the edge, move the cursor instead of beeping,
- consecutive uses with no prefix use the first prefix in the sequence.")

(defun-eli-up/down eli-scroll-XX-1 "p" nil
  "Like `eli-scroll-XX' with a default of one line.")

(defun-eli-up/down eli-scroll-XX-1-stay "p" t
  "Like `eli-scroll-XX-1' but stay in the same place.")
