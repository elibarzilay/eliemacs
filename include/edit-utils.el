;;; edit-utils.el --- Editing utilities
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;;-----------------------------------------------------------------------------
;; Setting some modes

;; these appear in "settings.el" too
(transient-mark-mode 1)
(setq shift-select-mode t)

;; (electric-indent-mode 1) ; would be nice, but indents the existing line too
                            ; also affects C-o in the same way
;; (electric-pair-mode 1) ; would be nice, but doesn't play well with delsel
                          ; eg, type "(" when there's an active region
(electric-layout-mode 1)

;;-----------------------------------------------------------------------------
;; Some generic utilities

;; Show stuff about my environment.
(defun eliemacs-quickref ()
  "Display the EliEmacs Quick Reference."
  (interactive)
  (let ((qr (concat eli-dir "QuickRef.txt")))
    (if (file-readable-p qr)
      (view-file qr)
      (error "Couldn't find the file (%S)" qr))))

;; Sort a whole buffer
(defun sort-buffer ()
  "Sort a whole buffer - using `sort-lines'."
  (interactive)
  (sort-lines nil (point-min) (point-max)))

;; Kill the current buffer immediatly, saving it if needed.
(defvar kill-save-buffer-delete-windows t
  "*Delete windows when `kill-save-buffer' is used.
If this is non-nil, then `kill-save-buffer' will also delete the corresponding
windows.  This is inverted by `kill-save-buffer' when called with a prefix.")
(defun kill-save-buffer (arg)
  "Save the current buffer (if needed) and then kill it.
Also, delete its windows according to `kill-save-buffer-delete-windows'.
A prefix argument ARG reverses this behavior."
  (interactive "P")
  (let ((del kill-save-buffer-delete-windows))
    (when arg (setq del (not del)))
    (let ((fname (buffer-file-name)))
      (when (and (buffer-modified-p) fname (not (file-directory-p fname)))
        (save-buffer)))
    (let ((buf (current-buffer)))
      (when del (delete-windows-on buf))
      (kill-buffer buf))))

;; Move to another window in the opposite direction
(defun eli-other-window (arg &optional all-frames)
  "Same as `other-window' but goes back when shifted."
  (interactive "p")
  (other-window (if this-command-keys-shift-translated (- arg) arg)
                all-frames))

;; Non-killing deletions
(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
(defun delete-sexp (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-sexp arg) (point))))
(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))
(defun backward-delete-sexp (arg)
  (interactive "p")
  (delete-sexp (- arg)))

(defun eli-kill-region (beg end)
  "Same as `kill-region', but does nothing with inactive mark or empty region."
  (interactive "r")
  (when (and mark-active (not (= beg end)))
    (setq last-command nil)
    (kill-region beg end)))

(defun eli-toggle-lines-mode ()
  "Toggle `truncate-lines', `line-move-visual', and `visual-line-mode'.
Use `line-move-visual' and `visual-line-mode' only if used successive times.
With a prefix argument go back to the default."
  (interactive)
  (let* ((cur (cond (truncate-lines 'truncate)
                    (word-wrap (if line-move-visual 'vwords 'words))
                    (line-move-visual 'vlines)
                    (t 'wrapped)))
         (repeated (eq this-command last-command))
         (disps  '((wrapped  "wrapped lines")
                   (truncate "truncated lines")
                   (vlines   "wrapped lines with visual movement")
                   (vwords   "word-wrapped lines with visual movement")
                   (words    "word-wrapped lines without visual movement")))
         (order (mapcar 'car disps))
         (next (cond (current-prefix-arg 'wrapped)
                     (repeated (or (cadr (memq cur order)) (car order)))
                     ;; not repeated -- toggle first two
                     ((eq cur (car order)) (cadr order))
                     ;; otherwise -> back to default
                     (t (car order)))))
    (visual-line-mode (if (eq next 'vwords) 1 -1))
    (setq truncate-lines   (eq next 'truncate))
    (setq word-wrap        (memq next '(vwords words)))
    (setq line-move-visual (memq next '(vlines vwords)))
    (save-excursion (forward-line 0) (sit-for 0)
                    (redraw-frame (selected-frame)))
    (message "Using %s." (cadr (assq next disps)))))

(defun eli-yank-really-pop (&optional arg)
  "Like `yank', but pops the value out of the `kill-ring-yank-pointer' top."
  (interactive "*p")
  (yank arg)
  ;; `yank' sets `this-command'
  (when (eq this-command 'yank)
    (if (cdr kill-ring-yank-pointer)
      (progn (setcar kill-ring-yank-pointer (cadr kill-ring-yank-pointer))
             (setcdr kill-ring-yank-pointer (cddr kill-ring-yank-pointer)))
      (setcar kill-ring-yank-pointer "")))
  nil)
(put 'eli-yank-really-pop 'delete-selection t)

;;-----------------------------------------------------------------------------
;; Functions for movement that are similar to the normal ones, except that
;; up/down next to the edges will first move the curser to the beginning/end
;; and only the next use will beep, and it also does an eol-tracking thing only
;; when moving explicitly to the end of the line.

(defvar eli-track-eol nil)
(defvar eli-line-movement-without-dings nil)

(defun eli-previous-line (&optional arg try-vscroll)
  "Like `previous-line', but ding only if cannot move + do my eol tracking."
  (interactive "^p\np")
  (or arg (setq arg 1))
  (if (bobp)
    (unless eli-line-movement-without-dings (ding))
    (let ((p (point)))
      (line-move (- arg) t nil try-vscroll)
      (when (= p (point))
        (goto-char (point-min))
        (when (and (not eli-line-movement-without-dings) (= p (point)))
          (ding)))))
  (setq this-command 'previous-line)
  nil)
(put 'eli-previous-line 'CUA 'move)

(defun eli-next-line (&optional arg try-vscroll)
  "Like `next-line', but ding only if cannot move + do my eol tracking."
  (interactive "^p\np")
  (or arg (setq arg 1))
  (if (eobp)
    (unless eli-line-movement-without-dings (ding))
    (let ((p (point)))
      (line-move arg t nil try-vscroll)
      (when (= p (point))
        (goto-char (point-max))
        (when (and (not eli-line-movement-without-dings) (= p (point)))
          (ding)))))
  (setq this-command 'next-line)
  nil)
(put 'eli-next-line 'CUA 'move)

(defun eli-end-of-line (&optional arg)
  "Like `end-of-line', but makes further up/down movements stick to the eol."
  (interactive "^p")
  (unless arg (setq arg 1))
  (if line-move-visual (end-of-visual-line arg) (end-of-line arg))
  ;;Eli: This makes the cursor stay at the end-of-line (won't reset next-line)
  (setq temporary-goal-column (float (* (window-width) (frame-char-width))))
  (setq this-command 'next-line))
(put 'eli-end-of-line 'CUA 'move)

(defun eli-beginning-of-line (&optional arg)
  "Like `beginning-of-line'."
  (interactive "^p")
  (unless arg (setq arg 1))
  (if line-move-visual (beginning-of-visual-line arg) (beginning-of-line arg)))
(put 'eli-beginning-of-line 'CUA 'move)

(eval-after-load "dired"
  '(progn (define-key dired-mode-map
            [remap eli-next-line] 'dired-next-line)
          (define-key dired-mode-map
            [remap eli-previous-line] 'dired-previous-line)
          (put 'dired-next-line 'CUA 'move)
          (put 'dired-previous-line 'CUA 'move)))

;;-----------------------------------------------------------------------------
;; More utilities.

;; Backward transposing
(defun transpose-chars-backward (arg)
  "Same as (transpose-chars ARG), but backwards."
  (interactive "*P")
  (transpose-chars (- (prefix-numeric-value arg))))
(defun transpose-words-backward (arg)
  "Same as (transpose-words ARG), but backwards."
  (interactive "*P")
  (transpose-words (- (prefix-numeric-value arg))))
(defun transpose-lines-backward (arg)
  "Same as (transpose-lines ARG), but backwards."
  (interactive "*P")
  (transpose-lines (- (prefix-numeric-value arg))))
(defun transpose-sexps-backward (arg)
  "Same as (transpose-sexps ARG), but backwards."
  (interactive "*P")
  (transpose-sexps (- (prefix-numeric-value arg))))

(defun eli-join-line (&optional arg)
  "Like `join-line', but with a default argument of 1."
  (interactive "*p")
  (while (not (zerop arg))
    (let ((s (buffer-substring (save-excursion (forward-line 0) (point))
                               (save-excursion (forward-line 1) (1- (point))))))
      (join-line (if (< arg 0) nil 1))
      (cond ((bolp) (insert s)) ; join to an empty line: use previous spaces
            ((looking-at "[ \t]+$") (delete-horizontal-space)))
      (setq arg (+ arg (if (< arg 0) 1 -1))))))

(defun eli-next-buffer-acyclic (n)
  "Like `next-buffer', but not cyclic.
A prefix argument determines how many buffers to skip (default is 1), if
negative, count from the end."
  (interactive "p")
  (unless (equal n 0)
    ;; similar to the scan that `switch-to-prev-buffer' does
    (let* ((buffers (append (window-prev-buffers)
                            (buffer-list)
                            (window-next-buffers)))
           (buffers (if (and n (< n 0)) (reverse buffers) buffers))
           (n       (1- (abs (or n 1))))
           (curbuf  (current-buffer))
           (pred    (frame-parameter nil 'buffer-predicate))
           (found   nil))
      ;; This code is similar to the code in `get-next-valid-buffer',
      ;; specifically, the test for good buffers.
      (while (and (not found) buffers)
        (let* ((buf* (car buffers))
               (buf  (if (consp buf*) (car buf*) buf*))
               (good (and (not (eq curbuf buf))
                          (buffer-live-p buf)
                          (or (null pred) (funcall pred buf))
                          (not (eq (aref (buffer-name buf) 0) ?\s))
                          (or (and (consp buf*)
                                   switch-to-visible-buffer)
                              (null (get-buffer-window buf))))))
          (when good (if (zerop n) (setq found buf*) (setq n (1- n))))
          (setq buffers (cdr buffers))))
      (if found
        (if (consp found)
          (progn (switch-to-buffer (nth 0 found))
                 ;; do the above to get the buffer to the top of the list
                 (set-window-buffer-start-and-point
                  nil (nth 0 found) (nth 1 found) (nth 2 found)))
          (switch-to-buffer found))
        (error "Not enough buffers")))))

(defun eli-write-or-move-file (new)
  "Like `write-file', but with a prefix argument delete the original file."
  (interactive
   (let ((prompt (if (and current-prefix-arg buffer-file-name)
                   "Move to file: " "Write file: ")))
     (list
      (if buffer-file-name
        (read-file-name prompt nil nil nil nil)
        (read-file-name prompt default-directory
                        (expand-file-name (file-name-nondirectory (buffer-name))
                                          default-directory)
                        nil nil)))))
  (let ((old (buffer-file-name))
        (movep (and current-prefix-arg buffer-file-name)))
    (write-file new t)
    (when movep
      (setq new (buffer-file-name))
      (when (and old new (not (equal old new)))
        (delete-file old)
        (message "Moved to %s" new)))))

(defun eli-eval-last-sexp-or-region ()
  "Like `eval-last-sexp' or `eval-region', depending on an active mark."
  (interactive)
  (setq this-command (if mark-active 'eval-region 'eval-last-sexp))
  (call-interactively this-command)
  ;; this uses `progn', so we still get the last result
  ;; (if mark-active
  ;;   (eval-expression
  ;;    (car (read-from-string (concat "(progn\n"
  ;;                                   (buffer-substring-no-properties
  ;;                                    (region-beginning) (region-end))
  ;;                                   "\n)")))
  ;;    current-prefix-arg)
  ;;   (call-interactively 'eval-last-sexp))
  )

;; Skip identical parts of this buffer and the last one, good for merging.
(defun eli-compare-two-buffers (ignore-whitespace)
  "Similar to `compare-windows' but for last two buffers instead of windows."
  (interactive "P")
  (let (w1 w2 b1 b2 p)
    (save-window-excursion
      (delete-other-windows)
      (split-window-vertically)
      (setq b1 (current-buffer)
            w1 (selected-window)
            w2 (next-window (selected-window)))
      (select-window w2)
      (setq b2 (switch-to-buffer nil))
      (select-window w1)
      (compare-windows ignore-whitespace)
      (select-window w2)
      (setq p (point))
      (select-window w1))
    ;; why doesn't this work? --> (with-current-buffer b2 (goto-char p))
    (switch-to-buffer b2)
    (goto-char p)
    (switch-to-buffer b1)))

(defun eli-term ()
  "Like `term', but runs a shell immediately (prefix arg: ask like `term')."
  (interactive)
  (if current-prefix-arg
    (call-interactively 'term)
    (term (or (and (boundp 'explicit-shell-file-name)
                   (symbol-value 'explicit-shell-file-name))
              shell-file-name
              (getenv "ESHELL") (getenv "SHELL")
              "/bin/sh"))))

;;-----------------------------------------------------------------------------
;; A function to do local key settings that are truly local -- if the buffer
;; doesn't have its own private local map in `buffer-local-keymap' then make
;; one.

(defvar buffer-local-keymap nil)
(make-variable-buffer-local 'buffer-local-keymap)

(defun buffer-local-set-key (key command)
  "Same as `local-set-key' but ensures the current buffer has its own map."
  (interactive "KSet key buffer-locally: \nCSet key %s locally to command: ")
  (unless buffer-local-keymap
    (setq buffer-local-keymap (make-sparse-keymap))
    (when (current-local-map)
      (set-keymap-parent buffer-local-keymap (current-local-map)))
    (use-local-map buffer-local-keymap)
    (message "NOTE: buffer-local keymap in effect, %s"
             "local-key operations will use it from now"))
  (local-set-key key command))

(defun buffer-local-unset-all ()
  "Undoes all bindings that `buffer-local-set-key' did in this buffer."
  (interactive)
  (message (if (not buffer-local-keymap)
             "Wasn't using a local map."
             (progn (use-local-map (keymap-parent buffer-local-keymap))
                    (setq buffer-local-keymap nil)
                    "Removed `buffer-local' bindings."))))

;;-----------------------------------------------------------------------------
;; Better character-pair inserting

(defvar eli-insert-pair-last-position nil)

(defun eli-insert-pair (&optional arg open close)
  "Similar to `insert-pair', except better.
- default is to insert the character in both places
- negative prefix argument will use words instead of sexprs
- always inserts spaces around the new pair
- calling this a second time will use the previous call's location
- always deactivate the mark and put the cursor in the beginning
- wraps pair around the next *sexprs*, skipping comments and newlines
- more cases for spaces (comments, symbols, etc)"
  (interactive "P")
  (unless (and open close)
    (let* ((ch (event-basic-type last-command-event))
           (pair (or (assq last-command-event insert-pair-alist)
                     (assq ch insert-pair-alist))))
      (if pair
        (if (nth 2 pair)
          (setq open (nth 1 pair) close (nth 2 pair))
          (setq open (nth 0 pair) close (nth 1 pair)))
        (setq open ch close ch))))
  (let (beg end (arg (if arg (prefix-numeric-value arg) 0)))
    (cond ((and (eq last-command this-command) eli-insert-pair-last-position)
           (setq beg (car eli-insert-pair-last-position)
                 end (cdr eli-insert-pair-last-position)))
          ((use-region-p)
           (setq beg (region-beginning)
                 end (region-end)))
          ((> arg 0) (forward-sexp 1)
                     (forward-sexp -1)
                     (setq beg (point))
                     (forward-sexp arg)
                     (setq end (point)))
          ((< arg 0) (forward-word 1)
                     (forward-word -1)
                     (setq beg (point))
                     (forward-word arg)
                     (setq end (point)))
          (t (setq beg (point) end (point))))
    (goto-char end)
    (insert close)
    (when (and parens-require-spaces (not (eobp)) (not (eolp))
               (memq (char-syntax (following-char))
                     (cons (char-syntax open) '(?w ?_ ?\" ?\' ?<))))
      (insert " "))
    (goto-char beg)
    (when (and parens-require-spaces (not (bobp)) (not (bolp))
               (memq (char-syntax (preceding-char))
                     (cons (char-syntax close) '(?w ?_ ?\" ?<))))
      (insert " ")
      ;; adjust locations (could be done with markers, but this should work)
      (setq beg (1+ beg) end (1+ end)))
    (insert open)
    (setq beg (1+ beg) end (1+ end))
    (setq eli-insert-pair-last-position (cons beg end))))

;;-----------------------------------------------------------------------------
;; A useful counter thing

(defvar counter-value 1 "*Counter value for the counter commands.")
(defvar counter-format nil "*Counter format spec (a format string)")
(defvar counter-inserted nil "`nil' on the first use of the counter.")
(defvar counter-last-position nil)
(defvar counter-last-increment 1)

(defun counter-set (initial)
  "Set the counter to the INITIAL argument value.
Use with no arguments to be prompted for a value -- and when you
do that you can enter a value with spaces or zero padding, which
will be used by `counter-insert'."
  (interactive "P")
  (setq counter-value initial counter-format nil counter-inserted nil)
  (unless (integerp initial)
    (unless (stringp initial)
      (setq initial (read-from-minibuffer "New value (optional padding): ")))
    (unless (string-match "\\`\\( *\\|0*\\)\\(\\(?:[1-9][0-9]*\\)?[0-9]\\)\\'"
                          initial)
      (error "Bad initial value: %s." initial))
    (setq counter-format
          (let ((pad (match-string 1 initial)))
            (and (not (equal pad ""))
                 (format "%%%s%dd"
                         (if (eq ?0 (aref pad 0)) "0" "")
                         (length initial))))
          counter-value
          (string-to-number (match-string 2 initial))))
  (message "Counter set to %s%s." counter-value
           (if counter-format (concat " (format: " counter-format ")") "")))

(defun counter-increment (inc)
   "Increments the counter by the INC argument value, if given, or 1."
   (interactive "P")
   (cond ((eq '- inc) (setq inc (- counter-last-increment)))
         ((not (numberp inc)) (setq inc counter-last-increment)))
   (setq counter-value (+ counter-value inc)
         counter-last-increment inc)
   (message "Macro counter incremented by %s to %s" inc counter-value))

(defun counter-insert (inc)
  "Increment the counter value and insert it (using the current format, if any).
Increment by INC, defaults to 1 or to a last explicit increment.
\(Note: does not increment on first use.)"
  (interactive "*P")
  (when (and (eq this-command last-command) counter-last-position)
    (delete-region (point) counter-last-position))
  (setq counter-last-position (point))
  (when counter-inserted (counter-increment inc))
  (insert (format (or counter-format "%d") counter-value))
  (setq counter-inserted t))
(put 'counter-insert 'delete-selection t)

;;-----------------------------------------------------------------------------
;; Comment-line toggling

(defvar eli-comment-line-last-col nil)

(defun eli-toggle-comment-line (n)
  "Toggle a comment on current N line(s) (disable line by line)."
  (interactive "p")
  (if comment-start
    (let* ((end    (cond ((or (not comment-end) (equal comment-end "")) "")
                         ((string-match "^ " comment-end) comment-end)
                         (t (concat " " comment-end))))
           (start  (cond ((string-match " $" comment-start) comment-start)
                         ((and (= (length comment-start) 1) (equal end ""))
                          (concat comment-start comment-start " "))
                         (t (concat comment-start " "))))
           (qstart (regexp-quote start))
           (qend   (regexp-quote end))
           (again  (eq last-command 'eli-toggle-comment-line))
           (col    (and again eli-comment-line-last-col))
           (mode   (and again (if col 'comment 'uncomment))))
      (catch 'done
        (beginning-of-line)
        (while (>= (setq n (1- n)) 0)
          (when (eobp) (throw 'done nil))
          (skip-chars-forward "\t ")
          (unless (eolp)
            (unless mode
              (setq mode (if (looking-at (concat qstart "\\(.*\\)" qend "$"))
                           'uncomment 'comment)))
            (let ((cur (current-column)))
              (cond ((and col (< col cur))
                     (move-to-column col t))
                    ((eq mode 'comment)
                     (setq col cur))))
            (cond ((eq mode 'comment)
                   (insert start) (end-of-line) (insert end))
                  ((eq mode 'uncomment)
                   (when (looking-at (concat qstart "\\(.*\\)" qend "$"))
                     (replace-match "\\1" t)))))
          (forward-line 1)))
      (setq eli-comment-line-last-col col))
    (message "Comments not availavle for this mode")))

;;-----------------------------------------------------------------------------
;; A Borland-like unindenting backspace.

(defun eli-backward-delete-char-unindent (arg &optional killp)
  "This is similar to `backward-delete-char-untabify', except when activated
without arguments at the first non-whitespace character of a line (or at
an eol of a whitespace line) - in this case it will delete indentation up to
the last indentation level."
  (interactive "*p\nP")
  (let ((p (point)))
    (if (or killp (bolp) (window-minibuffer-p (selected-window))
            (save-excursion
              (beginning-of-line) (skip-chars-forward " \t") (/= p (point))))
      (backward-delete-char-untabify arg killp)
      (let ((col (current-column)) (delnum nil))
        (save-excursion
          (forward-line -1)
          (while (not (or delnum (bobp)))
            (skip-chars-forward " \t")
            (if (and (not (eolp)) (< (current-column) col))
              (setq delnum (- col (current-column)))
              (forward-line -1)))
          (when (bobp) (setq delnum col)))
        (if delnum
          (while (> delnum 0)
            (backward-delete-char-untabify 1)
            (setq delnum (1- delnum)))
          (backward-delete-char-untabify arg killp))))))
(put 'eli-backward-delete-char-unindent 'delete-selection 'supersede)

;;-----------------------------------------------------------------------------
;; Change the default font size

(defun eli-use-larger-font (n)
  (interactive "P")
  (let* ((n (cond ((eq n '-) -10) ((not n) 10) (t n)))
         (size1 (face-attribute 'default :height))
         (size2 (+ size1 n)))
    (set-face-attribute 'default nil :height size2)
    (message "Font size changed: %S --[%s%S]--> %S"
             size1 (if (> n 0) "+" "") n size2)))

(defun eli-use-smaller-font (n)
  (interactive "P")
  (let* ((n (cond ((eq n '-) -10) ((not n) 10) (t n))))
    (eli-use-larger-font (- n))))

;;-----------------------------------------------------------------------------
;; show tabs and extra spaces for files

;; this one is better:
(add-hook 'find-file-hooks 'whitespace-mode)
(eval-after-load "whitespace"
  ;; no indicator for this
  '(setcar (cdr (assq 'whitespace-mode minor-mode-alist)) nil))

;; (defun eli-turn-on-indicators ()
;;   (interactive)
;;   (let ((f (if (eq eli-text-indications t)
;;              (lambda (var sym)
;;                (set (make-variable-buffer-local var) t))
;;              (lambda (var sym)
;;                (when (memq sym eli-text-indications)
;;                  (set (make-variable-buffer-local var) t))))))
;;     (funcall f 'show-trailing-whitespace 'trailing-whitespaces)
;;     (funcall f 'x-stretch-cursor         'cursor-stretch)))
;; (add-hook 'find-file-hooks 'eli-turn-on-indicators)

;;-----------------------------------------------------------------------------
;; A variable & hook function to hide the local-variable section.

(defvar hide-local-variable-section nil
  "*Hide the local variables sections of a file.
This is used in the function `hide-local-variable-section' in `find-file' hook.
The purpose of this variable is to be set in the local variables section of a
file when you want this section to be hidden.")
(make-variable-buffer-local 'hide-local-variable-section)

(defun hide-local-variable-section ()
  "See the documentation for the variable `hide-local-variable-section'."
  (when hide-local-variable-section
    (let ((locals-start
           (save-excursion
             (goto-char (point-max))
             (search-backward "\n\^L" (max (- (point-max) 3000) (point-min))
                              'move)
             ;; the `\040' is so this line would not be found - otherwise when
             ;; this file is opened we get random error msgs
             (search-forward "Local\040Variables:" nil t)
             (forward-line 0)
             (1- (point)))))
      (narrow-to-region (point-min) locals-start))))

(add-hook 'find-file-hooks 'hide-local-variable-section)
(put 'hide-local-variable-section 'safe-local-variable 'booleanp)

;;-----------------------------------------------------------------------------
;; Dynamic completion (does nothing until used)

(unless (fboundp 'complete)
  (defun complete (&optional arg)
    "Wrapper for complete, will initialize completion when called"
    (interactive "*p")
    (require 'completion)
    (dynamic-completion-mode)
    (complete arg)))

;;-----------------------------------------------------------------------------
;; Convenient scrolling with settable number of lines for view mode

(defvar eli-View-scroll-page-lines nil)
(defun eli-View-scroll-page-forward (&optional lines)
  "Like `View-scroll-page-forward', but with a prefix argument determines the
number of lines to use for future calls.  Use a `C-u' prefix to go back to
full page scrolling."
  (interactive "P")
  (if (eq lines '-)
    (eli-View-scroll-page-backward nil)
    (progn (when lines
             (setq eli-View-scroll-page-lines (and (integerp lines) lines)))
           (View-scroll-page-forward eli-View-scroll-page-lines))))
(defun eli-View-scroll-page-backward (&optional lines)
  "Like `View-scroll-page-backward', but with a prefix argument determines the
number of lines to use for future calls.  Use a `C-u' prefix to go back to
full page scrolling."
  (interactive "P")
  (if (eq lines '-)
    (eli-View-scroll-page-forward nil)
    (progn (when lines
             (setq eli-View-scroll-page-lines (and (integerp lines) lines)))
           (View-scroll-page-backward eli-View-scroll-page-lines))))


;;; edit-utils.el ends here
