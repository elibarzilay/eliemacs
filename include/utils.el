;;; utils.el --- General utilities
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;; from v23 there is `ignore-errors'.
(defmacro no-errors (&rest body)
  "Ignore errors in BODY."
  `(condition-case nil (progn ,@body) (error nil)))
(defmacro no-errors-beep (&rest body)
  "Ignore errors in BODY, but beep if there were."
  `(condition-case nil (progn ,@body) (error (beep))))

(defun filter (pred list)
  "Returns the list of values from LIST that PRED verifies."
  (let ((r nil))
    (while list
      (when (funcall pred (car list)) (push (car list) r))
      (setq list (cdr list)))
    (nreverse r)))

(defun findif (pred list &optional default)
  "Finds the first element that satisfies PRED in LIST, or DEFAULT if none."
  (catch 'done
    (dolist (x list)
      (when (funcall pred x) (throw 'done x)))
    default))

;; Replace strings & regexps the quick way
(defun eli-replace (from to)
  "Replace FROM to TO in the current buffer (for use in elisp)."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (search-forward from nil t) (replace-match to nil t)))))
(defun eli-replace-regexp (from to)
  "Replace FROM to TO in the current buffer (for use in elisp)."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (re-search-forward from nil t) (replace-match to nil nil)))))

;; Useful for debugging and more
(defun message-sit (&optional sit &rest args)
  "Same as `message', but a first argument specifies how much time to sit after
the message.  If it is negative, sit for that time and then hide the message."
  (cond ((not sit) (setq sit -1))
        ((stringp sit) (setq args (cons sit args) sit -1)))
  (apply 'message args)
  (sit-for (abs sit))
  (when (< sit 0) (message nil)))

(defun eli-get-key (prompt &optional not-this-command)
  "Like `read-key-sequence-vector', but arranges for the cursor to be in the
echo area, does the usual thing for C-g, and possibly returns nil if you use the
same key as for `this-command'."
  (let* ((cursor-in-echo-area t)
         (key (read-key-sequence-vector prompt nil t))
         (bind (key-binding key)))
    (message nil)
    (cond ((and not-this-command (eq bind this-command)) nil)
          ((eq bind 'keyboard-quit) (keyboard-quit))
          (t key))))

;; A convenient function to define and redefine many keys at once.
(defun define-keys (&rest keys/funcs-or-maps)
  "Define many keys.
The KEYS/FUNCS-OR-MAPS arguments are a list of:
  - (KEY FUNC)    - bind KEY to function FUNC
  - (FUNC1 FUNC2) - replace FUNC1 globally-bound keys with FUNC2 in the map
  - (FUNC1 FUNC2 t) - same but replace locally bound FUNC1 keys
  - keymap - use this keymap from now on.
  - keymap-symbol - a symbol that should evaluate to a keymap, 'global or
    'local (error if the symbol's value is not a keymap).
  - nil - silently ignored."
  (let ((keymap (current-global-map)) x)
    (while keys/funcs-or-maps
      (setq x (car keys/funcs-or-maps)
            keys/funcs-or-maps (cdr keys/funcs-or-maps))
      (cond
        ((null x) nil) ; this allow nils to appear (good for conditionals)
        ((keymapp x) (setq keymap x))
        ((symbolp x) (setq keymap (cond ((eq x 'global) (current-global-map))
                                        ((eq x 'local) (current-local-map))
                                        ((boundp x) (eval x))
                                        (t nil)))
         (when (and keymap (not (keymapp keymap)))
           (error "%S did not evaluate to a keymap" x)))
        ;; makes keymap symbols that are unbound have no effect
        ((null keymap) nil)
        ((and (listp x) (symbolp (car x)) (commandp (car x)))
         (substitute-key-definition
          (car x) (cadr x) keymap
          (if (eq t (nth 2 x)) keymap (current-global-map))))
        ((listp x) (apply 'define-key keymap x))))))

;;; utils.el ends here
