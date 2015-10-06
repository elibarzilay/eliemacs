;;; macro-keys.el --- Convenient macro keys.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(defvar macro-key-record-key nil
  "When a macro-key is recorded this is the key vector, null otherwise.")
(defvar macro-key-record-keymap nil
  "When a macro-key is recorded this is the keymap it should be defined in.")
(defvar macro-key-record-name nil
  "When a macro-key is recorded, this is the key name.")
(defvar macro-key-record-mode nil
  "When a macro-key is recorded, this is 'global, 'local, or 'append.")
(defvar macro-key-saved-binding nil
  "When a macro-key is being recorded, this holds its previous binding.")
(defvar macro-key-saved-last-kbd-macro nil
  "When a macro-key is being recorded, this saves `last-kbd-macro's value.")
(defvar macro-key-last-key+mode nil
  "Last (key . mode) that we attempted to make a macro key.")

(defun macro-key-start-recording (key mode)
  ;; mode is 'global, 'local, or 'append
  (let* ((keyname (key-description key))
         (keymap  (cond ((eq mode 'local) (current-local-map))
                        ((eq mode 'global) (current-global-map))
                        (t nil)))
         (binding (if keymap (lookup-key keymap key) (key-binding key))))
    (unless (or (not binding)
                (arrayp binding) ; already a macro
                (eq binding 'self-recording-macro-key)
                (and (eq this-command last-command)
                     (equal (cons key mode) macro-key-last-key+mode)))
      (setq macro-key-last-key+mode (cons key mode))
      (error "`%s' is bound to `%s', repeat to rebind." keyname binding))
    (when (and (eq mode 'append) (not (arrayp binding)))
      (error "`%s' is not bound to a macro." keyname))
    (unless keymap
      (setq keymap
            (cond ((lookup-key (current-local-map)  key) (current-local-map))
                  ((lookup-key (current-global-map) key) (current-global-map))
                  (t (error "Could not find the binding of `%s'," keyname)))))
    (setq macro-key-last-key+mode        nil
          macro-key-record-key           key
          macro-key-record-name          keyname
          macro-key-record-keymap        keymap
          macro-key-record-mode          mode
          macro-key-saved-binding        binding
          macro-key-saved-last-kbd-macro last-kbd-macro
          last-kbd-macro                 (if (eq mode 'append) binding nil))
    (define-key keymap key 'end-kbd-macro)
    (add-hook 'post-command-hook 'macro-key-post-command)
    (if (eq mode 'append) (start-kbd-macro t t) (start-kbd-macro nil))
    (message "%s macro key `%s'..."
             (cond ((eq mode 'local)  "Recording local")
                   ((eq mode 'global) "Recording global")
                   ((eq mode 'append) "Appending to"))
             keyname)))

(defun macro-key-post-command ()
  (unless defining-kbd-macro (macro-key-done-recording)))

(defun macro-key-done-recording ()
  (remove-hook 'post-command-hook 'macro-key-post-command)
  (let ((errorp (not (and last-kbd-macro macro-key-record-key)))
        (name macro-key-record-name))
    ;; either bind to new macro, or reset to saved value
    (define-key macro-key-record-keymap macro-key-record-key
      (or last-kbd-macro macro-key-saved-binding))
    ;; restore state
    (setq macro-key-record-key nil
          last-kbd-macro       macro-key-saved-last-kbd-macro)
    (if errorp
      (progn (sit-for 1)
             (message "Macro %s aborted!" name)
             (beep)
             (sleep-for 0.2))
      (message
       (cond ((eq macro-key-record-mode 'local) "Local macro key `%s' recorded.")
             ((eq macro-key-record-mode 'global) "Macro key `%s' recorded.")
             ((eq macro-key-record-mode 'append) "Appended to `%s' macro key."))
       name))))

(defun macro-key-get-key+macro (for-what)
  (let* ((key  (eli-get-key (format "Keyboard macro to %s: " for-what)))
         (lval (local-key-binding key))
         (gval (and (not lval) (global-key-binding key))))
    (unless (arrayp (or lval gval))
      (error "`%s' is not bound to a keyboard macro" (key-description key)))
    (cons key
          (if lval (cons 'local-set-key lval) (cons 'global-set-key gval)))))

(autoload 'edmacro-format-keys "edmacro")

(defun macro-key-save-macro (key mac)
  (let ((buf (get-buffer-create "*Macro*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert ";; Grab this definition and put it somewhere like \"~/.emacs\"\n")
      (insert (format "(global-set-key (kbd %S) (read-kbd-macro %S))"
                      (edmacro-format-keys key)
                      (concat "\n " (format-kbd-macro mac)))))
    (display-buffer buf)))

(defun macro-key-menu ()
  (let* ((op (eli-get-key "[a]ppend [e]dit [d]ebug [s]ave "))
         (op (cond ((equal [?a] op) 'append)
                   ((equal [?e] op) 'edit)
                   ((equal [?d] op) 'debug)
                   ((equal [?s] op) 'save)
                   (t (error "Unknown choice"))))
         (x   (macro-key-get-key+macro op))
         (key (car x))
         (x   (cdr x))
         (set (car x))
         (mac (cdr x)))
    (require 'kmacro)
    (require 'edmacro)
    (cond ((equal op 'append) (macro-key-start-recording key 'append))
          ((equal op 'save) (macro-key-save-macro key mac))
          ((equal op 'edit)
           (setq last-kbd-macro mac)
           (edit-kbd-macro 'call-last-kbd-macro current-prefix-arg
                           `(lambda () (,set ,key last-kbd-macro))))
          ((equal op 'debug)
           (let ((last-kbd-macro mac))
             (call-interactively 'kmacro-step-edit-macro)
             (funcall set key last-kbd-macro)))
          (t (error "Unknown choice")))))

(defun self-recording-macro-key ()
  "Start a macro key recording for the key that invoked this.
Stop recording if this key is being recorded.
The macro key that gets recorded is always global."
  (interactive)
  (macro-key-start-recording (this-single-command-keys) current-prefix-arg))

(defun macro-key ()
  "Main entry point for recording and managing macro keys."
  (interactive)
  (cond (defining-kbd-macro (end-kbd-macro))
        (executing-kbd-macro (error "Cannot use `macro-key' from macros."))
        (t (let* ((localp current-prefix-arg)
                  (key (eli-get-key (format "%s to record (repeat for options): "
                                            (if localp "Local key" "Key")))))
             (if (eq this-command (key-binding key))
               (macro-key-menu)
               (macro-key-start-recording key (if localp 'local 'global)))))))

(add-hook 'emacs-startup-hook ; add a mode-line indicator for recorded key
          (lambda ()
            (let ((indicator
                   '(macro-key-record-name (" Def:" macro-key-record-name)
                                           " Def"))
                  (x (assq 'defining-kbd-macro minor-mode-alist)))
              (setcar (cdr x) indicator)))
          t)

;;; macro-keys.el ends here
