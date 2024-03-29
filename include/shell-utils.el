;;; shell-utils.el --- Shell related utilities
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;;-----------------------------------------------------------------------------
;; Friendlier `shell' and `shell-command'

(defvar eli-shell-default-command nil
  "Default shell name used by `eli-shell', or `nil' to use whatever
default thing `shell' runs (`shell-file-name').
Setting this makes it possible to have a specific shell for interactions
while maintaining another for normal uses (still `shell-file-name').
Note that it's used via `explicit-shell-file-name', so to set arguments
different than the default \"-i\" you need to set `explicit-<name>-args'.")

(defvar-local eli-shell-current-executable nil
  "Buffer-local value indicating the name of the executable that is used
for the current eli-shell run.")

(defvar eli-last-shell nil)
(defvar eli-last-shells nil)
(defun eli-shell (arg)
  "Similar to `shell' but extended as follows:
- a positive numeric argument means jump to that shell window,
  with 1 being the default \"*shell*\", 2 is \"*shell*<2>\" etc,
- a 0 numeric argument will jump to a new shell window (the next
  one), without asking about the name,
- no argument means jump to the last visited shell window, if it's dead the
  one visited before that, etc,
- with a `C-u' prefix, just call `shell',
- it switches to the buffer as usual, in the current window: no
  switching to a half-screen, and preserve the buffer visit
  history so there's no inconsistent buffer switching
  later."
  (interactive "P")
  (require 'shell) ; for `explicit-shell-file-name' below
  (let* ((ex-name (and explicit-shell-file-name
                       (replace-regexp-in-string "\\.exe$" ""
                         (downcase (file-name-nondirectory
                                    explicit-shell-file-name)))))
         (name (if ex-name (format "*shell: %s*" ex-name) "*shell*"))
         (s (cond ((eq arg 0) (generate-new-buffer-name name))
                  ((eq arg 1) name)
                  ((and (integerp arg) (> arg 1)) (format "%s<%S>" name arg))
                  ((not arg)
                   (let ((s nil) (p (assoc ex-name eli-last-shells)))
                     (while (and (not s) (cdr p))
                       (if (get-buffer (cadr p))
                         (setq s (cadr p))
                         (setcdr p (cddr p))))
                     (or s name)))
                  (t nil)))
         (isnew (and s (not (get-buffer s)))))
    (let ((explicit-shell-file-name (or explicit-shell-file-name
                                        eli-shell-default-command)))
      (if s (shell s) (call-interactively 'shell))
      (setq eli-shell-current-executable explicit-shell-file-name))
    (when isnew
      (setq-local truncate-lines t)
      (setq-local bs-buffer-show-mark 'always))
    (add-hook 'post-command-hook
              (lambda ()
                (unless (equal eli-last-shell (buffer-name))
                  (setq eli-last-shell (buffer-name))
                  (unless (assoc ex-name eli-last-shells)
                    (setq eli-last-shells
                          (cons (cons ex-name nil) eli-last-shells)))
                  (let ((p (assoc ex-name eli-last-shells)))
                    (unless p (setq p (list ex-name)) (push p eli-last-shells))
                    (setcdr p (cons eli-last-shell
                                    (remove eli-last-shell (cdr p)))))))
              nil t)))

;; comint.el sets $EMACS to "t" unless it's set, which messes up running
;; makefiles inside emacs.  Prevent that by setting it here.
(unless (getenv "EMACS") (setenv "EMACS" "emacs"))

(declare-function shell-mode "shell" ())
(declare-function dired-get-filename ; used below
                  "dired" (&optional localp no-error-if-not-filep))
(defun eli-shell-command (command &optional output-buffer error-buffer)
  "Similar to `shell-command' but sets convenient environment variables when
the command runs if the current buffer is associated with a file, and use the
current selected region (if active) as stdin.  Also, shows the pwd and the
command in the buffer list.

The environment variables set are:

  $f, $F, $Fh: current file name, full path, relative to $HOME
               (full path if it's not in $HOME)
               The value is taken from `buffer-file-name'

  $D, $Dh:     full directory name, relative to $HOME (same as above)
               The value is taken from `buffer-file-name' or
               `default-directory'.

Notes:
- in dired mode only the directory paths are set
- behaves as if `async-shell-command-buffer' is 'new-buffer"
  (interactive ; args and this are the same as in `shell-command'
   (list
    (read-shell-command
     (if (use-region-p) "Shell command on region: " "Shell command: ")
     nil nil
     (let ((filename (or buffer-file-name
                         (and (eq major-mode 'dired-mode)
                              (dired-get-filename nil t)))))
       (and filename (file-relative-name filename))))
    current-prefix-arg
    shell-command-default-error-buffer))
  (let* ((process-environment process-environment) ; restore the env later
         (path (buffer-file-name))
         (file (and path (file-name-nondirectory path)))
         (ddir default-directory)
         (dir  (if path (file-name-directory path) ddir))
         (HOME (getenv "HOME"))
         (command0 command)
         (command (replace-regexp-in-string "[ \t]*&[ \t]*\\'" "" command0))
         (async-p (not (equal command command0)))
         (region-p (use-region-p))
         (obuf (cond ((stringp output-buffer) output-buffer)
                     ((bufferp output-buffer) output-buffer)
                     ((not output-buffer) (if async-p "*Async Shell Command*"
                                              "*Shell Command Output*"))
                     (t (barf-if-buffer-read-only) (current-buffer)))))
    (setenv "f"  file)
    (setenv "F"  path)
    (setenv "Fh" (and path (file-relative-name path HOME)))
    (setenv "D"  dir)
    (setenv "Dh" (file-relative-name dir HOME))
    (when (and async-p region-p)
      (message-sit 1 "Cannot run an async command on selection, ignoring it")
      (setq region-p nil))
    (unless (bufferp obuf)
      (setq obuf (get-buffer-create
                  (if (not (and async-p (get-buffer-process obuf))) obuf
                      (generate-new-buffer obuf)))))
    (let* ((win (selected-window)) (prev (window-prev-buffers win)))
      (prog1 (if async-p
               (save-window-excursion ; messes up: shows it twice, display below
                 ;; (shell-command command obuf error-buffer)
                 ;; => copied & modified from the async part of `shell-command`
                 (with-current-buffer obuf
                   (setq buffer-read-only nil)
                   (unless output-buffer
                     (let ((inhibit-read-only t)) (erase-buffer)))
                   (display-buffer obuf)
                   ;; this is probably done in case of preexisting
                   ;; output buffer in a different directory
                   (setq default-directory ddir)
                   (push-mark (point) 'nomsg)
                   (let ((proc (start-process "Shell" obuf shell-file-name
                                              shell-command-switch command)))
                     (setq mode-line-process '(":%s"))
                     (unless output-buffer (require 'shell) (shell-mode))
                     (set-process-sentinel proc 'shell-command-sentinel)
                     (set-process-filter proc 'comint-output-filter)
                     ;; this is needed, otherwise comint barfs
                     (setq-local comint-last-output-start (make-marker))
                     ;; this makes the output appear at the current point
                     (set-marker (process-mark proc) (point)))))
               (let ((max-mini-window-height 1) ; make it more predictable
                     (resize-mini-windows t)
                     (range (if (not region-p) (list (point) (point))
                                (list (region-beginning) (region-end)))))
                 (shell-command-on-region
                  (car range) (cadr range) command
                  obuf current-prefix-arg error-buffer)))
        (with-current-buffer obuf
          (setq default-directory dir) ; make sure we're in the right directory
          (setq list-buffers-directory ; show "<dir>$ <cmd>" in buffer lists
                (concat (replace-regexp-in-string
                         "/?\\'" "" (abbreviate-file-name dir))
                        "$ " command))
          (unless output-buffer (view-mode 1)))
        (when async-p
          (display-buffer obuf)
          ;; the window is not selected, so avoid switching to it if we
          ;; use `delete-other-windows'
          (set-window-prev-buffers win prev))))))

(defun eli-shell-command-insert-output ()
  "Similar to `eli-shell-command' with output inserted in the current buffer
(i.e., using it with a prefix argument), but does not replace the selected
text.

(This is currently a hack that needs to be robustified.)"
  (interactive)
  (let ((current-prefix-arg nil)
        (wconf (current-window-configuration)))
    (when (< (point) (mark t)) (exchange-point-and-mark))
    (call-interactively 'eli-shell-command)
    (insert-buffer-substring "*Shell Command Output*")
    (set-window-configuration wconf)
    (activate-mark 'lambda)))

;;-----------------------------------------------------------------------------
;; * Make it possible for comint to send input immediately.
;; * Convenient C-up/C-down keys for history or navigation in comint

(defun comint-send-now ()
  "Send everything written so far immediately."
  (interactive)
  ;; Note that the input string does not include its terminal newline.
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc)
      (error "Current buffer has no process")
      (let* ((pmark (process-mark proc))
             (input (progn (goto-char (point-max))
                           (buffer-substring pmark (point)))))
        (when comint-process-echoes
          (delete-region pmark (point)))
        (run-hook-with-args 'comint-input-filter-functions input)
        (set-marker comint-last-input-start pmark)
        (set-marker comint-last-input-end (point))
        (set-marker (process-mark proc) (point))
        (set-marker comint-accum-marker nil)
        (comint-send-string proc input)
        (run-hook-with-args 'comint-output-filter-functions "")))))

(defun comint-quoted-send ()
  "Read a character the same way as quoted-insert does, then send it
immediately to the process using `comint-send-now' (so previous input
must be sent as well)."
  (interactive)
  (message "Enter a character...")
  (insert-and-inherit (read-quoted-char))
  (comint-send-now))

;; see also override for comint-previous-matching-input-from-input that
;; leaves point at the end of the line.
(defun comint-previous-matching-input-from-input-or-scroll (n)
  (interactive "p")
  (if (comint-after-pmark-p)
    (progn (setq this-command 'comint-previous-matching-input-from-input)
           (comint-previous-matching-input-from-input n))
    (scroll-down-1-stay n)))
(defun comint-next-matching-input-from-input-or-scroll (n)
  (interactive "p")
  (if (comint-after-pmark-p)
    (progn (setq this-command 'comint-next-matching-input-from-input)
           (comint-next-matching-input-from-input n))
    (scroll-up-1-stay n)))

;; the field-restricted motion is useful only on the current prompt line
(defun eli-comint-beginning-of-line (&optional arg)
  "Like `beginning-of-line'."
  (interactive "^p")
  (unless arg (setq arg 1))
  (let* ((p (get-buffer-process (current-buffer)))
         (p (and p (process-mark p)))
         (inhibit-field-text-motion
          (not (and p (>= (point) (marker-position p))))))
    (if line-move-visual
      (beginning-of-visual-line arg)
      (beginning-of-line arg))))
(defun eli-comint-end-of-line (&optional arg)
  "Like `end-of-line'."
  (interactive)
  (call-interactively 'eli-end-of-line)
  (unless arg (setq arg 1))
  (let* ((p (get-buffer-process (current-buffer)))
         (p (and p (process-mark p)))
         (inhibit-field-text-motion
          (not (and p (>= (point) (marker-position p))))))
    (if line-move-visual
      (end-of-visual-line arg)
      (end-of-line arg))))

(defun eli-comint-is-output (p)
  (memq (get-text-property p 'field) '(output boundary)))
(defun eli-comint-go-to-beginning-of-input ()
  (let ((p (point)))
    (when (and (> p (point-min))
               (not (eli-comint-is-output p))
               (not (eli-comint-is-output (1- p))))
      (setq p (previous-single-property-change (point) 'field))
      (when p (goto-char p)))))
(defun eli-comint-clear-except-command ()
  "Erase buffer, leaving the current command-line, and/or copy an old command
if the cursor is on one."
  (interactive)
  (let* ((p (or (get-buffer-process (current-buffer))
                (user-error "Current buffer has no process")))
         (p (and p (process-mark p))))
    (when (and p (< (point) (marker-position p)))
      (if (and (eli-comint-is-output (point))
               (or (<= (point) (point-min))
                   (eli-comint-is-output (1- (point)))))
        (goto-char (point-max))
        (comint-copy-old-input)))
    (let ((inhibit-modification-hooks t))
      (delete-region
       (point-min)
       (save-excursion
         (eli-comint-go-to-beginning-of-input)
         (forward-line 0) (point))))))
(defun eli-comint-clear-previous-lines (&optional arg)
  "Erase buffer lines before point.

With a positive prefix argument, leave that many lines before the current.

Any other pprefix argument: clear the whole buffer."
  (interactive "P")
  (let ((n (or arg 0)))
    (if n
      (let ((inhibit-modification-hooks t))
        (delete-region
         (point-min)
         (save-excursion
           (eli-comint-go-to-beginning-of-input)
           (forward-line (- n)) (point))))
      (eli-comint-clear-except-command))))

(with-eval-after-load "comint"
  (define-keys comint-mode-map
    '("M-q"           comint-quoted-send)
    '("<C-up>"        comint-previous-matching-input-from-input-or-scroll)
    '("<C-down>"      comint-next-matching-input-from-input-or-scroll)
    '("<home>"        eli-comint-beginning-of-line)
    '("<end>"         eli-comint-end-of-line)
    '("<S-backspace>" eli-comint-clear-previous-lines)))

;;; shell-utils.el ends here
