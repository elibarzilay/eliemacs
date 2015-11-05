;;; show-buffers.el --- Buffer menu stuff.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(defun eli-show-buffers ()
  "Show an interactive buffer list.  (Uses `bs'.)
Consecutive uses of this command (right after the list is shown) changes the
list configuration, and if used in the list (but not consecutive) quit it."
  (interactive)
  (cond ((not (eq major-mode 'bs-mode))
         (setq bs-default-configuration "files+"
               bs-default-sort-name "by nothing")
         (bs-show nil))
        ((eq last-command this-command)
         (bs-select-next-configuration))
        (t (bs-kill))))

(setq bs-configurations ; simple two options
      '(; name show-rx show-fun dont-show-rx dont-show-fun sort-fun
        ("all" nil nil nil nil nil)
        ;; because there must be some dont-show, so a bogus function
        ("files+" nil nil nil eli-bs-hide-buffers nil))
      bs-string-current        ">"
      bs-string-marked         "*"
      bs-string-current-marked "#"
      bs-string-show-normally  "."
      bs-string-show-always    "+"
      bs-string-show-never     "-"
      bs-minimal-buffer-name-column 12)

(defvar eli-bs-show-buffers-rx
  (concat "^[*]"
          (regexp-opt '("scratch" "shell"))
          "\\(?: *[0-9]*\\)?[*]$"))
(defun eli-bs-hide-buffers (buf)
  (not (or (buffer-file-name buf)
           (string-match-p eli-bs-show-buffers-rx (buffer-name buf))
           (with-current-buffer buf
             (and (boundp 'dired-subdir-alist) dired-subdir-alist)))))

(defun eli-bs-post-command ()
  (when (< (point) (eli-bs--top-point)) (goto-char (eli-bs--top-point))))

(defvar-local eli-bs-header-end nil)
(defun eli-bs--top-point ()
  (unless eli-bs-header-end
    (save-excursion (goto-char (point-min))
                    (forward-line bs-header-lines-length)
                    (setq eli-bs-header-end (point))))
  eli-bs-header-end)

(setq bs-attributes-list
      '((""       1  1 left  bs--get-marked-string)
        ("M"      1  1 left  eli-bs--get-modified-string)
        ("R"      2  2 left  eli-bs--get-readonly-string)
        ("Buffer" bs--get-name-length 10 left eli-bs--get-name)
        (""       1  1 left  " ")
        ("Size"   5  5 right eli-bs--get-size-string)
        (""       1  1 left  " ")
        ("Mode"  12 12 right eli-bs--get-mode-name)
        (""       2  2 left  "  ")
        ("File"  12 12 left  eli-bs--get-file-name)
        (""       2  2 left  "  ")))
(defun eli-bs--get-modified-string (start-buffer all-buffers)
  (let ((str (bs--get-modified-string start-buffer all-buffers)))
    (if (string-match-p str "^ *$") str
        (propertize str 'font-lock-face (eli-bs--get-face)))))
(defun eli-bs--get-readonly-string (start-buffer all-buffers)
  (let ((str (bs--get-readonly-string start-buffer all-buffers)))
    (if (string-match-p str "^ *$") str
        (propertize str 'font-lock-face (eli-bs--get-face)))))
(defun eli-bs--get-name (start-buffer all-buffers)
  (propertize (bs--get-name start-buffer all-buffers)
              'font-lock-face (eli-bs--get-face)))
(defun eli-bs--get-size-string (_start-buffer _all-buffers)
  (replace-regexp-in-string "^\\([0-9][0-9][0-9]+\\)\\.[0-9]+" "\\1"
                            (file-size-human-readable (buffer-size))))
(defun eli-bs--get-mode-name (start-buffer all-buffers)
  (propertize (bs--get-mode-name start-buffer all-buffers)
              'font-lock-face (simple-make-face 'italic)))
(defun eli-bs--get-file-name (start-buffer all-buffers)
  (propertize (bs--get-file-name start-buffer all-buffers)
              'font-lock-face (eli-bs--get-face)))
(defun eli-bs--get-face ()
  (simple-make-face
   (cond ((eq major-mode 'dired-mode) 'h9ef/h600-bold)
         ((eq major-mode 'wdired-mode)
          (if (buffer-modified-p) 'h6f7/ha00-bold 'h6f7/h600-bold))
         (buffer-read-only
          (if (buffer-modified-p) 'hfed/ha00-bold 'hfed/h600-bold ))
         (t (if (buffer-modified-p) 'hff6/ha00-bold 'hff6/h600-bold)))))

(eval-after-load "bs" '(progn

(add-hook 'bs-mode-hook
  (lambda ()
    (hl-line-mode)
    (add-hook 'post-command-hook 'eli-bs-post-command nil t)
    (font-lock-mode t)))

(setq bs-mode-font-lock-keywords
  `((,(concat (bs--make-header-match-string) "\n")
     0 ',(simple-make-face '*/h333-bold) append)
    ("^>.*\n?" 0 ',(simple-make-face '*/h030) append)
    ("^#.*\n?" 0 ',(simple-make-face '*/h080) append)
    ("^[*].*\n?" 0 ',(simple-make-face '*/h060) append)))

;; Better bindings
(define-keys bs-mode-map
  '("<up>" nil) '("<down>" nil) ; use regular bindings, no cycle
  '("j" eli-next-line) '("k" eli-previous-line) ; gmail-like
  '("C-d" bs-delete)            ; not backwards
  '("D"   bs-delete-backward)   ; this makes sense to go back
  '("<backspace>" bs-delete-backward) '("<delete>" bs-delete) ; useful
  '("." bs-toggle-current-to-show) ; easier to remember
  )

;; override: no dash underlines
(defun bs--show-header ()
  "Insert header for Buffer Selection Menu in current buffer."
  (insert (bs--create-header-line #'identity) "\n"))
(setq bs-header-lines-length 1)

;; override: first make it shown (more common)
(defun bs-toggle-current-to-show ()
  "Toggle status of showing flag for buffer in current line."
  (interactive)
  (let ((res
         (with-current-buffer (bs--current-buffer)
           (setq bs-buffer-show-mark
                 (pcase bs-buffer-show-mark
                   (`nil 'always) (`always 'never) (_ nil))))))
    (bs--update-current-line)
    (bs--set-window-height)
    (bs--show-config-message res)))

;; override: plain next-line, no cycles
(defun bs--mark-unmark (count fun)
  "Call FUN on COUNT consecutive buffers of *buffer-selection*."
  (let ((dir (if (> count 0) 1 -1)))
    (catch 'bs-done
      (dotimes (_i (abs count))
        (let ((buffer (bs--current-buffer)))
          (when buffer (funcall fun buffer))
          (bs--update-current-line)
          (forward-line dir)
          (when (if (< dir 1)
                  (< (point) (eli-bs--top-point))
                  (>= (point) (point-max)))
            (throw 'bs-done nil)))))
    (forward-line 0)))

))

;;; show-buffers.el ends here
