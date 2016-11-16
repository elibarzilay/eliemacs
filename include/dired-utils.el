;;; dired-utils.el --- Additional dired functionality.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(defun eli-bury-dired-buffer (buf)
  (bury-buffer buf)
  ;; avoid 'quit-restore going back there
  (set-window-parameter nil 'quit-restore nil)
  ;; make sure that it's not a prev-buffer too (for S-tab)
  (set-window-prev-buffers
   nil (filter (lambda (pb) (not (equal (car pb) buf)))
               (window-prev-buffers))))

(defvar eli-last-direds nil)
(add-hook 'dired-mode-hook
  (lambda ()
    (add-hook 'post-command-hook
      (lambda ()
        (let ((buf (current-buffer)))
          (unless (eq (car eli-last-direds) buf)
            (setq eli-last-direds
                  (cons buf (filter (lambda (b) (and (not (eq b buf))
                                                     (buffer-live-p b)))
                                    eli-last-direds))))))
      nil t)))

(defun eli-last-dired-buffer (n)
  "Switch to the Nth last dired buffer"
  (interactive "p")
  (unless eli-last-direds (user-error "No known dired buffers"))
  (let* ((buf     (current-buffer))
         (isdired (eq major-mode 'dired-mode))
         (revp    (< n 0))
         (direds  (if revp (reverse eli-last-direds) eli-last-direds))
         (n       (1- (abs n)))
         (buffers (if (eq buf (car direds)) `(,@(cdr direds) ,buf) direds)))
    (while (> n 0) (setq buffers (or (cdr buffers) eli-last-direds) n (1- n)))
    (switch-to-buffer (car buffers))
    ;; the above sets `eli-last-direds': override the value now
    (when isdired
      (setq eli-last-direds
            (if revp `(,buf ,@(remq buf eli-last-direds))
                     `(,@(remq buf eli-last-direds) ,buf)))
      (eli-bury-dired-buffer buf))))

(defun eli-next-dired-buffer (n)
  "Switch to the Nth next dired buffer"
  (interactive "p")
  (eli-last-dired-buffer (- n)))

;; Make navigation from a dired buffer to another bury the former, so we
;; mostly see less dired buffers cluttering the top of the buffer list

(dolist (fun '(dired-find-file dired-up-directory))
  (advice-add fun :around
    (lambda (orig &rest args)
      "When navigating between dired buffers, bury original"
      (interactive)
      (let ((buf1 (current-buffer))
            (ret  (apply orig args))
            (buf2 (current-buffer)))
        (when (and (eq major-mode 'dired-mode)
                   (buffer-live-p buf1)
                   (not (equal buf1 buf2)))
          (eli-bury-dired-buffer buf1))
        ret))))

;; Override from "dired-x.el": default suffix by the file on current line

(defun read-suffix-based-on-current-line (args verb)
  (cons (or (car args)
            (let* ((name (dired-get-filename 'no-dir t))
                   (ext  (save-match-data
                           (and (stringp name)
                                (string-match "[.][^./]+$" name)
                                (substring name (match-beginning 0))))))
              (or (read-from-minibuffer (format "%s extension: " verb) ext)
                  "")))
        (cdr args)))

(defun require-dired-x ()
  (require 'dired-x) (remove-hook 'dired-mode-hook 'require-dired-x))
(add-hook 'dired-mode-hook 'require-dired-x)

(eval-after-load "dired-x" '(progn
  (dolist (fun+verb '((dired-mark-extension "Marking")
                      (dired-flag-extension "Deleting")))
    (advice-add (car fun+verb) :filter-args
      (lambda (args) "Add default extension from current line."
        (interactive)
        (read-suffix-based-on-current-line args (cadr fun+verb)))))  ))

(eval-after-load "dired" '(progn
;; Override from "dired.el": delete dired buffers without questions
(defun dired-clean-up-after-deletion (fn)
  "Clean up after a deleted file or directory FN.
Removes any expanded subdirectory of deleted directory.
If `dired-x' is loaded and `dired-clean-up-buffers-too' is non-nil,
also offers to kill buffers visiting deleted files and directories."
  (save-excursion (and (cdr dired-subdir-alist)
		       (dired-goto-subdir fn)
		       (dired-kill-subdir)))
  ;; Offer to kill buffer of deleted file FN.
  (when (and (featurep 'dired-x) dired-clean-up-buffers-too)
    (let ((buf (get-file-buffer fn)))
      (and buf
           (funcall #'y-or-n-p
                    (format "Kill buffer of %s, too? "
                            (file-name-nondirectory fn)))
           (kill-buffer buf)))
    (let ((buf-list (dired-buffers-for-dir (expand-file-name fn))))
      (and buf-list     '; <-- Eli: skip the question
           (y-or-n-p (format "Kill Dired buffer%s of %s, too? "
                             (dired-plural-s (length buf-list))
                             (file-name-nondirectory fn)))
           (dolist (buf buf-list)
             (kill-buffer buf))))))))

;;; dired-utils.el ends here
