;;; misc.el --- Misc stuff.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;; both are already the default, but better be safe
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(add-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom)

;; Replace the true home directory name by the HOME environment value, this is
;; necessary for automounters that map some name to another
;; (automount-dir-prefix doesn't help much).
(unless (or (eq 'windows-nt system-type)
            (equal (file-truename (getenv "HOME")) (getenv "HOME")))
  (add-to-list 'directory-abbrev-alist
               (cons (concat "^" (file-truename (getenv "HOME")))
                     (getenv "HOME"))))

;; Set `abbreviated-home-dir' on windows (hack: rely on its value, it would
;; break if this value already has ranges).
(when (eq 'windows-nt system-type)
  (if (string-match "\\[" abbreviated-home-dir)
    (warn "looks like `abbreviated-home-dir' has a range, not hacking it")
    (setq abbreviated-home-dir
          (replace-regexp-in-string "/" "[/\\\\]" abbreviated-home-dir))))

;; get user <-> homedir mappings from /etc/passwd
(defvar eli-user-homedirs
  ;; taken from "mailalias.el"
  (when (file-readable-p "/etc/passwd")
    (with-current-buffer (generate-new-buffer " passwd")
      (insert-file-contents "/etc/passwd" nil nil nil t)
      (goto-char (point-min))
      (let ((r nil))
        (while (not (eobp))
          ;;Recognize lines like
          ;;  nobody:*:65534:65534::/:
          ;;  +demo::::::/bin/csh
          ;;  +ethanb
          ;;while skipping
          ;;  +@SOFTWARE
          ;; Match the username and home dir
          (if (looking-at (concat "[+-]?\\([^:@\n+]+\\):[^:\n]*:[^:\n]*"
                                  ":[^:\n]*:[^:\n]*:\\([^\n:]+\\):"))
            (setq r (cons (cons (match-string 1) (match-string 2)) r)))
          (beginning-of-line 2))
        (kill-buffer (current-buffer))
        ;; remove users that have a "/" homedir, and sort
        (sort (filter (lambda (x) (not (equal "/" (cdr x)))) r)
              (lambda (x y) (string-lessp (car x) (car y)))))))
  "Alist of local user -- homedir mapping.  t = uninitialized.")

(dolist (u-h (sort (append eli-user-homedirs '())
                   ;; add in reverse, eg, when ~plt = ~scheme/plt
                   (lambda (x y) (string-lessp (cdr x) (cdr y)))))
  ;; avoid masking root out, and no need for ~eli
  (unless (or (equal "/" (cdr u-h)) (equal user-login-name (car u-h)))
    (add-to-list 'directory-abbrev-alist
                 (cons (concat "^" (cdr u-h) "\\(/\\|$\\)")
                       (concat "~" (car u-h) "/")))))

;; dired stuff
(add-hook 'dired-mode-hook (lambda () (hl-line-mode)))

;;-----------------------------------------------------------------------------
;; Make display-buffer on some buffers pop into the buffer

;; Using a custom action that does a pop-to, and allow some convenient
;; customizations
(defvar in-display-buffer-pop-to nil)
(defun display-buffer-pop-to (buf alist)
  (and (not in-display-buffer-pop-to)
       (let ((in-display-buffer-pop-to t))
         (pop-to-buffer buf)
         (and (eq (current-buffer) (get-buffer buf))
              (with-current-buffer buf
                (let ((tl (assq 'truncate-lines alist)))
                  (when tl (setq truncate-lines (cdr tl))))
                t)))))

(add-to-list 'display-buffer-alist
  '("^[*]shell\\(?:: .+?\\)?[*]\\(?:<[0-9]+>\\)?$"
    (display-buffer-reuse-window display-buffer-same-window)))
(add-to-list 'display-buffer-alist
  '("^[*]Shell Command Output[*]$" display-buffer-pop-to (truncate-lines . t)))
(add-to-list 'display-buffer-alist
  '("^[*]Async Shell Command[*]" display-buffer-pop-up-window))
(add-to-list 'display-buffer-alist
  '("^[*]grep[*]" display-buffer-pop-to (truncate-lines . t)))

;; Also, in grep, turn on always-kill
(add-hook 'grep-setup-hook (lambda () (setq-local compilation-always-kill t)))

;;; misc.el ends here
