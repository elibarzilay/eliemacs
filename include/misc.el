;;; misc.el --- Misc stuff.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;; both are already the default, but better be safe
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(add-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom)

;; `q' removes the grep buffer, switch to the grep buffer when started
(add-hook 'grep-mode-hook
  (lambda ()
    (define-keys 'grep-mode-map
      '("q" (lambda () (interactive) (quit-window nil (selected-window)))))
    (setq truncate-lines t)
    (run-with-idle-timer 0 nil ; ugly hack...
      `(lambda () (select-window (get-buffer-window ,(current-buffer)))))))

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

;;; misc.el ends here
