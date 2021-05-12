;;; desktop-init.el --- desktop settings.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(eval-when-compile (require 'desktop))

(defun eli-use-desktop (&optional no-load-buffers no-save-on-exit)
  (require 'desktop)
  (setq desktop-save 'ask-if-new
        desktop-load-locked-desktop t
        desktop-path '("." "~") ; maybe go back to just "~"?
        desktop-missing-file-warning nil
        desktop-file-name-format 'absolute
        desktop-restore-frames (not no-load-buffers)
        ;; would be nice to use some lazy loading, but if Emacs exits before
        ;; the lazy loading, then these buffers are lost
        desktop-restore-eager t
        desktop-lazy-verbose t
        desktop-lazy-idle-delay 5
        ;;
        desktop-globals-to-save
          (nconc '((extended-command-history .  60)
                   (file-name-history        . 100)
                   (grep-history             .  30)
                   (minibuffer-history       .  50)
                   (query-replace-history    .  60)
                   (read-expression-history  .  60)
                   (regexp-history           .  60)
                   (shell-command-history    .  50)
                   (yes-or-no-p-history      .   2)
                   (faces-history            .  40))
                 (delq 'file-name-history desktop-globals-to-save))
        desktop-locals-to-save
          (nconc '(word-wrap line-move-visual text-scale-mode-amount)
                 desktop-locals-to-save)
        desktop-minor-mode-handlers
          (nconc '((text-scale-mode
                    . (lambda (locals)
                        (require 'face-remap)
                        (text-scale-mode)
                        (let ((amt (assq 'text-scale-mode-amount locals)))
                          (when amt (text-scale-set (cdr amt)))))))
                 desktop-minor-mode-handlers)
        desktop-buffers-not-to-save ; skip " *" names & completions
          (concat "\\(^[ ]\\|^.completions$\\|\\(?:"
                  ;; the next line is the default
                  "^/[^/:]*:"
                  "\\)\\)"))
  (when no-load-buffers
    (fset 'desktop-create-buffer (lambda (&rest _) nil))
    (setq desktop-restore-frames nil)
    (setq desktop-buffers-not-to-save-function (lambda (&rest _) nil)))
  (when no-save-on-exit
    (remove-hook 'kill-emacs-hook 'desktop-kill)
    (setq desktop-auto-save-timeout nil))
  (desktop-save-mode 1)
  (add-hook 'emacs-startup-hook 'desktop-read))

;;; desktop-init.el ends here
