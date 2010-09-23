;;; desktop-init.el --- desktop settings.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(defun eli-use-desktop (load-buffers)
  (require 'desktop)
  (unless load-buffers ; avoid loading buffers, don's save the desktop
    (fset 'desktop-create-buffer (lambda (&rest _) nil))
    (remove-hook 'kill-emacs-hook 'desktop-kill))
  (setq desktop-save 'ask-if-new
        desktop-load-locked-desktop t
        desktop-path '("." "~") ; maybe go back to just "~"?
        desktop-missing-file-warning nil
        desktop-file-name-format 'absolute
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
                 desktop-globals-to-save)
        desktop-locals-to-save
          (nconc '(word-wrap line-move-visual) desktop-locals-to-save)
        desktop-buffers-not-to-save ; skip " *" names & completions
          (concat "\\(^[ ]\\|^.completions$\\|\\(?:"
                  ;; the next line is the default
                  "^/[^/:]*:"
                  "\\)\\)"))
  (desktop-save-mode 1))

(add-hook 'emacs-startup-hook
  (lambda ()
    ;; Decide whether to load the previous desktop or not
    (if (member "--fast" command-line-args)
      ;; not using desktop at all
      (setq command-line-args (delete "--fast" command-line-args))
      ;; otherwise, the question is whether we load buffers or not (so desktop
      ;; is always used here)
      (let (;; ask about restoring desktop only if there are no command-line
            ;; arguments left (ignore the first -- it is the executable)
            (load-buffers (null (cdr command-line-args))))
        (when load-buffers
          (let* ((key (eli-logo
                       (concat "Hit any key to continue, escape/right-click:"
                               " don't load previous desktop.")))
                 (key (if (integerp key) key (event-basic-type key))))
            (when (memq key '(27 escape mouse-3))
              (setq load-buffers nil))))
        (eli-use-desktop load-buffers)
        (desktop-read) ; do this because we're running after `after-init-hook'
        (message nil)))))

;;; desktop-init.el ends here
