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

;; Decide whether to load the previous desktop or not
;; (Do that now, not in the startup hook, since there it cannot change
;; `command-line-args')
(defvar is-fast-p nil)
(setq is-fast-p
      (and (member "--fast" command-line-args)
           (progn (setq command-line-args (delete "--fast" command-line-args))
                  t)))
(defvar fake-initial-key nil
  "If this is set, it's used as the initial keypress result (and no
logo-question will appear).")
(add-hook 'emacs-startup-hook
  (lambda ()
    ;; don't use the desktop if we have `--fast' or some other command-line
    ;; argument
    (unless (or is-fast-p (cdr command-line-args))
      (let* ((key (or fake-initial-key
                      (eli-logo
                       (concat "Hit any key to continue, escape/right-click:"
                               " don't load previous desktop."))))
             (key (if (integerp key) key (event-basic-type key))))
        (eli-use-desktop (not (memq key '(27 escape mouse-3))))
        (desktop-read) ; do this because we're running after `after-init-hook'
        (message nil)))))

;;; desktop-init.el ends here
