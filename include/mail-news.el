;;; mail-news.el --- Mail and news setup.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(defvar main-mail-directory             ; main mail directory
  (findif 'file-accessible-directory-p '("~/mail/" "~/Mail/" "~/MAIL/") "~/"))

;; Prefix for quoted text - used as global default in EliEmacs
(setq mail-yank-prefix "> ")

(unless mail-signature                  ; sign mails (use ~/.signature or text)
  (if (file-readable-p "~/.signature")
    (setq mail-signature t)
    (setq mail-signature (concat "-- \n" (capitalize (user-full-name)) "\n"))))
(unless (and (boundp 'mail-signature-file) mail-signature-file)
  (setq mail-signature-file
        (and (file-readable-p "~/.signature") "~/.signature")))

;; (setq mail-personal-alias-file "~/.mailrc") -- the default is fine

(setq mail-citation-prefix-regexp       ; allow deeper quotation nesting
      "[ \t]*[-a-z0-9A-Z]*>[ \t>]*\\|[ \t]*")

(setq mail-from-style 'angles)          ; angle-brackets from field

(setq mail-header-separator             ; separator for message composing
      (concat "----- " (capitalize (user-login-name)) " says -----"))

(setq mail-archive-file-name            ; saved out-going messages
      (concat main-mail-directory "sent-mail"))

;; Copy values from mail-* variables.
(setq message-yank-prefix    mail-yank-prefix
      message-from-style     mail-from-style
      message-signature      mail-signature
      message-signature-file mail-signature-file)

;; Useful tabbing between fields and text.
(defun eli-mail-tab-command ()
  "Move to next mail header, or to the mail body."
  (interactive)
  (let* ((headers-regexp
          "^\\(\\(Resent-\\)?To\\|Subject\\|CC\\|BCC\\):.*\\(\n[ \t]+.*\\)*")
         (headers-end nil)
         (set-headers-end
          (lambda ()
            (setq headers-end
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward (concat mail-header-separator "\n")
                                    2000 t))))))
    (funcall set-headers-end)
    (if (and headers-end (< (point) headers-end))
      (progn
        (cond
          ((save-excursion (beginning-of-line) (looking-at "FCC: "))
           (completion-at-point))
          ((looking-at "[ \t]*\n") (expand-abbrev)))
        (funcall set-headers-end)
        (goto-char (if (re-search-forward headers-regexp headers-end t)
                     (min headers-end (match-end 0))
                     headers-end)))
      (indent-relative))))

(defun eli-mail-setup-hook ()
  ;; Use autofill except for headers.
  (auto-fill-mode)
  (setq fill-column 70)
  (make-local-variable 'auto-fill-inhibit-regexp)
  (setq auto-fill-inhibit-regexp
        "^\\(\\(Resent-\\)?To\\|Subject\\|CC\\|BCC\\): ")
  (local-set-key "\t" 'eli-mail-tab-command) ; my magic tab
  (mail-abbrevs-setup))

(add-hook 'mail-setup-hook    'eli-mail-setup-hook t) ; use auto-fill etc in
(add-hook 'message-setup-hook 'eli-mail-setup-hook t) ; mails and messages

;; Gnus stuff
(setq gnus-interactive-exit           nil
      gnus-large-newsgroup            300
      gnus-decay-scores               t
      gnus-no-groups-message          "No more news"
      gnus-show-threads               t
      gnus-thread-indent-level        4
      gnus-use-full-window            t
      gnus-summary-make-false-root    'dummy
      gnus-use-trees                  nil
      gnus-asynchronous               t
      gnus-build-sparse-threads       'some
      gnus-fetch-old-headers          3
      gnus-thread-ignore-subject      t
      ;; gnus-show-mime                  nil
      gnus-mode-non-string-length     40
      gnus-subscribe-newsgroup-method 'gnus-subscribe-interactively
      gnus-subscribe-hierarchical-interactive t
      gnus-group-mode-line-format     "Gnus: %%b {%S}")
(add-hook 'gnus-started-hook
          '(lambda ()
             (define-keys gnus-group-mode-map
               '(save-buffer gnus-group-save-newsrc))))
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(simple-make-face 'yellow-bold-underline 'gnus-header-subject-face)
(simple-make-face 'green1-bold           'gnus-header-from-face)

;;; mail-news.el ends here
