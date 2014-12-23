;; Copyright © 2008  Eric Schulte
;;
;; WYSWYG, html mail composition using orgmode
;;
;; For mail composed using the orgstruct-mode minor mode, this
;; provides the option of sending the mail in html format using
;; org-export-as-html.
;;
;; To use place this file in your path, and add the following to you
;; .emacs file
;;
;; ;; org-mode in my mail
;; (defun turn-on-full-org-mailing ()
;;   ;;(turn-on-orgstruct)
;;   (turn-on-orgstruct++)
;;   (turn-on-orgtbl)
;;   (load "org-html-mail.el"))
;; (add-hook 'mail-mode-hook 'turn-on-full-org-mailing)
;;
;; Then when composing mail send as an html message by using a prefix
;; argument on the send command, so "\C-u\C-c\C-c".  Your mail will be
;; converted to html using org's export command, the appropriate mime
;; headers will be attached, and then your normal send command will be
;; executed.
;;
;; For discussion see "sending html mail using VM" at
;; http://groups.google.com/group/gnu.emacs.vm.info/browse_frm/month/2008-01

(defun orgstruct-hijacker-command-21 (arg)
  "In Structure, run `org-ctrl-c-ctrl-c'. Outside of Structure
check for a prefix argument and if buffer name contains `mail',
and run orgstruct-send-as-html, or run the binding of
`\C-c\C-c'."
  (interactive "p")
  (vm-inform 6 "calling html send mail")
  (save-excursion
    (if (org-context-p (quote headline) (quote item))
        (org-run-like-in-org-mode (quote org-ctrl-c-ctrl-c))
      (if (orgstruct-send-as-html-should-i-p arg)
          (progn (vm-inform 6 "sending as html mail") (orgstruct-send-as-html))
        (let (orgstruct-mode)
          (call-interactively
           (key-binding "\C-c\C-c")))))))

(defun orgstruct-send-as-html-should-i-p (arg)
  "lets be pretty sure we have a prefix argument and are actually
in a mail buffer"
  (goto-char (point-min))
  (if (and arg
           (> arg 1)
           (equal major-mode 'mail-mode))
      t))

(defun orgstruct-send-as-html ()
  "Export the body of the mail message to html using
`org-export-as-html' then send the results as a text/html
Content-Type message"
  ;; adjust mime type
  (goto-char (point-min))
  (insert "MIME-Version: 1.0\n")
  (insert "Content-Type: text/html\n")
  (search-forward mail-header-separator)
  (let* ((mail-text-point (point))
         (mail-buffer (current-buffer))
         ;; have to write the file because org needs a path to export
         (tmp-file (make-temp-name 
		    (expand-file-name "mail" temporary-file-directory)))
         ;; because we probably don't want to skip part of our mail
         (org-export-skip-text-before-1st-heading nil)
         ;; makes the replies with ">"s look nicer
         (org-export-preserve-breaks t)
         ;; takes care of setting all my org-local-vars, if no
         ;; previous org usage
         (org-local-vars (or org-local-vars
                             (org-get-local-variables)))
         (html
          (progn
            (write-file tmp-file)
            ;; convert to html
            ;; mimicing org-run-like-in-org-mode
            (eval (list 'let org-local-vars
                        (list 'org-export-region-as-html
                              'mail-text-point
                              '(point-max) 't ''string))))))
    (switch-to-buffer mail-buffer)
    (set-visited-file-name nil)
    (delete-file tmp-file)
    ;; replace text with html
    (goto-char mail-text-point)
    (delete-region (point) (point-max))
    (insert "\n")
    (insert html)
    ;; send the mail
    (let (orgstruct-mode)
      (call-interactively
       (key-binding "\C-c\C-c")))))
