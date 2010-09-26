(add-to-list 'load-path
             (concat eli-extras-dir "vm/vm-8.1.93a/lisp"))
(add-to-list 'Info-default-directory-list
             (concat eli-extras-dir "vm/vm-8.1.93a/info"))

;; these are needed before requiring vm, for some reason
(require 'vm)

(setq
 ;; >> vm
 vm-assimilate-new-messages-sorted nil
 vm-init-file (concat main-mail-directory ".vm")
 vm-preferences-file (concat main-mail-directory ".vm.preferences")
 vm-folder-directory main-mail-directory
 vm-primary-inbox (concat main-mail-directory "INBOX")
 vm-crash-box (concat main-mail-directory "INBOX.crash")
 vm-fetched-message-limit 10
 vm-index-file-suffix nil ; disable index files
 ;; vm-spool-files '("/var/spool/mail/eli") ; <-- set automatically
 vm-spool-file-suffixes '(".spool") ; automatic spools
 vm-crash-box-suffix ".crash" ; crash needed for the above
 vm-auto-get-new-mail t
 vm-mail-check-interval 60
 ;; vm-default-folder-type 'From_
 ;; vm-default-From_-folder-type 'From_
 ;; vm-trust-From_-with-Content-Length nil
 vm-default-new-folder-line-ending-type 'lf
 vm-check-folder-types t
 vm-convert-folder-types t
 vm-visible-headers
   '("Resent-"
     "From:"
     ;; "Sender:"
     "To:"
     "Apparently-To:"
     "Cc:"
     "Subject:"
     "Date:"
     "Reply-To:")
 vm-invisible-header-regexp nil
 vm-highlighted-header-regexp "From:\\|Subject:\\|Reply-To:"
 vm-preview-lines 0
 vm-preview-read-messages nil
 vm-word-wrap-paragraphs nil ; why not use emacs word-wrapping?
     ;; NEWS file says: "Set to nil to enable the usual filling functionality"
 vm-word-wrap-paragraphs-in-reply nil ; doesn't work well
 vm-fill-paragraphs-containing-long-lines nil ; cute, but too slow
 vm-paragraph-fill-column 72
 vm-display-using-mime t
 ;; ?? vm-mime-ignore-mime-version t
 ;; ?? vm-mime-require-mime-version-header nil
 ;; ?? vm-mime-ignore-composite-type-opaque-transfer-encoding t
 ;; ?? vm-mime-ignore-missing-multipart-boundary t
 vm-send-using-mime t
 vm-honor-mime-content-disposition t ; and see below: no images
 vm-auto-decode-mime-messages t
 vm-mime-decode-for-preview nil ; no preview lines anyway
 vm-auto-displayed-mime-content-types
   '("text" "multipart" "message/rfc822") ; no images
 vm-auto-displayed-mime-content-type-exceptions '("image")
 vm-mime-internal-content-types t
 vm-mime-internal-content-type-exceptions '("text/calendar")
 vm-mime-external-content-types-alist
   '(;("text/html" "firefox") ("image" "qiv") ("video" "mplayer")
     )
 vm-mime-external-content-type-exceptions '()
 vm-mime-delete-viewer-processes t
 vm-mime-type-converter-alist
   (cond
     ;; ((findif 'file-exists-p '("/usr/bin/w3m" "/usr/local/bin/w3m"))
     ;;  ;; use w3m to render html->text
     ;;  (add-to-list 'vm-mime-internal-content-type-exceptions "text/html")
     ;;  '(("text/html" "text/plain" "w3m -T text/html -no-cookie -dump")))
     ;; ((findif 'file-exists-p '("/usr/bin/lynx" "/usr/local/bin/lynx"))
     ;;  ;; use lynx to render html->text
     ;;  (add-to-list 'vm-mime-internal-content-type-exceptions "text/html")
     ;;  '(("text/html" "text/plain" "lynx -force_html -dump /dev/stdin")))
     (t nil))
 vm-mime-alternative-select-method
   '(favorite-internal "text/plain" "text/html" "text")
 vm-mime-text/html-handler 'auto-select
 vm-mime-text/html-blocker "<img[^>]*\\s-src=."
 vm-mime-text/html-blocker-exceptions '()
 vm-mime-default-face-charsets t ; just show all, emacs can deal with a lot now
 vm-mime-default-face-charset-exceptions '() ; maybe add spam fonts?
 vm-mime-use-image-strips nil ; this can be very slow on big messages
 ;; vm-mime-display-image-strips-incrementally t
 ;; vm-imagemagick-convert-program "/usr/bin/convert"
 ;; vm-imagemagick-identify-program "/usr/bin/identify"
 vm-mime-delete-after-saving nil
 vm-mime-confirm-delete t
 vm-mime-savable-types '("application" "x-unknown" "application/x-gzip")
 vm-mime-savable-type-exceptions '("text") ; maybe remove this?
 vm-mime-deletable-types '("application" "x-unknown" "application/x-gzip")
 vm-mime-deletable-type-exceptions '("text")
 vm-mime-parts-display-separator "\n" ; maybe two newlines?
 ;; vm-mime-8bit-text-transfer-encoding 'quoted-printable
 ;; needed?-- vm-mime-8bit-composition-charset "utf-8" ; avoid 2022-jp encoding
 vm-mime-composition-armor-from-lines t ; nice to not have `>From's
 ;; vm-mime-encode-headers-type 'Q
 vm-mime-max-message-size (* 19 1000 1000) ; split bigger messages
 vm-mime-attachment-save-directory      ; saved attachments to some temp
   (expand-file-name
    (findif
     'file-accessible-directory-p
     (let ((tmps '("tmp/" "temp/" "Temp/" "TMP/")))
       (append (mapcar (lambda (x) (concat "~/" x)) tmps)
               (mapcar (lambda (x) (concat main-mail-directory x)) tmps)))
     "~/"))
 vm-mime-attachment-source-directory (expand-file-name "~/")
 ;; ?? vm-mime-all-attachments-directory nil
 vm-mime-yank-attachments t ; leave only buttons (it'd be nice to remove too)
 vm-infer-mime-types t
 vm-mime-attachment-infer-type-for-text-attachments nil
 vm-mime-avoid-folding-content-type t
 vm-mime-base64-decoder-program "base64-decode"
 vm-mime-base64-decoder-switches '()
 vm-mime-base64-encoder-program "base64-encode"
 vm-mime-base64-encoder-switches '()
 vm-mime-qp-decoder-program "qp-decode"
 vm-mime-qp-decoder-switches '()
 vm-mime-qp-encoder-program "qp-encode"
 vm-mime-qp-encoder-switches '()
 vm-mime-uuencode-decoder-program "uudecode"
 vm-mime-uuencode-decoder-switches '()
 vm-auto-next-message t
 vm-honor-page-delimiters nil
 ;; vm-page-continuation-glyph "...press SPACE to see more..." ; prvws & pages
 vm-expunge-before-quit nil
 vm-expunge-before-save nil
 vm-confirm-quit 0
 vm-confirm-new-folders t
 vm-delete-empty-folders t
 ;; ?? vm-folder-file-precious-flag t
 ;; ?? vm-flush-interval 90
 vm-visit-when-saving 0
 vm-virtual-mirror t
 vm-included-text-prefix mail-yank-prefix
 vm-keep-sent-messages 1
 vm-confirm-mail-send nil
 vm-mail-mode-hidden-headers '("References" "In-Reply-To" "X-Mailer")
 vm-mail-header-insert-date t
 vm-mail-header-insert-message-id t
 vm-do-fcc-before-mime-encode nil ; fcc with encoded (includes attachments)
 vm-reply-subject-prefix "Re: "
 ;; vm-reply-include-presentation nil ; looks like it's not used in the code
 vm-include-text-from-presentation t ; experimental? (this is actually used)
   ;; works, but looks like it will send an image, only it doesn't include it
 ;; ?? vm-in-reply-to-format "%i"
 vm-included-mime-types-list nil ; include all types that are known
   ;; could use: '("text/plain" "text/enriched" "message/rfc822")
 ;; ?? vm-included-text-headers '()
 ;; ?? vm-included-text-discard-header-regexp nil
 vm-forwarding-subject-format "[Forwarded from %F] %s"
 ;; ?? vm-forwarded-headers '()
 ;; ?? vm-unforwarded-header-regexp "none-to-be-dropped"
 vm-forwarding-digest-type "mime" ; nil => textually include it
 ;; ?? vm-mime-forward-local-external-bodies nil
 ;; ?? vm-burst-digest-messages-inherit-labels t
 vm-digest-preamble-format "%F: %s"
 vm-digest-center-preamble nil
 ;; ?? vm-digest-identifier-header-format "X-Digest: %s\n"
 vm-digest-burst-type "guess"
 vm-digest-send-type "mime"
 vm-summary-format "%n%*%a%-12.12F %-3.3m %2d %4S %I%s\n" ; function-S below
 vm-summary-attachment-indicator "$" ; used with %P - but doesn't work
 ;; vm-summary-attachment-mime-types '()
 ;; vm-summary-attachment-mime-type-exceptions '()
 vm-summary-arrow "->" ; broken: "vm-summary.el" expects this arrow only
 vm-mouse-track-summary nil ; no annoying highlights
 vm-summary-show-threads nil ; buffer-local anyway
 vm-summary-thread-indentation-by-references t ; nil = parents in folder
 vm-summary-thread-indent-level 2
 vm-summary-maximum-thread-indentation 20
 vm-thread-using-subject t
 vm-sort-threads-by-youngest-date t
 vm-summary-uninteresting-senders-arrow "To: "
 vm-auto-center-summary 0
 vm-subject-ignored-prefix "^ *\\(re: *\\)+"
 vm-subject-ignored-suffix "\\( +[[(]\\(?:fwd\\|forward\\)[])]\\| \\)+$"
 vm-subject-significant-chars nil
 vm-folders-summary-database (concat main-mail-directory ".vm.folders.db")
 vm-folders-summary-format "  %12f %4t total, %n new, %u unread, %s spooled\n"
 vm-folders-summary-directories (list main-mail-directory)
 vm-mutable-windows t  ; change windows, but
 vm-mutable-frames nil ; no new frames
 vm-raise-frame-at-startup nil ; and don't mess with my windows!
 vm-frame-per-folder nil ; no frames, no no no.
 vm-frame-per-summary nil
 vm-frame-per-folders-summary nil
 vm-frame-per-composition nil
 vm-frame-per-edit nil
 vm-frame-per-help nil
 vm-frame-per-completion nil
 vm-search-other-frames nil ; not this too
 ;; ?? vm-image-directory nil
 vm-popup-menu-on-mouse-3 t
 ;; vm-url-retrieval-methods '(lynx wget fetch curl w3m) ; maybe disable with nil?
 vm-url-browser 'browse-url ; 'vm-mouse-send-url-to-firefox
 vm-url-search-limit 24000
 ;; vm-display-xfaces nil ; bleh
 vm-startup-with-summary t
 vm-follow-summary-cursor t
 vm-jump-to-new-messages nil
 vm-jump-to-unread-messages nil
 vm-skip-deleted-messages t
 vm-skip-read-messages nil
 vm-move-after-deleting 'x
 vm-move-after-undeleting nil
 vm-move-after-killing 'x
 vm-delete-after-saving t
 vm-delete-after-archiving nil
 vm-delete-after-bursting t
 vm-circular-folders nil
 vm-search-using-regexps nil
 vm-move-messages-physically nil
 vm-edit-message-mode 'indented-text-mode
 ;; vm-print-command "lpr"
 ;; vm-print-command-switches '()
 ;; ?? vm-strip-reply-headers nil
 vm-mime-deleted-object-label "[Deleted %f (%d;%t)]\n"
 vm-emit-messages-for-mime-decoding t ; shows only nontrivial decoding
 vm-default-folder-permission-bits #o600
 vm-coding-system-priorities '(utf-8 iso-8859-1 iso-8859-15)
 vm-mime-ucs-list '(utf-8 ctext escape-quoted)
 ;; vm-drop-buffer-name-chars "[^ a-zA-Z0-9.,_\"'+-]"
 vm-buffer-name-limit 50
 vm-summary-enable-thread-folding t    ; \
 vm-summary-show-thread-count t        ;  > Experimental
 vm-summary-thread-folding-on-motion t ; /
 vm-enable-thread-operations t
 ;; vm-vs-attachment-regexp "^Content-Disposition: attachment"
 vm-spam-words-file (concat main-mail-directory ".spam-words")
 vm-vs-spam-score-headers
   '(("X-Spam-Score:" "[-+]?[0-9]*\\.?[0-9]+" string-to-number)
     ("X-Spam-Status:" "[-+]?[0-9]*\\.?[0-9]+" string-to-number)
     ("X-Spam-Level:" "\\*+" length))
 vm-enable-addons '(check-recipients check-for-empty-subject encode-headers)
 vm-summary-enable-faces nil ; doesn't seem to work anyway
 vm-mime-thumbnail-max-geometry "80x80"
 vm-delete-message-action "vm-next-message"
 ;; --- IMAP (all experimental)
 vm-load-headers-only t ; effective only for IMAP
 vm-imap-max-message-size (* 4 1024 1024)
 vm-imap-messages-per-session nil
 vm-imap-bytes-per-session nil
 ;; vm-imap-expunge-after-retrieving t <-- "when used as a spool file"??
 ;; vm-imap-auto-expunge-alist '()
 vm-imap-folder-cache-directory
   (findif 'file-accessible-directory-p
           (list (concat main-mail-directory "imap/")))
 vm-imap-save-to-server t
 vm-imap-ensure-active-sessions t
 vm-imap-sync-on-get t ; maybe set this to nil?
 ;; >> vm/vm-faces
 vm-highlighted-header-face (simple-make-face '*/h004-bold-underline)
 vm-mime-button-face (simple-make-face 'cyan1/brown4-bold)
 vm-summary-highlight-face (simple-make-face 'yellow/purple4-bold)
 vm-highlight-url-face (simple-make-face 'bold-underline)
 ;; >> vm/vm-summary-faces
 ;; not working anyway
 ;; vm-summary-faces-alist
 ;;   '(((or (header "Priority: urgent")
 ;;          (header "Importance: high")
 ;;          (header "X-Priority: 1")
 ;;          (label "!")
 ;;          (label "\\flagged")
 ;;          (header "X-VM-postponed-data:"))
 ;;      vm-summary-high-priority)
 ;;     ((deleted) vm-summary-deleted)
 ;;     ((new) vm-summary-new)
 ;;     ((unread) vm-summary-unread)
 ;;     ((marked) vm-summary-marked)
 ;;     ((replied) vm-summary-replied)
 ;;     ((or (filed) (written)) vm-summary-saved)
 ;;     ((or (forwarded) (redistributed)) vm-summary-forwarded)
 ;;     ((edited) vm-summary-edited)
 ;;     ((outgoing) vm-summary-outgoing)
 ;;     ((any) vm-summary-default))
 ;; vm/vm-rfaddons
 vm-fill-long-lines-in-reply-column nil
 ;; vm-spamassassin-strip-report "spamassassin -d"
 ;; ?? vm-mail-subject-prefix-replacements
 ;; ??   '(("\\(\\(re\\|aw\\|antw\\)\\(\\[[0-9]+\\]\\)?:[ \t]*\\)+" . "Re: ")
 ;; ??     ("\\(\\(fo\\|wg\\)\\(\\[[0-9]+\\]\\)?:[ \t]*\\)+" . "Fo: "))
 ;; works only with (add-hook 'vm-mail-mode-hook 'vm-mail-subject-cleanup):
 ;; vm-mail-subject-number-reply nil
 ;; doesn't look useful:
 ;; vm-handle-return-receipt-mode 'edit
 ;; vm-handle-return-receipt-peek 500
 ;; vm-mime-attach-files-in-directory-default-type nil
 ;; vm-mime-attach-files-in-directory-default-charset 'guess
 ;; vm-mime-auto-save-all-attachments-subdir nil
 vm-mail-prompt-if-subject-empty t ; maybe do a hook that adds "(no subject)"?
 vm-assimilate-html-command "striptags" ; is it needed with the include thing?
 vm-assimilate-html-mixed t
 ;; used only in `vm-mail-mode-citation-clean-up':
 ;; vm-mail-mode-citation-kill-regexp-alist
 ;;   '(("^\\( > [|{}>:;][^\n]*\n\\)+" . "[...]\n")
 ;;     ("^\\([^|{}>:;]+.*\\)\n > [|{}>:;]*$" . "\\1")
 ;;     ("^ > [|{}>:;]*\n\\([^|{}>:;]\\)" . "\\1")
 ;;     ("^ > [|{}>:;]*\\s-*\n\\( > [|{}>:;]*\\s-*\n\\)+" . " > \n")
 ;;     ("\n\n\n+" . "\n\n")
 ;;     ("^ > --[^\n]*\n\\( > [^\n]*\n\\)+" . "\n")
 ;;     ("^ > ________[^\n]*\n\\( > [^\n]*\n\\)+" . "\n"))
 vm-summary-attachment-label "$" ; does this have any effect?
 vm-mail-mode-open-line-regexp "[ \t]*>"
 ;; used only in `vm-mail-mode-elide-reply-region':
 ;; vm-mail-mode-elide-reply-region "[...]\n"
 ;; ??vm-mime-display-internal-multipart/mixed-separater
 ;;   "\n-----------------------------------------------------------------\n"
 ;; ?? vm-mail-mode-fake-date-p t
 )

;; For times in quote attributions
(defun eli-reply-time-ago (msg msg-time real-msg-date)
  (let* ((now-time (current-time))
         (secs (round (- (time-to-seconds now-time)
                         (time-to-seconds msg-time))))
         (mins (round secs 60))
         (hrs  (round secs (* 60 60)))
         ;; days goes by the actual day (my timezone)
         (days (- (time-to-days now-time) (time-to-days msg-time)))
         (weeks (round days 7))
         (msg-date (decode-time msg-time))
         (dow (nth (nth 6 msg-date) '("Sunday" "Monday" "Tuesday" "Wednesday"
                                      "Thursday" "Friday" "Saturday")))
         (2str (lambda (n)
                 (case n ((1) "One") ((2) "Two") ((3) "Three") ((4) "Four")
                         (t (number-to-string n))))))
    (cond
      ;; includes negatives -- assume that it's always very recent
      ((<= secs 45)  "A few seconds ago")
      ;; being precise  isn't important
      ((<= secs 90)  "About a minute ago")
      ((<= mins 10)  (format "%s minutes ago" (funcall 2str mins)))
      ((<= mins 30)  (format "%s minutes ago"
                             (funcall 2str (* 5 (round mins 5)))))
      ((<= mins 54)  (format "%s minutes ago"
                             (funcall 2str (* 10 (round mins 10)))))
      ((<= mins 75)  "An hour ago")
      ;; can't really say that it's 1 or 2 hours
      ((<= mins 105) "An hour and a half ago")
      ((<= hrs 12)   (format "%s hours ago" (funcall 2str hrs)))
      ;; more than this, and we switch to days (based on my timezone)
      ((<= days 0)   "Earlier today")
      ((<= days 1)   "Yesterday")
      ;; this would be "shilshom"
      ((<= days 2)   (format "%s days ago" (funcall 2str days)))
      ;; switch to the day's name
      ((<= days 6)   (format "On %s" dow))
      ;; and now to a count of weeks
      ((<= days 7)   "A week ago")
      ;; can't really say that it's a week
      ((<= weeks 1)  "More than a week ago")
      ;; weeks are longer, so say "about"
      ((<= weeks 3)  (format "About %s weeks ago"
                             (downcase (funcall 2str weeks))))
      ;; note that this counts by days
      ((<= days 40)  "About a month ago")
      ;; finally switch to the date, same year => drop it
      ;; (use the time from the message, might be different than in my zone)
      (t (let ((D (nth 3 real-msg-date))
               (M (nth 4 real-msg-date))
               (Y (nth 5 real-msg-date)))
           (concat
            "On "
            (nth (1- M) '("January" "February" "March" "April" "May" "June"
                          "July" "August" "September" "October" "November"
                          "December"))
            " " (number-to-string D)
            (if (= 1 (/ D 10)) "th"
                (case (% D 10) ((1) "st") ((2) "nd") ((3) "rd") (t "th")))
            (if (= (nth 5 (decode-time)) Y)
              "" (concat " " (number-to-string Y)))))))))
(defun vm-summary-function-R (msg)
  (let* ((author    (vm-full-name-of msg))
         (msg-time  (vm-summary-sprintf "%y-%02M-%02d %h %z" msg))
         (msg-time* (parse-time-string msg-time))
         (msg-time  (apply 'encode-time msg-time*)))
    (format "%s, %s wrote:\n"
            (eli-reply-time-ago msg msg-time msg-time*)
            author)))
(setq vm-included-text-attribution-format "%UR")

(setq
 vm-mode-line-format
 '("" "%&%& "
   (;; "VM " vm-version ": "
    "VM: "
    ;; (vm-folder-read-only "read-only ")
    (vm-folder-read-only "READ-ONLY ")
    (vm-virtual-folder-definition (vm-virtual-mirror "mirrored "))
    "%b"
    (vm-mail-buffer (vm-ml-sort-keys ("" " by " vm-ml-sort-keys)))
    (vm-message-list
     ;; ("   " vm-ml-message-number " (of " vm-ml-highest-message-number ")")
     (" " vm-ml-message-number "/" vm-ml-highest-message-number)
     ;; (vm-folder-type "   (unrecognized folder type)" "   (no messages)")
     (vm-folder-type " (UNRECOGNIZED FOLDER TYPE)" " (no messages)")))
   (vm-spooled-mail-waiting " Mail")
   (vm-message-list
    ("  %[ " vm-ml-message-attributes-alist
     (vm-ml-labels ("; " vm-ml-labels)) " %]    ")
    ("  %[%]   "))
   "%p" "   " global-mode-string))

;; Hack ignored from field using user-mail-address with and without local
;; machine name.  This must be done after loading Emacs, since it is only set
;; then.
(let ((add (downcase
            (if (string-match "^\\([^@]*\\)@[^.]*[.]\\(.*\\..*\\)$"
                              user-mail-address)
              (concat (regexp-quote (match-string 1 user-mail-address))
                      "@\\([a-z]*\\.\\)?"
                      (regexp-quote (match-string 2 user-mail-address)))
              (regexp-quote user-mail-address)))))
  (setq vm-summary-uninteresting-senders
        (if vm-summary-uninteresting-senders
          (concat "\\(" add "\\|" vm-summary-uninteresting-senders "\\)")
          add))
  (add-to-list 'vm-reply-ignored-addresses add))

;;; This detects standard signatures and selects them so it is easy to delete
;;; them when replying, especially with delsel on.  For now, just go there.
(defun citation-yank-goto-sig ()
  "Goto the sig of a cited mail"
  (interactive)
  (let ((qprefix (regexp-quote vm-included-text-prefix)) end sig start)
    (save-excursion
      (end-of-buffer)
      (setq end (search-backward-regexp
                 "-- " (and (> (point) 400) (- (point) 400)) t))
      (setq sig (and end (search-backward-regexp
                          (concat "^" qprefix "\\(-?-?[A-Za-z]+\\.?\\|-- *\\)$")
                          nil t)))
      (when (and sig (> (count-lines sig end) 5))
        (goto-char sig)
        (setq sig nil))
      (forward-line -1)
      (while (looking-at "^[>|: \t]*$") (forward-line -1))
      (forward-line 1)
      (setq start (point)))
    (when (and end start (> (1- end) start))
      ;; (ensure-nomark)
      (goto-char (1- end))
      ;; (ensure-mark)
      (goto-char start))))
(add-hook 'vm-reply-hook 'citation-yank-goto-sig)

;; Keep the mail indication more useful when we're doing stuff
(add-hook 'vm-arrived-messages-hook 'display-time-update)
(add-hook 'vm-quit-hook 'display-time-update)

(defun vm-expunge-and-quit ()
  (interactive)
  (vm-expunge-folder)
  (vm-quit))

(define-keys vm-mode-map
  '("#"  vm-expunge-folder)
  '("Q"  vm-expunge-and-quit)
  '("X"  vm-quit-no-change)
  '("x"  vm-expunge-folder)
  '("\t" vm-move-to-next-button)
  '([(? )]       vm-scroll-forward)
  '([(shift ? )] vm-scroll-backward)
  '([(control ?j)] vm-scroll-forward-one-line)
  '([(control ?m)] vm-scroll-forward-one-line)
  '([(return)]     vm-scroll-forward-one-line)
  '([(control shift ?j)] vm-scroll-backward-one-line)
  '([(control shift ?m)] vm-scroll-backward-one-line)
  '([S-return]           vm-scroll-backward-one-line)
  '([(shift return)]     vm-scroll-backward-one-line)
  '("\\" vm-toggle-thread)
  '("/"  vm-toggle-thread)
  '(save-buffer vm-save-folder))

(add-hook 'vm-summary-mode-hook
  ;; HACK: cua-set-rectable-mark cannot be overridden in a normal keymap, so
  ;; instead the S-return key is bound to `eli-override-cua-set-rectangle-mark'
  ;; which can be overridden via this buffer local variable
  (lambda ()
    (setq eli-override-cua-set-rectangle-mark 'vm-scroll-backward-one-line)))
