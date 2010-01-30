(add-to-list 'load-path
             (concat eli-extras-dir "vm/share/emacs/site-lisp/vm"))
(add-to-list 'Info-default-directory-list
             (concat eli-extras-dir "vm/share/info/"))
;; these are needed before requiring vm, for some reason
(require 'vm-version)
(require 'vm-vars)
(require 'vm)
;; fake these to avoid the annoying beeping
(provide 'bbdb) (provide 'bbdb-autoloads) (provide 'bbdb-com)
;; and some more things which are loaded vm-autoloads
(require 'vm-autoloads)
(require 'vm-pine) ; needed for `vm-mime-yank-attachments'

(setq
 ;; >> vm
 vm-assimilate-new-messages-sorted nil
 vm-init-file (concat main-mail-directory ".vm")
 vm-preferences-file (concat main-mail-directory ".vm.preferences")
 vm-folder-directory main-mail-directory
 vm-primary-inbox (concat main-mail-directory "INBOX")
 vm-crash-box (concat main-mail-directory "INBOX.crash")
 vm-index-file-suffix nil ; ".idx" disable index files
 vm-spool-file-suffixes '(".spool") ; automatic spools
 vm-crash-box-suffix ".crash" ; crash for the above
 vm-auto-get-new-mail t
 vm-mail-check-interval 60
 ;; vm-default-folder-type 'From_
 ;; vm-default-From_-folder-type 'From_
 vm-default-new-folder-line-ending-type 'lf
 vm-check-folder-types t
 vm-convert-folder-types t
 ;; vm-trust-From_-with-Content-Length nil
 vm-visible-headers
   '("From:" "To:" "Apparently-To:" "Cc:" "Subject:" "Date:"
     "^Reply-To:" "Resent-From:" "Resent-To:")
 vm-highlighted-header-regexp "From:\\|Subject:\\|Reply-To:"
 vm-preview-lines 0
 vm-preview-read-messages nil
 ;; ?? vm-always-use-presentation-buffer nil
 vm-fill-paragraphs-containing-long-lines nil ; cute, but too slow
 vm-paragraph-fill-column 72
 vm-display-using-mime t
 ;; ?? vm-mime-ignore-mime-version t
 ;; ?? vm-mime-require-mime-version-header t
 ;; ?? vm-mime-ignore-composite-type-opaque-transfer-encoding t
 ;; ?? vm-mime-ignore-missing-multipart-boundary t
 vm-send-using-mime t
 vm-honor-mime-content-disposition t ; and see below: no images
 vm-auto-decode-mime-messages t
 vm-mime-decode-for-preview nil ; no preview lines anyway
 vm-auto-displayed-mime-content-types '("text" "multipart") ; no images
 vm-auto-displayed-mime-content-type-exceptions '("text/html")
 vm-mime-internal-content-types t
 vm-mime-internal-content-type-exceptions '("text/calendar")
 vm-mime-external-content-types-alist
   '(;("text/html" "galeon") ("image" "qiv") ("video" "mplayer")
     )
 vm-mime-delete-viewer-processes t
 vm-mime-alternative-select-method 'best-internal
 vm-mime-default-face-charsets t ; just show all, emacs can deal with a lot now
 vm-mime-use-image-strips nil ; this can be very slow on big messages
 ;; vm-mime-display-image-strips-incrementally t
 ;; vm-imagemagick-convert-program "/usr/bin/convert"
 ;; vm-imagemagick-identify-program "/usr/bin/identify"
 vm-mime-delete-after-saving nil
 vm-mime-confirm-delete t
 ;; vm-mime-8bit-text-transfer-encoding 'quoted-printable
 vm-mime-8bit-composition-charset "utf-8" ; avoid the 2022-jp encoding
 vm-mime-composition-armor-from-lines t ; nice to not have `>From's
 ;; vm-mime-encode-headers-type 'Q
 vm-mime-max-message-size (* 15 1024 1024) ; split bigger messages
 vm-mime-attachment-save-directory      ; saved attachments
   (expand-file-name
    (findif
     'file-accessible-directory-p
     (let ((tmps '("tmp/" "temp/" "Temp/" "TMP/")))
       (append (mapcar (lambda (x) (concat "~/" x)) tmps)
               (mapcar (lambda (x) (concat main-mail-directory x)) tmps)))
     "~/"))
 vm-mime-attachment-source-directory (expand-file-name "~/")
 vm-mime-yank-attachments t
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
 vm-reply-subject-prefix "Re: "
 vm-included-text-attribution-format "On %-3.3m %2d, %F wrote:\n"
 vm-forwarding-subject-format "[Forwarded from %F] %s"
 vm-summary-format "%n%*%a%-12.12F %-3.3m %2d %4US %I%s\n" ; function-S below
 vm-summary-arrow ">"
 vm-mouse-track-summary nil ; no annoying highlights
 vm-summary-show-threads nil
 vm-summary-thread-indent-level 2
 vm-thread-using-subject t
 vm-summary-uninteresting-senders-arrow "To: "
 vm-auto-center-summary 0
 vm-subject-ignored-prefix "^ *\\(re: *\\)+"
 vm-subject-ignored-suffix              ; subject suffix to ignore
   "\\( +[[(]\\(?:fwd\\|forward\\)[])]\\| \\)+$"
 vm-subject-significant-chars nil
 vm-folders-summary-database (concat main-mail-directory ".vm.folders.db")
 vm-folders-summary-format "  %12f %4t total, %n new, %u unread, %s spooled\n"
 vm-folders-summary-directories (list main-mail-directory)
 vm-mutable-windows t  ; change windows, but
 vm-mutable-frames nil ; no new frames
 vm-raise-frame-at-startup nil
 vm-frame-per-folder nil
 vm-frame-per-summary nil
 vm-frame-per-folders-summary nil
 vm-frame-per-composition nil
 vm-frame-per-edit nil
 vm-frame-per-help nil
 vm-frame-per-completion nil
 vm-search-other-frames nil
 vm-configure-datadir   (concat eli-extras-dir "vm/share")
 vm-configure-pixmapdir (concat eli-extras-dir "vm/share/vm")
 vm-popup-menu-on-mouse-3 t
 vm-url-retrieval-methods '(lynx wget fetch curl w3m) ; maybe disable with nil?
 vm-url-browser 'vm-mouse-send-url-to-firefox
 vm-url-browser-switches '()
 vm-url-search-limit 24000
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
 vm-mime-deleted-object-label "/[Deleted %f (%d;%t)]\n"
 vm-mime-show-alternatives t ; ?
 vm-default-folder-permission-bits #o600
 vm-coding-system-priorities '(utf-8 iso-8859-1 iso-8859-15)
 vm-mime-ucs-list '(utf-8 ctext escape-quoted)
 vm-enable-addons '(check-recipients check-for-empty-subject encode-headers
                    take-action-on-attachment)
;; >> vm/vm-faces
 vm-highlighted-header-face (simple-make-face '*/h004-bold-underline)
 vm-mime-button-face (simple-make-face 'cyan1/brown4-bold)
 vm-summary-highlight-face (simple-make-face 'yellow/purple4-bold)
 vm-highlight-url-face 'bold-underline
 )

(setq
 vm-mode-line-format
 '("" "%&%& "
   ("VM: "
    (vm-folder-read-only "READ-ONLY ")
    (vm-virtual-folder-definition (vm-virtual-mirror "mirrored "))
    "%b"
    (vm-mail-buffer (vm-ml-sort-keys ("" " by " vm-ml-sort-keys)))
    (vm-message-list
     (" " vm-ml-message-number "/" vm-ml-highest-message-number)
     (vm-folder-type
      " (UNRECOGNIZED FOLDER TYPE)"
      " (no messages)")))
   (vm-spooled-mail-waiting " Mail")
   (vm-message-list
    ("  %[ " vm-ml-message-attributes-alist
     (vm-ml-labels ("; " vm-ml-labels)) " %]    ")
    ("  %[%]   "))
   "%p" "   " global-mode-string))

(cond
  ((findif 'file-exists-p '("/usr/bin/w3m" "/usr/local/bin/w3m"))
   ;; use w3m to render html->text
   (setq vm-mime-type-converter-alist
        '(("text/html" "text/plain" "w3m -T text/html -no-cookie -dump")))
   (add-to-list 'vm-mime-internal-content-type-exceptions "text/html"))
  ((findif 'file-exists-p '("/usr/bin/lynx" "/usr/local/bin/lynx"))
   ;; use lynx to render html->text
   (setq vm-mime-type-converter-alist
        '(("text/html" "text/plain" "lynx -force_html -dump /dev/stdin")))
   (add-to-list 'vm-mime-internal-content-type-exceptions "text/html")))

;; Better display for message size
(defun vm-summary-function-S (msg)
  (let* ((str (vm-byte-count-of msg))
         (num (if str
                (string-to-number str)
                (- (vm-text-end-of (vm-real-message-of m))
                   (vm-text-of (vm-real-message-of m))))))
    (unless str (vm-set-byte-count-of m (int-to-string num)))
    (cond
      (num (cond ((< num 1000) (format "%sB" num))
                 ((< num (* 1000 1024)) (format "%sK" (round (/ num 1024.0))))
                 (t (format "%sM" (round (/ num 1024.0 1024.0))))))
      ;; these shouldn't happen
      (str (concat str "b"))
      (t "???"))))

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
;;; them when replying, especially with delsel on.
(defun citation-yank-select-sig ()
  "Nuke a .sig from cited mail"
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
(add-hook 'vm-reply-hook 'citation-yank-select-sig)

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
  '("\n" vm-scroll-forward-one-line)
  '("\r" vm-scroll-forward-one-line)
  '(save-buffer vm-save-folder))
