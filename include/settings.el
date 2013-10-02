;;; settings.el --- Settings - mostly variables.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(set-language-environment "UTF-8")
(when (functionp 'set-fringe-mode) (set-fringe-mode 2))
(require 'uniquify)

(setq-default
 ;; >> editing/killing
 delete-active-region t
 kill-ring-max 100
 save-interprogram-paste-before-kill t
 ;; kill-do-not-save-duplicates nil ?? use t?
 yank-pop-change-selection nil
 kill-read-only-ok nil
 backward-delete-char-untabify-method 'untabify
 kill-whole-line t
 x-select-enable-clipboard t
 x-select-enable-primary t
 select-active-regions nil
 ;; >> editing/indent
 standard-indent 2
 tab-always-indent t
 indent-tabs-mode nil
 ;; >> editing/paragraphs
 paragraph-ignore-fill-prefix t
 ;; >> editing/fill
 sentence-end-double-space t
 fill-individual-varying-indent nil
 colon-double-space nil
 adaptive-fill-mode t
 default-justification 'left
 fill-column 72
 ;; >> editing/fill/comment
 comment-fill-column nil
 comment-style 'indent
 ;; >> editing/fill/longlines
 longlines-auto-wrap t
 longlines-show-hard-newlines t
 longlines-show-effect (propertize "<@>\n" 'face 'escape-glyph)
 ;; >> editing/electricity
 electric-indent-mode t
 electric-pair-skip-self nil ; would be nice, but doesn't play well with delsel
 electric-pair-mode nil      ; eg, type "(" when there's an active region
 electric-layout-mode t
 ;; >> editing/matching
 case-replace t
 case-fold-search t
 ;; query-replace-from-history-variable 'query-replace-history-from
 ;; query-replace-to-history-variable 'query-replace-history-to
 ;; query-replace-skip-read-only nil
 query-replace-show-replacement t
 query-replace-highlight t
 query-replace-lazy-highlight t
 ;; >> editing/matching/paren-matching/paren-blinking
 blink-matching-paren nil ; use `show-paren-mode'
 blink-matching-paren-distance 200000
 blink-matching-delay 0.5
 ;; >> editing/matching/paren-matching/paren-showing ;;??? dead?
 show-paren-style 'parenthesis
 show-paren-delay 0.125
 ;; >> editing/matching/isearch
 search-exit-option t
 search-upper-case 'not-yanks
 search-nonincremental-instead t
 search-whitespace-regexp "\\s-+"
 search-invisible 'open
 isearch-hide-immediately t
 search-ring-max 32
 regexp-search-ring-max 32
 search-highlight t
 isearch-lazy-highlight t
 isearch-allow-scroll t
 ;; >> editing/matching/isearch/lazy-highlight
 lazy-highlight-cleanup t
 lazy-highlight-initial-delay 0.25
 lazy-highlight-interval 0
 lazy-highlight-max-at-a-time 20
 ispell-lazy-highlight t
 ;; >> editing/matching/isearch/multi-isearch
 multi-isearch-search t ; ?
 multi-isearch-pause t  ; ?
 ;; >> editing/matching/completion
 completion-show-help t
 enable-completion t
 save-completions-flag t
 save-completions-retention-time (* 24 7 4)
 completions-file-versions-kept 1
 completion-search-distance 150000
 ;; >> editing/emulation/cua
 ;; Done in "eli-cua.el"
 ;; >> editing/mouse
 mouse-yank-at-point nil
 mouse-drag-copy-region t ; questionable
 mouse-1-click-follows-link 450
 mouse-1-click-in-non-selected-windows t
 mouse-scroll-delay 0.1
 mouse-scroll-min-lines 1
 mouse-buffer-menu-maxlen 40
 mouse-wheel-scroll-amount '(5 ((shift) . 1) ((control)))
 mouse-wheel-progressive-speed t
 mouse-wheel-follow-mouse t
 mouse-highlight 1
 make-pointer-invisible t
 ;; >> editing/mouse/tooltip
 tooltip-mode t ; should really be done via a call
 tooltip-delay 0.8
 tooltip-short-delay 0.1
 tooltip-hide-delay 10
 tooltip-x-offset 5
 tooltip-y-offset 20
 tooltip-use-echo-area nil
 ;; >> editing/undo
 undo-limit 200000
 undo-strong-limit 300000
 undo-outer-limit 30000000
 ;; >> editing/editing-basics
 read-quoted-char-radix 10
 require-final-newline t ; try nil with the following t?
 mode-require-final-newline t
 change-major-mode-with-file-name t
 use-empty-active-region nil ; t could be cute, but inconsistent for macros
 shift-select-mode t
 transient-mark-mode t
 next-line-add-newlines nil
 track-eol nil ; I have an end-of-line function that does this better
 line-move-ignore-invisible t
 line-move-visual nil ; can turn on with C-S-f12
 mark-even-if-inactive t
 parse-sexp-ignore-comments t
 ;; words-include-escapes nil ??
 ;; open-paren-in-column-0-is-defun-start t ??
 ;; >> editing/editing-basics/pc-select ;; all of these are dead?
 pc-select-override-scroll-error t
 pc-select-meta-moves-sexps t
 ;; >> convenience
 confirm-kill-emacs nil
 ;; >> convenience/visual-line
 visual-line-fringe-indicators '(up-arrow down-arrow)
 ;; >> convenience/Buffer-menu
 Buffer-menu-use-header-line t
 Buffer-menu-buffer+size-width 25
 Buffer-menu-mode-width 10
 ;; >> convenience/whitespace
 whitespace-style '(face                  ; use faces (=> customizable)
                    tabs ;newline spaces  ; only tabs
                    trailing              ; trailing blanks
                    ;; lines              ; long lines (whole line)
                    lines-tail            ; long lines (only the tail)
                    indentation           ; spaces/tabs, depends on tabs-mode
                    empty                 ; empty lines at the beginning/end
                    space-after-tab       ; bad spaces
                    space-before-tab      ; bad spaces
                    ;; space-mark         ; spaces via display table
                    ;; tab-mark           ; tabs via display table
                    ;; newline-mark       ; newlines via display table
                    )
 whitespace-line-column 79
 whitespace-global-modes t ; maybe only in scheme mode and turn on global mode?
 ;; whitespace-action '(report-on-bogus) ; too extreme
 ;; >> convenience/partial-completion
 PC-first-char nil ; allways do smart completion
 PC-meta-flag t ; complete on TAB
 PC-word-delimiters "-_./:| " ; same as `completion-pcm-word-delimiters'
 ;; >> convenience/bs
 bs-must-always-show-regexp "[*]shell"
 bs-default-configuration "all"
 bs-alternative-configuration "files-and-scratch"
 bs-default-sort-name "by nothing" ; by visit order
 ;; >> convenience/bs/bs-appearance
 bs-attributes-list '(("" 1 1 left bs--get-marked-string)
                      ("M" 1 1 left bs--get-modified-string)
                      ("R" 2 2 left bs--get-readonly-string)
                      ("Buffer" bs--get-name-length 10 left bs--get-name)
                      ("" 1 1 left " ")
                      ("Size" 8 8 right bs--get-size-string)
                      ("" 1 1 left " ")
                      ("Mode" 8 8 left bs--get-mode-name)
                      ("" 1 1 left " ")
                      ("File" 12 12 left bs--get-file-name))
 bs-max-window-height 20
 bs-maximal-buffer-name-column 45
 bs-minimal-buffer-name-column 15
 bs-string-show-always "+"
 bs-string-show-never "-"
 bs-string-current "."
 bs-string-current-marked "#"
 bs-string-marked "*"
 bs-string-show-normally " "
 ;; >> convenience/linum
 linum-format 'dynamic
 linum-eager t
 linum-delay 0.5
 ;; >> files
 find-file-wildcards t
 find-file-suppress-same-file-warnings nil ; tell me when already visited
 large-file-warning-threshold 50000000
 auto-mode-case-fold t
 ;; >> files/backup
 ;; done in "eli-backup.el"
 ;; >> files/find-file
 find-file-existing-other-name t
 find-file-visit-truename t
 revert-without-query '() ; I have a better hack anyway
 find-file-run-dired t
 enable-local-variables t ; ask if there are unsafe vars
 enable-local-eval 'maybe
 confirm-nonexistent-file-or-buffer 'after-completion ; nice feature!
 ;; >> files/uniquify
 uniquify-buffer-name-style 'post-forward
 uniquify-after-kill-buffer-p t
 uniquify-ask-about-buffer-names-p nil
 uniquify-min-dir-content 0
 uniquify-strip-common-suffix t
 ;; >> files/dired
 list-directory-brief-switches "-CF"
 list-directory-verbose-switches "-laFh"
 ;; directory-free-space-program "df"
 directory-free-space-args "-h"
 dired-listing-switches "-laFh"
 dired-subdir-switches nil
 ;; dired-chown-program "chown"
 ;; dired-use-ls-dired t
 ;; dired-chmod-program "chmod"
 ;; dired-touch-program "touch"
 dired-copy-preserve-time t
 dired-auto-revert-buffer t
 dired-recursive-deletes 'top
 dired-no-confirm '() ; might be useful to add stuff here
 dired-recursive-copies 'top
 completion-ignored-extensions `(".bak" "~" "#" ".obj"
                                 ,@completion-ignored-extensions)
 ;; >> files/dired/find-dired
 find-dired-find-program "find"
 find-exec-terminator "+"
 find-ls-option '("-exec ls -ldaF {} +" . "-ldaF") ; adding "h" would be nice
 find-ls-subdir-switches "-laF"                    ; but => bad rendering
 ;; >> files/dired/wdired
 wdired-use-interactive-rename nil
 wdired-confirm-overwrite t
 wdired-use-dired-vertical-movement nil
 wdired-allow-to-redirect-links t
 wdired-allow-to-change-permissions t
 ;; >> files/auto-save
 ;; done in "eli-backup.el"
 ;; >> wp/view
 view-read-only nil
 view-scroll-auto-exit nil
 ;; >> external/processes/comint
 comint-prompt-read-only nil
 comint-input-autoexpand nil
 comint-input-ignoredups t
 comint-scroll-to-bottom-on-input nil
 comint-move-point-for-output nil
 comint-scroll-show-maximum-output 1
 comint-buffer-maximum-size 5000
 comint-input-ring-size 500
 ;; t is nice, but sometimes screws up (try `cat', or enter a number in mz)
 ;; a way to fix this is to use `stty echo'
 comint-process-echoes t
 comint-eol-on-send t
 comint-use-prompt-regexp nil
 ;; >> external/processes/comint/comint-completion
 comint-completion-addsuffix t
 ;; >> external/processes/shell
 shell-input-autoexpand nil ; let the shell do that
 ;; >> external/processes/compilation
 compilation-window-height 10
 compilation-ask-about-save t
 compilation-skip-threshold 1
 compilation-skip-visited nil
 compilation-scroll-output t
 mode-compile-save-all-p nil
 mode-compile-always-save-buffer-p t
 ;; >> external/processes/compilation/next-error
 next-error-highlight t
 next-error-highlight-no-select t
 ;; >> external/processes/grep
 grep-window-height 10
 grep-highlight-matches 'auto
 grep-scroll-output nil ; unlike compile
 ;; >> external/processes/term
 term-input-autoexpand nil
 term-input-ignoredups t
 term-scroll-to-bottom-on-output 'others
 term-scroll-show-maximum-output nil
 term-buffer-maximum-size 4096
 ;; >> programming/languages/sh/sh-script
 sh-indentation 2
 sh-basic-offset 2 ; also needed
 ;; >> programming/languages/ruby
 ruby-indent-tabs-mode nil
 ruby-indent-level 2
 ruby-comment-column 32
 ruby-insert-encoding-magic-comment t
 ruby-use-encoding-map t
 ;; >> programming/languages/lisp
 eval-expression-print-level 5
 eval-expression-print-length 20
 eval-expression-debug-on-error t
 parens-require-spaces t
 ;; >> programming/languages/lisp/scheme
 scheme-mit-dialect nil
 scheme-program-name "racket"
 ;; >> programming/languages/c
 ;; c-basic-offset 2 <--?
 ;; >> programming/languages/js
 js-indent-level 2
 ;; >> programming/languages/pascal
 pascal-indent-level 2
 ;; >> programming/languages/perl
 perl-indent-level 2
 ;; >> programming/languages/tcl
 tcl-indent-level 2
 ;; >> programming/tools/calculator
 calculator-electric-mode t
 calculator-bind-escape t
 ;; >> programming/tools/compare-windows
 ;; compare-windows-sync nil ; no syncing, but might be useful to use something
 compare-windows-sync 'compare-windows-sync-default-function
 compare-windows-sync-string-size 24
 compare-windows-highlight t
 ;; >> programming/tools/vc
 vc-follow-symlinks 'ask ; shouldn't matter with `find-file-visit-truename'
 vc-display-status t
 vc-suppress-confirm nil
 ;; >> programming/tools/vc/vc-git
 vc-git-diff-switches t ; might be useful to add stuff like `-x -w'
 ;; >> programming/tools/vc/vc-svn
 vc-svn-diff-switches t ; might be useful to add stuff like `-x -w'
 ;; >> applications/ispell
 ispell-highlight-p 'block
 ;; ispell-highlight-face 'isearch <-- use default (flyspell changes it)
 ispell-check-comments t
 ispell-query-replace-choices nil
 ispell-following-word nil
 ispell-help-in-bufferp nil
 ispell-quietly nil
 ispell-format-word-function (lambda (s) (concat "\"" s "\""))
 ispell-use-framepop-p nil
 ;; >> applications/calendar
 ;; done below
 ;; >> applications/mail
 read-mail-command 'rmail ; change to vm?
 mail-user-agent 'sendmail-user-agent ; maybe use feedmail
 send-mail-function 'sendmail-send-it
 ;;     or use this (as feedmail says): send-mail-function 'feedmail-send-it
 ;; >> development/docs/info
 Info-fontify-visited-nodes t
 Info-use-header-line t
 Info-additional-directory-list '()
 ;; >> development/internal
 major-mode 'indented-text-mode
 ;; >> development/internal/alloc
 garbage-collection-messages nil
 ;; >> development/internal/limits
 max-specpdl-size 10000
 max-lisp-eval-depth 2000
 ;; >> development/debug
 debug-on-error nil
 debug-on-quit nil
 message-log-max 400
 ;; >> environment
 remote-shell-program "ssh"
 ;; >> environment/initialization
 initial-buffer-choice nil
 inhibit-startup-screen t
 inhibit-startup-echo-area-message (user-login-name) ; always apply
 ;; inhibit-default-init t ; might be useful to ignore distro-stupidities
 inhibit-startup-buffer-menu t ; no need for this silly thing on startup
 initial-major-mode 'indented-text-mode
 initial-scratch-message nil ; v23: no scratch text
 ;; >> environment/minibuffer
 completion-auto-help t
 ;; completion-styles '(basic partial-completion emacs22) ; what's this??
 ;; seems like this works better, `basic' will claim that `select-frame' (for
 ;; example) is a sole completion; and `emacs22' adds a completion based on the
 ;; stuff before the cursor
 completion-styles '(partial-completion)
 completion-cycle-threshold nil
 completions-format 'vertical ; easier to find things
 insert-default-directory t
 completion-ignore-case t
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case t
 completion-pcm-word-delimiters "-_./:| " ; same as `PC-word-delimiters'
 completion-pcm-complete-word-inserts-delimiters nil
 echo-keystrokes 1
 enable-recursive-minibuffers nil
 history-length 500
 history-delete-duplicates t
 minibuffer-auto-raise nil
 ;; minibuffer-prompt-properties '(read-only t face minibuffer-prompt point-entered minibuffer-avoid-prompt)
 ;; file-name-shadow-mode t ; doesn't really matter if we do the electric thing
 ;; file-name-shadow-properties '(face file-name-shadow field shadow)
 ;; file-name-shadow-tty-properties '(before-string "{" after-string "} " field shadow)
 ;; >> environment/terminals/terminal
 terminal-escape-char 30
 terminal-redisplay-interval 3000 ; not used heavily anyway
 ;; >> environment/frames
 ;; some stuff in win-init.el
 ;; >> environment/frames/scrolling
 scroll-error-top-bottom t ; doesn't matter anyway
 auto-hscroll-mode t
 ;; >> environment/frames/cursor
 display-hourglass t
 cursor-type t ; not a custom
 cursor-in-non-selected-windows '(hbar . 4)
 ;; >> environment/frames/fringe
 fringe-mode 2
 indicate-empty-lines t
 indicate-buffer-boundaries 'left
 overflow-newline-into-fringe t
 ;; >> environment/frames/desktop
 ;; done in "desktop-init.el"
 ;; >> environment/mode-line
 ;; done in "modeline.el"
 ;; >> environment/display
 idle-update-delay 0.5
 ctl-arrow t
 truncate-lines nil
 word-wrap nil
 visible-bell nil
 truncate-partial-width-windows 40
 line-number-display-limit 5000000
 line-number-display-limit-width 1000
 highlight-nonselected-windows nil
 mouse-autoselect-window nil
 ;; scalable-fonts-allowed t ; '("") allows all?  I don't see how to use this.
 x-stretch-cursor t
 text-scale-mode-step 1.2
 ;; >> environment/keyboard
 help-event-list '(help f1) ; maybe remove f1?
 suggest-key-bindings 1
 ;; >> environment/keyboard/chistory
 list-command-history-max 320
 ;; >> environment/menu
 yank-menu-length 40
 buffers-menu-max-size 40
 buffers-menu-buffer-name-length 40
 buffers-menu-show-directories 'unless-uniquify
 buffers-menu-show-status t
 use-dialog-box t
 use-file-dialog t
 menu-prompting t
 ;; >> environment/menu/tmm
 tmm-mid-prompt "=>"
 ;; >> environment/windows
 window-min-height 2
 window-min-width 4
 split-window-keep-point t
 pop-up-frames nil
 display-buffer-reuse-frames nil
 pop-up-windows t ; maybe not?
 even-window-heights nil
 recenter-positions '(middle top bottom)
 scroll-up-aggressively 0.2   ; Try hard to get plain 1-line scrolling
 scroll-down-aggressively 0.2 ; (0.0 should do it, but it doesn't)
 next-screen-context-lines 2
 scroll-preserve-screen-position 'in-place ; use my scroll-in-place feature
 window-combination-resize t ; try to see if this is useful
 scroll-step 0
 scroll-conservatively 10
 scroll-margin 0 ; maybe use 1? (forces a line, even at bottom or on C-0 C-L)
 hscroll-margin 1 ; this works better, it seems
 hscroll-step 4
 ;; >> help
 ;; help-window-select 'other    what does this do?
 help-downcase-arguments t
 ;; >> help/customize/custom-buffer
 custom-unlispify-remove-prefixes nil
 custom-unlispify-tag-names nil
 custom-buffer-sort-alphabetically nil
 custom-buffer-order-groups 'last
 custom-buffer-style 'links
 custom-buffer-done-kill t
 custom-buffer-indent 3
 custom-magic-show-button t
 ;; >> help/apropos
 apropos-do-all t
 apropos-sort-by-scores t
 apropos-documentation-sort-by-scores t ; (this is on by default)
 ;; >> misc (non-custom)
 minibuffer-allow-text-properties nil
 resize-mini-windows t        ; exactly as needed
 max-mini-window-height 0.25  ; up to 1/4 screen height
 redisplay-dont-pause t       ; faster display response
 )

;; Racket files
(add-to-list 'auto-mode-alist '("\\.rkt.?$" . scheme-mode))

;; Enables
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)

;; avoid the annoying split window when loading multiple files
(add-hook 'emacs-startup-hook 'delete-other-windows)

;; should be on by default
(unless auto-compression-mode (auto-compression-mode 1))

;; remember lots of files
(put 'file-name-history 'history-length 500)

;; need to do this to make it have an effect
(eval '(put 'inhibit-startup-echo-area-message 'saved-value t))

;; do it this way to avoid auto-loading cc-mode
(eval-after-load "cc-mode" '(c-add-style "java" '((c-basic-offset . 2))))

(unless mouse-wheel-mode (mwheel-install)) ; should be on

(when (eq system-type 'windows-nt)
  (setq-default
   ;;
   w32-alt-is-meta t
   w32-apps-modifier 'super
   w32-lwindow-modifier 'super
   w32-rwindow-modifier 'super
   w32-pass-alt-to-system nil
   w32-pass-lwindow-to-system t ; they still work fine
   w32-pass-rwindow-to-system t ;   as a super modifier
   w32-capslock-is-shiftlock nil
   w32-enable-caps-lock t
   w32-enable-num-lock t
   w32-scroll-lock-modifier nil
   ;;
   w32-enable-synthesized-fonts nil
   w32-list-proportional-fonts t
   ;; w32-ansi-code-page 1252
   w32-use-w32-font-dialog t
   ;;
   buffer-file-coding-system 'mule-utf-8-unix ; LFs by default
   w32-allow-system-shell t
   w32-quote-process-args t
   w32-downcase-file-names nil
   w32-get-true-file-attributes t
   ;;
   w32-grab-focus-on-raise t
   ;; No mail indicator on mode-line.
   display-time-mail-file 'x
   ))

;; Lisp indentations
(put 'block                 'lisp-indent-function 1)
(put 'case                  'lisp-indent-function 1)
(put 'cond                  'lisp-indent-function 0)
(put 'do                    'lisp-indent-function 2)
(put 'do*                   'lisp-indent-function 2)
(put 'dolist                'lisp-indent-function 1)
(put 'dotimes               'lisp-indent-function 1)
(put 'eval-when             'lisp-indent-function 1)
(put 'flet                  'lisp-indent-function 1)
(put 'handler-case          'lisp-indent-function 1)
(put 'if                    'lisp-indent-function 1)
(put 'labels                'lisp-indent-function 1)
(put 'let                   'lisp-indent-function 1)
(put 'let*                  'lisp-indent-function 1)
(put 'macrolet              'lisp-indent-function 1)
(put 'multiple-value-bind   'lisp-indent-function 2)
(put 'unless                'lisp-indent-function 1)
(put 'select                'lisp-indent-function 1)
(put 'simple-error          'lisp-indent-function 1)
(put 'symbol-macrolet       'lisp-indent-function 1)
(put 'tagbody               'lisp-indent-function 0)
(put 'typecase              'lisp-indent-function 1)
(put 'unwind-protect        'lisp-indent-function 1)
(put 'with-clipping         'lisp-indent-function 1)
(put 'with-clipping-box     'lisp-indent-function 1)
(put 'with-delayed-redraw   'lisp-indent-function 1)
(put 'with-foreground-color 'lisp-indent-function 1)
(put 'with-background-color 'lisp-indent-function 1)
(put 'with-background-mode  'lisp-indent-function 1)
(put 'with-events-disabled  'lisp-indent-function 1)
(put 'with-font             'lisp-indent-function 1)
(put 'with-hourglass        'lisp-indent-function 0)
(put 'with-line-width       'lisp-indent-function 1)
(put 'with-lock             'lisp-indent-function 1)
(put 'with-mouse-captured   'lisp-indent-function 1)
(put 'with-open-file        'lisp-indent-function 1)
(put 'with-open-stream      'lisp-indent-function 1)
(put 'with-output-to-string 'lisp-indent-function 1)
(put 'with-paint-operation  'lisp-indent-function 1)
(put 'when                  'lisp-indent-function 1)
(put 'when-eli-feature      'lisp-indent-function 1)
(put 'while                 'lisp-indent-function 1)

;;-----------------------------------------------------------------------------
;; Calendar & Stuff...

(setq
 ;; Calendar
 calendar-week-start-day        0 ; weeks start on sunday
 calendar-offset                0 ; show main month at center
 calendar-date-display-form
   '((if dayname (concat dayname ", ")) day " " monthname " " year)
 calendar-time-display-form        ; 24-hour format time
   '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")"))
 calendar-today-visible-hook       ; mark today's date
   'calendar-mark-today
 ;; Holidays
 calendar-view-holidays-initially-flag nil ; no holidays display on calendar
 calendar-mark-holidays-flag          t   ; but do show marked dates
 calendar-hebrew-all-holidays-flag    t   ; show all hebrew things
 calendar-christian-all-holidays-flag nil ; but only major christian things
 calendar-islamic-all-holidays-flag   nil ; and major islamic things
 ;; Appt (disabled)
 ;; appt-issue-message             t ; check appointments
 ;; appt-message-warning-time     12 ; start nagging 12 mins before appt
 ;; appt-audible                   t ; beep before appt
 ;; appt-visible                   t ; also show message
 ;; appt-display-mode-line       nil ; and show time left in modeline
 ;; appt-msg-window                t ; show appt msg in another window
 ;; appt-display-duration         10 ; appt msg is on for 10 seconds
 ;; appt-display-diary             t ; display new diary every midnight
 ;; appt-display-interval          3 ; check apptmnts every 3 minutes
 ;; Diary (disabled)
 ;; view-diary-entries-initially   t ; open diary on calendar
 ;; number-of-diary-entries        1 ; show one day's diary
 ;; mark-diary-entries-in-calendar t
 ;; diary-file                     "~/.diary"
 ;; abbreviated-calendar-year      t ; allow [0-9][0-9] year specs
 ;; european-calendar-style        t ; use european date style
 ;; holidays-in-diary-buffer       t ; show holydays in diary
 ;; diary-list-include-blanks      t ; show days with no diary entries
 )
;; (add-hook 'list-diary-entries-hook 'sort-diary-entries) ; sort entries by time
;; (add-hook 'list-diary-entries-hook 'include-other-diary-files) ; parse #include
;; (add-hook 'mark-diary-entries-hook 'mark-included-diary-files) ; same
;; (add-hook 'diary-display-hook 'fancy-diary-display) ; fancy diary
;; (add-hook 'diary-display-hook 'appt-make-list) ; make appointments
;; (add-hook 'nongregorian-diary-listing-hook 'list-hebrew-diary-entries)
;; ;; (add-hook 'nongregorian-diary-listing-hook 'list-islamic-diary-entries)
;; (add-hook 'nongregorian-diary-marking-hook 'mark-hebrew-diary-entries)
;; ;; (add-hook 'nongregorian-diary-marking-hook 'mark-islamic-diary-entries)

'
(progn ; Code for inserting all custom-declared variables
  (require 'custom)
  (require 'cus-edit)
  (defun my-show-list (x)
    (insert "(")
    (cond ((consp x)
           (my-show-value (car x))
           (while (consp (setq x (cdr x)))
             (insert " ")
             (my-show-value (car x)))
           (unless (null x)
             (insert " . ")
             (my-show-value x))))
    (insert ")"))
  (defun my-show-vector (x)
    (insert "[")
    (let ((len (length x)))
      (unless (zerop len)
        (let ((i 0))
          (my-show-value (aref x i))
          (while (< (setq i (1+ i)) len)
            (insert " ")
            (my-show-value (aref x i))))))
    (insert "]"))
  (defun my-show-string (x)
    (insert "\"")
    (let ((len (length x)))
      (let ((i 0))
        (while (< i len)
          (let ((ch (aref x i)))
            (insert (cond ((eq ch ?\\) "\\\\")
                          ((eq ch ?\") "\\\"")
                          ((eq ch ?\n) "\\n")
                          ((eq ch ?\r) "\\r")
                          ((eq ch ?\f) "\\f")
                          ((eq ch ?\t) "\\t")
                          ((eq ch ?\e) "\\e")
                          ((eq ch ?\0) "\\0")
                          ((< ch 32) (format "\\0%o" ch))
                          ((< 127 ch) (format "\\0%o" ch))
                          (t ch))))
          (setq i (1+ i)))))
    (insert "\""))
  (defun my-show-value (x &optional booleanp)
    (cond ((null x) (insert (if booleanp "nil" "()")))
          ((consp x) (my-show-list x))
          ((vectorp x) (my-show-vector x))
          ((stringp x) (my-show-string x))
          ((or (subrp x) (byte-code-function-p x)) (insert "#<procedure>"))
          (t (insert (format "%S" x)))))
  (defun my-show-custom-variable (name)
    (setq customs-seen (cons name customs-seen))
    (custom-load-symbol name)
    (insert (format "(setq %S " name))
    (let ((x (symbol-value name))
          (boolp (eq 'boolean (get name 'custom-type))))
      (when (or (and (symbolp x)
                     (or (not (memq x '(t nil)))
                         (not boolp)))
                (consp x))
        (insert "'"))
      (my-show-value x boolp))
    (insert ")\n"))
  (defun my-show-custom-group (name nums names)
    (setq customs-seen (cons name customs-seen))
    (custom-load-symbol name)
    (insert (format "(progn ; %s%s`%S':\n;; >> %s\n"
                    (mapconcat 'number-to-string nums ".")
                    (if (null nums) "" ". ")
                    name
                    names))
    (let ((subnum 0))
      (mapc (lambda (item)
              (if (and (listp item)
                       (= 2 (length item))
                       (symbolp (car item))
                       (symbolp (cadr item)))
                (let ((sub (car item)) (type (cadr item)))
                  (cond
                    ((memq sub customs-seen) 'already-shown)
                    ((eq 'custom-group type)
                     (my-show-custom-group
                      sub
                      (append nums (list (setq subnum (1+ subnum))))
                      (format "%s%s%s"
                              names (if (equal names "") "" "/") sub )))
                    ((eq 'custom-variable type)
                     (when (boundp sub) (my-show-custom-variable sub)))
                    ((eq 'custom-face type) 'dont-show-faces)
                    (t (error "don't know about this type: %S" (cadr item)))))
                (error "bad item: %S" item)))
            (custom-sort-items (get name 'custom-group) nil 'last)))
    (insert ")\n"))
  (defun my-show-customs ()
    (let ((customs-seen '()))
      (buffer-disable-undo)
      (my-show-custom-group 'emacs '() "")
      (buffer-enable-undo)))
  (my-show-customs)
  )

;;; settings.el ends here
