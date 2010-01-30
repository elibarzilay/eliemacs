;;; settings.el --- Settings - mostly variables.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(set-language-environment "UTF-8")
(set-fringe-mode 2)
(require 'uniquify)

(setq-default
 ;; >> emacs/editing/killing
 kill-ring-max 100
 yank-pop-change-selection nil
 kill-read-only-ok nil
 backward-delete-char-untabify-method 'untabify
 kill-whole-line t
 select-active-regions nil
 x-select-enable-clipboard t
 x-select-enable-primary t
 ;; >> emacs/editing/indent
 standard-indent 2
 tab-always-indent t
 indent-tabs-mode nil
 ;; >> emacs/editing/paragraphs
 paragraph-ignore-fill-prefix t
 ;; >> emacs/editing/fill
 sentence-end-double-space t
 fill-individual-varying-indent nil
 colon-double-space nil
 adaptive-fill-mode t
 default-justification 'left
 fill-column 72
 ;; >> emacs/editing/fill/comment
 comment-fill-column nil
 comment-style 'indent
 ;; >> emacs/editing/fill/longlines
 longlines-auto-wrap t
 longlines-show-hard-newlines t
 longlines-show-effect (propertize "<@>\n" 'face 'escape-glyph)
 ;; >> emacs/editing/view
 view-read-only nil
 view-scroll-auto-exit nil
 ;; >> emacs/editing/matching
 case-replace t
 case-fold-search t
 ;; query-replace-from-history-variable 'query-replace-history-from
 ;; query-replace-to-history-variable 'query-replace-history-to
 ;; query-replace-skip-read-only nil
 query-replace-show-replacement t
 query-replace-highlight t
 query-replace-lazy-highlight t
 ;; >> emacs/editing/matching/paren-matching/paren-blinking
 blink-matching-paren nil ; use `show-paren-mode'
 blink-matching-paren-distance 200000
 blink-matching-delay 0.5
 ;; >> emacs/editing/matching/paren-matching/paren-showing
 show-paren-style 'parenthesis
 show-paren-delay 0.125
 ;; >> emacs/editing/matching/isearch
 search-exit-option t
 search-upper-case 'not-yanks
 search-nonincremental-instead t
 search-whitespace-regexp "\\s-+"
 search-invisible 'open
 isearch-hide-immediately t
 search-highlight t
 isearch-lazy-highlight t
 isearch-allow-scroll t
 ;; >> emacs/editing/matching/isearch/lazy-highlight
 lazy-highlight-cleanup t
 lazy-highlight-initial-delay 0.25
 lazy-highlight-interval 0
 lazy-highlight-max-at-a-time 20
 ispell-lazy-highlight t
 ;; >> emacs/editing/matching/isearch/multi-isearch
 multi-isearch-search t ; ?
 multi-isearch-pause t  ; ?
 ;; >> emacs/editing/matching/completion
 completion-show-help t
 enable-completion t
 save-completions-flag t
 save-completions-retention-time (* 24 7 4)
 completions-file-versions-kept 1
 completion-search-distance 150000
 ;; >> emacs/editing/emulation/cua
 ;; done in "eli-cua.el"
 ;; >> emacs/editing/mouse
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
 ;; >> emacs/editing/mouse/tooltip
 tooltip-mode t ; should really be done via a call
 tooltip-delay 0.8
 tooltip-short-delay 0.1
 tooltip-x-offset 5
 tooltip-y-offset 20
 ;; >> emacs/editing/undo
 undo-limit 200000
 undo-strong-limit 300000
 undo-outer-limit 30000000
 ;; >> emacs/editing/editing-basics
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
 ;; >> emacs/editing/editing-basics/pc-select
 pc-select-override-scroll-error t
 pc-select-meta-moves-sexps t
 ;; >> emacs/external/processes/comint
 comint-input-ring-size 500 ; (this is not a custom variable)
 comint-prompt-read-only nil
 comint-input-autoexpand nil
 comint-input-ignoredups t
 comint-scroll-to-bottom-on-input nil
 comint-move-point-for-output nil
 comint-scroll-show-maximum-output 1
 comint-buffer-maximum-size 5000
 ;; t is nice, but sometimes screws up (try `cat', or enter a number in mz)
 ;; a way to fix this is to use `stty echo'
 comint-process-echoes t
 comint-eol-on-send t
 comint-use-prompt-regexp nil
 ;; >> emacs/external/processes/comint/comint-completion
 comint-completion-addsuffix t
 ;; >> emacs/external/processes/compilation
 compilation-window-height 10
 compilation-ask-about-save t
 compilation-skip-threshold 1
 compilation-skip-visited nil
 compilation-scroll-output t
 ;; >> emacs/external/processes/compilation/next-error
 next-error-highlight t
 next-error-highlight-no-select t
 ;; >> emacs/external/processes/grep
 grep-window-height 10
 grep-scroll-output nil ; unlike compile
 ;; >> emacs/external/processes/shell
 shell-input-autoexpand nil ; let the shell do that
 ;; >> emacs/external/processes/term
 term-input-autoexpand nil
 term-input-ignoredups t
 term-scroll-to-bottom-on-output 'others
 term-scroll-show-maximum-output nil
 ;; >> emacs/convenience
 confirm-kill-emacs nil
 ;; >> emacs/convenience/visual-line
 visual-line-fringe-indicators '(up-arrow down-arrow)
 ;; >> emacs/convenience/Buffer-menu
 Buffer-menu-use-header-line t
 Buffer-menu-buffer+size-width 25
 Buffer-menu-mode-width 10
 ;; >> emacs/convenience/bs
 bs-must-always-show-regexp "[*]shell"
 bs-default-configuration "all"
 bs-alternative-configuration "files-and-scratch"
 bs-default-sort-name "by nothing" ; by visit order
 ;; >> emacs/convenience/bs/bs-appearance
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
 ;; >> emacs/convenience/calculator
 calculator-electric-mode t
 calculator-bind-escape t
 ;; >> emacs/convenience/linum
 linum-format 'dynamic
 linum-eager t
 linum-delay 0.5
 ;; >> emacs/convenience/partial-completion
 PC-first-char nil ; allways do smart completion
 PC-meta-flag t ; complete on TAB
 PC-word-delimiters "-_. :" ; same as `completion-pcm-word-delimiters'
 ;; >> emacs/programming/languages/lisp
 eval-expression-print-level 5
 eval-expression-print-length 20
 eval-expression-debug-on-error t
 parens-require-spaces t
 ;; >> emacs/programming/languages/lisp/scheme
 scheme-mit-dialect nil
 scheme-program-name "mzscheme"
 ;; >> emacs/programming/languages/c
 ;; c-basic-offset 2 <--?
 ;; >> emacs/programming/languages/pascal
 pascal-indent-level 2
 ;; >> emacs/programming/languages/perl
 perl-indent-level 2
 ;; >> emacs/programming/languages/sh/sh-script
 sh-require-final-newline '((sh . t) (csh . t) (pdksh . t) (bash . t)
                            (zsh . t))
 sh-indentation 2
 ;; >> emacs/programming/languages/tcl
 tcl-indent-level 2
 ;; >> emacs/programming/tools/compare-windows
 ;; compare-windows-sync nil ; no syncing, but might be useful to use something
 compare-windows-sync 'compare-windows-sync-default-function
 compare-windows-sync-string-size 32
 compare-windows-highlight t
 ;; >> emacs/programming/tools/vc
 vc-follow-symlinks 'ask ; shouldn't matter with `find-file-visit-truename'
 vc-display-status t
 vc-suppress-confirm nil
 vc-svn-diff-switches t ; might be useful to add stuff like `-x -w'
 ;; >> emacs/applications/uniquify
 uniquify-buffer-name-style 'post-forward
 uniquify-after-kill-buffer-p t
 uniquify-strip-common-suffix t
 ;; >> emacs/applications/mail
 read-mail-command 'rmail ; change to vm?
 mail-user-agent 'sendmail-user-agent ; maybe use feedmail
 ;;     or use this (as feedmail says): send-mail-function 'feedmail-send-it
 ;; >> emacs/applications/ispell
 ispell-highlight-p 'block
 ;; ispell-highlight-face 'isearch <-- use default (flyspell changes it)
 ispell-check-comments t
 ispell-query-replace-choices nil
 ispell-following-word nil
 ispell-help-in-bufferp nil
 ispell-quietly nil
 ispell-format-word-function 'upcase
 ispell-use-framepop-p nil
 ;; >> emacs/development/docs/info
 Info-fontify-visited-nodes t
 Info-use-header-line t
 Info-additional-directory-list '()
 ;; >> emacs/development/internal
 major-mode 'indented-text-mode
 ;; >> emacs/development/internal/alloc
 garbage-collection-messages nil
 ;; >> emacs/development/internal/limits
 max-specpdl-size 10000
 max-lisp-eval-depth 2000
 ;; >> emacs/development/debug
 debug-on-error nil
 debug-on-quit nil
 message-log-max 400
 ;; >> emacs/environment
 remote-shell-program "ssh"
 ;; >> emacs/environment/minibuffer
 completion-auto-help t
 ;; completion-styles '(basic partial-completion emacs22) ; what's this??
 ;; seems like this works better, `basic' will claim that `select-frame' (for
 ;; example) is a sole completion; and `emacs22' adds a completion based on the
 ;; stuff before the cursor
 completion-styles '(partial-completion)
 insert-default-directory t
 completion-ignore-case t
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case t
 completion-pcm-word-delimiters "-_. :" ; same as `PC-word-delimiters'
 ;; file-name-shadow-mode t ; doesn't really matter if we do the electric thing
 echo-keystrokes 1
 enable-recursive-minibuffers nil
 history-length 500
 history-delete-duplicates t
 minibuffer-auto-raise nil
 ;; minibuffer-prompt-properties '(read-only t face minibuffer-prompt)
 ;; file-name-shadow-properties '(face file-name-shadow field shadow)
 ;; file-name-shadow-tty-properties '(before-string "{" after-string "} " field shadow)
 ;; >> emacs/development/internal/initialization
 initial-buffer-choice nil
 inhibit-startup-screen t
 inhibit-startup-echo-area-message (user-login-name) ; always apply
 ;; inhibit-default-init t ; might be useful to ignore distro-stupidities
 inhibit-startup-buffer-menu t ; no need for this silly thing on startup
 initial-major-mode 'indented-text-mode
 initial-scratch-message nil ; v23: no scratch text
 ;; >> emacs/environment/terminals/terminal
 terminal-escape-char 30
 terminal-redisplay-interval 3000 ; not used heavily anyway
 ;; >> emacs/environment/frames
 pop-up-frames nil
 ;; >> emacs/environment/frames/scrolling
 auto-hscroll-mode t
 ;; >> emacs/environment/frames/cursor
 display-hourglass t
 cursor-type t ; not a custom
 cursor-in-non-selected-windows '(hbar . 4)
 ;; >> emacs/environment/frames/fringe
 indicate-empty-lines t
 indicate-buffer-boundaries 'left
 overflow-newline-into-fringe t
 ;; >> emacs/environment/display
 idle-update-delay 0.5
 truncate-lines nil
 word-wrap nil
 visible-bell nil
 truncate-partial-width-windows 40
 line-number-display-limit 5000000
 line-number-display-limit-width 1000
 highlight-nonselected-windows nil
 mouse-autoselect-window nil
 x-stretch-cursor t
 text-scale-mode-step 1.2
 ;; scalable-fonts-allowed t ; '("") allows all?  I don't see how to use this.
 ;; >> emacs/environment/dired
 list-directory-brief-switches "-CF"
 list-directory-verbose-switches "-laF"
 directory-free-space-program "df"
 directory-free-space-args "-h"
 completion-ignored-extensions (append '(".bak" "~" "#" ".obj")
                                       completion-ignored-extensions)
 dired-listing-switches "-laFh"
 dired-copy-preserve-time t
 dired-recursive-deletes 'top
 dired-recursive-copies 'top
 dired-no-confirm '() ; might be useful to add stuff here
 ;; >> emacs/environment/dired/find-dired
 find-dired-find-program "find"
 find-ls-option '("-exec ls -ldaFh {} \\;" . "-ldaFh")
 find-ls-subdir-switches "-laFh"
 ;; >> emacs/environment/dired/wdired
 wdired-use-interactive-rename nil
 wdired-confirm-overwrite t
 wdired-use-dired-vertical-movement nil
 wdired-allow-to-redirect-links t
 wdired-allow-to-change-permissions t
 ;; >> emacs/environment/keyboard
 suggest-key-bindings 1
 ;; keypad-setup nil                 ; can use these
 ;; keypad-numlock-setup nil         ; to setup how
 ;; keypad-shifted-setup nil         ; the keypad gets
 ;; keypad-numlock-shifted-setup nil ; used.
 ;; >> emacs/environment/keyboard/chistory
 list-command-history-max 320
 ;; >> emacs/environment/menu
 yank-menu-length 40
 buffers-menu-max-size 40
 buffers-menu-buffer-name-length 40
 buffers-menu-show-directories 'unless-uniquify
 buffers-menu-show-status t
 use-dialog-box t
 use-file-dialog t
 menu-prompting t
 ;; >> emacs/environment/windows
 pop-up-frames nil
 pop-up-windows t ; maybe not?
 split-window-keep-point t
 scroll-up-aggressively 0.01   ; Try hard to get plain 1-line scrolling
 scroll-down-aggressively 0.01 ; (0.0 should do it, but it doesn't)
 even-window-heights nil
 next-screen-context-lines 2
 window-min-height 2
 window-min-width 4
 scroll-preserve-screen-position 'always ; but use scroll-in-place anyway
 scroll-step 1
 scroll-conservatively 30
 scroll-margin 0 ; maybe use 1? (forces a line, even at bottom or on C-0 C-L)
 hscroll-step 4
 hscroll-margin 1 ; this works better, it seems
 ;; >> emacs/data/whitespace
 whitespace-style '(tabs ;newline spaces  ; only tabs
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
 ;; whitespace-space 'whitespace-space <- faces for these things
 ;; whitespace-hspace 'whitespace-hspace
 ;; whitespace-tab 'whitespace-tab
 ;; whitespace-newline 'whitespace-newline
 ;; whitespace-trailing 'whitespace-trailing
 ;; whitespace-line 'whitespace-line
 ;; whitespace-space-before-tab 'whitespace-space-before-tab
 ;; whitespace-indentation 'whitespace-indentation
 ;; whitespace-empty 'whitespace-empty
 ;; whitespace-space-after-tab 'whitespace-space-after-tab
 whitespace-global-modes t ; maybe only in scheme mode and turn on global mode?
 ;; whitespace-action '(report-on-bogus) ; too extreme
 ;; >> emacs/files
 find-file-wildcards t
 find-file-suppress-same-file-warnings nil ; tell me when already visited
 large-file-warning-threshold 50000000
 auto-mode-case-fold t
 ;; >> emacs/files/backup
 ;; done in "eli-backup.el"
 ;; >> emacs/files/find-file
 find-file-existing-other-name t
 find-file-visit-truename t
 revert-without-query '() ; I have a better hack anyway
 find-file-run-dired t
 enable-local-variables t ; ask if there are unsafe vars
 enable-local-eval 'maybe
 confirm-nonexistent-file-or-buffer 'after-completion ; nice feature!
 ;; >> emacs/files/auto-save
 ;; done in "eli-backup.el"
 ;; >> emacs/help/customize/custom-buffer
 custom-unlispify-remove-prefixes nil
 custom-unlispify-tag-names nil
 custom-buffer-done-kill t
 custom-buffer-sort-alphabetically nil
 custom-buffer-order-groups 'last
 custom-magic-show-button t
 ;; >> emacs/help/apropos
 apropos-do-all t
 apropos-sort-by-scores t ; try this
 apropos-documentation-sort-by-scores t ; (this is on by default)
 ;; >> emacs/misc (non-custom)
 minibuffer-allow-text-properties nil
 resize-mini-windows t        ; exactly as needed
 max-mini-window-height 0.25  ; up to 1/4 screen height
 redisplay-dont-pause t       ; faster display response
 )

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
   w32-apps-modifier 'hyper
   w32-lwindow-modifier 'hyper
   w32-rwindow-modifier 'hyper
   w32-pass-alt-to-system nil
   w32-pass-lwindow-to-system nil
   w32-pass-rwindow-to-system nil
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
  (defun my-show-custom-group (name &optional nums)
    (setq customs-seen (cons name customs-seen))
    (custom-load-symbol name)
    (insert (format "(progn ; %s%s`%S':\n"
                    (mapconcat 'number-to-string nums ".")
                    (if (null nums) "" ". ")
                    name))
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
                      sub (append nums (list (setq subnum (1+ subnum))))))
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
      (my-show-custom-group 'emacs)
      (buffer-enable-undo)))
  (my-show-customs)
  )

;;; settings.el ends here
