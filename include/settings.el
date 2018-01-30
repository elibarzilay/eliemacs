;;; settings.el --- Settings - mostly variables.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;; Maybe this is not needed? -- At least on Windows, Eli Zaretski said:
;; "I would advise against setting the UTF-8 language environment on
;; MS-Windows. [...] You shouldn't need to do this, not in Emacs 25
;; anyway." -- No, it looks like it's needed after all, when we're
;; dealing with random files (with UTF-8 by default) and processes (in
;; cygwin, mostly).
(set-language-environment "UTF-8")

(when (functionp 'set-fringe-mode) (set-fringe-mode 2))
(require 'uniquify)

(setq-default
 ;; >> editing
 ;; >> editing/killing
 delete-active-region t
 kill-ring-max 100
 save-interprogram-paste-before-kill t
 kill-do-not-save-duplicates nil ; <- t is bad for macros!
 ;; kill-append-merge-undo nil ; ??
 yank-pop-change-selection nil
 kill-read-only-ok nil
 backward-delete-char-untabify-method 'untabify
 kill-whole-line t
 select-enable-clipboard t
 select-enable-primary t
 x-select-enable-clipboard t ; aliases for
 x-select-enable-primary t   ; the above
 select-active-regions nil
 x-select-enable-clipboard-manager t
 ;; >> editing/indent
 standard-indent 2
 tab-always-indent t ; should try 'complete
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
 ;; >> editing/fill/filladapt [uninteresting]
 ;; >> editing/fill/align [uninteresting]
 ;; >> editing/electricity
 electric-indent-mode nil    ; see comment at the top of "edit-utils.el"
 electric-pair-mode nil      ; --""-- should try it
 electric-pair-skip-self t
 electric-pair-open-newline-between-pairs nil ; ?
 electric-pair-skip-whitespace 'chomp
 electric-layout-mode t
 electric-quote-mode nil     ; maybe use this instead of the "‘’" display hack?
 ;; >> editing/matching
 case-replace t
 replace-char-fold nil ; try t?
 ;; replace-lax-whitespace t        ; maybe use t here, or leave this to
 ;; replace-regexp-lax-whitespace t ; "smart" searches, when really wanted
 ;; query-replace-from-history-variable 'query-replace-history-from
 ;; query-replace-to-history-variable 'query-replace-history-to
 query-replace-skip-read-only t ; is this a good idea?
 query-replace-show-replacement t
 query-replace-highlight t
 query-replace-lazy-highlight t
 case-fold-search t ; see also `search-default-mode' below
 ;; >> editing/matching/paren-matching
 ;; >> editing/matching/paren-matching/paren-blinking
 blink-matching-paren nil ; use `show-paren-mode'
 blink-matching-paren-distance 200000
 blink-matching-delay 0.5
 ;; >> editing/matching/paren-matching/paren-showing -- mic-paren still better!
 ;; show-paren-style 'parenthesis
 ;; show-paren-when-point-inside-paren t ; try this...
 ;; show-paren-when-point-in-periphery nil ; activates in weird places
 ;; >> editing/matching/paren-matching/mic-paren-matching [in mic-paren]
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
 isearch-allow-scroll t ; note my hack that exits isearch when scrolling away
 isearch-lax-whitespace t          ; (not custom options)
 isearch-regexp-lax-whitespace nil ; these are the defaults
 search-default-mode 'char-fold-to-regexp ; do char-folding too
 ;; >> editing/matching/isearch/lazy-highlight
 lazy-highlight-cleanup t
 lazy-highlight-initial-delay 0.25
 lazy-highlight-interval 0
 lazy-highlight-max-at-a-time 20
 ispell-lazy-highlight t
 ;; >> editing/matching/isearch/multi-isearch
 multi-isearch-search t ; ?
 multi-isearch-pause t  ; ?
 ;; >> editing/matching/bookmark [uninteresting]
 ;; >> editing/matching/completion
 completion-show-help t
 enable-completion t
 save-completions-flag t
 save-completions-retention-time (* 24 7 4)
 completions-file-versions-kept 1
 completion-search-distance 150000
 ;; >> editing/matching/completion/iswitchb [gone?]
 ;; >> editing/matching/ffap [uninteresting]
 ;; >> editing/matching/imenu [uninteresting]
 ;; >> editing/emulations
 ;; >> editing/emulations/cua [in "eli-cua.el"]
 ;; >> editing/emulations/edt [uninteresting]
 ;; >> editing/emulations/viper [uninteresting]
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
 ;; double-click-time 500
 ;; double-click-fuzz 3
 ;; >> editing/mouse/tooltip
 tooltip-mode t ; should really be done via a call
 tooltip-delay 0.8
 tooltip-short-delay 0.1
 tooltip-hide-delay 10
 tooltip-x-offset 5
 tooltip-y-offset 20
 tooltip-use-echo-area nil
 ;; >> editing/mouse/artist [uninteresting]
 ;; >> editing/mouse/avoid [uninteresting]
 ;; >> editing/mouse/goto-address [uninteresting]
 ;; >> editing/mouse/msb [uninteresting]
 ;; >> editing/mouse/strokes [uninteresting]
 ;; >> editing/i18n
 ;; >> editing/i18n/mule
 input-method-verbose-flag t
 input-method-highlight-flag t
 input-method-use-echo-area t
 ;; >> editing/i18n/mule/leim [uninteresting]
 ;; >> editing/i18n/double [uninteresting]
 ;; >> editing/i18n/iso-ascii [uninteresting]
 ;; >> editing/i18n/latin1-display [uninteresting]
 ;; >> editing/i18n/ogonek [uninteresting]
 ;; >> editing/undo
 undo-limit 200000 ; larger values
 undo-strong-limit 300000
 undo-outer-limit 30000000
 ;; >> editing/editing-basics
 require-final-newline t ; try nil with the following t?
 mode-require-final-newline t
 change-major-mode-with-file-name t
 read-quoted-char-radix 10
 use-empty-active-region nil ; t could be cute, but inconsistent for macros
 shift-select-mode t
 next-line-add-newlines nil
 track-eol nil ; I have an end-of-line function that does this better
 line-move-ignore-invisible t
 line-move-visual nil ; can turn on with C-S-f12
 transient-mark-mode t
 mark-even-if-inactive t
 parse-sexp-ignore-comments t
 ;; words-include-escapes nil ??
 ;; open-paren-in-column-0-is-defun-start t ??
 ;; >> editing/rectangle
 rectangle-preview t
 ;; >> convenience
 confirm-kill-emacs nil
 repeat-on-final-keystroke t
 ;; >> convenience/visual-line
 visual-line-fringe-indicators '(up-arrow down-arrow)
 ;; >> convenience/register [uninteresting]
 ;; >> convenience/Buffer-menu
 Buffer-menu-use-header-line t
 Buffer-menu-buffer+size-width 25
 Buffer-menu-name-width 19
 Buffer-menu-size-width 7
 Buffer-menu-mode-width 14
 Buffer-menu-use-frame-buffer-list t
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
 whitespace-global-modes t
 ;; whitespace-action '(report-on-bogus) ; too extreme
 ;; >> convenience/kmacro [uninteresting]
 ;; >> convenience/abbrev [uninteresting]
 ;; >> convenience/auto-insert [uninteresting]
 ;; >> convenience/auto-revert [uninteresting]
 ;; >> convenience/bs [uninteresting]
 ;; >> convenience/emacs-lock [uninteresting]
 ;; >> convenience/file-cache [uninteresting]
 ;; >> convenience/filesets [uninteresting]
 ;; >> convenience/follow [uninteresting]
 ;; >> convenience/hl-line [uninteresting]
 ;; >> convenience/ibuffer [uninteresting]
 ;; >> convenience/ido [uninteresting]
 ;; >> convenience/linum
 linum-format 'dynamic
 linum-eager t
 linum-delay 0.5
 ;; >> convenience/org-protocol [uninteresting]
 ;; >> convenience/reveal [uninteresting]
 ;; >> convenience/ruler-mode [uninteresting]
 ;; >> convenience/speedbar [uninteresting]
 ;; >> convenience/vcursor [uninteresting]
 ;; >> convenience/windmove [uninteresting]
 ;; >> files
 find-file-wildcards t
 find-file-suppress-same-file-warnings nil ; tell me when already visited
 large-file-warning-threshold 50000000
 auto-mode-case-fold t
 ;; >> files/backup [in "eli-backup.el"]
 ;; >> files/find-file
 find-file-existing-other-name t
 find-file-visit-truename t
 revert-without-query '() ; I have a better hack anyway
 find-file-run-dired t
 enable-local-variables t ; ask if there are unsafe vars
 enable-local-eval 'maybe
 confirm-nonexistent-file-or-buffer 'after-completion ; nice feature!
 ;; >> files/find-file/ff [uninteresting]
 ;; >> files/uniquify
 uniquify-buffer-name-style 'post-forward
 uniquify-after-kill-buffer-p t
 uniquify-ask-about-buffer-names-p nil ; undocumented?
 uniquify-min-dir-content 0
 uniquify-strip-common-suffix t
 ;; >> files/auto-save [in "eli-backup.el"]
 ;; >> files/ange-ftp [uninteresting]
 ;; >> files/dired
 list-directory-brief-switches "-CF"
 list-directory-verbose-switches "-laFh"
 ;; directory-free-space-program "df"
 directory-free-space-args "-h"
 dired-listing-switches "-laFh --dired --time-style=iso"
 dired-subdir-switches nil
 ;; dired-chown-program "chown"
 ;; dired-use-ls-dired t
 ;; dired-chmod-program "chmod"
 ;; dired-touch-program "touch"
 dired-dwim-target t          ; try this for a while
 dired-copy-preserve-time t
 dired-dnd-protocol-alist nil ; disable file copying on d&d to a dired buffer
                              ; so d&d always does the same thing
 dired-hide-details-hide-symlink-targets nil
 dired-hide-details-hide-information-lines t
 dired-auto-revert-buffer t
 dired-recursive-deletes 'top
 dired-no-confirm '() ; might be useful to add stuff here
 dired-recursive-copies 'always
 ;; >> files/dired/ls-lisp
 ls-lisp-emulation nil
 ls-lisp-ignore-case nil
 ls-lisp-use-string-collate nil
 ls-lisp-UCA-like-collation nil
 ls-lisp-dirs-first t
 ;; no link count, username, group (mostly useless, but very long)
 ;; ("ls-lisp" is used on windows to do ls by default)
 ls-lisp-verbosity '()
 ls-lisp-use-insert-directory-program nil ; to use ls-lisp (default on w32)
 ls-lisp-support-shell-wildcards t
 ls-lisp-format-time-list '("%m-%d %H:%M" "%Y-%m-%d")
 ls-lisp-use-localized-time-format nil
 ;; >> files/dired/dired-mark
 ;; dired-keep-marker-rename t ; maybe disable these things?
 ;; dired-keep-marker-copy 67
 ;; dired-keep-marker-hardlink 72
 ;; dired-keep-marker-symlink 89
 ;; >> files/dired/dired-faces [just faces]
 ;; >> files/dired/dired-x
 dired-find-subdir nil
 dired-enable-local-variables t
 dired-clean-up-buffers-too t
 dired-x-hands-off-my-keys t
 ;; >> files/dired/find-dired
 find-exec-terminator "+"
 ;; -h would be nice below, but messes up output alignment,
 ;; also, could try `--group-directories-first' `long-iso' always shows
 ;; everything, `iso' shows less info (no year for recents, no time for
 ;; old stuff)
 find-ls-option '("-exec ls -ldaF --time-style=iso {} +" . "-ldaF")
 find-ls-subdir-switches "-ldaF --time-style=iso"
 ;; >> files/dired/wdired
 wdired-use-interactive-rename nil
 wdired-confirm-overwrite t
 wdired-use-dired-vertical-movement 'sometimes
 wdired-allow-to-redirect-links t
 wdired-allow-to-change-permissions t
 wdired-keep-marker-rename t
 ;; >> files/plstore [uninteresting]
 ;; >> files/recentf [uninteresting]
 ;; >> files/shadow [uninteresting]
 ;; >> files/tramp [uninteresting]
 ;; >> wp
 ;; >> wp/outlines [uninteresting]
 ;; >> wp/tex [uninteresting]
 ;; >> wp/picture [uninteresting]
 ;; >> wp/bib [uninteresting]
 ;; >> wp/columns [uninteresting]
 ;; >> wp/lpr [uninteresting]
 ;; >> wp/ps-print [uninteresting]
 ;; >> wp/ebnf2ps [uninteresting]
 ;; >> wp/enriched [uninteresting]
 ;; >> wp/nroff [uninteresting]
 ;; >> wp/printing [uninteresting]
 ;; >> wp/refbib [uninteresting]
 ;; >> wp/refer [uninteresting]
 ;; >> wp/relax-ng [uninteresting]
 ;; >> wp/rst [uninteresting]
 ;; >> wp/table [uninteresting]
 ;; >> wp/tildify [uninteresting]
 ;; >> wp/view
 view-read-only nil
 view-scroll-auto-exit nil
 ;; >> data
 ;; >> data/compression [uninteresting]
 ;; >> data/epg [uninteresting]
 ;; >> data/archive [uninteresting]
 ;; >> data/conf [uninteresting]
 ;; >> data/dns-mode [uninteresting]
 ;; >> data/doc-view [uninteresting]
 ;; >> data/forms [uninteresting]
 ;; >> data/generic-x [uninteresting]
 ;; >> data/hexl [uninteresting]
 ;; >> data/remember [uninteresting]
 ;; >> data/save-place [uninteresting]
 ;; >> data/snmp [uninteresting]
 ;; >> data/sort [uninteresting]
 ;; >> data/tar [uninteresting]
 ;; >> data/time-stamp [uninteresting]
 ;; >> data/timeclock [uninteresting]
 ;; >> external
 ;; >> external/processes
 ;; >> external/processes/execute [uninteresting]
 ;; >> external/processes/processes-basics [uninteresting]
 ;; >> external/processes/ansi-colors
 ansi-color-for-comint-mode t
 ;; >> external/processes/comint
 comint-prompt-read-only nil
 comint-input-autoexpand nil
 comint-input-ignoredups t
 comint-scroll-to-bottom-on-input nil
 comint-move-point-for-output nil
 comint-scroll-show-maximum-output 1
 comint-buffer-maximum-size 5000
 comint-input-ring-size 2000
 ;; t is nice, but sometimes screws up (try `cat', or enter a number in racket)
 ;; a way to fix this is to use `stty echo'; better: only use t on linux (but
 ;; set to nil in case of non-shells, like running racket directly via
 ;; `explicit-shell-file-name')
 comint-process-echoes (not (eq system-type 'windows-nt))
 comint-eol-on-send t
 comint-use-prompt-regexp nil
 ;; >> external/processes/comint/comint-completion
 comint-completion-addsuffix t
 ;; >> external/processes/gud [uninteresting]
 ;; >> external/processes/pcomplete
 ;; => activate with (add-hook 'shell-mode-hook 'pcomplete-shell-setup)
 pcomplete-ignore-case t
 pcomplete-autolist t
 pcomplete-termination-string " "
 ;; >> external/processes/compilation
 compilation-window-height 10
 compilation-read-command t
 compilation-ask-about-save t
 compilation-auto-jump-to-first-error nil
 compilation-skip-threshold 1
 compilation-skip-visited nil
 compilation-scroll-output t ; scroll to bottom on output
 compilation-always-kill nil
 mode-compile-save-all-p nil
 mode-compile-always-save-buffer-p t
 ;; >> external/processes/compilation/next-error
 next-error-highlight t
 next-error-highlight-no-select t
 next-error-recenter nil
 ;; >> external/processes/shell
 shell-input-autoexpand nil ; let the shell do that
 ;; >> external/processes/shell/shell-directories [uninteresting]
 ;; >> external/processes/shell/shell-faces [just faces]
 ;; >> external/processes/shell/dirtrack [uninteresting]
 ;; >> external/processes/executable [uninteresting]
 ;; >> external/processes/flyspell [uninteresting]
 ;; >> external/processes/grep
 grep-window-height 10
 grep-scroll-output nil
 grep-highlight-matches 'always
 ;; >> external/processes/metamail [uninteresting]
 ;; >> external/processes/proced [uninteresting]
 ;; >> external/processes/rlogin [uninteresting]
 ;; >> external/processes/socks [uninteresting]
 ;; >> external/processes/SQL [uninteresting]
 ;; >> external/processes/term
 term-input-autoexpand nil
 term-input-ignoredups t
 term-scroll-to-bottom-on-output 'others
 term-scroll-show-maximum-output nil
 term-buffer-maximum-size 4096
 ;; >> external/man [uninteresting]
 ;; >> external/browse-url [uninteresting]
 ;; >> external/postscript [uninteresting]
 ;; >> external/locate [uninteresting]
 ;; >> external/pinentry [uninteresting]
 ;; >> external/server [uninteresting]
 ;; >> comm
 ;; >> comm/url [uninteresting]
 ;; >> comm/netrc [uninteresting]
 ;; >> comm/gnutls [uninteresting]
 ;; >> comm/tls [uninteresting]
 ;; >> comm/nsm [uninteresting]
 ;; >> comm/bug-reference [uninteresting]
 ;; >> comm/dig [uninteresting]
 ;; >> comm/eudc [uninteresting]
 ;; >> comm/gravatar [uninteresting]
 ;; >> comm/ldap [uninteresting]
 ;; >> comm/net-utils [uninteresting]
 ;; >> comm/ntlm [uninteresting]
 ;; >> programming
 ;; >> programming/languages
 ;; >> programming/languages/prog-mode [uninteresting]
 ;; >> programming/languages/lisp
 eval-expression-print-level 5
 eval-expression-print-length 20
 eval-expression-debug-on-error t
 parens-require-spaces t
 ;; >> programming/languages/lisp/eldoc
 eldoc-minor-mode-string " Doc"
 global-eldoc-mode t
 ;; >> programming/languages/lisp/bytecomp [uninteresting]
 ;; >> programming/languages/lisp/pp [uninteresting]
 ;; >> programming/languages/lisp/find-function [uninteresting]
 ;; >> programming/languages/lisp/advice [uninteresting]
 ;; >> programming/languages/lisp/checkdoc [uninteresting]
 ;; >> programming/languages/lisp/lisp-indent [uninteresting]
 ;; >> programming/languages/lisp/edebug [uninteresting]
 ;; >> programming/languages/lisp/elp [uninteresting]
 ;; >> programming/languages/lisp/ert [empty]
 ;; >> programming/languages/lisp/gmm [uninteresting]
 ;; >> programming/languages/lisp/ielm [uninteresting]
 ;; >> programming/languages/lisp/inferior-lisp [uninteresting]
 ;; >> programming/languages/lisp/profiler [uninteresting]
 ;; >> programming/languages/lisp/re-builder [uninteresting]
 ;; >> programming/languages/lisp/scheme
 scheme-mit-dialect nil
 scheme-program-name "racket"
 ;; >> programming/languages/lisp/scheme/cmuscheme [uninteresting]
 ;; >> programming/languages/lisp/lisp-shadow [uninteresting]
 ;; >> programming/languages/lisp/testcover [uninteresting]
 ;; >> programming/languages/lisp/trace [uninteresting]
 ;; >> programming/languages/lisp/warnings [uninteresting]
 ;; >> programming/languages/lisp/xscheme [uninteresting]
 ;; >> programming/languages/c
 ;; c-basic-offset 2 <--?
 ;; >> programming/languages/c/c-macro [uninteresting]
 ;; >> programming/languages/c/cpp [uninteresting]
 ;; >> programming/languages/c/hide-ifdef [uninteresting]
 ;; >> programming/languages/vhdl [uninteresting]
 ;; >> programming/languages/nxml [uninteresting]
 ;; >> programming/languages/ada [uninteresting]
 ;; >> programming/languages/antlr [uninteresting]
 ;; >> programming/languages/asm [uninteresting]
 ;; >> programming/languages/bat-mode [empty?]
 ;; >> programming/languages/cfengine [uninteresting]
 ;; >> programming/languages/cperl [uninteresting]
 ;; >> programming/languages/css
 css-indent-offset 2
 ;; >> programming/languages/smie [uninteresting]
 ;; >> programming/languages/dcl [uninteresting]
 ;; >> programming/languages/f90 [uninteresting]
 ;; >> programming/languages/fortran [uninteresting]
 ;; >> programming/languages/icon [uninteresting]
 ;; >> programming/languages/idlwave [uninteresting]
 ;; >> programming/languages/info-lookup [uninteresting]
 ;; >> programming/languages/sgml [uninteresting]
 ;; >> programming/languages/js
 js-indent-level 2
 js-expr-indent-offset   0 ; expr continuation
 js-paren-indent-offset  0 ; after open paren and immediate newline
 js-square-indent-offset 0 ; same
 js-curly-indent-offset  0 ; same
 js-switch-indent-offset 0
 ;; >> programming/languages/ld-script [uninteresting]
 ;; >> programming/languages/m4 [uninteresting]
 ;; >> programming/languages/meta-font [uninteresting]
 ;; >> programming/languages/modula2 [uninteresting]
 ;; >> programming/languages/octave [uninteresting]
 ;; >> programming/languages/opascal
 opascal-indent-level 2
 ;; >> programming/languages/pascal
 pascal-indent-level 2
 ;; >> programming/languages/perl
 perl-indent-level 2
 ;; >> programming/languages/prolog [uninteresting]
 ;; >> programming/languages/PostScript [uninteresting]
 ;; >> programming/languages/python [uninteresting]
 ;; >> programming/languages/ruby
 ruby-indent-tabs-mode nil
 ruby-indent-level 2
 ruby-comment-column 32
 ruby-insert-encoding-magic-comment t
 ruby-use-encoding-map t
 ;; >> programming/languages/sh
 ;; >> programming/languages/sh/sh-script
 sh-indentation 2
 sh-basic-offset 2 ; also needed?
 ;; >> programming/languages/sieve [uninteresting]
 ;; >> programming/languages/simula [uninteresting]
 ;; >> programming/languages/tcl
 tcl-indent-level 2
 ;; >> programming/languages/vera [uninteresting]
 ;; >> programming/languages/verilog-mode [uninteresting]
 ;; >> programming/tools
 ;; >> programming/tools/semantic [uninteresting]
 ;; >> programming/tools/diff-mode [uninteresting]
 ;; >> programming/tools/tempo [uninteresting]
 ;; >> programming/tools/gdb [uninteresting]
 ;; >> programming/tools/which-func [uninteresting]
 ;; >> programming/tools/diff [uninteresting]
 ;; >> programming/tools/change-log [uninteresting]
 ;; >> programming/tools/calculator
 calculator-electric-mode t
 calculator-bind-escape t
 ;; >> programming/tools/check-declare [uninteresting]
 ;; >> programming/tools/compare-windows
 ;; compare-windows-sync nil ; no syncing, but might be useful to use something
 compare-windows-sync 'compare-windows-sync-default-function
 compare-windows-sync-string-size 24
 compare-windows-highlight t
 ;; >> programming/tools/copyright [uninteresting]
 ;; >> programming/tools/ebrowse [uninteresting]
 ;; >> programming/tools/ede [uninteresting]
 ;; >> programming/tools/makefile [uninteresting]
 ;; >> programming/tools/ediff [uninteresting]
 ;; >> programming/tools/elide-head [uninteresting]
 ;; >> programming/tools/emerge [uninteresting]
 ;; >> programming/tools/project-vc [uninteresting -- maybe will get interesting?]
 ;; >> programming/tools/xref [uninteresting]
 ;; >> programming/tools/etags [uninteresting]
 ;; >> programming/tools/flymake [uninteresting]
 ;; >> programming/tools/glasses [uninteresting]
 ;; >> programming/tools/pcl-cvs [uninteresting]
 ;; >> programming/tools/smerge [uninteresting]
 ;; >> programming/tools/soap-client [uninteresting]
 ;; >> programming/tools/srecode [uninteresting]
 ;; >> programming/tools/vc
 vc-follow-symlinks 'ask ; shouldn't matter with `find-file-visit-truename'
 vc-display-status t
 vc-suppress-confirm nil
 ;; >> programming/tools/vc/vc-git
 vc-git-diff-switches t ; might be useful to add stuff like `-x -w'
 ;; >> programming/tools/vc/vc-cvs [uninteresting]
 ;; >> programming/tools/vc/vc-bzr [uninteresting]
 ;; >> programming/tools/vc/vc-hg [uninteresting]
 ;; >> programming/tools/vc/vc-mtn [uninteresting]
 ;; >> programming/tools/vc/vc-rcs [uninteresting]
 ;; >> programming/tools/vc/vc-sccs [uninteresting]
 ;; >> programming/tools/vc/vc-src [uninteresting]
 ;; >> programming/tools/vc/vc-svn
 vc-svn-diff-switches t ; might be useful to add stuff like `-x -w'
 ;; >> applications
 ;; >> applications/package [uninteresting?]
 ;; >> applications/calendar [done below]
 ;; >> applications/mail [mostly elsewhere]
 read-mail-command 'rmail ; change to vm?
 mail-user-agent 'sendmail-user-agent ; maybe use feedmail
 ;; >> applications/mail/sendmail
 send-mail-function 'smtpmail-send-it
 ;;     or use this (as feedmail says): send-mail-function 'feedmail-send-it
 ;; >> applications/mail/gnus [elsewhere]
 ;; >> applications/news [elsewhere]
 ;; >> applications/games [uninteresting]
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
 ;; >> applications/eshell [uninteresting]
 ;; >> applications/calc [uninteresting]
 ;; >> applications/erc [uninteresting]
 ;; >> applications/htmlfontify [uninteresting]
 ;; >> applications/mpc [uninteresting]
 ;; >> applications/newsticker [uninteresting]
 ;; >> applications/rcirc [elsewhere]
 ;; >> applications/ses [uninteresting]
 ;; >> development
 ;; >> development/docs
 ;; >> development/docs/info
 Info-fontify-visited-nodes t
 Info-use-header-line t
 Info-additional-directory-list '()
 ;; >> development/docs/makeinfo [uninteresting]
 ;; >> development/docs/texinfo [uninteresting]
 ;; >> development/extensions [uninteresting]
 ;; >> development/internal
 major-mode 'indented-text-mode
 ;; >> development/internal/alloc
 garbage-collection-messages nil
 ;; >> development/internal/limits
 max-specpdl-size 10000
 max-lisp-eval-depth 2000
 ;; >> development/maint
 ;; >> development/maint/elint [uninteresting]
 ;; >> development/maint/lisp-mnt [uninteresting]
 ;; >> development/debug
 debug-on-error nil
 debug-on-quit nil
 message-log-max 400
 ;; >> development/debug/debugger [uninteresting]
 ;; >> environment
 remote-shell-program "ssh"
 ;; >> environment/minibuffer
 completion-auto-help t
 ;; completion-styles '(basic partial-completion emacs22) ; what's this??
 ;; seems like this works better, `basic' will claim that `select-frame' (for
 ;; example) is a sole completion; and `emacs22' adds a completion based on the
 ;; stuff before the cursor
 completion-styles '(partial-completion)
 ;; completion-category-overrides '() ; if there’s a need for different completion styles
 completion-cycle-threshold nil
 completions-format 'vertical ; easier to find things
 read-file-name-completion-ignore-case t
 insert-default-directory t
 completion-pcm-word-delimiters "-_./:| " ; same as `PC-word-delimiters'
 completion-pcm-complete-word-inserts-delimiters nil
 ;; file-name-shadow-properties '(face file-name-shadow field shadow)
 ;; file-name-shadow-tty-properties '(before-string "{" after-string "} " field shadow)
 ;; file-name-shadow-mode t ; doesn't really matter if we do the electric thing
 minibuffer-eldef-shorten-default t
 echo-keystrokes 1
 enable-recursive-minibuffers nil
 history-length 1000
 history-delete-duplicates t
 read-buffer-completion-ignore-case t
 ;; minibuffer-prompt-properties '(read-only t face minibuffer-prompt point-entered minibuffer-avoid-prompt)
 minibuffer-auto-raise nil
 completion-ignore-case t ; manually added
 ;; >> environment/minibuffer/icomplete [uninteresting]
 ;; >> environment/minibuffer/savehist [uninteresting]
 ;; >> environment/initialization
 initial-buffer-choice nil
 inhibit-startup-screen t
 inhibit-startup-echo-area-message (user-login-name) ; always apply
 ;; inhibit-default-init t ; might be useful to ignore distro-stupidities
 inhibit-startup-buffer-menu t ; no need for this silly thing on startup
 initial-major-mode 'indented-text-mode
 initial-scratch-message nil ; v23: no scratch text
 ;; >> environment/initialization/fancy-splash-screen [uninteresting]
 ;; >> environment/w32 [below]
 ;; >> environment/hardware [uninteresting]
 ;; >> environment/terminals [uninteresting]
 ;; >> environment/unix [empty]
 ;; >> environment/x [uninteresting]
 ;; >> environment/frames [some in "win-init.el"]
 ;; >> environment/frames/window-divider
 ;; window-divider-mode t ; maybe try this ?
 ;; window-divider-default-places t
 ;; window-divider-default-bottom-width 6
 ;; window-divider-default-right-width 6
 ;; >> environment/frames/cursor
 cursor-in-non-selected-windows '(hbar . 4)
 display-hourglass t
 ;; >> environment/frames/fringe
 fringe-mode 2
 indicate-empty-lines t
 indicate-buffer-boundaries 'left
 overflow-newline-into-fringe t
 ;; >> environment/frames/desktop [in "desktop-init.el"]
 ;; >> environment/frames/two-column [uninteresting]
 ;; >> environment/mode-line [in "modeline.el"]
 ;; >> environment/display
 ;; visual-order-cursor-movement t ; maybe use this and `right-char' etc?
 idle-update-delay 0.5
 tty-menu-open-use-tmm nil
 cursor-type t
 ctl-arrow t
 truncate-lines nil
 word-wrap nil
 selective-display-ellipses t
 visible-bell nil
 truncate-partial-width-windows 40
 line-number-display-limit 5000000
 line-number-display-limit-width 1000
 highlight-nonselected-windows nil
 mouse-autoselect-window nil
 ;; scalable-fonts-allowed t ; '("") allows all?  I don't see how to use this.
 x-stretch-cursor t
 text-scale-mode-step 1.2
 auto-hscroll-mode t ; not a custom
 ;; >> environment/installation [uninteresting]
 ;; >> environment/keyboard
 suggest-key-bindings 1
 help-event-list '(help f1) ; maybe remove f1?
 ;; >> environment/keyboard/chistory
 list-command-history-max 320
 ;; >> environment/keyboard/type-break [uninteresting]
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
 ;; >> environment/dnd [uninteresting]
 ;; >> environment/windows
 window-min-height 2
 window-min-width 4
 switch-to-visible-buffer t ; also affects `eli-next-buffer-acyclic' with arg
 split-window-keep-point t
 pop-up-frames nil
 display-buffer-reuse-frames nil
 pop-up-windows t ; maybe not?
 even-window-heights nil
 recenter-positions '(middle top bottom)
 scroll-error-top-bottom t    ; doesn't matter anyway
 scroll-up-aggressively 0.0   ; scrolling against the edge: no context
 scroll-down-aggressively 0.0 ;   (0.0 finally works, see also scroll-margin)
 ;; frame-resize-pixelwise t  ; maybe use this?
 next-screen-context-lines 2
 scroll-preserve-screen-position 'in-place ; uses *my* scroll-in-place feature
 window-combination-resize t  ; try to see if this is useful
 ;; window-resize-pixelwise t ; this would go with frame-resize-pixelwise
 scroll-step 1             ; not sure about this one
 scroll-conservatively 500 ; 0 or small => isearch to far place: point at top
 scroll-margin 0 ; >0 values don't work too well (and issues with C-up/down)
 hscroll-margin 2
 hscroll-step 4
 make-cursor-line-fully-visible t
 scroll-bar-adjust-thumb-portion nil
 ;; >> environment/windows/winner [uninteresting]
 ;; >> environment/dos-fns [uninteresting]
 ;; >> faces [in colors.el]
 ;; >> help
 help-window-select t ; always select the help window
 help-enable-auto-load t
 help-downcase-arguments t
 ;; >> help/customize
 ;; >> help/customize/custom-browse [uninteresting]
 ;; >> help/customize/custom-buffer
 custom-unlispify-remove-prefixes nil
 custom-unlispify-tag-names nil
 custom-buffer-sort-alphabetically nil
 custom-buffer-order-groups 'last
 custom-buffer-style 'links
 custom-buffer-done-kill t
 custom-buffer-indent 3
 custom-magic-show-button t
 ;; >> help/customize/custom-menu [uninteresting]
 ;; >> help/electric-help [uninteresting]
 ;; >> help/woman [uninteresting]
 ;; >> help/apropos
 apropos-do-all t
 apropos-sort-by-scores t
 apropos-documentation-sort-by-scores t ; (this is on by default)
 ;; >> help/help-at-pt [uninteresting]
 ;; >> multimedia
 ;; >> multimedia/image [uninteresting]
 image-animate-loop t
 ;; >> multimedia/image/iimage [uninteresting, but maybe useful]
 ;; >> multimedia/image-dired [uninteresting]
 ;; >> multimedia/thumbs [uninteresting]
 ;; >> misc (non-custom)
 minibuffer-allow-text-properties nil
 resize-mini-windows t        ; exactly as needed
 max-mini-window-height 0.25  ; up to 1/4 screen height
 redisplay-dont-pause t       ; faster display response
 gnutls-min-prime-bits nil    ; mainly avoid a warning, also default = better(?)
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

;; handle txz & tlz files
(eval-after-load "jka-cmpr-hook"
  '(progn
     (push ["\\.txz\\'" "XZ compressing" "xz" ("-c" "-q")
            "XZ uncompressing" "xz" ("-c" "-q" "-d") t nil "\3757zXZ\0"]
           jka-compr-compression-info-list)
     (push ["\\.tlz\\'" "LZip compressing" "lzip" ("-c" "-q")
            "LZip uncompressing" "lzip" ("-c" "-q" "-d") t nil "LZIP\1"]
           jka-compr-compression-info-list)
     (push '("\\.txz\\'" . tar-mode) jka-compr-mode-alist-additions)
     (push '("\\.tlz\\'" . tar-mode) jka-compr-mode-alist-additions)
     (jka-compr-update)))
;; should be on by default
(unless auto-compression-mode (auto-compression-mode 1))

;; remember lots of files
(put 'file-name-history 'history-length 500)

;; need to do this to make it have an effect (see also startup code)
(put 'inhibit-startup-echo-area-message 'saved-value t)

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
   w32-pass-multimedia-buttons-to-system t ; let players do their thing
   ;; w32-pass-extra-mouse-buttons-to-system <- might be needed
   w32-capslock-is-shiftlock nil
   w32-enable-caps-lock t
   w32-enable-num-lock t
   w32-scroll-lock-modifier nil
   ;;
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

' ; Code for inserting all custom-declared variables (second run is better)
(progn
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
          ((or (subrp x) (byte-code-function-p x)) (insert "#<proc>"))
          (t (insert (format "%S" x)))))
  (defun my-show-custom-variable (name)
    (setq customs-seen (cons name customs-seen))
    (custom-load-symbol name)
    ;; (insert (format "(setq %S " name))
    (insert (format " %S " name))
    (let ((x (symbol-value name))
          (boolp (eq 'boolean (get name 'custom-type))))
      (when (or (and (symbolp x)
                     (or (not (memq x '(t nil)))
                         (not boolp)))
                (consp x))
        (insert "'"))
      (my-show-value x boolp))
    ;; (insert ")\n")
    (insert "\n"))
  (defun my-show-custom-group (name nums names)
    (setq customs-seen (cons name customs-seen))
    (custom-load-symbol name)
    ;; (insert (format "(progn ; %s%s`%S':\n"
    ;;                 (mapconcat 'number-to-string nums ".")
    ;;                 (if (null nums) "" ". ")
    ;;                 name))
    (insert (format " ;; >> %s\n" names))
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
    ;; (insert ")\n")
    )
  (defun my-show-customs ()
    (let ((customs-seen '()))
      (buffer-disable-undo)
      (my-show-custom-group 'emacs '() "")
      (buffer-enable-undo)))
  (my-show-customs)
  )

;;; settings.el ends here
