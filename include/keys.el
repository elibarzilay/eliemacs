;;; keys.el --- Useful key bindings.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(define-keys

  ;; My help stuff
  'help-map
  '("e"               eliemacs-quickref)

  ;; General: don't use `return', `escape', `tab', `backspace' -- use the
  ;; control characters instead -- otherwise, many bindings (for `RET', `ESC',
  ;; `TAB', `DEL') won't work

  ;; cursor keys
  'global
  ;; -- some not needed because they're always the default.
  ;;    (shift-select-mode is not an excuse because it might be disabled).
  '("<left>"          backward-char)
  '("<C-left>"        backward-word)
  '("<M-left>"        backward-sexp)
  '("<right>"         forward-char)
  '("<C-right>"       forward-word)
  '("<M-right>"       forward-sexp)
  '("<up>"            eli-previous-line)
  '("<C-up>"          scroll-down-1-stay)
  '("<M-up>"          backward-paragraph)
  '("<down>"          eli-next-line)
  '("<C-down>"        scroll-up-1-stay)
  '("<M-down>"        forward-paragraph)
  '("<home>"          eli-beginning-of-line)
  '("<C-home>"        beginning-of-buffer)
  '("<M-home>"        backward-sentence)
  '("<C-M-home>"      beginning-of-line) ; original
  '("C-c <home>"      beginning-of-line) ; ...
  '("<end>"           eli-end-of-line)
  '("<C-end>"         end-of-buffer)
  '("<M-end>"         forward-sentence)
  '("<prior>"         scroll-down)
  '("<C-prior>"       scroll-down-1)
  '("<M-prior>"       backward-page)
  '("<next>"          scroll-up)
  '("<C-next>"        scroll-up-1)
  '("<M-next>"        forward-page)
  ;; insert/delete/backspace
  '("<insert>"        overwrite-mode)
  '("<S-insert>"      yank)
  '("<C-insert>"      kill-ring-save)
  '("<M-insert>"      yank-pop)
  '("<C-S-insert>"    eli-yank-really-pop)
  '("<M-S-insert>"    eli-yank-really-pop)
  '("<delete>"        delete-char)
  '("<S-delete>"      eli-kill-region)
  '("<C-delete>"      kill-word)
  '("<M-delete>"      kill-sexp)
  '("<C-S-delete>"    delete-word)
  '("<M-S-delete>"    delete-sexp)
  '("DEL"             eli-backward-delete-char-unindent)
  '(backward-delete-char-untabify eli-backward-delete-char-unindent)
  lisp-mode-shared-map
  '("DEL"             eli-backward-delete-char-unindent)
  '(backward-delete-char-untabify eli-backward-delete-char-unindent)
  'global
  '("<C-backspace>"   backward-kill-word)
  '("<M-backspace>"   backward-kill-sexp)
  '("<C-S-backspace>" backward-delete-word)
  '("<M-S-backspace>" backward-delete-sexp)
  '("<C-tab>"         eli-other-window)
  '("<C-S-tab>"       nil) ; the above deals with shift
  '("<C-M-tab>"       indent-region)
  '("<S-tab>"         eli-next-buffer-acyclic)
  '("<backtab>"       eli-next-buffer-acyclic)
  ;; tab completes a symbol when editing a minibuffer lisp expression
  read-expression-map
  '("TAB"             completion-at-point)
  ;; try to fix the mess with `backtab` or `iso-lefttab`: just use `tab`
  function-key-map
  '([S-tab] nil)
  local-function-key-map
  '([S-tab] nil) '([S-iso-lefttab] [S-tab]) '([iso-lefttab] [S-tab])
  '([C-S-iso-lefttab] [C-S-tab]) '([C-iso-lefttab] [C-S-tab])
  ;; '([S-backtab] [S-tab]) --> no --> need to do this on all new frames
  'global
  ;; F1 - file/buffer operations
  '("<f1>"            find-file)
  '("<C-f1>"          eli-show-buffers)
  '("<S-f1>"          find-file-literally)
  '("<C-S-f1>"        find-dired)
  '("ESC <M-f1>"      speedbar-get-focus)
  '("ESC ESC <f1>"    speedbar-get-focus)
  ;; F2 - writing files
  '("<f2>"            save-buffer)
  '("<C-f2>"          save-some-buffers)
  '("<S-f2>"          eli-write-or-move-file)
  '("<C-S-f2>"        save-buffers-kill-emacs)
  ;; F3 - inserting stuff
  '("<f3>"            insert-file)
  '("<C-f3>"          insert-buffer)
  '("<S-f3>"          insert-file-literally)
  '("<C-S-f3>"        insert-register)
  ;; F4 - shell stuff
  '("<f4>"            eli-shell-command)
  '("<C-f4>"          eli-shell)
  '("<S-f4>"          eli-shell-command-insert-output)
  '("<C-S-f4>"        eli-term)
  ;; F5 - replacing
  '("<f5>"            query-replace)
  '("<C-f5>"          replace-string)
  '("<S-f5>"          query-replace-regexp)
  '("<C-S-f5>"        replace-regexp)
  ;; F6 - spelling
  '("<f6>"            ispell-word)
  '("<C-f6>"          ispell-buffer)
  '("<S-f6>"          ispell-region)
  '("<C-S-f6>"        ispell-complete-word)
  '("C-x <f6>"        flyspell-mode)
  '("ESC <f6>"        flyspell-mode)
  ;; F7 - searching
  '("<f7>"            isearch-forward)
  '("<C-f7>"          isearch-forward-regexp)
  '("<S-f7>"          isearch-backward)
  '("<C-S-f7>"        isearch-backward-regexp)
  ;; F8 - evaluating
  '("<f8>"            eli-eval-last-sexp-or-region)
  '("C-x C-e"         eli-eval-last-sexp-or-region)
  '("<C-f8>"          eval-buffer)
  '("<S-f8>"          eval-region)
  '("<C-S-f8>"        load-file)
  ;; F9 - apropos/compiling
  '("<f9>"            apropos)
  '("<C-f9>"          mode-compile)
  '("<S-f9>"          grep)
  '("<C-S-f9>"        next-error)
  '("C-c <C-right>"   next-error)
  '("C-c <C-left>"    previous-error)
  ;; F10 - buffer killing / menu emulation
  '("<f10>"           kill-buffer)
  '("<C-f10>"         kill-save-buffer)
  '("<S-f10>"         menu-bar-open)
  '("<C-S-f10>"       kill-some-buffers)
  ;; F11 - register stuff
  '("<f11>"           eli-marker)
  '("<C-f11>"         eli-marker-copy)
  ;; '("<S-f11>"      ...)
  ;; '("<C-S-f11>"    ...)
  ;; F12 - window misc
  '("<f12>"           eli-delete-other-windows-or-restore)
  '("<C-f12>"         goto-line)
  '("<S-f12>"         goto-char)
  '("<C-S-f12>"       eli-toggle-lines-mode)
  ;; some WM-like bindings
  '("<M-f1>" raise-frame)
  '("<M-f3>" lower-frame)
  '("<M-f9>" iconify-or-deiconify-frame)
  '("<M-f10>" toggle-frame-maximized)
  '("<M-f11>" toggle-frame-fullscreen)

  ;; misc stuff
  'global
  '("RET"             newline-and-indent)
  '("C-j"             newline)
  '("C-\\"            undo)

  ;; splitting and changing window sizes
  'global
  '("C-x <end>"       split-window-vertically)
  '("C-x <home>"      split-window-horizontally)
  '("C-x <up>"        eli-resize-window)
  '("C-x <down>"      eli-resize-window)
  '("C-x <left>"      eli-resize-window)
  '("C-x <right>"     eli-resize-window)
  ;; these are still as the default
  '("C-x <C-left>"    unbury-buffer)
  '("C-x <C-right>"   bury-buffer)

  ;; pairs
  'global
  '("M-("             eli-insert-pair)
  (and window-system '("M-[" eli-insert-pair))
  '("M-{"             eli-insert-pair)
  '("M-<"             eli-insert-pair)
  '("M-\""            eli-insert-pair)
  '("M-'"             eli-insert-pair)
  '("M-`"             eli-insert-backquote-pair)
  '("M-*"             eli-insert-pair)
  '("M-_"             eli-insert-pair)

  ;; counter
  'global
  '("ESC M-="         counter-set)
  '("ESC M-+"         counter-increment)
  '("ESC M-*"         counter-insert)
  '("M-+"             increment-integer)

  ;; backward transpose keys.
  'global
  '("C-S-t"           transpose-chars-backward)
  '("M-T"             transpose-words-backward)
  '("C-x C-S-t"       transpose-lines-backward)
  '("C-M-S-t"         transpose-sexps-backward)

  ;; isearch keys:
  'global
  '("M-s"             isearch-forward-regexp)
  '("M-r"             isearch-backward-regexp)
  '("M-S"             isearch-backward-regexp)
  '("C-S-s"           isearch-backward)
  ;; have these also repeat a search
  isearch-mode-map
  ;; '("M-s"          isearch-repeat-forward)
  ;; '("M-r"          isearch-repeat-backward)
  ;; '("M-S"          isearch-repeat-backward)
  '("C-S-s"           isearch-repeat-backward)
  '("<f7>"            isearch-repeat-forward)
  '("<C-f7>"          isearch-repeat-forward)
  '("<S-f7>"          isearch-repeat-backward)
  '("<C-S-f7>"        isearch-repeat-backward)
  ;; toggles
  '("M-a"             isearch-highlight-regexp)
  '("M-w"             isearch-toggle-word)
  '("M-s"             isearch-toggle-symbol)
  '("M-r"             isearch-toggle-regexp)
  '("M-SPC"           isearch-toggle-lax-whitespace)
  '("<f5>"            isearch-query-replace)
  '("<S-f5>"          isearch-query-replace-regexp)
  ;; sane yanking
  '("C-w"             isearch-yank-word)
  '("C-f"             isearch-yank-char)
  '("C-k"             isearch-yank-line)
  '("C-y"             isearch-yank-kill)
  ;; some more stuff
  '("C-t"             eli-toggle-case-and-char-fold) ; isearch-toggle-case-fold
  '("M-c"             isearch-toggle-case-fold)
  '("M-e"             isearch-edit-string)
  '("C-^"             isearch-edit-string)
  ;; fix backspace & delete to behave the same in isearch mode
  '("DEL"             isearch-delete-char)
  '("<delete>"        isearch-delete-char)
  '("<S-backspace>"   isearch-del-char)
  ;; and fix f1 too
  '("<f1>"
    (lambda () (interactive) (isearch-done) (call-interactively 'find-file)))

  ;; key binding/unbinding
  'global
  '("C-c k"           global-set-key)
  '("C-c l"           local-set-key)
  '("C-c b"           buffer-local-set-key)
  '("C-c K"           global-unset-key)
  '("C-c L"           local-unset-key)
  '("C-c B"           buffer-local-unset-all)

  ;; variable setting
  'global
  '("C-c s"           set-variable)
  '("C-c C-s"         set-variable)

  ;; macro keys
  'global
  '("M-z"             macro-key)
  '("C-`"             self-recording-macro-key)
  '("C-~"             self-recording-macro-key)
  ;; include high function keys (useful with, eg, logitech gaming keyboard)
  '("<f13>"           self-recording-macro-key)
  '("<f14>"           self-recording-macro-key)
  '("<f15>"           self-recording-macro-key)
  '("<f16>"           self-recording-macro-key)
  '("<f17>"           self-recording-macro-key)
  '("<f18>"           self-recording-macro-key)

  ;; misc
  'global
  '("C-x <f1>"        eli-logo)
  '("C-x <f2>"        maze)
  '("C-x <f12>"       widen)
  '("<C-return>"      calculator)
  (and (not window-system) '("M-RET" calculator))
  '("M-."             dired-jump)
  '("M-j"             eli-join-line)
  '("C-;"             eli-toggle-comment-line)
  '("M-C"             eli-compare-two-buffers)
  '("C-S-c"           compare-windows)
  '("C-c C-n"         browse-url-at-point)
  '("C-<"             eli-use-smaller-font)
  '("C->"             eli-use-larger-font)
  '("C-S-q"           eli-flip-antialias)
  '("C-S-f"           font-lock-mode)
  '("C-S-l"           font-lock-fontify-buffer)
  '("C-S-a"           add-color-pattern)
  '("C-S-r"           remove-added-color-pattern)
  '("C-S-h"           hide-text)
  '("C-x C-S-a"       set-regexp-face)
  '("C-x C-S-r"       remove-set-face-overlays)
  '("C-x C-S-o"       set-region-face)
  '("M-o o"           set-region-face) ; better than facemenu
  '("M-o M-o"         set-region-face)
  ;; '("M-/"          complete) ; use dabbrev

  ;; XEmacs-like mouse bindings (clear menu bindings, use down for menus)
  'global
  '("<S-down-mouse-1>" nil)
  '("<S-mouse-1>"      mouse-save-then-kill)
  '("<mouse-3>"        nil)
  '("<down-mouse-3>"   mouse-major-mode-menu)
  '("<S-mouse-3>"      nil)
  '("<S-down-mouse-3>" mouse-appearance-menu)

  )

(eval-after-load "dired"
  '(progn (define-keys dired-mode-map
            '("l" eli-last-dired-buffer)
            '("f" eli-next-dired-buffer)
            '("r" wdired-change-to-wdired-mode)
            '("w" wdired-change-to-wdired-mode) ; I always used that
            '("W" dired-copy-filename-as-kill)  ; originally on w
            '([remap eli-next-line] dired-next-line)
            '([remap eli-previous-line] dired-previous-line)
            '("<M-next>"  dired-next-dirline)
            '("<M-prior>" dired-prev-dirline)
            '("* ." dired-mark-extension)
            '("* d" dired-flag-extension)
            ;; dired rebinds M-s as a prefix for its special searches
            ;; (which seem nice but broken), so disable it
            '("M-s" nil))
          (add-hook 'dired-mode-hook
            (lambda ()
              (setq truncate-lines t)
              (when list-buffers-directory
                (setq list-buffers-directory
                      (abbreviate-file-name list-buffers-directory)))))))

(eval-after-load "info"
  '(define-keys Info-mode-map
     '("l"       Info-history-back)
     '("DEL"     Info-history-back)
     '("<S-tab>" nil)
     '("M-s"     nil)
     '("SPC"     Info-scroll-up)
     '("S-SPC"   Info-scroll-down)))

(eval-after-load "help-mode"
  '(define-keys help-mode-map
     '("l"           help-go-back)
     '("b"           help-go-back)
     '("f"           help-go-forward)
     ;; hack: use `backspace' (not `DEL') so view mode will not grab it for
     ;; scrolling back
     '("<backspace>" help-go-back)
     '("<S-tab>"     nil)))

(eval-after-load "cus-edit"
  '(define-keys custom-mode-map
     '("<S-tab>" nil)))

(eval-after-load "view"
  '(define-keys view-mode-map
     '("SPC"   eli-View-scroll-page-forward)
     '("S-SPC" eli-View-scroll-page-backward)))

;;; keys.el ends here
