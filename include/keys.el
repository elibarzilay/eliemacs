;;; keys.el --- Useful key bindings.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(define-keys

  ;; My help stuff
  'help-map
  '("e"                         eliemacs-quickref)

  ;; General: don't use `return', `escape', `tab', `backspace' -- use the
  ;; control characters instead -- otherwise, many bindings (for `RET', `ESC',
  ;; `TAB', `DEL') won't work

  ;; cursor keys
  'global
  ;; -- some not needed because they're always the default.
  ;;    (shift-select-mode is not an excuse because it might be disabled).
  '([(left)]                    backward-char)
  '([(control left)]            backward-word)
  '([(meta left)]               backward-sexp)
  '([(right)]                   forward-char)
  '([(control right)]           forward-word)
  '([(meta right)]              forward-sexp)
  '([(up)]                      eli-previous-line)
  '([(control up)]              scroll-down-1-stay)
  '([(meta up)]                 backward-paragraph)
  '([(down)]                    eli-next-line)
  '([(control down)]            scroll-up-1-stay)
  '([(meta down)]               forward-paragraph)
  '([(home)]                    eli-beginning-of-line)
  '([(control home)]            beginning-of-buffer)
  '([(meta home)]               backward-sentence)
  '([(control meta home)]       beginning-of-line) ; original
  '([(control ?c) (home)]       beginning-of-line) ; ...
  '([(end)]                     eli-end-of-line)
  '([(control end)]             end-of-buffer)
  '([(meta end)]                forward-sentence)
  '([(prior)]                   scroll-down)
  '([(control prior)]           scroll-down-1)
  '([(meta prior)]              backward-page)
  '([(next)]                    scroll-up)
  '([(control next)]            scroll-up-1)
  '([(meta next)]               forward-page)
  ;; insert/delete/backspace
  '([(insert)]                  overwrite-mode)
  '([(shift insert)]            yank)
  '([(control insert)]          kill-ring-save)
  '([(meta insert)]             yank-pop)
  '([(control shift insert)]    eli-yank-really-pop)
  '([(meta shift insert)]       eli-yank-really-pop)
  '([(delete)]                  delete-char)
  '([(shift delete)]            eli-kill-region)
  '([(control delete)]          kill-word)
  '([(meta delete)]             kill-sexp)
  '([(control shift delete)]    delete-word)
  '([(meta shift delete)]       delete-sexp)
  '([(?\C-?)]                   eli-backward-delete-char-unindent)
  '(backward-delete-char-untabify eli-backward-delete-char-unindent)
  lisp-mode-shared-map
  '([(?\C-?)]                   eli-backward-delete-char-unindent)
  '(backward-delete-char-untabify eli-backward-delete-char-unindent)
  'global
  '([(control backspace)]       backward-kill-word)
  '([(meta backspace)]          backward-kill-sexp)
  '([(control shift backspace)] backward-delete-word)
  '([(meta shift backspace)]    backward-delete-sexp)
  '([(control tab)]             eli-other-window)
  '([(control shift tab)]       nil) ; the above deals with shift
  '([(control meta tab)]        indent-region)
  '([(shift tab)]               eli-next-buffer-acyclic)
  '([(shift iso-lefttab)]       eli-next-buffer-acyclic) ; for some X setups
  ;; tab completes a symbol when editing a minibuffer lisp expression
  read-expression-map
  '([(tab)]                     lisp-complete-symbol)
  'global

  ;; F1 - file/buffer operations
  '([(f1)]                      find-file)
  '([(control f1)]              electric-buffer-list)
  '([(shift f1)]                find-file-literally)
  '([(control shift f1)]        speedbar-get-focus)
  ;; F2 - writing files
  '([(f2)]                      save-buffer)
  '([(control f2)]              save-some-buffers)
  '([(shift f2)]                eli-write-or-move-file)
  '([(control shift f2)]        save-buffers-kill-emacs)
  ;; F3 - inserting stuff
  '([(f3)]                      insert-file)
  '([(control f3)]              insert-buffer)
  '([(shift f3)]                insert-file-literally)
  '([(control shift f3)]        insert-register)
  ;; F4 - shell stuff
  '([(f4)]                      eli-shell-command)
  '([(control f4)]              eli-shell)
  '([(shift f4)]                eli-shell-command-on-buffer)
  '([(control shift f4)]        eli-term)
  ;; F5 - replacing
  '([(f5)]                      query-replace)
  '([(control f5)]              replace-string)
  '([(shift f5)]                query-replace-regexp)
  '([(control shift f5)]        replace-regexp)
  ;; F6 - spelling
  '([(f6)]                      ispell-word)
  '([(control f6)]              ispell-buffer)
  '([(shift f6)]                ispell-region)
  '([(control shift f6)]        ispell-complete-word)
  '([(control ?x) (f6)]         flyspell-mode)
  '([(?\e) (f6)]                flyspell-mode)
  ;; F7 - searching
  '([(f7)]                      isearch-forward)
  '([(control f7)]              isearch-forward-regexp)
  '([(shift f7)]                isearch-backward)
  '([(control shift f7)]        isearch-backward-regexp)
  ;; F8 - evaluating
  '([(f8)]                      eli-eval-last-sexp-or-region)
  '([(control ?x) (control ?e)] eli-eval-last-sexp-or-region)
  '([(control f8)]              eval-buffer)
  '([(shift f8)]                eval-region)
  '([(control shift f8)]        load-file)
  ;; F9 - apropos/compiling
  '([(f9)]                      apropos)
  '([(control f9)]              mode-compile)
  '([(shift f9)]                grep)
  '([(control shift f9)]        next-error)
  '([(control ?c) (control right)] next-error)
  '([(control ?c) (control left)]  previous-error)
  ;; F10 - buffer killing / menu emulation
  '([(f10)]                     kill-buffer)
  '([(control f10)]             kill-save-buffer)
  '([(shift f10)]               tmm-menubar)
  '([(control shift f10)]       kill-some-buffers)
  ;; F11 - register stuff
  '([(f11)]                     eli-marker)
  '([(control f11)]             eli-marker-copy)
  ;; '([(shift f11)]               ...)
  ;; '([(control shift f11)]       ...)
  ;; F12 - window misc
  '([(f12)]                     delete-other-windows)
  '([(control f12)]             goto-line)
  '([(shift f12)]               goto-char)
  '([(control shift f12)]       eli-toggle-lines-mode)
  ;; misc stuff
  'global
  '([(control ?m)]              newline-and-indent)
  '([(control ?j)]              newline)
  '([(control ?\\)]             undo)

  ;; splitting and changing window sizes
  'global
  '([(control ?x) (end)]        split-window-vertically)
  '([(control ?x) (home)]       split-window-horizontally)
  '([(control ?x) (up)]         shrink-window)
  '([(control ?x) (down)]       enlarge-window)
  '([(control ?x) (left)]       shrink-window-horizontally)
  '([(control ?x) (right)]      enlarge-window-horizontally)
  ;; these are still as the default
  '([(control ?x) (control left)]  unbury-buffer)
  '([(control ?x) (control right)] bury-buffer)

  ;; pairs
  'global
  '([(meta ?\()]                eli-insert-pair)
  (and window-system '([(meta ?\[)] eli-insert-pair))
  '([(meta ?\{)]                eli-insert-pair)
  '([(meta ?\<)]                eli-insert-pair)
  '([(meta ?\")]                eli-insert-pair)
  '([(meta ?\')]                eli-insert-pair)
  '([(meta ?\`)]                eli-insert-pair)
  '([(meta ?\*)]                eli-insert-pair)
  '([(meta ?\_)]                eli-insert-pair)

  ;; counter
  'global
  '([?\e ?\e ?=]                counter-set)
  '([?\e ?\e ?+]                counter-increment)
  '([?\e ?\e ?*]                counter-insert)

  ;; backward transpose keys.
  'global
  '([(control ?T)]              transpose-chars-backward)
  '([(meta ?T)]                 transpose-words-backward)
  '([(control ?x) (control ?T)] transpose-lines-backward)
  '([(control meta ?T)]         transpose-sexps-backward)

  ;; isearch keys:
  'global
  '([(meta ?s)]                 isearch-forward-regexp)
  '([(meta ?r)]                 isearch-backward-regexp)
  '([(meta ?S)]                 isearch-backward-regexp)
  '([(control ?S)]              isearch-backward)
  ;; have these also repeat a search
  isearch-mode-map
  '([(meta ?s)]                 isearch-repeat-forward)
  ;; '([(meta ?r)]                 isearch-repeat-backward)
  '([(meta ?S)]                 isearch-repeat-backward)
  '([(control ?S)]              isearch-repeat-backward)
  '([(f7)]                      isearch-repeat-forward)
  '([(control f7)]              isearch-repeat-forward)
  '([(shift f7)]                isearch-repeat-backward)
  '([(control shift f7)]        isearch-repeat-backward)
  ;; toggles
  '([(meta ?a)]                 isearch-highlight-regexp)
  '([(meta ?w)]                 isearch-toggle-word)
  '([(meta ?r)]                 isearch-toggle-regexp)
  '([(f5)]                      isearch-query-replace)
  '([(shift f5)]                isearch-query-replace-regexp)
  ;; sane yanking
  '([(control ?w)]              isearch-yank-word)
  '([(control ?f)]              isearch-yank-char)
  '([(control ?k)]              isearch-yank-line)
  '([(control ?y)]              isearch-yank-kill)
  ;; some more stuff
  '([(control ?t)]              isearch-toggle-case-fold)
  '([(control ?^)]              isearch-edit-string)
  ;; fix backspace & delete to behave the same in isearch mode
  '([(?\C-?)]                   isearch-delete-char)
  '([(delete)]                  isearch-delete-char)
  '([(shift backspace)]         isearch-del-char)
  ;; and fix f1 too
  '([(f1)]
    (lambda () (interactive) (isearch-done) (call-interactively 'find-file)))

  ;; key binding/unbinding
  'global
  '([(control ?c) ?k]           global-set-key)
  '([(control ?c) ?l]           local-set-key)
  '([(control ?c) ?b]           buffer-local-set-key)
  '([(control ?c) ?K]           global-unset-key)
  '([(control ?c) ?L]           local-unset-key)
  '([(control ?c) ?B]           buffer-local-unset-all)

  ;; variable setting
  'global
  '([(control ?c) ?s]           set-variable)
  '([(control ?c) (control ?s)] set-variable)

  ;; macro keys
  'global
  '([(meta ?z)]                 macro-key)
  '([(control ?`)]              self-recording-macro-key)
  '([(control ?~)]              self-recording-macro-key)

  ;; misc
  'global
  '([(control ?x) (f1)]         eli-logo)
  '([(control ?x) (f2)]         maze)
  '([(control ?x) (f12)]        widen)
  '([(control return)]          calculator)
  (and (not window-system) '([(meta control ?m)] calculator))
  '([(meta ?j)]                 eli-join-line)
  '([(control ?\;)]             eli-toggle-comment-line)
  '([(meta shift ?c)]           eli-compare-two-buffers)
  '([(control shift ?c)]        compare-windows)
  '([(control ?c) (control ?n)] browse-url-at-point)
  '([(control ?<)]              eli-use-smaller-font)
  '([(control ?>)]              eli-use-larger-font)
  '([(control shift ?f)]        font-lock-mode)
  '([(control shift ?l)]        font-lock-fontify-buffer)
  '([(control shift ?a)]        add-color-pattern)
  '([(control shift ?r)]        remove-added-color-pattern)
  '([(control ?x) (control shift ?a)] set-regexp-face)
  '([(control ?x) (control shift ?r)] remove-set-face-overlays)
  '([(control ?x) (control shift ?o)] set-region-face)
  '([(meta ?o) ?o]                    set-region-face) ; better than facemenu
  '([(meta ?o) (meta ?o)]             set-region-face)
  ;; '([(meta ?/)]                 complete) try dabbrev

  ;; XEmacs-like mouse bindings
  'global
  '([(shift down-mouse-1)]      nil)
  '([(shift mouse-1)]           mouse-save-then-kill)
  '([(mouse-3)]                 nil)
  '([(down-mouse-3)]            mouse-major-mode-menu)
  '([(shift mouse-3)]           nil)
  '([(shift down-mouse-3)]      mouse-appearance-menu)

  )

(eval-after-load "dired"
  '(define-keys dired-mode-map
     '([(?r)] wdired-change-to-wdired-mode)
     '([(?w)] wdired-change-to-wdired-mode) ; I always used that
     '([(?W)] dired-copy-filename-as-kill) ; originally on w
     ))

(eval-after-load "info"
  '(define-keys Info-mode-map
     '([(?l)]        Info-history-back)
     '([(?\C-?)]     Info-history-back)
     '([(shift tab)] nil)
     '([(meta ?s)]   nil)
     '([(? )]        Info-scroll-up)
     '([(shift ? )]  Info-scroll-down)))

(eval-after-load "help-mode"
  '(define-keys help-mode-map
     '([(?l)]        help-go-back)
     '([(?b)]        help-go-back)
     '([(?f)]        help-go-forward)
     ;; hack: use `backspace' (not `?\C-?') so view mode will not grab it for
     ;; scrolling back
     '([(backspace)] help-go-back)
     '([(shift tab)] nil)))

(eval-after-load "cus-edit"
  '(define-keys custom-mode-map
     '([(shift tab)] nil)))

(eval-after-load "view"
  '(define-keys view-mode-map
     '([(? )] eli-View-scroll-page-forward)
     '([(shift ? )] eli-View-scroll-page-backward)))

;;; keys.el ends here
