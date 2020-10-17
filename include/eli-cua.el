;;; eli-cua.el --- Eli's tweaks for cua.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(require 'cua-base)

(setq cua-enable-cua-keys t
      cua-remap-control-v t
      cua-remap-control-z t
      ;; cua-highlight-region-shift-only nil
      cua-prefix-override-inhibit-delay 0.5 ; longer
      cua-delete-selection t ; turns on (delete-selection-mode)
      cua-keep-region-after-copy nil
      cua-toggle-set-mark t
      cua-auto-mark-last-change nil ; maybe this is useful as t?
      cua-enable-register-prefix 'not-ctrl-u
      cua-delete-copy-to-register-0 t
      ;; cua-enable-region-auto-help nil
      ;; cua-enable-modeline-indications nil
      ;; cua-check-pending-input t
      cua-paste-pop-rotate-temporarily t
      cua-virtual-rectangle-edges t
      cua-auto-tabify-rectangles nil
      cua-rectangle-mark-key [(shift return)] ; before loading cua below
      cua-rectangle-modifier-key 'meta
      cua-enable-rectangle-auto-help t
      cua-global-mark-keep-visible t
      cua-global-mark-blink-cursor-interval 0.3
      cua-enable-cursor-indications t
      cua-normal-cursor-color      "yellow"
      cua-read-only-cursor-color   "orangered1"
      cua-overwrite-cursor-color   "green"
      cua-global-mark-cursor-color "cyan")

(put 'kill-word            'delete-selection 'supersede)
(put 'delete-word          'delete-selection 'supersede)
(put 'kill-sexp            'delete-selection 'supersede)
(put 'delete-sexp          'delete-selection 'supersede)
(put 'backward-kill-word   'delete-selection 'supersede)
(put 'backward-delete-word 'delete-selection 'supersede)
(put 'backward-kill-sexp   'delete-selection 'supersede)
(put 'backward-delete-sexp 'delete-selection 'supersede)

(put 'skeleton-pair-insert-maybe  'delete-selection t)
(put 'sh-maybe-here-document      'delete-selection t)
(put 'comint-delchar-or-maybe-eof 'delete-selection 'supersede)
(put 'completion-separator-self-insert-command     'delete-selection t)
(put 'completion-separator-self-insert-autofilling 'delete-selection t)

;; Not using completion mode now, and if this is reenabled, it should be fixed
;; (it makes the first use of cua-rectangle not work)

;; make it play nicely with the dynamic-completion-mode's remapping
;; (defadvice cua--init-rectangles (before add-more-self-inserts last preactivate)
;;   (define-key cua--rectangle-keymap
;;     [remap completion-separator-self-insert-command]
;;     'cua-insert-char-rectangle)
;;   (define-key cua--rectangle-keymap
;;     [remap completion-separator-self-insert-autofilling]
;;     'cua-insert-char-rectangle))

(cua-mode 1)

;; HACK: redefine `cua-set-rectangle-mark' so it can do something else
;; also avoid it in the minibuffer
(defvar-local eli-override-cua-set-rectangle-mark 'cua-set-rectangle-mark)
(defvar eli-tweaked-cua-rectangle-keymap nil)
(defun eli-cua-set-rectangle-mark ()
  "Calls `eli-override-cua-set-rectangle-mark' which normally holds
`cua-set-rectangle-mark'.  The indirection is so it can be overridden."
  (interactive)
  (call-interactively
   (if (minibufferp) (key-binding "\r") eli-override-cua-set-rectangle-mark))
  (when (and (not eli-tweaked-cua-rectangle-keymap)
             (boundp 'cua--rectangle-keymap))
    (define-keys cua--rectangle-keymap
      '("<right>"  cua-resize-rectangle-right)
      '("<left>"   cua-resize-rectangle-left)
      '("<up>"     cua-resize-rectangle-up)
      '("<down>"   cua-resize-rectangle-down)
      '("<home>"   cua-resize-rectangle-bol)
      '("<end>"    cua-resize-rectangle-eol)
      '("<C-home>" cua-resize-rectangle-top)
      '("<C-end>"  cua-resize-rectangle-bot)
      '("<prior>"  cua-resize-rectangle-page-up)
      '("<next>"   cua-resize-rectangle-page-down)
      '("DEL"      cua-delete-char-rectangle)
      )))
(define-keys
  cua-global-keymap
  '("<S-return>" eli-cua-set-rectangle-mark))

;; make M-mouse-1 activate cua-rect (load it when needed)
(defun eli-activate-cua-rect (e)
  "activate cua-rect"
  (interactive "e")
  (require 'cua-rect)
  (cua-mouse-set-rectangle-mark e))
(define-keys 'global
  '("<M-mouse-1>"      nil)
  '("<M-drag-mouse-1>" nil)
  '("<M-down-mouse-1>" eli-activate-cua-rect))

;; plain mouse-1 cancels an active rect and goes to the point
(defun eli-mouse-cua-cancel (e)
  "cancel cua-rect and set point"
  (interactive "e")
  (cua-cancel)
  (mouse-set-point e))

(eval-after-load 'cua-rect
  (lambda ()
    (cua--init-rectangles)
    (define-keys
      'global
      '("<M-mouse-1>"      nil)
      cua-global-keymap
      ;; '("<M-mouse-1>"      nil)
      '("<M-down-mouse-1>" cua-mouse-set-rectangle-mark)
      '("<M-drag-mouse-1>" cua-mouse-resize-rectangle)
      cua--rectangle-keymap
      '("<M-down-mouse-1>" cua-mouse-resize-rectangle)
      '("<mouse-1>"        eli-mouse-cua-cancel)
      )))

;;; eli-cua.el ends here
