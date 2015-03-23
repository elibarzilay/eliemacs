;;; ebuff-init.el --- Initialize electric-buffer-list stuff.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;; maybe switch to `bs-show' instead?
;;   (including `bs-cycle-previous' and `bs-cycle-next' instead of S-tab?)

(define-keys 'global '(list-buffers electric-buffer-list))

(eval-after-load "ebuff-menu" '(progn

(define-keys
  ;; Make the electric-buffer-menu a bit more user friendly (its a symbol so
  ;; nothing will happen if it is unbound when not using the electric version).
  'electric-buffer-menu-mode-map
  ;; Minimal protection against getting out by mistake
  '(previous-buffer             Electric-buffer-menu-quit)
  '(next-buffer                 Electric-buffer-menu-quit)
  '(other-window                Electric-buffer-menu-quit)
  '(eli-other-window            Electric-buffer-menu-quit)
  '(eli-next-buffer-acyclic     Electric-buffer-menu-quit)
  ;; '(burry-buffers                Electric-buffer-menu-quit)
  '(list-buffers                Electric-buffer-menu-quit)
  '(electric-buffer-list        Electric-buffer-menu-quit)
  ;; and make escape exit (if in a window system)
  (and window-system '([?\e]    Electric-buffer-menu-quit))
  (and window-system '([escape] Electric-buffer-menu-quit))
  (unless window-system '([?\e ?O] nil)) ; allow arrows in text mode
  '([?1]                        Electric-buffer-menu-select)
  '([?x]                        Buffer-menu-execute)
  ;; kind of works, except that it throws an error because of the below hook
  '([down-mouse-1]              Electric-buffer-menu-mouse-select)
  '([mouse-1]                   Electric-buffer-menu-mouse-select)
  )

;; Add a hook to fix 'ebuff-menu.el' -- exit Ebuff when click outside (like in
;; ehelp) - should be done by Emacs.
(add-hook
 'electric-buffer-menu-mode-hook
 (lambda ()
   ;; The following messes up clicking, but losing that is minor
   ;; compared to getting out of the ebuff window by mistake.
   (add-hook 'mouse-leave-buffer-hook 'Electric-buffer-menu-quit nil t)
   (add-hook 'kill-buffer-hook
             '(lambda ()
                (condition-case nil (Electric-buffer-menu-quit)
                  (error nil)))
             nil t)
   (fit-window-to-buffer)))

))

;;; ebuff-init.el ends here
