;;; eliemacs.el --- EliEmacs main file

(eval-and-compile (defconst eli-version
;;     /***************************************************************\    ;;
;;    /*****************************************************************\   ;;
;;    ***                                                             ***   ;;
;;    ***               Eli's Emacs initialization file               ***   ;;
                                "[2015-04-04]"
;;    *** Written by Eli Barzilay: Maze is Life!   <eli@barzilay.org> ***   ;;
;;    ***                                                             ***   ;;
;;    \*****************************************************************/   ;;
;;     \***************************************************************/    ;;
))


;;-----------------------------------------------------------------------------
;; Directories & setup for this file

(eval-and-compile
  (defvar eli-dir
    (expand-file-name
     ;; make it work when just evaluating this expression, and when compiling
     (file-name-directory
      (or load-file-name buffer-file-name default-directory)))
    "The full path of the EliEmacs directory.")
  (add-to-list 'load-path eli-dir)
  ;; including files from "include/", to generate a single .elc file
  (defvar eli-include-dir (concat eli-dir "include/")))
(defmacro load/include (file)
  (let ((include-file (concat eli-include-dir file ".el")))
    (if (file-readable-p include-file)
      (with-temp-buffer
        (insert "(progn\n")
        (insert-file-contents include-file)
        (goto-char (point-max))
        (insert ")")
        (goto-char (point-min))
        (read (current-buffer)))
      `(load ,file))))

(load/include "user-vars")
(load/include "utils")
(load/include "misc")
(load/include "settings")
(load/include "overrides")
(load/include "colors")
(load/include "win-init")
(load/include "edit-utils")
(load/include "shell-utils")
(load/include "eli-backup")
(load/include "mail-news")
(load/include "ebuff-init")
(load/include "minibuf")
(load/include "modeline")
(load/include "macro-keys")
(load/include "eli-marker")
(load/include "eli-cua")
(load/include "scroll-in-place")
(load/include "keys")
(when (file-accessible-directory-p (concat eli-dir "extras"))
  (load (concat eli-dir "extras/extras") t t))
(load/include "eli-logo")
(load/include "desktop-init")

;; I used to do it with an idle-timer and not emacs-startup-hook so it's done
;; after everything is initialized, but that makes the first key not update the
;; display.
(add-hook 'emacs-startup-hook
  (lambda ()
    (unless (input-pending-p)
      (let* ((eli-msg (concat "---===###>>>   Eli Barzilay: Maze is Life!  "
                              eli-version "   <<<###===---"))
             (msgs `(,eli-msg "Use `C-h e' for a quick reference." ,eli-msg)))
        (while msgs
          (message (car msgs))
          (setq msgs (and (sit-for 3 t) (cdr msgs))))))
    (message nil))
  t)

;;; eliemacs.el ends here
