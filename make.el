;; This file is a simple hook to compile "~/EliEmacs/eliemacs.el"

(require 'dired)
(require 'info)
(require 'shell)
(require 'help-mode)
(require 'cus-edit)
(require 'view)
(require 'bs)
(require 'server)
(require 'cua-base)
(require 'cua-rect)

(setq byte-compile-cond-use-jump-table nil)
(byte-compile-file "~/EliEmacs/eliemacs.el")
