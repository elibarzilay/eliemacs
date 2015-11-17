;; This file is a simple hook to compile "~/EliEmacs/eliemacs.el"

(require 'dired)
(require 'info)
(require 'shell)
(require 'help-mode)
(require 'cus-edit)
(require 'view)
(require 'bs)
(require 'server)

(byte-compile-file "~/EliEmacs/eliemacs.el")
