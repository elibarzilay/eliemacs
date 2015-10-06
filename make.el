;; This file is a simple hook to compile "~/EliEmacs/eliemacs.el"

(require 'dired)
(require 'info)
(require 'help-mode)
(require 'cus-edit)
(require 'view)
(require 'server)

(byte-compile-file "~/EliEmacs/eliemacs.el")
