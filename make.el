;; This file is a simple hook to compile "~/EliEmacs/eliemacs.el"

(defvar x-select-enable-clipboard nil)
(require 'time)
(require 'longlines)
(require 'uniquify)
(require 'cus-edit)
(require 'desktop)
(require 'ring)
(require 'comint)
(require 'completion)
(require 'view)
;; (load "w32-vars" t t)

(byte-compile-file "~/EliEmacs/eliemacs.el")
