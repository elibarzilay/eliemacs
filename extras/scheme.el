;;; scheme.el --- Scheme (and DSSSL) editing mode

;; Copyright (C) 1986, 1987, 1988, 1997, 1998, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009  Free Software Foundation, Inc.

;; Author: Bill Rozas <jinx@martigny.ai.mit.edu>
;; Adapted-by: Dave Love <d.love@dl.ac.uk>
;; Keywords: languages, lisp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The major mode for editing Scheme-type Lisp code, very similar to
;; the Lisp mode documented in the Emacs manual.  `dsssl-mode' is a
;; variant of scheme-mode for editing DSSSL specifications for SGML
;; documents.  [As of Apr 1997, some pointers for DSSSL may be found,
;; for instance, at <URL:http://www.sil.org/sgml/related.html#dsssl>.]
;; All these Lisp-ish modes vary basically in details of the language
;; syntax they highlight/indent/index, but dsssl-mode uses "^;;;" as
;; the page-delimiter since ^L isn't normally a valid SGML character.
;;
;; For interacting with a Scheme interpreter See also `run-scheme' in
;; the `cmuscheme' package and also the implementation-specific
;; `xscheme' package.

;; Here's a recipe to generate a TAGS file for DSSSL, by the way:
;; etags --lang=scheme --regex='/[ \t]*(\(mode\|element\)[ \t
;; ]+\([^ \t(
;; ]+\)/\2/' --regex='/[ \t]*(element[ \t
;; ]*([^)]+[ \t
;; ]+\([^)]+\)[ \t
;; ]*)/\1/' --regex='/(declare[^ \t
;; ]*[ \t
;; ]+\([^ \t
;; ]+\)/\1/' "$@"

;;; Code:

(require 'lisp-mode)

(defvar scheme-mode-syntax-table
  (let ((st (make-syntax-table))
	(i 0))

    ;; Default is atom-constituent.
    (while (< i 256)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))

    ;; Word components.
    (setq i ?0)
    (while (<= i ?9)
      (modify-syntax-entry i "w   " st)
      (setq i (1+ i)))
    (setq i ?A)
    (while (<= i ?Z)
      (modify-syntax-entry i "w   " st)
      (setq i (1+ i)))
    (setq i ?a)
    (while (<= i ?z)
      (modify-syntax-entry i "w   " st)
      (setq i (1+ i)))

    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{ "(}  " st)
    (modify-syntax-entry ?} "){  " st)
    ;; ELI: was "\" 23bn" but that's a mess in some cases like scribble and
    ;; comments
    (modify-syntax-entry ?\| "  23bn" st)
    ;; Guile allows #! ... !# comments.
    ;; But SRFI-22 defines the comment as #!...\n instead.
    ;; Also Guile says that the !# should be on a line of its own.
    ;; It's too difficult to get it right, for too little benefit.
    ;; (modify-syntax-entry ?! "_ 2" st)

    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    ;;ELI {{{ from http://debbugs.gnu.org/cgi/bugreport.cgi?bug=3824
    ;; It's used for single-line comments as well as for #;(...) sexp-comments.
    ;; (modify-syntax-entry ?\; "< 2 " st)
    (modify-syntax-entry ?\; "<   " st)
    ;;ELI }}}
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?' "'   " st)
    (modify-syntax-entry ?` "'   " st)

    ;; Special characters
    (modify-syntax-entry ?, "'   " st)
    (modify-syntax-entry ?@ "'   " st)
    (modify-syntax-entry ?# "' 14b" st)
    (modify-syntax-entry ?\\ "\\   " st)
    st))

(defvar scheme-mode-abbrev-table nil)
(define-abbrev-table 'scheme-mode-abbrev-table ())

(defvar scheme-imenu-generic-expression
      '((nil
	 "^(def\\(?:ine\\)?\\(\\|-?\\(generic\\(\\|-procedure\\)\\|method\\|matcher\\)\\)*\\s-+(?\\(\\sw+\\)" 4)
	("Types"
	 "^(def\\(?:ine\\)?-\\(?:class\\|type\\)\\s-+(?\\(\\sw+\\)" 1)
	("Macros"
	 "^(\\(def\\(?:macro\\|subst\\)\\|define-\\(?:macro\\|syntax\\(?:-rule\\)?\\)\\)\\s-+(?\\(\\sw+\\)" 2))
  "Imenu generic expression for Scheme mode.  See `imenu-generic-expression'.")

(defun scheme-mode-variables ()
  (set-syntax-table scheme-mode-syntax-table)
  (setq local-abbrev-table scheme-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'normal-auto-fill-function);ELI
  (setq normal-auto-fill-function 'lisp-mode-auto-fill);ELI
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp ";;; \\|(....")
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (set (make-local-variable 'comment-add) 1)
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")
  (set (make-local-variable 'font-lock-comment-start-skip) ";+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'lisp-indent-function)
  (setq lisp-indent-function 'scheme-indent-function)
  (setq mode-line-process '("" scheme-mode-line-process))
  (set (make-local-variable 'imenu-case-fold-search) t)
  (setq imenu-generic-expression scheme-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist)
	'(("+-*/.<>=?!$%_&~^:" . "w")))
  (set (make-local-variable 'font-lock-defaults)
       '((scheme-font-lock-keywords
          scheme-font-lock-keywords-1 scheme-font-lock-keywords-2)
         nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
         beginning-of-defun
         (font-lock-mark-block-function . mark-defun)
         ;;ELI {{{
         ;; (font-lock-syntactic-face-function
         ;;  . scheme-font-lock-syntactic-face-function)
         (font-lock-syntactic-keywords . scheme-font-lock-syntactic-keywords)
         ;;ELI }}}
         (parse-sexp-lookup-properties . t)
         (font-lock-extra-managed-props syntax-table)))
  (set (make-local-variable 'lisp-doc-string-elt-property)
       'scheme-doc-string-elt))

(defvar scheme-mode-line-process "")

(defvar scheme-mode-map
  (let ((smap (make-sparse-keymap))
	(map (make-sparse-keymap "Scheme")))
    (set-keymap-parent smap lisp-mode-shared-map)
    (define-key smap [menu-bar scheme] (cons "Scheme" map))
    (define-key map [run-scheme] '("Run Inferior Scheme" . run-scheme))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)
    smap)
  "Keymap for Scheme mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

;; Used by cmuscheme
(defun scheme-mode-commands (map)
  ;;(define-key map "\t" 'indent-for-tab-command) ; default
  (define-key map "\177" 'backward-delete-char-untabify)
  (define-key map "\e\C-q" 'indent-sexp))

;;;###autoload
(defun scheme-mode ()
  "Major mode for editing Scheme code.
Editing commands are similar to those of `lisp-mode'.

In addition, if an inferior Scheme process is running, some additional
commands will be defined, for evaluating expressions and controlling
the interpreter, and the state of the process will be displayed in the
modeline of all Scheme buffers.  The names of commands that interact
with the Scheme process start with \"xscheme-\" if you use the MIT
Scheme-specific `xscheme' package; for more information see the
documentation for `xscheme-interaction-mode'.  Use \\[run-scheme] to
start an inferior Scheme using the more general `cmuscheme' package.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{scheme-mode-map}
Entry to this mode calls the value of `scheme-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map scheme-mode-map)
  (setq major-mode 'scheme-mode)
  (setq mode-name "Scheme")
  (scheme-mode-variables)
  (run-mode-hooks 'scheme-mode-hook))

(defgroup scheme nil
  "Editing Scheme code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'lisp)

(defcustom scheme-mit-dialect t
  "If non-nil, scheme mode is specialized for MIT Scheme.
Set this to nil if you normally use another dialect."
  :type 'boolean
  :group 'scheme)

(defcustom dsssl-sgml-declaration
  "<!DOCTYPE style-sheet PUBLIC \"-//James Clark//DTD DSSSL Style Sheet//EN\">
"
  "*An SGML declaration for the DSSSL file.
If it is defined as a string this will be inserted into an empty buffer
which is in `dsssl-mode'.  It is typically James Clark's style-sheet
doctype, as required for Jade."
  :type '(choice (string :tag "Specified string")
                 (const :tag "None" :value nil))
  :group 'scheme)

(defcustom scheme-mode-hook nil
  "Normal hook run when entering `scheme-mode'.
See `run-hooks'."
  :type 'hook
  :group 'scheme)

(defcustom dsssl-mode-hook nil
  "Normal hook run when entering `dsssl-mode'.
See `run-hooks'."
  :type 'hook
  :group 'scheme)

;; This is shared by cmuscheme and xscheme.
(defcustom scheme-program-name "scheme"
  "*Program invoked by the `run-scheme' command."
  :type 'string
  :group 'scheme)

(defvar dsssl-imenu-generic-expression
  ;; Perhaps this should also look for the style-sheet DTD tags.  I'm
  ;; not sure it's the best way to organize it; perhaps one type
  ;; should be at the first level, though you don't see this anyhow if
  ;; it gets split up.
  '(("Defines"
     "^(define\\s-+(?\\(\\sw+\\)" 1)
    ("Modes"
     "^\\s-*(mode\\s-+\\(\\(\\sw\\|\\s-\\)+\\)" 1)
    ("Elements"
     ;; (element foo ...) or (element (foo bar ...) ...)
     ;; Fixme: Perhaps it should do `root'.
     "^\\s-*(element\\s-+(?\\(\\(\\sw\\|\\s-\\)+\\))?" 1)
    ("Declarations"
     "^(declare\\(-\\sw+\\)+\\>\\s-+\\(\\sw+\\)" 2))
  "Imenu generic expression for DSSSL mode.  See `imenu-generic-expression'.")

;;ELI {{{
(defconst scheme-font-lock-syntactic-keywords
  ;; Treat sexp-comment markers as "whitespace".
  '(("#\\(;\\)"
     (1 (if (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
            ;; Check parser state to avoid problem with #|comment1|#;comment2
            nil '(6))))))
;;ELI }}}

(defconst scheme-font-lock-keywords-1
  (eval-when-compile
    (list
     ;;
     ;; Declarations.  Hannes Haug <hannes.haug@student.uni-tuebingen.de> says
     ;; this works for SOS, STklos, SCOOPS, Meroon and Tiny CLOS.
     (list (concat "(\\("
                   "module\\|"
                   "def\\(?:ine\\)?\\*?\\("
		   ;; Function names.
		   "\\(\\|-values\\|-public"
                   "\\|-?\\(?:\\|before\\|after\\|around\\)method\\|-?generic"
                   "\\(-procedure\\)?\\)\\|"
		   ;; Macro names, as variable names.  A bit dubious, this.
		   "\\(-syntax\\|-\\(?:-id\\)?macro\\|subst\\)\\|"
		   ;; Class names.
		   "-?\\(?:entity\\)?class\\|-?struct"
                   ;; Guile modules.
                   ;; "\\|-module"
		   "\\)\\)\\>"
		   ;; Any whitespace and declared object.
		   "[ \t]*(?"
		   "\\(\\sw+\\)?")
           '(1 font-lock-keyword-face)
           '(6 (cond ((match-beginning 3) font-lock-function-name-face)
                     ((match-beginning 5) font-lock-variable-name-face)
                     (t font-lock-type-face))
               nil t))
     ))
  "Subdued expressions to highlight in Scheme modes.")

;;ELI {{{
(defun scheme-font-lock-sexp-comment (limit)
  (when (search-forward "#;" limit t)
    (let ((beg (match-beginning 0)))
      (if (nth 8 (save-excursion (syntax-ppss beg)))
          ;; Not a sexp-comment: keep looking.
          (scheme-font-lock-sexp-comment limit)
        (ignore-errors
          (forward-sexp 1)
          (set-match-data (list beg (point)))
          (point))))))
;;ELI }}}
;;ELI {{{ my addition
(defvar sexpr-comment-face (simple-make-face '*/h442211 'sexpr-comment-face))
;;ELI }}}

(defconst scheme-font-lock-keywords-2
  (append
   ;;ELI {{{
   '((scheme-font-lock-sexp-comment (0 sexpr-comment-face t)))
   ;;ELI }}}
   scheme-font-lock-keywords-1
   (eval-when-compile
     (list
      ;;
      ;; Control structures.
      (cons
       (concat
        "(" (regexp-opt
             '("begin" "begin-for-syntax"
               "call-with-current-continuation" "call/cc"
               "call-with-escape-continuation" "call/ec"
               "call-with-input-file" "call-with-input-file*"
               "call-with-output-file" "call-with-output-file*" "case" "cond"
               "do" "else" "for-each" "if" "lambda" "case-lambda"
               "lambda/kw" "define/kw"
               "let" "let*" "let-syntax" "letrec" "letrec-syntax"
               "let-id-macro" "let-macro" "letmacro" "letsubst" "letsyntax"
               "let/cc" "let/ec"
               ;; Hannes Haug <hannes.haug@student.uni-tuebingen.de> wants:
               "and" "or" "delay" "force" "lazy"
               ;; Stefan Monnier <stefan.monnier@epfl.ch> says don't bother:
               "quasiquote" "quote" "unquote" "unquote-splicing"
               "map" "syntax-rules" "syntax" "syntax-id-rules"
               ;; New MzScheme stuff
               "syntax/loc" "quote-syntax" "quasisyntax" "quasisyntax/loc"
               "syntax-case" "syntax-case*" "with-syntax"
               "module" "provide" "require"
               ;; Some more
               "when" "unless" "begin0"
               "thunk" "while" "until"
               "generic" "method" "beforemethod" "aftermethod" "aroundmethod"
               "class" "entityclass"
               "setf!" "psetf!" "pset!" "error" "raise"
               "with-slots" "with-accessors"
               "dynamic-wind" "let-values" "let*-values" "fluid-let"
               "parameterize" "parameterize*" "with-handlers" "with-handlers*"
               "ignore-errors"
               "no-errors" "cases" "match"
               "for" "for*" "for/list" "for*/list" "for/fold" "for*/fold"
               ) t)
        "\\>") 1)
      ;;
      ;; It wouldn't be Scheme w/o named-let.
      '("(let\\s-+\\(\\sw+\\)"
        (1 font-lock-function-name-face))
      ;;
      ;; David Fox <fox@graphics.cs.nyu.edu> for SOS/STklos class specifiers.
      '("\\(\\<<\\sw+>\\>\\|([ \t]*singleton[ \t]*[^ \t\n\r)]*[ \t]*)\\)" .
        font-lock-type-face)
      (list (concat "([ \t]*"
                    "\\(\\sw+\\)[ \t]*"
                    "\\(\\<<\\sw+>\\>\\|"
                    "([ \t]*singleton[ \t]*'[^ \t\n\r)]*[ \t]*)\\)[ \t]*"
                    "\\([^ \n\r()]*([^ \n\r()]*)[^ \n\r()]*\\|[^ \n\r()]+\\)?"
                    "[ \t]*)")
            '(1 font-lock-variable-name-face nil t))
      ;;
      ;; Scheme `:' and `#:' keywords as builtins.
      '("\\<#?:\\sw+\\>" . font-lock-builtin-face)
      )))
  "Gaudy expressions to highlight in Scheme modes.")

(defvar scheme-font-lock-keywords scheme-font-lock-keywords-2
  "Default expressions to highlight in Scheme modes.")

(defconst scheme-sexp-comment-syntax-table
  (let ((st (make-syntax-table scheme-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?#  "'" st)
    st))

(put 'lambda 'scheme-doc-string-elt 2)
;; Docstring's pos in a `define' depends on whether it's a var or fun def.
(put 'define 'scheme-doc-string-elt
     (lambda ()
       ;; The function is called with point right after "define".
       (forward-comment (point-max))
       (if (eq (char-after) ?\() 2 0)))

(defun scheme-font-lock-syntactic-face-function (state)
  (when (and (null (nth 3 state))
             (eq (char-after (nth 8 state)) ?#)
             (eq (char-after (1+ (nth 8 state))) ?\;))
    ;; It's a sexp-comment.  Tell parse-partial-sexp where it ends.
    (save-excursion
      (let ((pos (point))
            (end
             (condition-case err
                 (let ((parse-sexp-lookup-properties nil))
                   (goto-char (+ 2 (nth 8 state)))
                   ;; FIXME: this doesn't handle the case where the sexp
                   ;; itself contains a #; comment.
                   (forward-sexp 1)
                   (point))
               (scan-error (nth 2 err)))))
        (when (< pos (- end 2))
          (put-text-property pos (- end 2)
                             'syntax-table scheme-sexp-comment-syntax-table))
        (put-text-property (- end 1) end 'syntax-table '(12)))))
  ;; Choose the face to use.
  (lisp-font-lock-syntactic-face-function state))

;;;###autoload
(define-derived-mode dsssl-mode scheme-mode "DSSSL"
  "Major mode for editing DSSSL code.
Editing commands are similar to those of `lisp-mode'.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{scheme-mode-map}
Entering this mode runs the hooks `scheme-mode-hook' and then
`dsssl-mode-hook' and inserts the value of `dsssl-sgml-declaration' if
that variable's value is a string."
  (make-local-variable 'page-delimiter)
  (setq page-delimiter "^;;;" ; ^L not valid SGML char
	major-mode 'dsssl-mode
	mode-name "DSSSL")
  ;; Insert a suitable SGML declaration into an empty buffer.
  ;; FIXME: This should use `auto-insert-alist' instead.
  (and (zerop (buffer-size))
       (stringp dsssl-sgml-declaration)
       (not buffer-read-only)
       (insert dsssl-sgml-declaration))
  (setq font-lock-defaults '(dsssl-font-lock-keywords
			     nil t (("+-*/.<>=?$%_&~^:" . "w"))
			     beginning-of-defun
			     (font-lock-mark-block-function . mark-defun)))
  (set (make-local-variable 'imenu-case-fold-search) nil)
  (setq imenu-generic-expression dsssl-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist)
       '(("+-*/.<>=?$%_&~^:" . "w"))))

;; Extra syntax for DSSSL.  This isn't separated from Scheme, but
;; shouldn't cause much trouble in scheme-mode.
;; (put 'element 'scheme-indent-function 1)
;; (put 'mode 'scheme-indent-function 1)
;; (put 'with-mode 'scheme-indent-function 1)
;; (put 'make 'scheme-indent-function 1)
;; (put 'style 'scheme-indent-function 1)
;; (put 'root 'scheme-indent-function 1)

(defvar dsssl-font-lock-keywords
  (eval-when-compile
    (list
     ;; Similar to Scheme
     (list "(\\(define\\(-\\w+\\)?\\)\\>[ 	]*\\\((?\\)\\(\\sw+\\)\\>"
	   '(1 font-lock-keyword-face)
	   '(4 font-lock-function-name-face))
     (cons
      (concat "(\\("
	      ;; (make-regexp '("case" "cond" "else" "if" "lambda"
	      ;; "let" "let*" "letrec" "and" "or" "map" "with-mode"))
	      "and\\|c\\(ase\\|ond\\)\\|else\\|if\\|"
	      "l\\(ambda\\|et\\(\\|*\\|rec\\)\\)\\|map\\|or\\|with-mode"
	      "\\)\\>")
      1)
     ;; DSSSL syntax
     '("(\\(element\\|mode\\|declare-\\w+\\)\\>[ 	]*\\(\\sw+\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-type-face))
     '("(\\(element\\)\\>[ 	]*(\\(\\S)+\\))"
       (1 font-lock-keyword-face)
       (2 font-lock-type-face))
     '("\\<\\sw+:\\>" . font-lock-constant-face) ; trailing `:' c.f. scheme
     ;; SGML markup (from sgml-mode) :
     '("<\\([!?][-a-z0-9]+\\)" 1 font-lock-keyword-face)
     '("<\\(/?[-a-z0-9]+\\)" 1 font-lock-function-name-face)))
  "Default expressions to highlight in DSSSL mode.")


(defvar calculate-lisp-indent-last-sexp)

;; Copied from lisp-indent-function, but with gets of
;; scheme-indent-{function,hook}.
(defun scheme-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
      ;; car of form doesn't seem to be a symbol
      (progn
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
        ;; Indent under the list or under the first sexp on the same
        ;; line as calculate-lisp-indent-last-sexp.  Note that first
        ;; thing on that line has to be complete sexp since we are
        ;; inside the innermost containing sexp.
        (backward-prefix-chars)
        (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (get (intern-soft function) 'scheme-indent-function)
                         (get (intern-soft function) 'scheme-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method state indent-point normal-indent)))))))


;;; Let is different in Scheme

(defun would-be-symbol (string)
  (not (string-equal (substring string 0 1) "(")))

(defun next-sexp-as-string ()
  ;; Assumes that it is protected by a save-excursion
  (forward-sexp 1)
  (let ((the-end (point)))
    (backward-sexp 1)
    (buffer-substring (point) the-end)))

;; This is correct but too slow.
;; The one below works almost always.
;;(defun scheme-let-indent (state indent-point)
;;  (if (would-be-symbol (next-sexp-as-string))
;;      (scheme-indent-specform 2 state indent-point)
;;      (scheme-indent-specform 1 state indent-point)))

(defun scheme-let-indent (state indent-point normal-indent)
  (skip-chars-forward " \t")
  (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
      (lisp-indent-specform 2 state indent-point normal-indent)
    (lisp-indent-specform 1 state indent-point normal-indent)))

;; (put 'begin 'scheme-indent-function 0), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'begin                 'scheme-indent-function 0)
(put 'begin-for-syntax      'scheme-indent-function 0)
(put 'case                  'scheme-indent-function 1)
(put 'cond                  'scheme-indent-function 0)
(put 'delay                 'scheme-indent-function 0)
(put 'do                    'scheme-indent-function 2)
(put 'lambda                'scheme-indent-function 1)
(put 'λ                     'scheme-indent-function 1)
(put 'lambda:               'scheme-indent-function 1)
(put 'case-lambda           'scheme-indent-function 0)
(put 'lambda/kw             'scheme-indent-function 1)
(put 'define/kw             'scheme-indent-function 'defun)
(put 'let                   'scheme-indent-function 'scheme-let-indent)
(put 'let*                  'scheme-indent-function 1)
(put 'letrec                'scheme-indent-function 1)
(put 'let-values            'scheme-indent-function 1)
(put 'let*-values           'scheme-indent-function 1)
(put 'fluid-let             'scheme-indent-function 1)
(put 'let/cc                'scheme-indent-function 1)
(put 'let/ec                'scheme-indent-function 1)
(put 'let-id-macro          'scheme-indent-function 2)
(put 'let-macro             'scheme-indent-function 2)
(put 'letmacro              'scheme-indent-function 1)
(put 'letsubst              'scheme-indent-function 1)
(put 'sequence              'scheme-indent-function 0) ; SICP, not r4rs
(put 'letsyntax             'scheme-indent-function 1)
(put 'let-syntax            'scheme-indent-function 1)
(put 'letrec-syntax         'scheme-indent-function 1)
(put 'syntax-rules          'scheme-indent-function 1)
(put 'syntax-id-rules       'scheme-indent-function 1)

(put 'call-with-input-file  'scheme-indent-function 1)
(put 'call-with-input-file* 'scheme-indent-function 1)
(put 'with-input-from-file  'scheme-indent-function 1)
(put 'with-input-from-port  'scheme-indent-function 1)
(put 'call-with-output-file 'scheme-indent-function 1)
(put 'call-with-output-file* 'scheme-indent-function 1)
(put 'with-output-to-file   'scheme-indent-function 'defun)
(put 'with-output-to-port   'scheme-indent-function 1)
(put 'with-slots            'scheme-indent-function 2)
(put 'with-accessors        'scheme-indent-function 2)
(put 'call-with-values      'scheme-indent-function 2)
(put 'dynamic-wind          'scheme-indent-function 'defun)

(put 'if                    'scheme-indent-function 1)
(put 'method                'scheme-indent-function 1)
(put 'beforemethod          'scheme-indent-function 1)
(put 'aftermethod           'scheme-indent-function 1)
(put 'aroundmethod          'scheme-indent-function 1)
(put 'when                  'scheme-indent-function 1)
(put 'unless                'scheme-indent-function 1)
(put 'thunk                 'scheme-indent-function 0)
(put 'while                 'scheme-indent-function 1)
(put 'until                 'scheme-indent-function 1)
(put 'parameterize          'scheme-indent-function 1)
(put 'parameterize*         'scheme-indent-function 1)
(put 'syntax-parameterize   'scheme-indent-function 1)
(put 'with-handlers         'scheme-indent-function 1)
(put 'with-handlers*        'scheme-indent-function 1)
(put 'begin0                'scheme-indent-function 1)
(put 'with-output-to-string 'scheme-indent-function 0)
(put 'ignore-errors         'scheme-indent-function 0)
(put 'no-errors             'scheme-indent-function 0)
(put 'matcher               'scheme-indent-function 1)
(put 'match                 'scheme-indent-function 1)
(put 'regexp-case           'scheme-indent-function 1)
(put 'dotimes               'scheme-indent-function 1)
(put 'dolist                'scheme-indent-function 1)

(put 'with-syntax           'scheme-indent-function 1)
(put 'syntax-case           'scheme-indent-function 2)
(put 'syntax-case*          'scheme-indent-function 3)
(put 'module                'scheme-indent-function 2)

(put 'syntax                'scheme-indent-function 0)
(put 'quasisyntax           'scheme-indent-function 0)
(put 'syntax/loc            'scheme-indent-function 1)
(put 'quasisyntax/loc       'scheme-indent-function 1)

(put 'cases                 'scheme-indent-function 1)

(put 'for                   'scheme-indent-function 1)
(put 'for*                  'scheme-indent-function 1)
(put 'for/list              'scheme-indent-function 1)
(put 'for*/list             'scheme-indent-function 1)
(put 'for/fold              'scheme-indent-function 2)
(put 'for*/fold             'scheme-indent-function 2)
(put 'for/and               'scheme-indent-function 1)
(put 'for*/and              'scheme-indent-function 1)
(put 'for/or                'scheme-indent-function 1)
(put 'for*/or               'scheme-indent-function 1)

(put 'nest                  'scheme-indent-function 1)



(provide 'scheme)

;;; scheme.el ends here
