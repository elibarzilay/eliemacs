;;; extras.el --- Setup packages in the extras directory.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;; Add the extras directory to the load-path.
(defvar eli-extras-dir (concat eli-dir "extras/"))
(add-to-list 'load-path eli-extras-dir)

;;-----------------------------------------------------------------------------
;; Calculator

;; Conversions
(setq calculator-user-operators '(("tf" cl-to-fr (+ 32 (/ (* X 9) 5)) 1)
                                  ("tc" fr-to-cl (/ (* (- X 32) 5) 9) 1)
                                  ("tp" kg-to-lb (/ X 0.453592)       1)
                                  ("tk" lb-to-kg (* X 0.453592)       1)
                                  ("tF" mt-to-ft (/ X 0.3048)         1)
                                  ("tM" ft-to-mt (* X 0.3048)         1)))

;;-----------------------------------------------------------------------------
;; Maze

(autoload 'maze "maze" "Eli Barzilay: Maze is Life!" t)

;;-----------------------------------------------------------------------------
;; Big font

(autoload 'bigfont-insert-big-string "bigfont" nil t)
(autoload 'bigfont-flash-big-string  "bigfont" nil t)
(autoload 'bigfont-scroll-big-string "bigfont" nil t)

;; ----------------------------------------------------------------------------
;; convenient input of unicode characters

(autoload 'eli-input-method/single "special-chars" nil t)
(autoload 'isearch-toggle-eli-input-method/single "special-chars" nil t)
(define-keys
  'global '([(meta return)] eli-input-method/single)
  isearch-mode-map '([(meta return)] isearch-toggle-eli-input-method/single))

;;-----------------------------------------------------------------------------
;; Git

(autoload 'git-status "git" nil t)
(autoload 'git-blame-mode "git-blame" nil t)

;;-----------------------------------------------------------------------------
;; Vkill

(autoload 'vkill "vkill" nil t)
(autoload 'list-unix-processes "vkill" nil t)

;;-----------------------------------------------------------------------------
;; mode-compile

(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key [?\C-c ?c] 'mode-compile)
(global-set-key [?\C-c ?\C-c] 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key [?\C-c ?\C-k] 'mode-compile-kill)
(setq mode-compile-prefered-default-makerule 'all)
(setq mode-compile-always-save-buffer-p t)
(setq mode-compile-expert-p t)
(setq mode-compile-reading-time 0.4)

(autoload 'compile-eliemacs "compile-eliemacs" "Compile EliEmacs." t)
(defun eli-elisp-compile ()
  "Hack around elisp-compile so my environment gets compiled properly."
  (if (and (< (length eli-include-dir) (length buffer-file-name))
           (equal (substring buffer-file-name 0 (length eli-include-dir))
                  eli-include-dir))
    (load (concat eli-dir "make.el"))
    (elisp-compile)))
(defun run-scheme-like-compile ()
  (setq
   mc--comp-lst          '("racket" "gracket" "raco make")
   mc--def-comp          '("racket")
   mc--compfile-regexp   ""
   mc--comp-varenv       "RACKET"
   mc--cflags-varenv     "RACKETFLAGS"
   mc--comp-options      "-t-"
   mc--source-ext-lst    '("rkt" "rktl" "scm" "ss")
   mc--head-ext-lst      '()
   mc--source-ext-regexp ""  ; <- always use the filename ;"\\.rkt.?$"
   mc--build-op-args     nil ; don't add -c -o crap
   mc--outfile-ext       ""
   )
  (mc--compile (mc--set-command)))
(eval-after-load "mode-compile"
  '(progn (setcar (cdr (assq 'emacs-lisp-mode mode-compile-modes-alist))
                  'eli-elisp-compile)
          (add-to-list 'mode-compile-modes-alist
                       '(scheme-mode . (run-scheme-like-compile
                                        kill-compilation)))))

;;-----------------------------------------------------------------------------
;; VM setup

;; Make my customizations loaded by autoloading through my file.
(let ((evm (concat eli-extras-dir "vm/eli-vm")))
  (when (file-readable-p (concat evm ".elc"))
    (autoload 'vm              evm "Start VM on your primary inbox" t)
    (autoload 'vm-other-frame  evm "Like `vm' but starts in another frame" t)
    (autoload 'vm-visit-folder evm "Start VM on an arbitrary folder" t)
    (autoload 'vm-visit-imap-folder evm "Start VM on an IMAP mailbox." t)
    (autoload 'vm-visit-virtual-folder evm "Visit a VM virtual folder" t)
    (autoload 'vm-mode         evm "Run VM major mode on a buffer" t)
    (autoload 'vm-mail         evm "Send a mail message using VM" t)
    (autoload 'vm-submit-bug-report evm "Send a bug report about VM" t)
    (autoload 'vm-compose-mail evm)))
(define-mail-user-agent 'vm-user-agent
  'vm-compose-mail 'vm-mail-send-and-exit nil nil)
(setq read-mail-command 'vm mail-user-agent 'vm-user-agent)
;; Still set the key, since going through `compose-mail' is still a mess
;; (eg, C-c C-a does some expand alias thing)
(global-set-key "\C-xm" 'vm-mail)

;;-----------------------------------------------------------------------------
;; W3M setup

;; Make my customizations loaded by autoloading through my file.
(let ((ew3m (concat eli-extras-dir "w3m/eli-w3m")))
  (when (file-readable-p (concat ew3m ".elc"))
    (autoload 'w3m ew3m "Start the W3M browser" t)))

;;-----------------------------------------------------------------------------
;; AucTex (disabled)
'
(let ((auctex-dir (concat eli-extras-dir "auctex/")))
  (when (file-accessible-directory-p auctex-dir)

(add-to-list 'load-path auctex-dir)

(defvar no-doc
  "This function is part of AUC TeX, but has not yet been loaded.
Full documentation will be available after autoloading the function."
  "Documentation for autoload functions.")

(defvar TeX-lisp-directory auctex-dir
  "*The directory where the AUC TeX lisp files are located.")

(defvar TeX-macro-global '("/usr/local/lib/texmf/tex/" "/usr/share/texmf/tex/")
  "Directories containing the sites TeX macro files and style files.
The directory names *must* end with a slash.")

;; This hook will store bibitems when you save a BibTeX buffer.
(add-hook 'bibtex-mode-hook 'BibTeX-auto-store)
(autoload 'BibTeX-auto-store "latex" no-doc t)

(autoload 'tex-mode                 "tex"      no-doc t)
(autoload 'plain-tex-mode           "tex"      no-doc t)
(autoload 'ams-tex-mode             "tex"      no-doc t)
(autoload 'TeX-auto-generate        "tex"      no-doc t)
(autoload 'TeX-auto-generate-global "tex"      no-doc t)
(autoload 'TeX-insert-quote         "tex"      no-doc t)
(autoload 'TeX-submit-bug-report    "tex"      no-doc t)
(autoload 'texinfo-mode             "tex-info" no-doc t)
(autoload 'latex-mode               "latex"    no-doc t)

(when (memq system-type '(windows-nt))
  ;; Try to make life easy for MikTeX users.
  (require 'tex-mik))

(eval-after-load "tex"
  '(progn
     (setq TeX-command-list
           (nconc
            '(("dvi-ps" "dvips %d -o %f"            TeX-run-command t nil)
              ("dvi-ps-a4"   "dvips %d -t a4 -o %f" TeX-run-command nil nil)
              ("dvi-ps-letter" "dvips %d -t letter -o %f"
                                                    TeX-run-command nil nil)
              ("dvi-ps-a4-type1"
               "dvips %d -Ppdf -j0 -G0 -t a4 -o %f" TeX-run-command nil nil)
              ("dvi-ps-letter-type1"
               "dvips %d -Ppdf -j0 -G0 -t letter -o %f" TeX-run-command nil nil)
              ("ps-pdf"       "ps2pdf %f %s.pdf"    TeX-run-command nil nil)
              ("ps-pdf-type1"
               "ps2pdf -dMaxSubsetPct=100 -dCompatibilityLevel=1.3 -dSubsetFonts=true -dEmbedAllFonts=true %f %s.pdf"
               TeX-run-command nil nil)
              ("ps-pdf-no-rotate"
               "ps2pdf -dAutoRotatePages=/All %f %s.pdf"
               TeX-run-command nil nil)
              ("ps-pdf-type1-no-rotate"
               "ps2pdf -dAutoRotatePages=/All -dMaxSubsetPct=100 -dCompatibilityLevel=1.3 -dSubsetFonts=true -dEmbedAllFonts=true %f %s.pdf"
               TeX-run-command nil nil)
              ("LaTeX-and-ps" "%l '\\nonstopmode\\input{%t}' && dvips %d -o %f"
                                                    TeX-run-command t nil)
              ("LaTeX-and-ps-letter"
               "%l '\\nonstopmode\\input{%t}' && dvips %d -t letter -o %f"
                                                    TeX-run-command nil nil)
              ("LaTeX-and-ps-a4"
               "%l '\\nonstopmode\\input{%t}' && dvips %d -t a4 -o %f"
                                                    TeX-run-command nil nil)
              ("LaTeX-and-ps-pdf"
               "%l '\\nonstopmode\\input{%t}' && dvips %d -o %f && ps2pdf %f %s.pdf"
                                                    TeX-run-command t nil)
              ("LaTeX-and-ps-pdf-letter"
               "%l '\\nonstopmode\\input{%t}' && dvips %d -t letter -o %f && ps2pdf %f %s.pdf"
                                                    TeX-run-command nil nil)
              ("LaTeX-and-ps-pdf-a4"
               "%l '\\nonstopmode\\input{%t}' && dvips %d -t a4 -o %f && ps2pdf %f %s.pdf"
                                                    TeX-run-command nil nil)
              ("PdfLaTeX"    "pdflatex '\\nonstopmode\\input{%t}'" TeX-run-LaTeX nil t)
              ("xdvi"        "xdvi %d"              TeX-run-discard nil nil)
              ("gv"          "gv %f"                TeX-run-discard nil nil)
              ("gv-full"     "gv-full %f"           TeX-run-discard nil nil)
              ("ghostview"   "ghostview %f"         TeX-run-discard nil nil)
              ("xpdf"        "xpdf %s.pdf"          TeX-run-discard nil nil)
              ("acroread"    "acroread %s.pdf"      TeX-run-discard nil nil))
            TeX-command-list))
     (setq TeX-font-list
           (nconc '((?\C-u "\\underline{" "}"))
                  TeX-font-list))
     ;; These are only here for font-latex, should eventually be removed
     (setq font-lock-display-type 'color)
     (setq font-lock-background-mode eli-color-style)
     (setq font-latex-is-Emacs20 t) ; hack: fool font-latex
     (load "font-latex")
     ;; (set-face-foreground font-latex-italic-face "seagreen1")
     ;; (set-face-foreground font-latex-bold-face "yellow")
     ;; (set-face-foreground font-latex-math-face "pink")
     ))

(defun my-LaTeX-mode-hook ()
  "Setup AucTeX."
  (require 'bib-cite)
  (turn-on-bib-cite)
  (auto-fill-mode)
  (local-set-key [(meta ?\$)] 'eli-insert-dollars)
  (local-set-key [(meta ?\q)] 'LaTeX-fill-paragraph))
(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hook)

))

;;; extras.el ends here
