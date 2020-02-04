;;; colors.el --- Make life colorful.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;;-----------------------------------------------------------------------------
;; Easy to use make-face

;; Original idea from an ancient "hilit19.el"
(defun simple-make-face (spec &optional face-name)
  "Get a symbol or a string SPEC, and make it into a face if it doesn't exist.
In order for it to properly create the face, the following naming convention
must be used:
    attr[-attr][...]
Example: (simple-make-face 'yellow/blue4-bold) creates and returns an
appropriate face named `yellow/blue4-bold'.  (Actually, the color spec can be
anywhere.)

Attribute can be one of: ultra-condensed extra-condensed condensed
semi-condensed semi-expanded expanded extra-expanded ultra-expanded ultra-bold
extra-bold bold semi-bold semi-light light extra-light ultra-light underline
ununderline overline unoverline inverse uninverse italic oblique reverse-italic
reverse-oblique.  It can also be `normal', which will be used for width,
weight, and slant (but attributes are processed from left to right).
\(See `set-face-attribute'.)

Attributes can also be in these forms:
* `fgcolor/bgcolor', where each color can be
  - a color name,
  - `hexH' (H is some hex color), or `hH', or `#H'
  - `*' or `default', or just empty
* `box[N]' for a box with width N, defaults to 1
* `big[N]', `small[N]' for a multiplier or divider of N0%, defaults to 25%
* `scale[N]' for a scale of N%
* `height[N]' for a fixed height of N

An optional argument FACE-NAME will make it be defined as the result face, and
force the face to be modified if it exists (good for setting properties of
existing faces)."
  (let* ((spec  (if (stringp spec) spec (symbol-name spec)))
         (spec  (if (equal "" spec) (error "Empty face spec") spec))
         (face  (or face-name (intern spec))))
    ;; optimize re-generating a simple face with same specs
    (unless (and (not face-name) (equal spec (get face 'simple-face-spec)))
      (let* ((attrs (split-string spec "-"))
             (attrs (mapcar (lambda (a) (replace-regexp-in-string "_" "-" a t))
                            attrs)))
        (unless (memq face (face-list)) (make-face face))
        (save-match-data
          (dolist (attr attrs) (simple-set-face-attribute face attr)))
        (unless face-name (put face 'simple-face-spec spec))))
    face))

(defun simple-set-face-attribute (face attr)
  (let* ((a (intern attr))
         (as (cond ((eq a 'normal)
                    (list :width a :weight a :slant a))
                   ((memq a '(ultra-condensed extra-condensed condensed
                              semi-condensed semi-expanded expanded
                              extra-expanded ultra-expanded))
                    (list :width a))
                   ((memq a '(ultra-bold extra-bold bold semi-bold
                              semi-light light extra-light ultra-light))
                    (list :weight a))
                   ((memq a '(italic oblique reverse-italic reverse-oblique))
                    (list :slant a))
                   ((eq a 'underline)   '(:underline t))
                   ((eq a 'ununderline) '(:underline nil))
                   ((eq a 'overline)    '(:overline t))
                   ((eq a 'unoverline)  '(:overline nil))
                   ((eq a 'inverse)     '(:inverse t))
                   ((eq a 'uninverse)   '(:inverse nil))
                   (t (simple-face-parse-compound-attr attr)))))
    (ignore-errors (apply 'set-face-attribute face nil as))))

(defun simple-face-parse-compound-attr (attr)
  (cond
    ((string-match "\\`\\(big\\|small\\|scale\\|height\\)\\([0-9]+\\)?\\'" attr)
     (let* ((op    (match-string 1 attr))
            (arg   (match-string 2 attr))
            (arg   (and arg (string-to-number arg)))
            (scale (/ (if (equal op "scale")
                        (or arg 100)
                        (+ 100 (if arg (* 10 arg) 25)))
                      100.0))
            (scale (if (equal op "small") (/ 1.0 scale) scale)))
       (list :height (if (equal op "height") arg scale))))
    ((string-match "\\`box\\([0-9]+\\)?\\'" attr)
     (list :box (if (match-beginning 1)
                  (list :line-width (string-to-number (match-string 1 attr)))
                  t)))
    ;; split `.../...' color spec to `.../' and `/...'
    ((string-match "\\`\\([^/]+\\)/\\([^/]+\\)\\'" attr)
     (let ((fg (concat (match-string 1 attr) "/"))
           (bg (concat "/" (match-string 2 attr))))
       (append (simple-face-parse-compound-attr fg)
               (simple-face-parse-compound-attr bg))))
    ;; default colors in several forms, do nothing
    ((member attr '("" "/" "*" "*/" "/*" "default" "default/" "/default"))
     nil)
    ;; fg/bg colors
    ((and (string-match
           "\\`\\(/\\)?\\(#\\|[hH]\\|hex\\|HEX\\)?\\([^/]*\\)\\(/\\)?\\'" attr)
          ;; only `.../' or `/...'
          (if (match-beginning 1) (not (match-beginning 4)) (match-beginning 4)))
     (let* ((rgb   (match-beginning 2))
            (color (match-string 3 attr))
            (color (if rgb (concat "#" color) color)))
       (list (if (match-beginning 1) :background :foreground) color)))
    ;; some color with no `/', used as fg
    ((string-match "\\`\\(?:#\\|[hH]\\|hex\\|HEX\\)\\([^/]*\\)\\'" attr)
     (list :foreground (concat "#" (match-string 1 attr))))
    ((assoc (downcase attr) color-name-rgb-alist) ; (defined-colors) better?
     (list :foreground attr))
    (t (error "Bad component in face spec: %S" attr))))

(defun simple-make-face-if-undefined (face)
  (let ((face (if (stringp face) (intern face) face)))
    (if (facep face) face (simple-make-face face))))

;;-----------------------------------------------------------------------------
;; Hacked `read-face-name' that uses simple-make-face

;; Redefined from "faces.el"
(defvar crm-separator) ; from crm.el
(defun read-face-name (prompt &optional default multiple)
  "Read one or more face names, prompting with PROMPT.
PROMPT should not end in a space or a colon.

Return DEFAULT if the user enters the empty string.
If DEFAULT is non-nil, it should be a single face or a list of face names
\(symbols or strings).  In the latter case, return the `car' of DEFAULT
\(if MULTIPLE is nil, see below), or DEFAULT (if MULTIPLE is non-nil).

If MULTIPLE is non-nil, this function uses `completing-read-multiple'
to read multiple faces with \"[ \\t]*,[ \\t]*\" as the separator regexp
and it returns a list of face names.  Otherwise, it reads and returns
a single face name.

** Modified to accept anything, and use `simple-make-face-if-undefined'."
  (if (and default (not (stringp default)))
      (setq default
            (cond ((symbolp default)
                   (symbol-name default))
                  (multiple
                   (mapconcat (lambda (f) (if (symbolp f) (symbol-name f) f))
                              default ", "))
                  ;; If we only want one, and the default is more than one,
                  ;; discard the unwanted ones.
                  (t (symbol-name (car default))))))
  (when (and default (not multiple))
    (require 'crm)
    ;; For compatibility with `completing-read-multiple' use `crm-separator'
    ;; to define DEFAULT if MULTIPLE is nil.
    (setq default (car (split-string default crm-separator t))))

  (let ((prompt (if default
                    (format "%s (default `%s'): " prompt default)
                  (format "%s: " prompt)))
        aliasfaces nonaliasfaces faces)
    ;; Build up the completion tables.
    (mapatoms (lambda (s)
                (if (facep s)
                    (if (get s 'face-alias)
                        (push (symbol-name s) aliasfaces)
                      (push (symbol-name s) nonaliasfaces)))))
    (if multiple
        (progn
          (dolist (face (completing-read-multiple
                         prompt
                         (completion-table-in-turn nonaliasfaces aliasfaces)
                         nil
                         nil ; ELI: don't require a match
                         nil 'face-name-history default))
            ;; Ignore elements that are not faces
            ;; (for example, because DEFAULT was "all faces")
            (if (facep face) (push (intern face) faces)
                (push (simple-make-face-if-undefined face) faces))) ; ELI
          (nreverse faces))
      (let ((face (completing-read
                   prompt
                   (completion-table-in-turn nonaliasfaces aliasfaces)
                   nil
                   nil ; ELI: don't require a match
                   nil 'face-name-history default)))
        (if (facep face) (intern face)
            (simple-make-face-if-undefined face)))))) ; ELI

;;-----------------------------------------------------------------------------
;; Color utilities

;; A convenient function - similar to `facemenu-add-face', but simpler, and
;; uses overlays when font-lock is active.
(defun set-region-face (beg end face &optional force-overlays dont-make-face)
  "Set the face of a region.
Make the region from BEG to END to have face FACE.

Will use face properties, unless font-lock is active -- in that case it will
use overlays.  If the optional argument FORCE-OVERLAYS is t (prefix argument
when called interactively), will use overlays anyway.  If its value is 'both,
will use both overlays and properties (overlays don't stick in copied text)."
  (interactive (if mark-active
                 (list (region-beginning) (region-end) (read-face-name "Face")
                       (and current-prefix-arg t) nil)
                 (error "Use the mark, luke.")))
  (let* ((face (if dont-make-face face (simple-make-face face)))
         (use-overlays   (or force-overlays
                             (and font-lock-mode font-lock-defaults)))
         (use-properties (or (not use-overlays) (eq 'both force-overlays)))
         deactivate-mark buffer-file-name buffer-file-truename)
    (with-silent-modifications
      (when use-overlays
        (let ((overlay (make-overlay beg end)))
          (overlay-put overlay 'set-region-face t)
          (overlay-put overlay 'face face)))
      (when use-properties
        (put-text-property beg end 'face face)))))
(put 'set-region-face 'safe-local-eval-function t)

(defun remove-set-face-overlays ()
  "Removes overlays set by `set-region-face'."
  (interactive)
  (remove-overlays nil nil 'set-region-face t))
(put 'remove-set-face-overlays 'safe-local-eval-function t)

;; Sort of a cheap version of `add-color-pattern', using the above function.
;; (But one-time highlighting, of course)
(defun set-regexp-face (regexp face &optional force-overlays)
  "Set the face of all occurrences of REGEXP to FACE.
FACE can be a a list of pairs of a subexp number and its color: (SUB . COLOR).
Uses `set-region-face' to set the faces."
  ;; interactive like `add-color-pattern'
  (interactive (list (read-from-minibuffer
                      "Pattern to highlight: "
                      (and (use-region-p)
                           (regexp-quote (buffer-substring-no-properties
                                          (region-beginning) (region-end))))
                      nil nil 'regexp-history)
                     (read-face-name "Face")
                     (and current-prefix-arg t)))
  (let ((face (if (listp face)
                (progn (dolist (f face) (setcdr f (simple-make-face (cdr f))))
                       face)
                (simple-make-face face))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t nil)
        (if (listp face)
          (dolist (f face)
            (when (match-beginning (car f))
              (set-region-face (match-beginning (car f)) (match-end (car f))
                               (cdr f) force-overlays t)))
          (set-region-face (match-beginning 0) (match-end 0) face
                           force-overlays t))))))
(put 'set-regexp-face 'safe-local-eval-function t)

;;-----------------------------------------------------------------------------
;; Interactive regexp highlights by adding keywords to font-lock (similar to
;; `hi-lock-mode': I had a hack for ages, and `hi-lock-mode' is not as good
;; (yet?).)

(defvar added-color-patterns nil
  "List of colors added using `add-color-pattern'.")

(defun add-color-pattern (regexp face &optional expnum override &rest more)
  "Add a pattern for this buffer's `font-lock-keywords'.
REGEXP is the pattern to be colored with FACE.  When called programatically - a
third integer argument EXPNUM specifies the regexp to hilight, and a fourth
argument OVERRIDE is the override flag.  Possible MORE arguments will specify
more faces, numbers and flags.

When called interactively, a positive prefix argument will make it an
overriding addition, negative prefix makes it a normal addition, and no prefix
make the style appended over any existing style.  When OVERRIDE is set to
anything other than `t' or `nil', the new pattern will be added to the end of
`font-lock-keywords'.
Examples:
 (add-color-pattern \"regexp\" 'face)
 (add-color-pattern \"\\\\(r\\\\)egexp\" 'face 1 t)
 (add-color-pattern \"\\\\(r\\\\)egex\\\\(p\\\\)\" 'face1 1 t 'face2 2 nil)"
  (interactive (list (read-from-minibuffer
                      "Pattern to highlight: "
                      (and (use-region-p)
                           (regexp-quote (buffer-substring-no-properties
                                          (region-beginning) (region-end))))
                      nil nil 'regexp-history)
                     (read-face-name "Face")))
  (when (called-interactively-p 'any)
    (setq override (cond ((not current-prefix-arg) 'prepend)
                         ((>= (prefix-numeric-value current-prefix-arg) 0) t)
                         (t nil))
          expnum 0))
  (let* ((face (if (facep face) face (simple-make-face face)))
         (face (list 'quote face))
         (add (list (or expnum 0) face override t))
         (add (if (and more (cdr more)) (list add) add)))
    (while (and more (cdr more))
      (let* ((face (pop more))
             (face (if (facep face) face (simple-make-face face)))
             (new (list (pop more) (list 'quote face) (pop more) t)))
        (nconc add (list new))))
    (push regexp add)
    (make-local-variable 'added-color-patterns)
    (unless (member add added-color-patterns)
      (push add added-color-patterns)
      (font-lock-add-keywords
       nil (list add) (if (memq override '(t nil)) nil t))
      (unless font-lock-defaults
        ;; we have a misbehaved mode, set defaults and turn on again
        ;; another way to fix this is (setq-default font-lock-defaults '(nil))
        (font-lock-mode -1)
        (setq font-lock-defaults '(nil))
        (font-lock-mode 1))
      (font-lock-flush))))
(put 'add-color-pattern 'safe-local-eval-function t)

;; used temporarily, to make it easy to choose a pattern
(defvar patterns-history-list nil)

(defun remove-added-color-pattern (&optional n)
  "Remove the a pattern added with `add-color-pattern'.
Automatically removes a single-added pattern, otherwise asks which one to
remove.  With a numeric prefix argumet, remove that number of patterns (last
one added first), if negative removes all."
  (interactive "P")
  (let ((n (and n (prefix-numeric-value n))))
    (cond
      ((null added-color-patterns) (message "No patterns to remove."))
      ((or (and n (< n 0)) (= 1 (length added-color-patterns)))
       (font-lock-remove-keywords nil added-color-patterns)
       (setq added-color-patterns nil))
      (n (while (and added-color-patterns (> n 0))
           (font-lock-remove-keywords nil (list (pop added-color-patterns)))
           (setq n (1- n))))
      (t (let* ((patterns-history-list (mapcar 'car added-color-patterns))
                (rem (assoc (completing-read "Pattern to unhighlight: "
                                             added-color-patterns nil t
                                             (caar added-color-patterns)
                                             (cons 'patterns-history-list 1))
                            added-color-patterns)))
           (font-lock-remove-keywords nil (list rem))
           (setq added-color-patterns (delq rem added-color-patterns))))))
  (font-lock-flush))
(put 'remove-added-color-pattern 'safe-local-eval-function t)

;;-----------------------------------------------------------------------------
;; Enable font-lock

(setq font-lock-global-modes       t
      font-lock-maximum-decoration t
      ;; font-lock-maximum-size (* 1024 1024) ; jit-lock-mode is on by default
      )

(setq jit-lock-stealth-verbose  nil
      jit-lock-stealth-time     3
      jit-lock-chunk-size       500
      jit-lock-stealth-nice     0.2)

(global-font-lock-mode 1)

;;-----------------------------------------------------------------------------
;; Set default colors

(setq-default frame-background-mode eli-color-style)
(mapc 'frame-set-background-mode (frame-list))
(set-cursor-color "purple")

(require 'whitespace) ; otherwise it will modify the faces set below

(mapc
 (if (eq 'dark eli-color-style)
   (lambda (x) (simple-make-face (nth 1 x) (car x)))
   (lambda (x) (simple-make-face (or (nth 2 x) (nth 1 x)) (car x))))
 `((default                      white/black            black/white)
   (fringe                       white/gray10           black/gray90)
   (scroll-bar                   white/gray10           black/gray90)
   (mode-line                    yellow/red2-uninverse-scale75
                                       red2/yellow-uninverse-scale75)
   (mode-line-inactive           orange3/red4-uninverse-scale65
                                       orange3/yellow4-uninverse-scale65)
   (mode-line-buffer-id bold)
   ;; (mode-line-highlight */green) ???
   (header-line                  yellow/green4-bold     green4/yellow-bold)
   (tooltip                      black/gray75-bold-small10)
   ;; Using just bold makes it a bit higher, so going to the minibuffer makes
   ;; the modeline jump up.  Scale it down to compensate. --> No longer true,
   ;; and using a scale messes up `comint-highlight-prompt' too.
   (minibuffer-prompt yellow/*-bold black/*-bold)
   ;;
   (region                       yellow/h339            blue4/yellow)
   (secondary-selection          yellow/h838            cyan4/yellow)
   (highlight                    yellow/purple4-bold    purple4/yellow-bold)
   (isearch                      yellow/red2            red2/yellow)
   (query-replace                yellow/red2            red2/yellow)
   (match                        */red4                 red2/*)
   (lazy-highlight               */red4                 red2/*)
   (next-error                   yellow/purple3         purple3/yellow)
   (trailing-whitespace          */gray20               */gray80)
   (whitespace-line              */gray20               */gray80) ; long lines
   (whitespace-empty             */gray20               */gray80)
   (whitespace-trailing          */gray20               */gray80)
   (whitespace-space             */gray20               */gray80)
   (whitespace-hspace            */gray20               */gray80)
   (whitespace-tab               */gray20               */gray80)
   (whitespace-indentation       */h220                 */hdd0)
   (whitespace-space-after-tab   */h220                 */hdd0)
   (whitespace-space-before-tab  */h440                 */hbb0)
   (button                       */grey20-underline     */grey80-underline)
   (help-argument-name           underline-italic)
   (buffer-menu-buffer           yellow/red4-bold       red4/yellow-bold)
   (shadow                       gray40/*)
   (escape-glyph                 gray60/*)
   (nobreak-space                gray30/*               gray70/*)
   (completions-common-part      gray60/*               gray40/*)
   (completions-first-difference yellow/red4            red4/yellow)
   ;;
   (font-lock-keyword-face       yellow1-underline      blue4-underline)
   (font-lock-function-name-face skyblue1-bold          purple4-bold)
   (font-lock-variable-name-face seagreen1-bold         green4-bold)
   (font-lock-comment-face       chocolate1             red3)
   (font-lock-comment-delimiter-face chocolate2         red2)
   (font-lock-string-face        indianred1             cyan4)
   (font-lock-constant-face      skyblue1-bold          skyblue4-bold)
   (font-lock-type-face          orange1-bold           orange3-bold)
   (font-lock-doc-face           indianred1-italic      indianred4-italic)
   (font-lock-builtin-face       yellow1-bold           pink4-bold)
   (font-lock-warning-face       yellow1/skyblue4-bold  yellow4/red4-bold)
   ;; (font-lock-negation-char-face underline)
   (font-lock-preprocessor-face bold)
   (font-lock-regexp-grouping-backslash underline)
   (font-lock-regexp-grouping-construct underline)
   ))

(setq mode-line-in-non-selected-windows t)
(setq display-time-mail-face (simple-make-face 'yellow/orangered4-bold))

;;-----------------------------------------------------------------------------
;; Paren coloring using "mic-paren" (still much better than paren-show-mode)

;; Load, set defaults, activate
(load/include "mic-paren")
(setq paren-priority            'both
      paren-highlight-at-point  t
      paren-highlight-offscreen t
      paren-display-message     'only
      paren-message-linefeed-display " ⏎ "
      paren-message-no-match    t
      paren-ding-unmatched      nil
      paren-delay               0.1
      paren-dont-touch-blink    nil)
(paren-activate)

;; Switchable paren coloring modes
(defvar paren-hilight-mode 1)
(defvar paren-hilight-modes
  [nil (paren-face-match . nil) (bold . t) (paren-face-match-full . t)])
(defun prev-paren-hilight-mode (arg)
  "Switch to the previous paren-hilight mode or to the ARGth one."
  (interactive "P")
  (set-paren-hilight-mode
   (or arg (mod (1- paren-hilight-mode) (length paren-hilight-modes)))))
(defun next-paren-hilight-mode (arg)
  "Switch to the next paren-hilight mode or to the ARGth one."
  (interactive "P")
  (set-paren-hilight-mode
   (or arg (mod (1+ paren-hilight-mode) (length paren-hilight-modes)))))
(defun set-paren-hilight-mode (n)
  (setq paren-hilight-mode (min n (1- (length paren-hilight-modes))))
  (let ((mode (elt paren-hilight-modes paren-hilight-mode)))
    (if mode
      (progn (setq paren-match-face (car mode) paren-sexp-mode (cdr mode))
             (paren-activate))
      (paren-deactivate))
    (message "Paren mode #%s" paren-hilight-mode)))
;; Set the keys
(define-keys 'global
  '("C-(" prev-paren-hilight-mode)
  '("C-)" next-paren-hilight-mode))

(simple-make-face 'yellow/h22a   'paren-face-match)
(simple-make-face 'yellow/h315   'paren-face-match-full)
(simple-make-face 'yellow/red3   'paren-face-mismatch)
(simple-make-face 'yellow/green3 'paren-face-no-match)

;;; colors.el ends here
