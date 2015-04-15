;;; markdown.el --- markdown text viewing

;; There are some markdown modes come with a whole slew of editing
;; markdown text, which is silly given that the format is very
;; intentionally simple so there's no need for all that baggage.  The
;; purpose of this code is to do just the highlighting of markdown text,
;; but do that properly (mostly) including proper treatment of
;; multi-line highlights.  It actually arranges for the highlights to
;; happen on no more than one line, but that's based on information that
;; does analyze multi-line blocks of text.  It's possible, but seems
;; useless, to keep the analysis as a multi-line thing but leave the
;; font-lock highlighting as single-lines: the saving is minor, and the
;; advantage is that it's possible to add regexp patterns that match
;; multiple lines.
;;
;; Does most of the commonmark stuff, but some parts are left out --
;; like lazy text in listitems and blockquotes.  Most of the left-out
;; stuff is not just laziness (in writing code), but things that just
;; look bad.  A more obvious example than lazy text is setext headers:
;; according to the commonmark spec, a single "-" is enough to make a
;; header, and that has higher precedence than a listitem -- the
;; analysis part of this code doesn't deal with headers so it parses a
;; "-" as a list bullet and it gets highlighted as such; it would be
;; nice if there's an easy way to do it right (eg, if tha analysis is
;; extended to parse headers), but if there isn't, then don't bother
;; about such weird edge cases because you (the text author) most likely
;; want to avoid depending on such edge cases, in addition to making the
;; text look reasonable.  (Looking reasonable is implicitly there in
;; that markdown is basically just human readable text, and that
;; justifies avoiding such edge cases in a tool like this.)
;;
;; Note: doesn't work with tabs.  Intentionally.
;;
;; Implementation note: it would have been easy if there was a way to
;; communicate a whole bunch of face decisions to font-lock, but I
;; didn't see any sane way to do that.  The closest thing is the
;; `font-lock-face' property, but that requires resetting the property
;; when things get rehighlighted -- which means relying on nothing else
;; messing with the property, or on identifiable unique face names, or
;; some such hack.  So instead, the way this code works is by storing
;; the parsing information in a hash-table, and then the matcher
;; functions that font-lock uses pull information from that table.  It
;; complicates things a bit, but it works.

(defgroup markdown nil
  "Markdown highlights etc."
  :group 'wp)

(defmacro markdown-debug (fmt/expr &rest args)
  ;; Use (markdown-debug "fmt" x ...) to spit out a message or
  ;; (markdown-debug E ...) to catch and show errors in evaluating
  ;; (progn E ...)
  (cond ((stringp fmt/expr)
         `(let ((str (format ,fmt/expr ,@args)))
            (with-current-buffer (get-buffer-create "*Markdown Debug*")
              (goto-char (point-max))
              (insert str "\n"))))
        ((null args)
         `(condition-case e ,fmt/expr
            (error (markdown-debug "ERROR in %S:\n  %S" ',fmt/expr e))))
        (t `(markdown-debug (progn ,fmt/expr ,@args)))))

;; Holds a (cons buffer-chars-modified-tick hash), where the hash maps
;; BOL locations to their type: (list TYPE N TYPE N) where TYPE is
;; 'blockquote or 'code (can have either one of these, or both (in this
;; order), but nothing else), and N is the position on the line where
;; TYPE starts.
(defvar-local markdown-blocks-cache (cons nil nil))

;; this is used after skipping initial indentation
(defconst markdown-listitem-rx
  (concat "\\("
          "[*+-]\\|" ; single char bullets
          "\\(?:[0-9]+\\|[a-zA-Z]\\)\\.\\|"
          "(\\(?:[0-9]+\\|[a-zA-Z]\\))"
          "\\)\\(?: \\|$\\)"))

(defun markdown-block-info* (p bound)
  ;; Assume that this is called at some toplevel text
  (save-excursion
    (goto-char p)
    (forward-line 0)
    (setq p (point))
    (let* ((linepos p) (spcs 0) (spcs0 nil) (r '())
           (indents+types '())
           (pending-types+indents0 '())
           (pending-types+indents '())
           (consumed-line
            ;; used below to set info on line between two blocks that
            ;; are the same type (for some prefix of the types of both)
            (lambda (l1 l2 b)
              (let ((r '()))
                (while (and l1 l2 (equal (car l1) (car l2)))
                  (setq r (cons b (cons (car l1) r))
                        l1 (cddr l1) l2 (cddr l2)))
                (nreverse r))))
           (new-line
            (lambda (new)
              (setq pending-types+indents (nreverse indents+types)
                    indents+types '())
              (when (listp new) ; doesn't apply to toplevel texts
                (let ((ts+is pending-types+indents) (p linepos) (pfx '()))
                  (while ts+is
                    (push (pop ts+is) pfx)
                    (push (setq p (min (point) (+ p (or (pop ts+is) 0)))) pfx))
                  (setq new `(,@(nreverse pfx) ,@new)))
                (when (listp (cdr (cadr r)))
                  (pcase (car r)
                    (`(,b . nil)
                     (let ((mid (funcall consumed-line new (cdr (cadr r))
                                         (1- linepos))))
                       (when mid (setcar r (cons b mid))))))))
              (push `(,linepos . ,new) r)
              (forward-line 1)
              (setq linepos (point))
              (if new
                (setq pending-types+indents0 pending-types+indents)
                (setq pending-types+indents pending-types+indents0))))
           (enter-block
            (lambda (type indent)
              (push type indents+types)
              (push (if (eq type (pop pending-types+indents))
                      (pop pending-types+indents)
                      (progn (setq pending-types+indents '()) indent))
                 indents+types))))
      (while (or spcs0 (and (<= (setq p (point)) bound) (not (eobp))))
        (setq spcs (or spcs0 (skip-chars-forward " ")) spcs0 nil)
        (cond
          ;; going into preexisting container
          ((and pending-types+indents (>= spcs (cadr pending-types+indents))
                (or (cdar r) ; blockquote doesn't continue after an empty line
                    (not (eq 'blockquote (car pending-types+indents)))))
           (push (pop pending-types+indents) indents+types)
           (push (pop pending-types+indents) indents+types)
           ;; spcs0 is used to simulate backtracking some spaces (also below)
           ;; (goto-char (+ (car indents+types) p)) =>
           (setq spcs0 (- spcs (car indents+types))
                 p (+ p (car indents+types))))
          ;; code
          ((and (>= spcs 4)
                (let ((last (cdar r)))
                  (or (not last) (and (consp last) (memq 'code last)))))
           (funcall new-line `(code ,(+ p 4))))
          ;; empty
          ((eolp) (funcall new-line '()))
          ;; closing containers
          ((and pending-types+indents
                (< spcs (cadr pending-types+indents))
                (not (memq (cadr indents+types) '(blockquote listitem))))
           (setq pending-types+indents '())
           ;; (goto-char p) =>
           (setq spcs0 spcs))
          ;; blockquote
          ((eq ?> (char-after))
           (funcall enter-block 'blockquote (+ 2 spcs))
           (forward-char 1) (when (eq ?\  (char-after)) (forward-char 1)))
          ;; listitem
          ((looking-at markdown-listitem-rx)
           (funcall enter-block 'listitem (+ (- (match-end 1) p) 1))
           (goto-char (match-end 0)))
          ;; other text
          (indents+types (funcall new-line '()))
          (t (funcall new-line 'text))))
      (when (and (eobp) (bolp)) (push `(,linepos . nil) r))
      (nreverse r))))

(defun markdown-block-info (p bound)
  ;; should always be called at a BOL, drop test only because it's
  ;; always checked before calling this
  ;; (unless (memq (char-before p) '(?\n nil))
  ;;   (error "markdown-block-info: should be called at a BOL"))
  (let ((h (cdr markdown-blocks-cache)) (none " nothing "))
    (let ((m (buffer-chars-modified-tick)))
      (unless (eq m (car markdown-blocks-cache))
        (setq h (make-hash-table))
        (setq markdown-blocks-cache (cons m h))))
    (let ((r (gethash p h none)))
      (if (not (eq none r)) r
          (progn (dolist (x (markdown-block-info* p bound))
                   (puthash (car x) (cdr x) h))
                 (let ((r (gethash p h none)))
                   (if (not (eq none r)) r
                       (error "internal error: no value at %S" p))))))))

;; return the list of just type symbols, dropping a prefix-only type;
;; useful to identify blocks of the same type
(defun markdown-line-type ()
  (save-excursion
    (beginning-of-line)
    (let ((p (markdown-block-info (point) (point-max))))
      (if (not (consp p)) p
          (let ((r '())) (while p (push (pop p) r) (pop p)) (nreverse r))))))

(defun markdown-match-blocks (bound type)
  (let* (;; The `save-excursion' here is important: without it we can
         ;; return a match that is before the starting point, which will
         ;; make font-lock spin in an infinite loop
         (bol (save-excursion (forward-line 0) (point)))
         (info (markdown-block-info bol bound))
         ;; skip empty lines (here and below)
         (info (and (consp info) (not (eq bol (car (last info)))) info))
         (info0 (cons bol info))
         (p (point)))
    (while (and info (<= (cadr info) p)) (setq info (cddr info)))
    (while (and (not (setq info (memq type info))) (< p bound))
      (forward-line 1)
      (setq info (markdown-block-info (setq p (point)) bound)
            info (and (consp info) (not (eq p (car (last info)))) info)
            info0 (cons p info)))
    (and info (< p bound)
         (let ((p0 p) (info* nil))
           ;; find starting point
           (while (and info0 (not (eq (cdr info0) info)))
             (setq info0 (cddr info0)))
           (when info0 (setq p0 (car info0)))
           ;; merge same kinds (optimization, not required)
           (while (not (equal info info*))
             (setq info* info)
             (pcase info
               (`(,type ,a ,type ,b . ,_) (setq info (cddr info)))))
           (set-match-data
            (pcase info
              (`(code ,b)
               (goto-char b) (forward-line 1) (setq p (point))
               ;; This can be used to have a different highlight for
               ;; first/last lines in a code block, if it'll be possible
               ;; to have border only on some sides in the future.
               ;; (let* ((c1 (save-excursion
               ;;              (forward-line -2)
               ;;              (if (bobp) 'edge
               ;;                  (markdown-block-info (point) bound))))
               ;;        (c1 (if (consp c1) (memq 'code c1) (eq c1 'edge)))
               ;;        (c2 (if (eobp) 'edge
               ;;                (markdown-block-info (point) bound)))
               ;;        (c2 (if (consp c2) (memq 'code c2) (eq c2 'edge))))
               ;;   (cond ((and c1 c2)
               ;;             `(,p0 ,p nil nil ,b ,p nil nil nil nil nil nil))
               ;;         (c1 `(,p0 ,p nil nil nil nil ,b ,p nil nil nil nil))
               ;;         (c2 `(,p0 ,p nil nil nil nil nil nil ,b ,p nil nil))
               ;;         (t `(,p0 ,p nil nil nil nil nil nil nil nil ,b ,p))))
               (list p0 p nil nil b p))
              (`(,(or `blockquote `listitem) ,b) ; includes rest of line
               (goto-char b) (forward-line 1) (setq p (point))
               (if (< b (1- p)) (list p0 p p0 b b p)
                   ;; don't highlight empty block lines (do just prefix)
                   (list p0 p p0 b nil nil)))
              (`(,(or `blockquote `listitem) ,b . ,_) ; no text content
               (goto-char (setq p b))
               (list p0 b p0 b nil nil))
              (_ (error "internal error: %S" info))))
           p))))

(defun markdown-match-code-blocks (bound)
  (markdown-debug (markdown-match-blocks bound 'code)))
(defun markdown-match-blockquote-blocks (bound)
  (markdown-debug (markdown-match-blocks bound 'blockquote)))
(defun markdown-match-listitem-blocks (bound)
  (markdown-debug (markdown-match-blocks bound 'listitem)))

(defface markdown-inline-code-face
  '((t :foreground "yellow" :background "#333" :weight bold
       :box (:color "#444" :line-width -2 :style released-button)))
  "Face for inline code.")
(defface markdown-code-block-face
  '((t :background "#333"))
  "Face for code blocks.")
(defvar markdown-header-magnify/level 0.025)
(defface markdown-header1-line-face
  '((t :background "#842400"))
  "Face for 1st level header line.")
(defface markdown-header1-text-face
  '((t :weight bold :underline "#30b000" :overline "#30b000"
       :inherit variable-pitch))
  "Face for 1st level header text.")
(defface markdown-header2-line-face
  '((t :background "#6e1e00"))
  "Face for 2nd level header line.")
(defface markdown-header2-text-face
  '((t :weight bold :underline "#2a9a00" :overline "#2a9a00"
       :inherit variable-pitch))
  "Face for 2nd level header text.")
(defface markdown-header3-line-face
  '((t :background "#581800"))
  "Face for 3rd level header line.")
(defface markdown-header3-text-face
  '((t :weight bold :underline "#248400" :overline "#248400"
       :inherit variable-pitch))
  "Face for 3rd level header text.")
(defface markdown-header4-line-face
  '((t :background "#421200"))
  "Face for 4th level header line.")
(defface markdown-header4-text-face
  '((t :weight bold :underline "#1e6e00" :overline "#1e6e00"
       :inherit variable-pitch))
  "Face for 4th level header text.")
(defface markdown-header5-line-face
  '((t :background "#2c0c00"))
  "Face for 5th level header line.")
(defface markdown-header5-text-face
  '((t :weight bold :underline "#185800" :overline "#185800"
       :inherit variable-pitch))
  "Face for 5th level header text.")
(defface markdown-header6-line-face
  '((t :background "#160600"))
  "Face for 6th level header line.")
(defface markdown-header6-text-face
  '((t :weight bold :underline "#124200" :overline "#124200"
       :inherit variable-pitch))
  "Face for 6th level header text.")
(defface markdown-horizontal-rule-line-face
  '((t :background "blue4"))
  "Face for horizontal rule line.")
(defface markdown-listitem-block-face
  '((t :foreground "yellow" :weight bold))
  "Face for prefix of listitem blocks.")
(defface markdown-listitem-text-face
  '((t :background "#121"))
  "Face for text in listitem blocks.")
(defface markdown-blockquote-block-face
  '((t :foreground "gray25"))
  "Face for prefix of blockquote blocks.")
(defface markdown-blockquote-text-face
  '((t :background "#224"))
  "Face for text in blockquote blocks.")
(defface markdown-html-comment-face
  '((t :foreground "gray40"))
  "Face for html comments.")
(defface markdown-bold-face
  '((t :inherit bold))
  "Face for inline bold text.")
(defface markdown-italic-face
  '((t :inherit italic))
  "Face for inline italic text.")
(defface markdown-bold-italic-face
  '((t :inherit bold-italic))
  "Face for inline bold-italic text.")
(defface markdown-markup-punctuation-face
  '((t :foreground "gray25" :height 0.5))
  "Face for the punctuation markup that wraps bold/italic text.
Still a little visible, in case of text mistakes.")
(defface markdown-invisible-markup-punctuation-face
  '((t :foreground "gray15" :height 0.15))
  "Face for markup that should be almost completely invisible.
Used for the backquotes of inline code, and trailing header hashes.")

(defvar markdown-keywords
  `(;; blocks first, supressing everything else below for code blocks
    (markdown-match-code-blocks
     (2 'markdown-code-block-face t t))
    ;; comments
    ("<!--.*?-->"
     (0 'markdown-html-comment-face t))
    ;; inline code
    ("\\(\\(`+\\) *\\)\\(.\\(?:.\\|\n[^\n]\\)*?\\)\\( *\\2\\)"
     (1 'markdown-markup-punctuation-face)
     (4 'markdown-markup-punctuation-face)
     (3 'markdown-inline-code-face))
    ;; other inline markdowns
    (,(concat "\\(?:[^*_`.a-zA-Z0-9\\]\\|\\`\\)\\(___\\|\\*\\*\\*\\)"
              "\\([^ \t\n_*]\\(?:.\\|\n[^\n]\\)*?\\)\\(\\1\\)")
     (1 'markdown-markup-punctuation-face)
     (3 'markdown-markup-punctuation-face)
     (2 'markdown-bold-italic-face))
    (,(concat "\\(?:[^*_`.a-zA-Z0-9\\]\\|\\`\\)\\(__\\|\\*\\*\\)"
              "\\([^ \t\n_*]\\(?:.\\|\n[^\n]\\)*?\\)\\(\\1\\)")
     (1 'markdown-markup-punctuation-face)
     (3 'markdown-markup-punctuation-face)
     (2 'markdown-bold-face))
    (,(concat "\\(?:[^*_`.a-zA-Z0-9\\]\\|\\`\\)\\([_*]\\)"
              "\\([^ \t\n_*]\\(?:.\\|\n[^\n]\\)*?\\)\\(\\1\\)")
     (1 'markdown-markup-punctuation-face)
     (3 'markdown-markup-punctuation-face)
     (2 'markdown-italic-face))
    ;; horizontal rules
    ("^ *\\([-_*]\\)\\( *\\1\\)\\{2,\\}\n"
     (0 'markdown-horizontal-rule-line-face))
    ;; h1/h2 headers, including a previous empty (or ^L) line, if any,
    ;; (makes it visually easy to put two newlines before these headers)
    ("^\\(?:\f?\n\\)?# +\\(.*[^ #\n]\\)?\\([ #]*\\)\\(?:\n\\|\\'\\)"
     (0 'markdown-header1-line-face append)
     (1 'markdown-header1-text-face append)
     (1 '(:height ,(+ 1.0 (* 6 markdown-header-magnify/level))) append)
     (2 'markdown-invisible-markup-punctuation-face append))
    ("^\\(?:\f?\n\\)?## +\\(.*[^ #\n]\\)?\\([ #]*\\)\\(?:\n\\|\\'\\)"
     (0 'markdown-header2-line-face)
     (1 'markdown-header2-text-face append)
     (1 '(:height ,(+ 1.0 (* 5 markdown-header-magnify/level))) append)
     (2 'markdown-invisible-markup-punctuation-face append))
    ;; setext headers -- a bit larger since the text is more pronounced
    ("^\\(?:\f?\n\\) *\\(.*[^ \n]\\) *\n=+ *\\(?:\n\\|\\'\\)"
     (0 'markdown-header1-line-face)
     (1 'markdown-header1-text-face append)
     (1 'markdown-invisible-markup-punctuation-face append))
    ("^\\(?:\f?\n\\) *\\(.*[^ \n]\\) *\n-+ *\\(?:\n\\|\\'\\)"
     (0 'markdown-header2-line-face)
     (1 'markdown-header2-text-face append)
     (1 'markdown-invisible-markup-punctuation-face append))
    ;; lower headers
    ("^### +\\(.*[^ #\n]\\)?\\([ #]*\\)\\(?:\n\\|\\'\\)"
     (0 'markdown-header3-line-face append)
     (1 'markdown-header3-text-face append)
     (1 '(:height ,(+ 1.0 (* 4 markdown-header-magnify/level))) append)
     (2 'markdown-invisible-markup-punctuation-face append))
    ("^#### +\\(.*[^ #\n]\\)?\\([ #]*\\)\\(?:\n\\|\\'\\)"
     (0 'markdown-header4-line-face append)
     (1 'markdown-header4-text-face append)
     (1 '(:height ,(+ 1.0 (* 3 markdown-header-magnify/level))) append)
     (2 'markdown-invisible-markup-punctuation-face append))
    ("^##### +\\(.*[^ #\n]\\)?\\([ #]*\\)\\(?:\n\\|\\'\\)"
     (0 'markdown-header5-line-face append)
     (1 'markdown-header5-text-face append)
     (1 '(:height ,(+ 1.0 (* 2 markdown-header-magnify/level))) append)
     (2 'markdown-invisible-markup-punctuation-face append))
    ("^###### +\\(.*[^ #\n]\\)?\\([ #]*\\)\\(?:\n\\|\\'\\)"
     (0 'markdown-header6-line-face append)
     (1 'markdown-header6-text-face append)
     (1 '(:height ,(+ 1.0 (* 1 markdown-header-magnify/level))) append)
     (2 'markdown-invisible-markup-punctuation-face append))
    ;; text blocks
    (markdown-match-listitem-blocks
     (1 'markdown-listitem-block-face append t)
     (2 'markdown-listitem-text-face append t))
    (markdown-match-blockquote-blocks
     (1 'markdown-blockquote-block-face append t)
     (2 'markdown-blockquote-text-face append t))
    )
  "Syntax highlighting for Markdown files.")

(eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
(defun markdown-extend-font-lock-region ()
  ;; Just extend it to text boundaries that are at 0 indentation
  (let ((changed nil))
    (goto-char font-lock-beg)
    (unless (bobp)
      (and (if (re-search-backward "\n\n[^ \t\r\n*=+-]" nil t)
             (forward-line 1)
             (goto-char (point-min)))
           (< (point) font-lock-beg)
           (setq changed t font-lock-beg (point))))
    (goto-char font-lock-end)
    (unless (eobp)
      (and (if (re-search-forward "\n\n+[^ \t\r\n*=+-]" nil t)
             (forward-line -1)
             (goto-char (point-max)))
           (< font-lock-end (point))
           (setq changed t font-lock-end (point))))
    changed))

(defun markdown-nobreak-p ()
  (let ((props (text-properties-at (point))))
    (and (plist-get props 'fontified)
         (eq 'markdown-inline-code-face (plist-get props 'face)))))

(defvar-local markdown-mode-undo nil
  "Set to a form that undoes the effect of `markdown-mode'")

(define-minor-mode markdown-mode
  "View text with markdown highlights etc.

The markdown highlights are added to whatever is already is
there, and a few other settings are done, like comment and
paragraph functionality.  Everything should be restored once the
mode is turned off."
  :lighter " MD"
  (if (not markdown-mode) (eval markdown-mode-undo)
      (let* ((undo '())
             (change-local
              (lambda (name value)
                (let ((old (symbol-value name)))
                  (unless (equal old value)
                    (if (eq value 'just-undo)
                      (push `(setq-local ,name ',old) undo)
                      (progn (set (make-local-variable name) value)
                             (push `(when (equal ,name ',value)
                                      (setq-local ,name ',old))
                                   undo))))))))
        ;;
        (funcall change-local 'fill-column 72)
        (unless auto-fill-function
          (auto-fill-mode 1)
          (push `(auto-fill-mode -1) undo))
        ;;
        (funcall change-local 'font-lock-keywords 'just-undo)
        (font-lock-mode -1)
        (font-lock-add-keywords nil markdown-keywords t)
        (funcall change-local 'font-lock-multiline t)
        (funcall change-local 'font-lock-extend-region-functions 'just-undo)
        (add-to-list 'font-lock-extend-region-functions
                     'markdown-extend-font-lock-region)
        (font-lock-mode +1)
        ;;
        (funcall
         change-local 'paragraph-start
         "\f\\|[> \t]*$\\|[ \t]*[*+-] \\|[ \t]*[0-9]+\\.[ \t]\\|[ \t]*: ")
        (funcall change-local 'paragraph-separate "\\(?:[> \t\f]*\\|.*  \\)$")
        (funcall change-local 'adaptive-fill-first-line-regexp "\\`[ \t>]*\\'")
        (funcall change-local 'fill-nobreak-predicate 'just-undo)
        (add-hook 'fill-nobreak-predicate 'markdown-nobreak-p nil t)
        ;;
        (funcall change-local 'comment-start "<!-- ")
        (funcall change-local 'comment-end " -->")
        (funcall change-local 'comment-start-skip "<!--[ \t\n]*")
        (funcall change-local 'comment-column 0)
        ;;
        (setq-local markdown-mode-undo (cons 'progn undo)))))

(define-derived-mode markdown-text-mode indented-text-mode "Text"
  "Major mode for editing markdown text with highlights etc.
Same as `indented-text-mode' with `markdown-mode'."
  ;; Seems silly: maybe it's better to just add it as a hook, but see
  ;; `paragraph-indent-text-mode'.
  (markdown-mode +1))

(provide 'markdown)

;;; markdown.el ends here
