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
;; Note: does not work with tabs.
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

;; =====================================================================

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
              (insert (format-time-string "[%H:%M:%S.%3N] ") str "\n"))))
        ((null args)
         `(condition-case e ,fmt/expr
            (error (markdown-debug "ERROR in %S:\n  %S" ',fmt/expr e))))
        (t `(markdown-debug (progn ,fmt/expr ,@args)))))

;; =====================================================================

;; Holds a (cons buffer-chars-modified-tick hash), where the hash maps
;; BOL locations to parse information: (cons block-info inline-info)
;; - block-info is (list TYPE N TYPE N) where TYPE is 'listitem,
;;   'blockquote, or 'code, and N is the position on the line where TYPE
;;   starts.  There can also be a terminating TYPE of 'text for toplevel
;;   text or 'empty for lines that have no text (possibly just prefix
;;   text) -- these have no N after them.
;; - inline-info is a list of inline entries that start somewhere on
;;   this line, each one is (cons delim-len (cons from to)), where
;;   delim-len is the length of delimiter that was used: 1 for italic, 2
;;   for bold, 3 for both, and 0 is used for the delimiter itself.
(defvar-local markdown-info-cache nil)

;; =====================================================================
;; Block-level parsing

;; this is used after skipping initial indentation
(defconst markdown-listitem-rx
  (concat "\\("
          "[*+-]\\|" ; single char bullets
          "\\(?:[0-9]+\\|[a-zA-Z]\\)\\.\\|"
          "(\\(?:[0-9]+\\|[a-zA-Z]\\))"
          "\\)\\(?: \\|$\\)"))

(defun markdown-get-block-info (p bound)
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
            ;; used below to set info on an empty line between two
            ;; blocks that are the same type (for some prefix of the
            ;; types of both)
            (lambda (l1 l2 b)
              (let ((r '()))
                (while (and l1 l2 (equal (car l1) (car l2)))
                  (setq r (cons b (cons (car l1) r))
                        l1 (cddr l1) l2 (cddr l2)))
                (nreverse (cons 'empty r)))))
           (new-line
            (lambda (new)
              (setq pending-types+indents (nreverse indents+types)
                    indents+types '())
              (let ((ts+is pending-types+indents) (p linepos) (pfx '()))
                (while ts+is
                  (push (pop ts+is) pfx)
                  (push (setq p (min (point) (+ p (or (pop ts+is) 0)))) pfx))
                (setq new `(,@(nreverse pfx) ,@new)))
              (let ((prev (cdr (cadr r))))
                (pcase (car r)
                  ((and `(,b empty) (guard (not (equal '(text) prev))))
                   (let ((mid (funcall consumed-line new prev (1- linepos))))
                     (when mid (setcar r (cons b mid)))))))
              (push `(,linepos . ,new) r)
              (forward-line 1)
              (setq linepos (point))
              (if (equal '(empty) new)
                (setq pending-types+indents pending-types+indents0)
                (setq pending-types+indents0 pending-types+indents))))
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
                ;; blockquote doesn't continue after an empty line
                (not (and (eq 'blockquote (car pending-types+indents))
                          (memq 'empty (cdar r)))))
           (push (pop pending-types+indents) indents+types)
           (push (pop pending-types+indents) indents+types)
           ;; spcs0 is used to simulate backtracking some spaces (also below)
           ;; (goto-char (+ (car indents+types) p)) =>
           (setq spcs0 (- spcs (car indents+types))
                 p (+ p (car indents+types))))
          ;; code
          ((and (>= spcs 4)
                (let ((last (cdar r)))
                  (or (not last) (memq 'empty last) (memq 'code last))))
           (funcall new-line `(code ,(+ p 4))))
          ;; empty
          ((eolp) (funcall new-line '(empty)))
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
          (t (funcall new-line '(text)))))
      (cond (indents+types (funcall new-line '(empty)))
            ((and (eobp) (bolp)) (push `(,linepos empty) r)))
      (nreverse r))))

;; =====================================================================
;; Inline parsing

;; (This is a rough translation of the code in "lib/inlines.js" in
;; https://github.com/jgm/commonmark.js)

(defconst markdown-inline-delim-rx
  "\\\\\\|[*]+\\|_+\\|\\(`+\\)[ \r\n]*\\|<")
(defconst markdown-inline-code-end-delim-rx
  ;; %s is the expected backtick delimiter; includes the last inline code char
  "\\(\\(?:[^ \r\n][ \r\n]+\\|[^`]\\)%s\\)\\(?:[^`]\\|\\'\\)")
(defconst markdown-autolink-rx
  "[a-z][a-z0-9.-]*:[^<> [:cntrl:]]*>")
(defconst markdown-autolink-email-rx
  (concat "[[:alnum:].!#$%&'*+/=?^_`{|}~-]+@"
          "[[:alnum:]]\\(?:[[:alnum:]-]\\{0,61\\}[[:alnum:]]\\)?"
          "\\(?:\\.[[:alnum:]]\\(?:[[:alnum:]-]\\{0,61\\}[[:alnum:]]\\)?\\)*"
          ">"))

(defun markdown-try-html (bound)
  (pcase (char-after)
    (?\/ (and (looking-at "/[a-z][[:alnum:]]*[[:space:]]*>")
              (progn (goto-char (match-end 0)) 'html)))
    (?\! (goto-char (1+ (point)))
         (cond ((looking-at "--")
                (goto-char (match-end 0))
                (and (not (looking-at "-?>"))
                     (search-forward "--" bound t)
                     (looking-at ">")
                     (progn (goto-char (match-end 0)) 'comment)))
               ((looking-at "[A-Z]+[[:space:]]")
                (and (search-forward ">" nil t) 'html))
               ((looking-at "\\[CDATA\\[")
                (and (search-forward "]]>" nil t) 'html))))
    (?\? (goto-char (1+ (point)))
         (and (search-forward "?>" nil t) 'html))
    (_ (and (looking-at "[a-z][[:alnum:]]*")
            (progn (goto-char (match-end 0))
                   (while (looking-at "[[:space:]]+[a-z_:][[:alnum:]_.:-]*")
                     (goto-char (match-end 0))
                     (when (looking-at "[[:space:]]*=[[:space:]]*")
                       (goto-char (match-end 0))
                       (when (or (looking-at "[^\"`'=<>]+")
                                 (looking-at "'[^']*'")
                                 (looking-at "\"[^\"]*\""))
                         (goto-char (match-end 0)))))
                   (and (looking-at "[[:space:]]*/?>")
                        (progn (goto-char (match-end 0)) 'html)))))))

;; returns (cons ranges delimiters), where
;; - ranges is a list of ranges, each one is (cons type (cons from to))
;;   where `type' is a symbol that indicates the type of range
;; - delimiters is a doubly-linked list of delimiters from the current
;;   point to `bound', each one is
;;     (cons (list char length pos can-open can-close) (cons prev next))
(defun markdown-raw-inline-ranges-and-delimiters (bound)
  (let ((ranges '()) (first nil) (last nil)
        (whitespacep ; inluding nil, which happens on buffer edges
         (lambda (ch cc) (or (eq cc 'Zs) (memq ch '(nil ?\n ?\r ?\t ?\f)))))
        (punctuationp
         (lambda (ch cc) (or (memq cc '(Pc Pd Pe Pf Pi Po Ps))
                             (memq ch '(?$ ?+ ?< ?= ?> ?^ ?` ?\| ?~)))))
        b e)
    (while (re-search-forward markdown-inline-delim-rx bound t)
      (setq b (match-beginning 0) e (match-end 0))
      (pcase (char-after b)
        (?\\ (goto-char (1+ (match-end 0))))
        (?\< (if (or (looking-at markdown-autolink-rx)
                     (looking-at markdown-autolink-email-rx))
               (let ((b2 (1- (match-end 0))) (e2 (match-end 0)))
                 (goto-char e2)
                 (setq ranges `((delim    . (,b2 . ,e2))
                                (autolink . (,e  . ,b2))
                                (delim    . (,b  . ,e ))
                                . ,ranges)))
               (let ((type (markdown-try-html bound)))
                 (if (not type) (goto-char e)
                     (setq ranges `((,type . (,b  . ,(point))) . ,ranges))))))
        (?\` (let ((delim (match-string-no-properties 1)))
               (when (re-search-forward
                      (format markdown-inline-code-end-delim-rx delim) bound t)
                 (let ((b2 (1+ (match-beginning 1))) (e2 (match-end 1)))
                   (goto-char e2)
                   (setq ranges `((delim . (,b2 . ,e2))
                                  (code  . (,e  . ,b2))
                                  (delim . (,b  . ,e ))
                                  . ,ranges))))))
        (c (let* ((bc (char-before b)) (ec (char-after e))
                  (bcc (and bc (get-char-code-property bc 'general-category)))
                  (ecc (and ec (get-char-code-property ec 'general-category)))
                  (left-flank
                   (and (not (funcall whitespacep ec ecc))
                        (not (and (funcall punctuationp ec ecc)
                                  (not (funcall whitespacep bc bcc))
                                  (not (funcall punctuationp bc bcc))))))
                  (right-flank
                   (and (not (funcall whitespacep bc bcc))
                        (not (and (funcall punctuationp bc bcc)
                                  (not (funcall whitespacep ec ecc))
                                  (not (funcall punctuationp ec ecc))))))
                  (openp  (and left-flank (or (= c ?*) (not right-flank))))
                  (closep (and right-flank (or (= c ?*) (not left-flank)))))
             (setq last (cons (list c (- e b) b openp closep)
                              (cons last nil)))
             (cond ((not first) (setq first last))
                   ((cadr last) (setcdr (cdr (cadr last)) last)))))))
    (cons ranges first)))

;; returns ranges as a list of (cons type (cons from to)), where `type'
;; here is the length of delimiters for emphasis ranges, 0 for the
;; emphasis delimiter text itself
(defun markdown-inline-emphasis-ranges (delim-dlist)
  (let ((ranges '()) r opener (closer delim-dlist))
    (setq closer (cddr closer))
    (while closer
      (while (and closer (not (nth 4 (car closer))))
        (setq closer (cddr closer)))
      (setq opener (cadr closer))
      (while (and opener (not (and (nth 3 (car opener))
                                   (= (caar opener) (caar closer)))))
        (setq opener (cadr opener)))
      (if (not opener)
        (setq closer (cddr closer))
        (let* ((o (cdar opener)) (olen (car o)) (opos (cadr o))
               (c (cdar closer)) (clen (car c)) (cpos (cadr c))
               (len (min olen clen))
               (len (cond ((< len 3) len) ((zerop (% clen 2)) 2) (t 1))))
          (setq ranges `((0    . (,cpos                  . ,(+ cpos len)))
                         (,len . (,(+ opos olen)         . ,cpos))
                         (0    . (,(+ opos (- olen len)) . ,(+ opos olen)))
                         ,@ranges))
          (unless (eq (cddr opener) closer)
            (setcdr (cdr opener) closer)
            (setcar (cdr closer) opener))
          (if (= olen len)
            (progn (setcar (cdr closer) (cadr opener))
                   (when (cadr closer) (setcdr (cdr (cadr closer)) closer)))
            (setcar o (- olen len)))
          (if (= clen len)
            (progn (setq closer (cddr closer))
                   (when closer
                     (setcar (cdr closer) (cadr (cadr closer)))
                     (when (cadr closer)
                       (setcdr (cdr (cadr closer)) closer))))
            (progn (setcar c (- clen len))
                   (setcar (cdr c) (+ cpos len)))))))
    (nreverse ranges)))

;; return normalized emphasis ranges: all disjoint, merge consecutive
;; ones, merge ranges of 1- and 2-delims (italic, bold) into 3-delims;
;; assumes a non-empty input list
(defun markdown-normalize-ranges (ranges)
  (let* ((mn (cadr (car ranges)))
         (mx (cddr (car ranges)))
         v last-i (last-len nil) r)
    (dolist (p (cdr ranges)) (when (< (cadr p) mn) (setq mn (cadr p))))
    (dolist (p (cdr ranges)) (when (> (cddr p) mx) (setq mx (cddr p))))
    (setq v (make-vector (- mx mn) nil))
    (dolist (p ranges)
      (let ((len (car p)) (from (- (cadr p) mn)) (to (- (cddr p) mn)))
        (dotimes (i (- to from))
          (let ((old (aref v (+ i from))))
            (unless (or (eq old len) (eq old 0))
              (aset v (+ i from) (if (or (not old) (eq len 0)) len 3)))))))
    (dotimes (i (- mx mn))
      (unless (eq last-len (aref v i))
        (when last-len (push (cons last-len (cons (+ mn last-i) (+ mn i))) r))
        (setq last-i i last-len (aref v i))))
    (when last-len (push (cons last-len (cons (+ mn last-i) mx)) r))
    (nreverse r)))

;; returns a list of ranges, each as the above two functions: so type is
;; 1/2/3 for emphasis ranges, 0 for their delimiters, or a symbol for
;; other ranges
(defun markdown-inline-ranges (bound)
  (let* ((raw (markdown-raw-inline-ranges-and-delimiters bound))
         (ranges1 (car raw))
         (ranges2 (markdown-inline-emphasis-ranges (cdr raw)))
         (ranges2 (if (null ranges2) '()
                      (markdown-normalize-ranges ranges2))))
    (sort (append ranges1 ranges2)
          (lambda (x y)
            (or (< (cadr x) (cadr y))
                (and (= (cadr x) (cadr y)) (< (cddr x) (cddr y))))))))

(defun markdown-add-inline-info (block-info)
  (let ((r '()) (lines block-info) (curblock '())
        line linetype lastlinetype (start nil))
    (while (or lines curblock)
      (setq line (pop lines) linetype '())
      (let ((l line)) (while l (pop l) (push (pop l) linetype)))
      (unless (equal linetype lastlinetype)
        (setq lastlinetype linetype)
        (when start
          (let ((inlines (save-excursion
                           (goto-char start)
                           (markdown-inline-ranges (car line)))))
            (setq curblock (nreverse curblock))
            (while curblock
              (let ((l (pop curblock)) (is '()))
                (while (and inlines
                            (or (null curblock)
                                (< (cadr (car inlines)) (caar curblock))))
                  (push (pop inlines) is))
                (push (cons (car l) (cons (cdr l) (nreverse is))) r)))))
        (setq start (and (not (memq (car linetype) '(empty code)))
                         (car line))))
      (when line
        (if start (push line curblock)
            (push (cons (car line) (cons (cdr line) nil)) r))))
    (setq r (append curblock r) curblock '())
    (nreverse r)))

;; =====================================================================
;; internal functions

(defun markdown-set+get-info (p bound)
  ;; should always be called at a BOL (actually at the beginning of a
  ;; block), drop test because it's always checked before calling it
  ;; (unless (memq (char-before p) '(?\n nil))
  ;;   (error "markdown-info: should be called at a BOL"))
  (let ((h (cdr markdown-info-cache)))
    (let ((m (buffer-chars-modified-tick)))
      (unless (eq m (car markdown-info-cache))
        (if h (clrhash h) (setq h (make-hash-table)))
        (setq markdown-info-cache (cons m h))))
    (or (gethash p h)
        (let* ((info (markdown-get-block-info p bound))
               (info (markdown-add-inline-info info)))
          (dolist (x info) (puthash (car x) (cdr x) h))
          (or (gethash p h)
              (error "internal error: no value set at %S" p))))))

(defun markdown-get-block-boundaries (b e)
  ;; look for toplevel block boundaries containing b--e, simple version:
  ;; just look for boundaries that are an empty line followed by a line
  ;; that starts with a non-whitespace; changes point
  (goto-char b)
  (unless (bobp)
    (setq b (if (re-search-backward "\n\n[^ \t\r\n*=+-]" nil t)
              (progn (forward-line 1) (point))
              (point-min))))
  (goto-char e)
  (unless (eobp)
    (setq e (if (re-search-forward "\n\n+[^ \t\r\n*=+-]" nil t)
              (progn (forward-line -1) (point))
              (point-max))))
  (cons b e))

(defun markdown-block-info*  (p bound) (car (markdown-set+get-info p bound)))
(defun markdown-inline-info* (p bound) (cdr (markdown-set+get-info p bound)))

;; =====================================================================
;; public functions

(defun markdown-info (&optional p)
  (unless p (setq p (point)))
  (unless (memq (char-before p) '(?\n nil))
    (save-excursion (goto-char p) (forward-line 0) (setq p (point))))
  (or (and (eq (car markdown-info-cache) (buffer-chars-modified-tick))
           (gethash p (cdr markdown-info-cache)))
      (let ((r (save-excursion (markdown-get-block-boundaries p p))))
        ;; first populate hash in the block around p, then get the value at p
        (markdown-set+get-info (car r) (cdr r))
        (markdown-set+get-info p (cdr r)))))

(defun markdown-block-info  (&optional p) (car (markdown-info p)))
(defun markdown-inline-info (&optional p) (cdr (markdown-info p)))

;; returns a list of just type symbols, which is the block info -- it's
;; a list of 'blockquote, 'listitem, or 'code (which cannot have the
;; former in it), and can end with 'empty to indicate an empty
;; (prefix-only) line or 'text for toplevel text lines;
;; useful to identify blocks of the same type
(defun markdown-line-type (&optional p)
  (let ((p (markdown-block-info p)) (r '()))
    (while p (push (pop p) r) (pop p))
    (nreverse r)))

;; =====================================================================
;; font-lock functionality

(eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
(defun markdown-extend-font-lock-region ()
  (let ((r (markdown-get-block-boundaries font-lock-beg font-lock-end))
        (changed nil))
    (unless (= font-lock-beg (car r)) (setq font-lock-beg (car r) changed t))
    (unless (= font-lock-end (cdr r)) (setq font-lock-end (cdr r) changed t))
    changed))

(defun markdown-match-blocks (bound type)
  (let* ((p     (point))
         ;; the `save-excursion' here is important: without it we can
         ;; return a match that is before the starting point, which will
         ;; make font-lock spin in an infinite loop
         (bol   (save-excursion (forward-line 0) (point)))
         (info  (markdown-block-info* bol bound))
         (info0 (cons bol info)))
    (while (and (cdr info) (<= (cadr info) p)) (setq info (cddr info)))
    (while (and (not (setq info (memq type info))) (< p bound))
      (forward-line 1)
      (setq p     (point)
            info  (and (not (eobp)) (markdown-block-info* p bound))
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
             (pcase info (`(,type ,a ,type ,b . ,_) (setq info (cddr info)))))
           (set-match-data
            (pcase info
              (`(code ,b)
               (goto-char b) (forward-line 1) (setq p (point))
               ;; This can be used to have a different highlight for
               ;; first/last lines in a code block, if it'll be possible
               ;; to have border only on some sides in the future it can
               ;; be used to have borders on the top/left/bottom sides.
               ;; (let* ((c1 (save-excursion
               ;;              (forward-line -2)
               ;;              (if (bobp) 'edge
               ;;                  (markdown-block-info* (point) bound))))
               ;;        (c1 (if (consp c1) (memq 'code c1) (eq c1 'edge)))
               ;;        (c2 (if (eobp) 'edge
               ;;                (markdown-block-info* (point) bound)))
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
                   (list p0 p p0 b)))
              (`(,(or `blockquote `listitem) ,b . ,_) ; no text content
               (goto-char (setq p b))
               (list p0 b p0 b))
              (_ (error "internal error: %S" info))))
           p))))

(defun markdown-match-inlines (bound types)
  (let* ((p      (point))
         (bol    (save-excursion (forward-line 0) (point))) ;; as above
         (ranges (markdown-inline-info* bol bound))
         (range  (pop ranges)))
    (while (and range (or (< (cadr range) p) (not (memq (car range) types))))
      (setq range (pop ranges)))
    (while (and (not range) (< p bound))
      (forward-line 1)
      (setq p      (point)
            ranges (and (not (eobp)) (markdown-inline-info* p bound))
            range (pop ranges))
      (while (and range (not (memq (car range) types)))
        (setq range (pop ranges))))
    (and range (< p bound)
         (let* ((type (car range)) (b (cadr range)) (e (cddr range))
                (md `(,b ,e)))
           (while (not (eq type (car types)))
             (setq types (cdr types) md (cons nil (cons nil md))))
           (goto-char e)
           (set-match-data (cons b (cons e md)))
           p))))

(defun markdown-match-code-blocks (bound)
  (markdown-debug (markdown-match-blocks bound 'code)))
(defun markdown-match-blockquote-blocks (bound)
  (markdown-debug (markdown-match-blocks bound 'blockquote)))
(defun markdown-match-listitem-blocks (bound)
  (markdown-debug (markdown-match-blocks bound 'listitem)))
(defun markdown-match-emphasis-inlines (bound)
  (markdown-debug (markdown-match-inlines bound '(0 1 2 3))))
(defun markdown-match-source*-inlines (bound) ; source and source-like
  (markdown-debug
   (markdown-match-inlines bound '(delim code autolink html comment))))

(defun markdown-nobreak-p ()
  (let ((props (text-properties-at (point))))
    (and (plist-get props 'fontified)
         (eq 'markdown-inline-code-face (plist-get props 'face)))))

;; =====================================================================

(defface markdown-inline-code-face
  '((t :foreground "yellow" :background "#282828"
       :box (:color "#444" :line-width -2 :style released-button)))
  "Face for inline code.")
(defface markdown-code-block-face
  '((t :background "#282828"))
  "Face for code blocks.")
(defface markdown-autolink-face
  '((t :foreground "yellow" :background "#226" :underline "gray50"))
  "Face for inline autolinks.")
(defface markdown-html-face
  '((t :foreground "gray55"))
  "Face for inline html tags.")
(defface markdown-html-comment-face
  '((t :foreground "gray35"))
  "Face for html comments.")
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
    ;; inlines
    (markdown-match-source*-inlines
     (1 'markdown-markup-punctuation-face nil t)
     (2 'markdown-inline-code-face nil t)
     (3 'markdown-autolink-face nil t)
     (4 'markdown-html-face nil t)
     (5 'markdown-html-comment-face nil t))
    (markdown-match-emphasis-inlines
     (1 'markdown-markup-punctuation-face nil t)
     (2 'markdown-italic-face append t)
     (3 'markdown-bold-face append t)
     (4 'markdown-bold-italic-face append t))
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
     (0 'markdown-header2-line-face append)
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
    ;; h4/.../h6 headers
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

;; =====================================================================

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

;; =====================================================================

(provide 'markdown)

;;; markdown.el ends here
