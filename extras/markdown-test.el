(makunbound 'markdown-keywords)
(load (concat
       (expand-file-name
        (file-name-directory (or load-file-name buffer-file-name)))
       "markdown.el"))

(defun >>simple-face (f)
  (intern (replace-regexp-in-string
           "\\`markdown-\\(.*\\)-face\\'" "\\1" (symbol-name f))))
(defun >>simple-faces (f)
  (if (listp f)
    (let ((r '()))
      (while f
        (if (keywordp (car f)) (setq f (cddr f))
            (push (>>simple-face (pop f)) r)))
      (nreverse r))
    (list (>>simple-face f))))

(defun >>faces-at (p)
  (cons p (>>simple-faces (plist-get (text-properties-at p) 'face))))

(defun >>expand-range (x)
  (let ((p (car x)))
    (cond ((symbolp p)
           (pcase (mapcar 'string-to-number (split-string (symbol-name p) "-"))
             (`(,from ,to)
              (mapcar (lambda (p) (cons p (cdr x)))
                      (number-sequence from to)))))
          ((integerp p) (list x)))))

(let ((n 0) (only nil) (mode nil) B E buf text expected result)
  (re-search-forward "^ *\\[\\[\\[[ \t\r\n]*")
  (when (re-search-forward "^ *;+ *O[N]LY *\n" nil t)
    (goto-char (match-end 0)) (setq only 1))
  (while (not (or (looking-at " *\\]\\]\\] *$")
                  (and (eq 0 only) (re-search-forward " *\\]\\]\\] *$"))))
    (setq B (point) expected (read (current-buffer)))
    (cond
      ((keywordp expected) (setq mode expected))
      (t (when only (setq only (1- only)))
         (setq n (1+ n))
         (setq text (read (current-buffer)) E (point)
               buf (get-buffer-create "*Markdown View Test*"))
         (with-current-buffer buf
           (erase-buffer)
           (insert (replace-regexp-in-string "〈\n" "\n" text t t))
           (markdown-debug "\n----\nTesting %S, expected result: %S"
                           mode expected)
           (setq result
                 (pcase mode
                   (:blocks (markdown-block-info* (point-min) (point-max)))
                   (:faces
                    (save-excursion
                      (switch-to-buffer buf)
                      (markdown-mode 1)
                      (redisplay t)
                      (setq expected
                            (apply 'append (mapcar '>>expand-range expected))
                            result
                            (mapcar '>>faces-at (mapcar 'car expected))))))))
         (kill-buffer buf)
         (unless (equal expected result)
           (run-with-idle-timer 0 nil
                                `(lambda ()
                                   (goto-char ,E)
                                   (set-mark ,B)
                                   (setq mark-active t)))
           (markdown-debug "Failure\n  Expected: %S\n       got: %S"
                           expected result)
           (error "Expected: %S\n     got: %S" expected result))))
    (when (looking-at "\\([ \t\r\n]+\\|;.*\n\\)+") (goto-char (match-end 0))))
  (goto-char (match-end 0))
  (message "%S Tests Passed" n))

;; * Keywords specify test modes: `:blocks', `:faces'
;; * Use "〈" before EOL after spaces (not needed, but avoids
;;   spaces-at-EOL in this file)
;; * Use "; ONLY" to run just one test, but need a mode keyword before
;;   that single test
[[[

:blocks

((1 . nil)) "\
"

((1 . nil) (2 . nil)) "\

"

((1 . text)) "\
foo"

((1 . text) (5 . nil)) "\
foo
"

((1 . text) (5 . text)) "\
foo
bar"

((1 . text) (5 . text) (9 . nil)) "\
foo
bar
"

((1 . text)) "\
 foo"

((1 . text)) "\
   foo"

((1 . text) (8 . text)) "\
   foo
bar"

((1 . text) (5 . text)) "\
foo
   bar"

((1 . (code 5))) "\
    foo"

((1 . (code 5))) "\
     foo"

((1 . (code 5))) "\
                         foo"

((1 . (code 5)) (9 . (code 13))) "\
    foo
    bar"

((1 . nil) (2 . (code 6))) "\

    foo"

((1 . nil)) "\
  "

((1 . nil) (4 . nil)) "\
  〈
  "

((1 . nil) (4 . nil) (7 . nil)) "\
  〈
  〈
"

((1 . nil) (2 . (code 6)) (10 . (code 14)) (18 . (code 22)) (26 . nil)) "\

    foo
    bar
    foo
"

((1 . nil) (2 . (code 6)) (10 . (code 14)) (18 . (code 22)) (26 . nil)) "\

    foo
       〈
    foo
"

((1 . nil) (2 . (code 6)) (10 . (code 14)) (24 . (code 28)) (32 . nil)) "\

    foo
          bar
    foo
"

((1 . nil) (2 . (code 6)) (10 . (code 14)) (24 . (code 28)) (32 . nil)) "\

    foo
             〈
    foo
"

((1 . nil) (2 . (code 6)) (10 . (code 10)) (11 . (code 15)) (19 . nil)) "\

    foo

    foo
"

((1 . nil) (2 . (code 6)) (10 . (code 11)) (12 . (code 16)) (20 . nil)) "\

    foo
 〈
    foo
"

((1 . nil) (2 . text) (6 . nil) (7 . text) (11 . nil)) "\

foo

foo
"

((1 . (blockquote 3))) "\
> foo"

((1 . nil) (2 . (blockquote 4)) (8 . nil)) "\

> foo
"

((1 . (blockquote 3)) (7 . (blockquote 9)) (13 . (blockquote 15))) "\
> foo
> bar
> baz"

((1 . (blockquote 3)) (7 . (blockquote 8)) (9 . (blockquote 11))) "\
> foo
>
> baz"

((1 . (blockquote 3)) (7 . (blockquote 9)) (10 . (blockquote 12))) "\
> foo
> 〈
> baz"

((1 . (blockquote 3)) (7 . (blockquote 8)) (9 . (blockquote 11)) (15 . nil)) "\
> foo
>
> baz
"

((1 . (blockquote 3)) (7 . (blockquote 9)) (10 . (blockquote 12)) (16 . nil)) "\
> foo
> 〈
> baz
"

((1 . (listitem 3))) "\
* foo"

((1 . (listitem 3))) "\
* foo"

((1 . (listitem 3)) (7 . (listitem 9)) (13 . nil)) "\
* foo
* foo
"

((1 . (listitem 3)) (7 . (listitem 7)) (8 . (listitem 10)) (14 . nil)) "\
* foo

* foo
"

((1 . (listitem 3)) (7 . (listitem 9))) "\
* foo
  bar"

((1 . (listitem 3)) (7 . (listitem 7)) (8 . (listitem 10))) "\
* foo

  bar"

((1 . (listitem 3)) (7 . (listitem 7)) (8 . (listitem 10)) (14 . nil)) "\
* foo

  bar
"

((1 . (listitem 3)) (7 . (listitem 7)) (8 . (listitem 10)) (14 . nil)) "\
* foo

   ba
"

((1 . (listitem 3)) (7 . (listitem 7)) (8 . (listitem 10)) (15 . nil)) "\
* foo

   bar
"

((1 . (listitem 3)) (7 . (listitem 7)) (8 . (listitem 10)) (16 . nil)) "\
* foo

    bar
"

((1 . (listitem 3)) (7 . (listitem 7)) (8 . (listitem 10)) (17 . nil)) "\
* foo

     bar
"

((1 . (listitem 3)) (7 . (listitem 7)) (8 . (listitem 10 code 14))) "\
* foo

      bar"

((1 . (listitem 3)) (7 . (listitem 7)) (8 . (listitem 10 code 14)) (18 . nil)) "\
* foo

      bar
"

((1 . (listitem 3)) (7 . (listitem 9 listitem 11))) "\
* foo
  - bar"

((1 . (listitem 3)) (7 . (listitem 7)) (8 . (listitem 10 code 14))) "\
* foo

      bar"

((1 . (listitem 3)) (7 . (listitem 7)) (8 . (listitem 10 listitem 12))) "\
* foo

  - bar"

((1 . (listitem 3)) (7 . (listitem 7)) (8 . (listitem 10 listitem 13))) "\
* foo

   - bar"

((1 . (listitem 3)) (7 . (listitem 7)) (8 . (listitem 10 listitem 12))
 (16 . (listitem 16 listitem 16)) (17 . (listitem 19 listitem 21))
 (25 . nil)) "\
* foo

  - bar

  - bar
"

((1 . (listitem 3)) (7 . (listitem 7)) (8 . (listitem 10 listitem 12))
 (16 . (listitem 18 listitem 20)) (24 . (listitem 24 listitem 24))
 (25 . (listitem 27 listitem 29 code 33)) (38 . (listitem 38 listitem 38))
 (39 . (listitem 41 listitem 43)) (47 . (listitem 47)) (48 . (listitem 50))
 (55 . nil)) "\
* foo

  - bar
    baz

        blah

  - meh

* fooo
"

((1 listitem 3) (7 . nil) (8 . text) (13 . nil) (14 . (code 18)) (22 . nil)
 (23 . text)) "\
* foo

bleh

    bar

baz"

((1 . text) (5 . text) (10 . nil)) "\
foo
 bar
"

((1 . text) (5 . text) (12 . nil)) "\
foo
   bar
"

((1 . text) (5 . text) (13 . nil)) "\
foo
    bar
"

((1 . (blockquote 3)) (7 . (blockquote 9)) (13 . nil)) "\
> foo
  bar
"

;; It should be relatively easy to make things like this work too: when
;; we get *any* 'text line after a blockquote/listitem, make it be one
;; too.  (The only complication is that we need to carry over the
;; `indents+types' information for the following lines.)  But leave it
;; out so it's easy to avoid "lazy markdown" which should really not
;; exist IMO.
;;   ((1 . (blockquote 3)) (7 . (blockquote 9)) (11 . nil)) "\
;;   > foo
;;   bar
;;   "

((1 . (blockquote 3)) (7 . nil) (8 . text) (14 . nil)) "\
> foo

  bar
"

((1 . (blockquote 3)) (7 . nil) (8 . (code 12)) (16 . nil)) "\
> foo

    bar
"

((1 . text) (5 . nil) (6 . (listitem 10)) (14 . (listitem 14))
 (15 . (listitem 19 listitem 21)) (25 . nil)) "\
foo

  * bar

    - baz
"

((1 . text) (5 . nil) (6 . (listitem 8))
 (12 . (listitem 14 listitem 16))
 (20 . (listitem 22 listitem 24 listitem 26))
 (30 . (listitem 32 listitem 34 listitem 36))
 (40 . nil) (41 . (blockquote 43))
 (47 . (blockquote 49 blockquote 51))
 (55 . (blockquote 57 blockquote 59 blockquote 61))
 (65 . (blockquote 65)) (66 . (blockquote 68))
 (72 . (blockquote 74 listitem 76))
 (81 . (blockquote 83 listitem 85 blockquote 87))
 (92 . (blockquote 94 listitem 96 listitem 98 blockquote 100 blockquote 102))
 (107)) "\
foo

* foo
  - foo
    * foo
    * foo

> foo
> > bar
> > > baz

> foo
> * blah
> * > blah
> * * > > blah
"

:faces

((1-3)) "\
foo"

((1 markup-punctuation) (2-4 italic) (5 markup-punctuation) (6)
 (7-8 markup-punctuation) (9-11 bold) (12-13 markup-punctuation) (14)
 (15-17 markup-punctuation) (18-20 bold-italic) (21-23 markup-punctuation)) "\
*foo*
**foo**
***foo***"

((1-2 header1-line) (3-5 header1-line header1-text)
 (6-7 header1-line invisible-markup-punctuation)) "\
# foo #"

((1-2 header1-line) (3-5 header1-line header1-text)
 (6-7 header1-line invisible-markup-punctuation) (8 header1-line)) "\
# foo #
"

((2-3 header1-line) (4-6 header1-line header1-text)
 (7-8 header1-line invisible-markup-punctuation) (9 header1-line)) "\

# foo #
"

((1) (2-3 header1-line) (11-12)) "\


# foo #


"

((1-2 header1-line)
 (3 markup-punctuation header1-line header1-text)
 (4-6 italic header1-line header1-text)
 (7 markup-punctuation header1-line header1-text)
 (8 header1-line)) "\
# *foo*
"

]]]
