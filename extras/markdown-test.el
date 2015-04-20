(eval-when-compile
  (when (boundp 'byte-compile--outbuffer)
    (error "refusing to compile this file")))

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

'
(let (t1 t2 (buf (get-buffer-create "*Markdown Timing*")))
  (with-current-buffer buf
    (erase-buffer)
    (insert-file "~/pl/lecture.txt")
    (setq t1 (current-time))
    (markdown-info (point-min) (point-max))
    (setq t2 (current-time)))
  (kill-buffer buf)
  (string-to-number (format-time-string "%S.%3N" (time-subtract t2 t1))))
;; => 0.385

(let ((n 0) (only nil) (mode nil) B E buf text expected result mn mx)
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
               buf (get-buffer-create "*Markdown Test*"))
         (with-current-buffer buf
           (erase-buffer)
           (insert (replace-regexp-in-string "〈\n" "\n" text t t))
           (setq mn (point-min) mx (point-max))
           (markdown-debug "Testing %S, expected result: %S" mode expected)
           (setq result (markdown-get-block-info mn mx))
           (when (eq :inlines mode)
             (setq result (mapcar 'car result)
                   result (mapcar (lambda (x) `(,x . ,(markdown-inline-info x)))
                                  result)
                   result (apply 'append result)))
           (when (eq :faces mode)
             (save-excursion
               (switch-to-buffer buf) (markdown-mode 1) (redisplay t)
               (setq expected (apply 'append (mapcar '>>expand-range expected))
                     result (mapcar '>>faces-at (mapcar 'car expected))))))
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

;; * Keywords specify test modes: `:blocks', `:inlines', `:faces'
;; * Use "〈" before EOL after spaces (not needed, but avoids
;;   spaces-at-EOL in this file)
;; * Use "; ONLY" to run just one test, but need a mode keyword before
;;   that single test
[[[

:blocks

((1 empty)) "\
"

((1 empty) (2 empty)) "\

"

((1 text)) "\
foo"

((1 text) (5 empty)) "\
foo
"

((1 text) (5 text)) "\
foo
bar"

((1 text) (5 text) (9 empty)) "\
foo
bar
"

((1 text)) "\
 foo"

((1 text)) "\
   foo"

((1 text) (8 text)) "\
   foo
bar"

((1 text) (5 text)) "\
foo
   bar"

((1 code 5)) "\
    foo"

((1 code 5)) "\
     foo"

((1 code 5)) "\
                         foo"

((1 code 5) (9 code 13)) "\
    foo
    bar"

((1 empty) (2 code 6)) "\

    foo"

((1 empty)) "\
  "

((1 empty) (4 empty)) "\
  〈
  "

((1 empty) (4 empty) (7 empty)) "\
  〈
  〈
"

((1 empty) (2 code 6) (10 code 14) (18 code 22) (26 empty)) "\

    foo
    bar
    foo
"

((1 empty) (2 code 6) (10 code 14) (18 code 22) (26 empty)) "\

    foo
       〈
    foo
"

((1 empty) (2 code 6) (10 code 14) (24 code 28) (32 empty)) "\

    foo
          bar
    foo
"

((1 empty) (2 code 6) (10 code 14) (24 code 28) (32 empty)) "\

    foo
             〈
    foo
"

((1 empty) (2 code 6) (10 code 10 empty) (11 code 15) (19 empty)) "\

    foo

    foo
"

((1 empty) (2 code 6) (10 code 11 empty) (12 code 16) (20 empty)) "\

    foo
 〈
    foo
"

((1 empty) (2 text) (6 empty) (7 text) (11 empty)) "\

foo

foo
"

((1 blockquote 3)) "\
> foo"

((1 empty) (2 blockquote 4) (8 empty)) "\

> foo
"

((1 blockquote 3) (7 blockquote 9) (13 blockquote 15)) "\
> foo
> bar
> baz"

((1 blockquote 3) (7 blockquote 8 empty) (9 blockquote 11)) "\
> foo
>
> baz"

((1 blockquote 3) (7 blockquote 9 empty) (10 blockquote 12)) "\
> foo
> 〈
> baz"

((1 blockquote 3) (7 blockquote 8 empty) (9 blockquote 11) (15 empty)) "\
> foo
>
> baz
"

((1 blockquote 3) (7 blockquote 9 empty) (10 blockquote 12) (16 empty)) "\
> foo
> 〈
> baz
"

((1 listitem 3)) "\
* foo"

((1 listitem 3)) "\
* foo"

((1 listitem 3) (7 listitem 9) (13 empty)) "\
* foo
* foo
"

((1 listitem 3) (7 listitem 7 empty) (8 listitem 10) (14 empty)) "\
* foo

* foo
"

((1 listitem 3) (7 listitem 9)) "\
* foo
  bar"

((1 listitem 3) (7 listitem 7 empty) (8 listitem 10)) "\
* foo

  bar"

((1 listitem 3) (7 listitem 7 empty) (8 listitem 10) (14 empty)) "\
* foo

  bar
"

((1 listitem 3) (7 listitem 7 empty) (8 listitem 10) (14 empty)) "\
* foo

   ba
"

((1 listitem 3) (7 listitem 7 empty) (8 listitem 10) (15 empty)) "\
* foo

   bar
"

((1 listitem 3) (7 listitem 7 empty) (8 listitem 10) (16 empty)) "\
* foo

    bar
"

((1 listitem 3) (7 listitem 7 empty) (8 listitem 10) (17 empty)) "\
* foo

     bar
"

((1 listitem 3) (7 listitem 7 empty) (8 listitem 10 code 14)) "\
* foo

      bar"

((1 listitem 3) (7 listitem 7 empty) (8 listitem 10 code 14) (18 empty)) "\
* foo

      bar
"

((1 listitem 3) (7 listitem 9 listitem 11)) "\
* foo
  - bar"

((1 listitem 3) (7 listitem 7 empty) (8 listitem 10 code 14)) "\
* foo

      bar"

((1 listitem 3) (7 listitem 7 empty) (8 listitem 10 listitem 12)) "\
* foo

  - bar"

((1 listitem 3) (7 listitem 7 empty) (8 listitem 10 listitem 13)) "\
* foo

   - bar"

((1 listitem 3) (7 listitem 7 empty) (8 listitem 10 listitem 12)
 (16 listitem 16 listitem 16 empty) (17 listitem 19 listitem 21)
 (25 empty)) "\
* foo

  - bar

  - bar
"

((1 listitem 3) (7 listitem 7 empty) (8 listitem 10 listitem 12)
 (16 listitem 18 listitem 20) (24 listitem 24 listitem 24 empty)
 (25 listitem 27 listitem 29 code 33) (38 listitem 38 listitem 38 empty)
 (39 listitem 41 listitem 43) (47 listitem 47 empty) (48 listitem 50)
 (55 empty)) "\
* foo

  - bar
    baz

        blah

  - meh

* fooo
"

((1 listitem 3) (7 empty) (8 text) (13 empty) (14 code 18) (22 empty)
 (23 text)) "\
* foo

bleh

    bar

baz"

((1 text) (5 text) (10 empty)) "\
foo
 bar
"

((1 text) (5 text) (12 empty)) "\
foo
   bar
"

((1 text) (5 text) (13 empty)) "\
foo
    bar
"

((1 blockquote 3) (7 blockquote 9) (13 empty)) "\
> foo
  bar
"

;; It should be relatively easy to make things like this work too: when
;; we get *any* 'text line after a blockquote/listitem, make it be one
;; too.  (The only complication is that we need to carry over the
;; `indents+types' information for the following lines.)  But leave it
;; out so it's easy to avoid "lazy markdown" which should really not
;; exist IMO.
;;   ((1 blockquote 3) (7 blockquote 9) (11 empty)) "\
;;   > foo
;;   bar
;;   "

((1 blockquote 3) (7 empty) (8 text) (14 empty)) "\
> foo

  bar
"

((1 blockquote 3) (7 empty) (8 code 12) (16 empty)) "\
> foo

    bar
"

((1 text) (5 empty) (6 listitem 10) (14 listitem 14 empty)
 (15 listitem 19 listitem 21) (25 empty)) "\
foo

  * bar

    - baz
"

((1 text) (5 empty) (6 listitem 8)
 (12 listitem 14 listitem 16)
 (20 listitem 22 listitem 24 listitem 26)
 (30 listitem 32 listitem 34 listitem 36)
 (40 empty) (41 blockquote 43)
 (47 blockquote 49 blockquote 51)
 (55 blockquote 57 blockquote 59 blockquote 61)
 (65 blockquote 65 empty) (66 blockquote 68)
 (72 blockquote 74 listitem 76)
 (81 blockquote 83 listitem 85 blockquote 87)
 (92 blockquote 94 listitem 96 listitem 98 blockquote 100 blockquote 102)
 (107 empty)) "\
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

((1 text) (10 text)) "\
*foo bar
*baz"

((1 text) (5 listitem 6 empty)) "\
foo
*"

((1 text) (5 blockquote 6 empty)) "\
foo
>"

((1 listitem 3) (7 listitem 9 empty)) "\
* foo
* "

:inlines

(1) "\
foo"

(1 (0 1 . 2) (1 2 . 5) (0 5 . 6)) "\
*foo*"

(1 (0 1 . 3) (2 3 . 6) (0 6 . 8)) "\
**foo**"

(1 (0 1 . 4) (3 4 . 7) (0 7 . 10)) "\
***foo***"

(1 (0 1 . 2) (1 2 . 5) (0 5 . 6)) "\
*foo* "

(1 (0 2 . 3) (1 3 . 6) (0 6 . 7)) "\
 *foo*"

(1 (0 2 . 3) (1 3 . 6) (0 6 . 7)) "\
 *foo* "

(1 (0 1 . 2) (1 2 . 5) (0 5 . 6)) "\
_foo_"

(1 (0 1 . 2) (1 2 . 5) (0 5 . 6)) "\
_foo_ "

(1 (0 2 . 3) (1 3 . 6) (0 6 . 7)) "\
 _foo_"

(1 (0 2 . 3) (1 3 . 6) (0 6 . 7)) "\
 _foo_ "

(1 5 6 (0 6 . 7) (1 7 . 10) (0 10 . 11)) "\
foo

*foo*"

(1 5 6 (0 6 . 7) (1 7 . 10) (0 10 . 11) 12 13 17) "\
foo

*bar*

baz
"

(1 (0 1 . 2) (1 2 . 9) 6 (0 9 . 10)) "\
*foo
bar*"

(1 6 7) "\
*foo

bar*"

(1 (0 1 . 3) (2 3 . 6) (0 6 . 10) (2 10 . 13) (0 13 . 15)) "\
**foo****bar**"

(1 (0 1 . 3) (2 3 . 6) (0 6 . 8)  (0 9 . 11) (2 11 . 14) (0 14 . 16)) "\
**foo*****bar**"

;; >> 6.1 Backslash escapes

;; #228
(1) "\
\\!\\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\\\]\\^\\_\\`\\{\\|\\}\\~"

;; #229 (not really relevant)
(1) "\
\\	\\A\\a\\ \\3\\φ\\«"

;; #230
(1 19 36 56 68 83 97 113 144) "\
\\*not emphasized*
\\<br/> not a tag
\\[not a link](/foo)
\\`not code`
1\\. not a list
\\* not a list
\\# not a header
\\[foo]: /url \"not a reference\"
"

;; #231
(1 (0 3 . 4) (1 4 . 12) (0 12 . 13)) "\
\\\\*emphasis*"

;; #232: NA

;; #233
(1 (delim 1 . 4) (code 4 . 8) (delim 8 . 11)) "\
`` \\[\\` ``"

;; #234-235: NA (should be done on inlines)

;; #236
(1 (delim 1 . 2) (autolink 2 . 28) (delim 28 . 29)) "\
<http://example.com?find=\\*>"

;; #237
(1 (html 1 . 19)) "\
<a href=\"/bar\\/)\">"

;; #238
;;!!!
;; "[foo](/bar\\* \"ti\\*tle\")"
;; <p><a href="/bar*" title="ti*tle">foo</a></p>

;; #239-240: NA

;; >> 6.3 Code spans

;; #253
(1 (delim 1 . 2) (code 2 . 5) (delim 5 . 6)) "\
`foo`"

;; #254
(1 (delim 1 . 4) (code 4 . 13) (delim 13 . 17)) "\
`` foo ` bar  ``"

;; #255
(1 (delim 1 . 3) (code 3 . 5) (delim 5 . 7)) "\
` `` `"

;; #256
(1 (delim 1 . 4) 4 (code 4 . 7) (delim 7 . 10) 8) "\
``
foo
``"

;; #257 (space collapsing isn't relevant here)
(1 (delim 1 . 2) (code 2 . 17) 12 (delim 17 . 18)) "\
`foo   bar
  baz`"

;; #258
(1 (delim 1 . 2) (code 2 . 12) (delim 12 . 13)) "\
`foo `` bar`"

;; #259
(1 (delim 1 . 2) (code 2 . 6) (delim 6 . 7)) "\
`foo\\`bar`"

;; #260
(1 (delim 5 . 6) (code 6 . 7) (delim 7 . 8)) "\
*foo`*`"

;; #261
(1 (delim 8 . 9) (code 9 . 19) (delim 19 . 20))
"[not a `link](/foo`)"

;; #262
(1 (delim 1 . 2) (code 2 . 11) (delim 11 . 12)) "\
`<a href=\"`\">`"

;; #263
(1 (html 1 . 13)) "\
<a href=\"`\">`"

;; #264
(1 (delim 1 . 2) (code 2 . 18) (delim 18 . 19)) "\
`<http://foo.bar.`baz>`"

;; #265
(1 (delim 1 . 2) (autolink 2 . 21) (delim 21 . 22)) "\
<http://foo.bar.`baz>`"

;; #266
(1) "\
```foo``"

;; >> 6.4 Emphasis and strong emphasis

;; #267
(1) "\
`foo"

;; #268
(1 (0 1 . 2) (1 2 . 9) (0 9 . 10)) "\
*foo bar*"

;; #269
(1) "\
a * foo bar*"

;; #270
(1) "\
a*\"foo\"*"

;; #271
(1) "\
* a *"

;; #272
(1 (0 4 . 5) (1 5 . 8) (0 8 . 9)) "\
foo*bar*"

;; #273
(1 (0 2 . 3) (1 3 . 4) (0 4 . 5)) "\
5*6*78"

;; #274
(1 (0 1 . 2) (1 2 . 9) (0 9 . 10)) "\
_foo bar_"

;; #275
(1) "\
_ foo bar_"

;; #276
(1) "\
a_\"foo\"_"

;; #277
(1) "\
foo_bar_"

;; #278
(1) "\
5_6_78"

;; #279
(1) "\
пристаням_стремятся_"

;; #280
(1) "\
aa_\"bb\"_cc"

;; #281
(1) "\
\"aa\"_\"bb\"_\"cc\""

;; #282
(1) "\
_foo*"

;; #283
(1) "\
*foo bar *"

;; #284
(1 10) "\
*foo bar
*"

;; #285
(1) "\
*(*foo)"

;; #286
(1 (0 1 . 2) (1 2 . 3) (0 3 . 4) (1 4 . 7) (0 7 . 8) (1 8 . 9) (0 9 . 10)) "\
*(*foo*)*"

;; #287
(1 (0 1 . 2) (1 2 . 5) (0 5 . 6)) "\
*foo*bar"

;; #288
(1) "\
_foo bar _"

;; #289
(1) "\
_(_foo)"

;; #290
(1 (0 1 . 2) (1 2 . 3) (0 3 . 4) (1 4 . 7) (0 7 . 8) (1 8 . 9) (0 9 . 10)) "\
_(_foo_)_"

;; #291
(1) "\
_foo_bar"

;; #292
(1) "\
_пристаням_стремятся"

;; #293
(1 (0 1 . 2) (1 2 . 13) (0 13 . 14)) "\
_foo_bar_baz_"

;; #294
(1 (0 1 . 3) (2 3 . 10) (0 10 . 12)) "\
**foo bar**"

;; #295
(1) "\
** foo bar**"

;; #296
(1) "\
a**\"foo\"**"

;; #297
(1 (0 4 . 6) (2 6 . 9) (0 9 . 11)) "\
foo**bar**"

;; #298
(1 (0 1 . 3) (2 3 . 10) (0 10 . 12)) "\
__foo bar__"

;; #299
(1) "\
__ foo bar__"

;; #300
(1 4) "\
__
foo bar__"

;; #301
(1) "\
a__\"foo\"__"

;; #302
(1) "\
foo__bar__"

;; #303
(1) "\
5__6__78"

;; #304
(1) "\
пристаням__стремятся__"

;; #305
(1 (0 1 . 3) (2 3 . 8) (0 8 . 10) (2 10 . 13) (0 13 . 15) (2 15 . 20)
   (0 20 . 22)) "\
__foo, __bar__, baz__"

;; #306
(1) "\
**foo bar **"

;; #307
(1) "\
**(**foo)"

;; #308
(1 (0 1 . 2) (1 2 . 3) (0 3 . 5) (3 5 . 8) (0 8 . 10) (1 10 . 11)
   (0 11 . 12)) "\
*(**foo**)*"

;; #309
(1 (0 1 . 3) (2 3 . 17) (0 17 . 18) (3 18 . 42) (0 42 . 43) (2 43 . 50)
 50 (0 50 . 51) (3 51 . 71) (0 71 . 72) (2 72 . 73) (0 73 . 75)) "\
**Gomphocarpus (*Gomphocarpus physocarpus*, syn.
*Asclepias physocarpa*)**"

;; #310
(1 (0 1 . 3) (2 3 . 8) (0 8 . 9) (3 9 . 12) (0 12 . 13) (2 13 . 18)
   (0 18 . 20))"\
**foo \"*bar*\" foo**"

;; #311
(1 (0 1 . 3) (2 3 . 6) (0 6 . 8)) "\
**foo**bar"

;; #312
(1) "\
__foo bar __"

;; #313
(1) "\
__(__foo)"

;; #314
(1 (0 1 . 2) (1 2 . 3) (0 3 . 5) (3 5 . 8) (0 8 . 10) (1 10 . 11)
   (0 11 . 12)) "\
_(__foo__)_"

;; #315
(1) "\
__foo__bar"

;; #316
(1) "\
__пристаням__стремятся"

;; #317
(1 (0 1 . 3) (2 3 . 16) (0 16 . 18)) "\
__foo__bar__baz__"

;; #318
(1 (0 1 . 2) (1 2 . 17) (0 17 . 18)) "\
*foo [bar](/url)*"

;; #319
(1 (0 1 . 2) (1 2 . 9) 6 ( 0 9 . 10)) "\
*foo
bar*"

;; #320
(1 (0 1 . 2) (1 2 . 6) (0 6 . 8) (3 8 . 11) (0 11 . 13) (1 13 . 17)
   (0 17 . 18)) "\
_foo __bar__ baz_"

;; #321
(1 (0 1 . 2) (1 2 . 6) (0 6 . 7) (1 7 . 10) (0 10 . 11) (1 11 . 15)
   (0 15 . 16)) "\
_foo _bar_ baz_"

;; #322
(1 (0 1 . 3) (1 3 . 6) (0 6 . 7) (1 7 . 11) (0 11 . 12)) "\
__foo_ bar_"

;; #323
(1 (0 1 . 2) (1 2 . 6) (0 6 . 7) (1 7 . 10) (0 10 . 12)) "\
*foo *bar**"

;; #324
(1 (0 1 . 2) (1 2 . 6) (0 6 . 8) (3 8 . 11) (0 11 . 13) (1 13 . 17)
   (0 17 . 18)) "\
*foo **bar** baz*"

;; #325
(1 (0 1 . 2) (1 2 . 5) (0 5 . 7) (1 7 . 10) (0 10 . 12) (1 12 . 15)
   (0 15 . 16)) "\
*foo**bar**baz*"

;; #326
(1 (0 1 . 4) (3 4 . 7) (0 7 . 9) (1 9 . 13) (0 13 . 14)) "\
***foo** bar*"

;; #327
(1 (0 1 . 2) (1 2 . 6) (0 6 . 8) (3 8 . 11) (0 11 . 14)) "\
*foo **bar***"

;; #328
(1 (0 1 . 2) (1 2 . 5) (0 5 . 7) (1 7 . 10) (0 10 . 11)) "\
*foo**bar***"

;; #329
(1 (0 1 . 2) (1 2 . 6) (0 6 . 8) (3 8 . 12) (0 12 . 13) (3 13 . 16) (0 16 . 17)
   (3 17 . 21) (0 21 . 23) (1 23 . 27) (0 27 . 28)) "\
*foo **bar *baz* bim** bop*"

;; #330
(1 (0 1 . 2) (1 2 . 7) (0 7 . 8) (1 8 . 11) (0 11 . 12) (1 12 . 19)
   (0 19 . 20)) "\
*foo [*bar*](/url)*"

;; #331
(1) "\
** is not an empty emphasis"

;; #332
(1) "\
**** is not an empty strong emphasis"

;; #333
(1 (0 1 . 3) (2 3 . 18) (0 18 . 20)) "\
**foo [bar](/url)**"

;; #334
(1 (0 1 . 3) (2 3 . 10) 7 (0 10 . 12)) "\
**foo
bar**"

;; #335
(1 (0 1 . 3) (2 3 . 7) (0 7 . 8) (3 8 . 11) (0 11 . 12) (2 12 . 16)
   (0 16 . 18)) "\
__foo _bar_ baz__"

;; #336
(1 (0 1 . 3) (2 3 . 7) (0 7 . 9) (2 9 . 12) (0 12 . 14) (2 14 . 18)
   (0 18 . 20)) "\
__foo __bar__ baz__"

;; #337
(1 (0 1 . 5) (2 5 . 8) (0 8 . 10) (2 10 . 14) (0 14 . 16)) "\
____foo__ bar__"

;; #338
(1 (0 1 . 3) (2 3 . 7) (0 7 . 9) (2 9 . 12) (0 12 . 16)) "\
**foo **bar****"

;; #339
(1 (0 1 . 3) (2 3 . 7) (0 7 . 8) (3 8 . 11) (0 11 . 12) (2 12 . 16)
   (0 16 . 18)) "\
**foo *bar* baz**"

;; #340
(1 (0 1 . 3) (1 3 . 6) (0 6 . 7) (1 7 . 10) (0 10 . 11)) "\
**foo*bar*baz**"

;; #341
(1 (0 1 . 4) (3 4 . 7) (0 7 . 8) (2 8 . 12) (0 12 . 14)) "\
***foo* bar**"

;; #342
(1 (0 1 . 3) (2 3 . 7) (0 7 . 8) (3 8 . 11) (0 11 . 14)) "\
**foo *bar***"

;; #343
(1 (0 1 . 3) (2 3 . 7) (0 7 . 8) (3 8 . 12) (0 12 . 14) (3 14 . 17) (0 17 . 19)
   (3 19 . 23) 20 (0 23 . 24) (2 24 . 28) (0 28 . 30)) "\
**foo *bar **baz**
bim* bop**"

;; #344
(1 (0 1 . 3) (2 3 . 8) (0 8 . 9) (3 9 . 12) (0 12 . 13) (2 13 . 20)
   (0 20 . 22)) "\
**foo [*bar*](/url)**"

;; #345
(1) "\
__ is not an empty emphasis"

;; #346
(1) "\
____ is not an empty strong emphasis"

;; #347
(1) "\
foo ***"

;; #348
(1 (0 5 . 6) (1 6 . 8) (0 8 . 9)) "\
foo *\\**"

;; #349
(1 (0 5 . 6) (1 6 . 7) (0 7 . 8)) "\
foo *_*"

;; #350
(1) "\
foo *****"

;; #351
(1 (0 5 . 7) (2 7 . 9) (0 9 . 11)) "\
foo **\\***"

;; #352
(1 (0 5 . 7) (2 7 . 8) (0 8 . 10)) "\
foo **_**"

;; #353
(1 (0 2 . 3) (1 3 . 6) (0 6 . 7)) "\
**foo*"

;; #354
(1 (0 1 . 2) (1 2 . 5) (0 5 . 6)) "\
*foo**"

;; #355
(1 (0 2 . 4) (2 4 . 7) (0 7 . 9)) "\
***foo**"

;; #356
(1 (0 4 . 5) (1 5 . 8) (0 8 . 9)) "\
****foo*"

;; #357
(1 (0 1 . 3) (2 3 . 6) (0 6 . 8)) "\
**foo***"

;; #358
(1 (0 1 . 2) (1 2 . 5) (0 5 . 6)) "\
*foo****"

;; #359
(1) "\
foo ___"

;; #360
(1 (0 5 . 6) (1 6 . 8) (0 8 . 9)) "\
foo _\\__"

;; #361
(1 (0 5 . 6) (1 6 . 7) (0 7 . 8)) "\
foo _*_"

;; #362
(1) "\
foo _____"

;; #363
(1 (0 5 . 7) (2 7 . 9) (0 9 . 11)) "\
foo __\\___"

;; #364
(1 (0 5 . 7) (2 7 . 8) (0 8 . 10)) "\
foo __*__"

;; #365
(1 (0 2 . 3) (1 3 . 6) (0 6 . 7)) "\
__foo_"

;; #366
(1 (0 1 . 2) (1 2 . 5) (0 5 . 6)) "\
_foo__"

;; #367
(1 (0 2 . 4) (2 4 . 7) (0 7 . 9)) "\
___foo__"

;; #368
(1 (0 4 . 5) (1 5 . 8) (0 8 . 9)) "\
____foo_"

;; #369
(1 (0 1 . 3) (2 3 . 6) (0 6 . 8)) "\
__foo___"

;; #370
(1 (0 1 . 2) (1 2 . 5) (0 5 . 6)) "\
_foo____"

;; #371
(1 (0 1 . 3) (2 3 . 6) (0 6 . 8)) "\
**foo**"

;; #372
(1 (0 1 . 3) (1 3 . 6) (0 6 . 8)) "\
*_foo_*"

;; #373
(1 (0 1 . 3) (2 3 . 6) (0 6 . 8)) "\
__foo__"

;; #374
(1 (0 1 . 3) (1 3 . 6) (0 6 . 8)) "\
_*foo*_"

;; #375
(1 (0 1 . 5) (2 5 . 8) (0 8 . 12)) "\
****foo****"

;; #376
(1 (0 1 . 5) (2 5 . 8) (0 8 . 12)) "\
____foo____"

;; #377
(1 (0 1 . 7) (2 7 . 10) (0 10 . 16)) "\
******foo******"

;; #378
(1 (0 1 . 4) (3 4 . 7) (0 7 . 10)) "\
***foo***"

;; #379
(1 (0 1 . 6) (3 6 . 9) (0 9 . 14)) "\
_____foo_____"

;; #380
(1 (0 1 . 2) (1 2 . 10) (0 10 . 11)) "\
*foo _bar* baz_"

;; #381
(1 (0 1 . 3) (1 3 . 6) (0 6 . 7) (1 7 . 10) (0 10 . 11)) "\
**foo*bar**"

;; #382
(1 (0 7 . 9) (2 9 . 16) (0 16 . 18)) "\
**foo **bar baz**"

;; #383
(1 (0 6 . 7) (1 7 . 14) (0 14 . 15)) "\
*foo *bar baz*"

;; #384
;;!!!
;() "\
;*[bar*](/url)"
;<p>*<a href="/url">bar*</a></p>
;
;; #385
;;!!!
;() "\
;_foo [bar_](/url)"
;;; <p>_foo <a href="/url">bar_</a></p>

;; #386
(1 (html 2 . 28)) "\
*<img src=\"foo\" title=\"*\"/>"

;; #387
(1 (html 3 . 16)) "\
**<a href=\"**\">"

;; #388
(1 (html 3 . 16)) "\
__<a href=\"__\">"

;; #389
(1 (0 1 . 2) (1 2 . 7) (delim 4 . 5) (code 5 . 6) (delim 6 . 7) (0 7 . 8)) "\
*a `*`*"

;; #390
(1 (0 1 . 2) (1 2 . 7) (delim 4 . 5) (code 5 . 6) (delim 6 . 7) (0 7 . 8)) "\
_a `_`_"

;; #391
(1 (delim 4 . 5) (autolink 5 . 25) (delim 25 . 26)) "\
**a<http://foo.bar/?q=**>"

;; #392
(1 (delim 4 . 5) (autolink 5 . 25) (delim 25 . 26)) "\
__a<http://foo.bar/?q=__>"

;; >> 6.7 Autolinks

;; #490
(1 (delim 1 . 2) (autolink 2 . 20) (delim 20 . 21)) "\
<http://foo.bar.baz>"

;; #491
(1 (delim 1 . 2) (autolink 2 . 47) (delim 47 . 48)) "\
<http://foo.bar.baz/test?q=hello&id=22&boolean>"

;; #492
(1 (delim 1 . 2) (autolink 2 . 24) (delim 24 . 25)) "\
<irc://foo.bar:2233/baz>"

;; #493
(1 (delim 1 . 2) (autolink 2 . 20) (delim 20 . 21)) "\
<MAILTO:FOO@BAR.BAZ>"

;; #494
(1) "\
<http://foo.bar/baz bim>"

;; #495
(1 (delim 1 . 2) (autolink 2 . 24) (delim 24 . 25)) "\
<http://example.com/\\[\\>"

;; #496
(1 (delim 1 . 2) (autolink 2 . 21) (delim 21 . 22)) "\
<foo@bar.example.com>"

;; #497
(1 (delim 1 . 2) (autolink 2 . 30) (delim 30 . 31)) "\
<foo+special@Bar.baz-bar0.com>"

;; #498
(1) "\
<foo\\+@bar.example.com>"

;; #499
(1) "\
<>"

;; #500
;; the code doesn't check the scheme (there are emough of them that I
;; think that it's better to leave it open)
;; (1) "\
;; <heck://bing.bong>"

;; #501
(1) "\
< http://foo.bar >"

;; #502
(1) "\
<foo.bar.baz>"

;; #503
;; same as #500
;; (1) "\
;; <localhost:5001/foo>"

;; #504
(1) "\
http://example.com"

;; #505
(1) "\
foo@bar.example.com"

;; >> 6.8 Raw HTML

;; #506
(1 (html 1 . 4) (html 4 . 9) (html 9 . 14)) "\
<a><bab><c2c>"

;; #507
(1 (html 1 . 5) (html 5 . 10)) "\
<a/><b2/>"

;; #508
(1 (html 1 . 7) (html 7 . 23) 11) "\
<a  /><b2
data=\"foo\" >"

;; #509
(1 (html 1 . 64) 37) "\
<a foo=\"bar\" bam = 'baz <em>\"</em>'
_boolean zoop:33=zoop:33 />"

;; #510
(1) "\
<__>"

;; #511
(1) "\
<a h*#ref=\"hi\">"

;; #512
(1) "\
<a href=\"hi'> <a href=hi'>"

;; #513
(1 7) "\
< a><
foo><bar/ >"

;; #514
(1) "\
<a href='bar'title=title>"

;; #515
(1 (html 1 . 5) 6 (html 6 . 13)) "\
</a>
</foo >"

;; #516
(1) "\
</a href=\"foo\">"

;; #517
(1 (comment 5 . 45) 20) "\
foo <!-- this is a
comment - with hyphen -->"

;; #518
(1) "\
foo <!-- not a comment -- two hyphens -->"

;; #519
(1 19 20) "\
foo <!--> foo -->

foo <!-- foo--->"

;; #520
(1 (html 5 . 22)) "\
foo <?php echo $a; ?>"

;; #521
(1 (html 5 . 24)) "\
foo <!ELEMENT br EMPTY>"

;; #522
(1 (html 5 . 20)) "\
foo <![CDATA[>&<]]>"

;; #523
(1 (html 1 . 18)) "\
<a href=\"&ouml;\">"

;; #524
(1 (html 1 . 14)) "\
<a href=\"\\*\">"

;; #525
(1) "\
<a href=\"\\\"\">"

:faces

((1-3)) "\
foo"

((1 markup-punctuation) (2-4 italic) (5 markup-punctuation) (6)
 (7-8 markup-punctuation) (9-11 bold) (12-13 markup-punctuation) (14)
 (15-17 markup-punctuation) (18-20 bold-italic) (21-23 markup-punctuation)
 (25-27 markup-punctuation) (28 markup-punctuation bold-italic)
 (29 inline-code bold-italic)
 (32 markup-punctuation bold-italic)
 (33-35 markup-punctuation)) "\
*foo*
**foo**
***foo***
***`foo`***"

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
 (8 header1-line header1-text)
 (9 markup-punctuation header1-line header1-text)
 (10-12 inline-code header1-line header1-text)
 (13 markup-punctuation header1-line header1-text)
 (14 header1-line header1-text)
 (15 markup-punctuation header1-line header1-text)
 (16 markup-punctuation italic header1-line header1-text)
 (17-19 inline-code italic header1-line header1-text)
 (20 markup-punctuation italic header1-line header1-text)
 (21 markup-punctuation header1-line header1-text)) "\
# *foo* `bar` _`baz`_
"

((1-2 blockquote-block) (3-6 blockquote-text)) "\
> foo
"

((1-2 blockquote-block) (3-6 blockquote-text)
 (7-8 blockquote-block) (9)
 (10-11 blockquote-block) (12-15 blockquote-text)) "\
> foo
> 〈
> bar
"

((1-2 blockquote-block) (3-6 blockquote-text)
 (7 blockquote-block) (8)
 (9-10 blockquote-block) (11-14 blockquote-text)) "\
> foo
>
> bar
"

]]]
