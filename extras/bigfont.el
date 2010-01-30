;;; bigfont.el --- Big font for Emacs.
;;----------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;;; Customizations

(defvar bigfont-default-font "bigfont-default-font.el"
  "*The default value of `bigfont-font'.")
(defvar bigfont-font nil
  "*The filename or the bigfont object used for bigfont operations.")
(defvar bigfont-directory t
  "*Default directory used when searching for bigfonts.
Can also be a list of directories to search, nil to search only the
current directory, and t to search `load-path'.")
(defvar bigfont-scroll-delay 0.075
  "*Value to `sit-for' after every column step of scrolling a string.
During scrolling, the arrow keys can be used to speed up or slow down
this value.")
(defvar bigfont-transformations nil
  "*List of transformations to apply on generated text rectangles.
Used to override transformations specified in the `text-funcs' font
property -- so if it is set, the `text-funcs' is ignored.  See
`bigfont-read-font' for transformation specifications.")

;;; Internal variables

(defvar bigfont-fonts '()
  "An association list of fonts names and bigfont objects read so far.")


;;; Implementation section

;; Use this for efficient lookup of ASCII characters, the rest is
;; probably not too common.  Could use `make-char-table', but that makes
;; huge arrays.
(defun bigfont-make-font ()
  "Create an empty bigfont object.

Such an object is a list, the first element is an array mapping ASCII
characters (integers <128) to their images, and the rest is an
association list mapping additional characters and properties."
  (cons (make-vector 128 nil) nil))

(defun bigfont-set-char-data (font char lines)
  "Set the contents in bigfont FONT for character CHAR to given LINES.
LINES should be a list of strings of equal length.  CHAR can also be a
symbol, used to set font properties."
  (if (and (integerp char) (< char 128))
    (aset (car font) char lines)
    (let ((a (assq char (cdr font))))
      (if a
        (setcdr a lines)
        (setcdr font (cons (cons char lines) (cdr font)))))))

(defsubst bigfont-get-char-data (char &optional font)
  "Return the given data stored for the character CHAR in FONT.
If FONT is null, use `bigfont-font' which is expected to be a bigfont
object.  CHAR can also be a symbol, used to get font properties."
  (if (and (integerp char) (< char 128))
    (aref (car (or font bigfont-font)) char)
    (cdr (assq char (cdr (or font bigfont-font))))))

(defun bigfont-read-font (file)
  "Read in a FILE object and return a bigfont object.

See `bigfont-directory' for how the file is searched.

FILE is an elisp file that contains lists.  A list made of strings,
where the first element is either a string of length 1 or a character is
a specification for this character.  The strings should form a block of
text.  These text blocks are expected to have equal height, but they can
have different lengths (except each character should have some fixed
width).  Note that this is not verified.

Instead of a character, a symbol is used to assign font properties.  In
this case, the rest of the list will be the value of this property.
Currently two properties are used:

* `char-funcs' -- functions and arguments to apply to each character
  after reading.  Each entry is a list, where the first element `X' is a
  symbol for a funcion `bigfont-X' applied to the rectangle and the rest
  of the elements on the list.  These will be applied in order.

* `text-funcs' -- similar to the above, but applied to every text
  rectangle after it is created.

For example:
  (char-funcs (pad :right-bottom 1))
  (text-funcs (pad :all 1)
              (pad :all 1 :char ?#)
              (slant -0.5))
will apply (bigfont-pad R :right-bottom 1) on every character, and
\(bigfont-pad R :all 1) followed by (bigfont-pad R :all 1 :char ?#) and
\(bigfont-slant R -0.5) on every text block.

These functions can use the dynamic bindings for for `text' which is the
character being currently transformed, or the string that was just
generated.

Note that `char-funcs' transformations apply only on characters
specified later -- the property value is not actually saved, and in
addition different transformations can be used for different characters
subsets.

The result of this function is cached into `bigfont-fonts'.  Use
`bigfont-clear-font-cache' to clear."
  (or (cdr (assoc file bigfont-fonts))
      (with-temp-buffer
        (insert-file-contents
         (or (cond ((consp bigfont-directory)
                    (locate-library file nil bigfont-directory))
                   ((eq bigfont-directory t) (locate-library file))
                   ((not bigfont-directory) file))
             (error "Font file `%s' not found" file)))
        (goto-char (point-min))
        (let ((font (bigfont-make-font))
              (ts '())
              x)
          (catch 'done
            (while (setq x (condition-case nil (read (current-buffer))
                             (error (throw 'done nil))))
              (when (consp x)
                (when (stringp (car x))
                  (setcar x (string-to-char (car x))))
                (if (eq (car x) 'char-funcs)
                  (setq ts (cdr x))
                  (bigfont-set-char-data
                   font (car x)
                   (if (integerp (car x))
                     (let ((text (car x)))
                       (bigfont-apply-transformations (cdr x) ts))
                     (cdr x)))))))
          (kill-buffer (current-buffer))
          (setq bigfont-fonts (cons (cons file font) bigfont-fonts))
          font))))

(defun bigfont-clear-font-cache ()
  "Clear the bigfont cache, forcing reload of fonts."
  (interactive)
  (setq bigfont-fonts nil bigfont-font nil))

;;; Rectangle management

(defun bigfont-concat-rectangles (rects)
  "Concatenate the given RECTS, returning a rectangle.
RECTS should not be null, and all of the same non-zero height."
  (and rects (car rects)
       (let ((rect (mapcar 'list (car rects))))
         ;; accumulate lists and then use concat -- cheaper this way
         (while (setq rects (cdr rects))
           (let ((ls rect) (l (car rects)))
             (while ls
               (setcar ls (cons (car l) (car ls)))
               (setq ls (cdr ls) l (cdr l)))))
         (mapcar (lambda (x) (apply 'concat (nreverse x)))
                 rect))))

(defun bigfont-string-to-rectangle (str)
  "Converts the string STR to a rectangle.

A list of strings is the same structure used by rectangle operations so
they can be used with this.  For example:
  (insert-rectangle (bigfont-string-to-rectangle \"xyz\"))
but see `bigfont-insert-big-string' for a better way of doing this.

This function will apply transformations specified in
`bigfont-transformations' and the `text-funcs' font property."
  (when (or (stringp bigfont-font) (not bigfont-font))
    (setq bigfont-font
          (bigfont-read-font (or bigfont-font bigfont-default-font))))
  (let ((text str)) ; dynamically available to transformations
    (bigfont-apply-transformations
     (bigfont-concat-rectangles
      (mapcar 'bigfont-get-char-data (string-to-list str)))
     (or bigfont-transformations
         (bigfont-get-char-data 'text-funcs)))))

(defun bigfont-insert-big-string (str)
  "Converts the string STR to a rectangle, and inserts it.
The difference from calling `insert-rectangle' on the result of
`bigfont-string-to-rectangle' is that the cursor is left at the top of
the inserted block so subsequent calls will just add more text.  Also,
newlines are treated appropriately (note: initial ones are ignored)."
  (interactive "sString: ")
  (let ((rects (mapcar 'bigfont-string-to-rectangle
                       (split-string str "[\r\n]+")))
        rect)
    (when rects
      (while rects
        (setq rect (car rects) rects (cdr rects))
        (insert-rectangle rect)
        (unless (null rects)
          (progn (forward-line 1) (when (eobp) (newline)))))
      (if (string-match "[\r\n]+\\'" str)
        (progn (forward-line 1) (when (eobp) (newline)))
        (let ((col (current-column)))
          (forward-line (- (1- (length rect))))
          (move-to-column col))))))


;;; Rectangle transformations

(defun bigfont-apply-transformations (rect ts)
  "Apply the list of transformations TS to rectangle RECT.

See `bigfont-read-font' for specifications of transformations."
  (mapc (lambda (trans)
          (let ((f (car trans)))
            (when (and f (symbolp f))
              (setq f (intern (concat "bigfont-" (symbol-name f))))
              (when (fboundp f)
                (setq rect (apply f rect (cdr trans)))))))
        ts)
  rect)

(defun bigfont-pad (rect &rest args)
  "Pad RECT.
The extra arguments ARGS specify values using keywords:
  `:char'
    - set character used for padding
  `:left', `:right', `:top', `:bottom'
    - set pad size for a side
  `:vertical', `:horizontal', `:top-left' `:right-bottom'
    - set two sides to some value
  `:all'
    - set all pad sizes"
  (let ((left 0) (right 0) (top 0) (bottom 0) (char ? ))
    (while args
      (case (car args)
        ((:left)   (setq left   (cadr args)))
        ((:right)  (setq right  (cadr args)))
        ((:top)    (setq top    (cadr args)))
        ((:bottom) (setq bottom (cadr args)))
        ((:horizontal)   (setq left  (setq right  (cadr args))))
        ((:vertical)     (setq top   (setq bottom (cadr args))))
        ((:left-top)     (setq left  (setq top    (cadr args))))
        ((:right-bottom) (setq right (setq bottom (cadr args))))
        ((:all)
         (setq left (setq right (setq top (setq bottom (cadr args))))))
        ((:char) (setq char (cadr args))))
      (setq args (cddr args)))
    (when (stringp char) (setq char (string-to-char char)))
    (when (or (> top 0) (> bottom 0))
      (setq
       rect
       (append
        (make-list top    (make-string (length (car rect)) char))
        rect
        (make-list bottom (make-string (length (car rect)) char)))))
    (if (or (> left 0) (> right 0))
      (let ((left  (make-string left  char))
            (right (make-string right char)))
        (mapcar (lambda (l) (concat left l right)) rect))
      rect)))

(defun bigfont-slant (rect slant &optional char)
  "Slant RECT by padding on the left.
SLANT is the difference between the pad size from one line to the
previous, so positive values make a normal italics and negative values
make the result lean left.  Optional CHAR specifies the character used
for padding.  Fractions can be used as well, their values will be
floored."
  (setq char (or char ? ))
  (when (stringp char) (setq char (string-to-char char)))
  (unless (zerop slant)
    (let* ((i 0)
           (pads (mapcar (lambda (x)
                           (prog1 (make-string (abs (floor i)) char)
                             (setq i (+ i slant))))
                         rect)))
      (let ((len (length (car (last pads)))))
        (when (>= slant 0) (setq pads (nreverse pads)))
        (mapcar (lambda (r)
                  (prog1
                      (concat
                       (car pads) r
                       (make-string (- len (length (car pads))) char))
                    (setq pads (cdr pads))))
                rect)))))


;;; User functions

(defun bigfont-get-string-arg ()
  "Internal function used for interactive string values, returns either
the selected buffer region if active or read a string using the
minibuffer."
  (if mark-active
    (buffer-substring (point) (mark))
    (read-from-minibuffer "String: ")))

(defun bigfont-flash-big-string (str &optional keepkey)
  "Flash a big string STR and wait for any key.
If optional argument KEEPKEY is non-nil, the key pressed is not
discarded."
  (interactive (list (bigfont-get-string-arg) nil))
  (let ((buf   (get-buffer-create " *BigFont*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buf)
          (delete-other-windows)
          (setq mode-line-format '"-%-")
          (setq truncate-lines t)
          (erase-buffer)
          (bigfont-insert-big-string str)
          (goto-char (point-min))
          (message nil)
          (while (not (input-pending-p)) (sit-for 10))
          (unless keepkey (while (input-pending-p) (read-event))))
      (kill-buffer buf))))

(defun bigfont-scroll-big-string (str &optional loop)
  "Insert a big string STR and scroll it, looping according to LOOP.

Newline characters in STR will flush the previous part before the next
one.

If LOOP is a negative number or any other non-nil, then loop until
stopped.  When called interactively, loop if there is a positive
argument.

During scrolling, the right/left keys can be used to speed or slow the
scrolling speed, set by the value of `bigfont-scroll-delay'."
  (interactive (list (bigfont-get-string-arg)
                     current-prefix-arg))
  (let* ((loop  (if (numberp loop) (> loop 0) loop))
         (buf   (get-buffer-create " *BigFont*"))
         (rects (mapcar 'bigfont-string-to-rectangle
                        (split-string str "[\r\n]+")))
         (i     0)
         (col   (mapcar (lambda (x) nil) (car rects)))
         (spcs  (mapcar (lambda (x) ? ) (car rects)))
         (len   (+ (length (caar rects)) (frame-width))))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buf)
          (delete-other-windows)
          (setq mode-line-format '"-%-")
          (setq truncate-lines t)
          (erase-buffer)
          (insert (make-string (length (car rects)) ?\n))
          (goto-char (point-min))
          (insert (make-string (1- (frame-width)) ? ))
          (when loop (setq rects (nconc rects rects)))
          (while rects
            (end-of-line)
            (if (< i (length (caar rects)))
              (let ((r (car rects)) (c col))
                (while r
                  (setcar c (aref (car r) i))
                  (setq r (cdr r) c (cdr c)))
                (insert-rectangle col))
              (insert-rectangle spcs))
            (setq i (1+ i))
            (unless (< i len)
              (setq rects (cdr rects)
                    i     0
                    len   (+ (length (caar rects)) (frame-width))))
            (delete-rectangle (1+ (point-min)) (point-max))
            (goto-char (point-min))
            (while (input-pending-p)
              (let ((inp (read-event)))
                (cond
                  ((eq inp 'left)  (setq bigfont-scroll-delay
                                         (* bigfont-scroll-delay 1.2)))
                  ((eq inp 'right) (setq bigfont-scroll-delay
                                         (/ bigfont-scroll-delay 1.2)))
                  ((memq inp '(escape ?\e ?\q))
                   (setq i len rects nil)))))
            (sit-for bigfont-scroll-delay)))
      (kill-buffer buf))))


(provide 'bigfont)

;;; bigfont.el ends here
