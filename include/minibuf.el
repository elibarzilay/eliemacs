;;; minibuf.el --- Some minibuffer improvements.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;;-----------------------------------------------------------------------------
;; General setup

(minibuffer-electric-default-mode 1)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq minibuffer-prompt-properties
          (nconc minibuffer-prompt-properties
                 '(point-entered minibuffer-avoid-prompt)))))

;;-----------------------------------------------------------------------------
;; Use `y-or-n-p', instead of the annoying full response.  Might be better to
;; somehow slap a completion around the usual thing.

(unless (eq eli-backup-method 'safe)
  (fset 'yes-or-no-p 'y-or-n-p))

;;-----------------------------------------------------------------------------
;; Use partial-completion

;; Not needed
;; (require 'complete)
;; (partial-completion-mode 1)

;; Iffy
;; ;; use in comint too
;; (defun PC-complete-as-file-name-no-popup ()
;;   (save-window-excursion (PC-complete-as-file-name)))
;; (eval-after-load "comint"
;;   '(add-hook 'comint-dynamic-complete-functions
;;              'PC-complete-as-file-name-no-popup))
;; (add-hook 'shell-mode-hook ; and in shells
;;           (lambda ()
;;             (require 'comint)
;;             (add-hook 'comint-dynamic-complete-functions
;;                       'PC-complete-as-file-name-no-popup)))

;;-----------------------------------------------------------------------------
;; Add completions for `~userid', based on `completion--embedded-envvar-table'
;; in "minibuffer.el".

(defconst eli-userid-re "\\(?:^\\|/\\)\\(~[a-zA-Z0-9_-]+\\)\\'")

(defvar eli-userid-table-cache nil)

(defun eli-userid-table (string pred action)
  (if (eq (car-safe action) 'boundaries)
      ;; Compute the boundaries of the subfield to which this
      ;; completion applies.
      (let ((suffix (cdr action)))
        (if (string-match eli-userid-re string)
            (list* 'boundaries
                   (match-beginning 1)
                   (string-match-p "/" (cdr action)))))
    (when (string-match eli-userid-re string)
      (unless (and eli-userid-table-cache
                   (eq eli-user-homedirs (car eli-userid-table-cache)))
        (setq eli-userid-table-cache
              (cons eli-user-homedirs
                    (mapcar (lambda (x) (concat "~" (car x) "/"))
                            eli-user-homedirs))))
      (let* ((beg (match-beginning 1))
             (table (cdr eli-userid-table-cache))
             (prefix (substring string 0 beg)))
        (completion-table-with-context
         prefix table (substring string beg) pred action)))))

(defalias 'read-file-name-internal
  (completion-table-in-turn 'eli-userid-table
                            'completion--embedded-envvar-table
                            'completion--file-name-table)
  "Internal subroutine for `read-file-name'.  Do not call this.")

;;-----------------------------------------------------------------------------
;; Redefine navigation keys so they place the cursor at the end and no errors

(defun eli-previous-history-element (n)
  (interactive "p")
  (no-errors-beep (previous-history-element n))
  (goto-char (point-max)))

(defun eli-next-history-element (n)
  (interactive "p")
  (no-errors-beep (next-history-element n))
  (goto-char (point-max)))

(defun eli-next-complete-history-element (n)
  (interactive "p")
  (no-errors-beep (next-complete-history-element n)))

(defun eli-previous-complete-history-element (n)
  (interactive "p")
  (no-errors-beep (previous-complete-history-element n)))

(defvar eli-last-minibuf-string ".")

(defun eli-next-history-contains (n)
  (interactive "p")
  (let ((search-str
         (cond
           ((and mark-active (not (= (point) (mark))))
            ;; mark is active and not empty - use selection
            (setq mark-active nil)
            (regexp-quote (buffer-substring (point) (mark))))
           ((not (and (eolp)
                      (memq last-command '(eli-previous-history-contains
                                           eli-next-history-contains))))
            ;; not end of line - use rest of line, but if at eoln and last
            ;; command was different, use the basic file name
            (when (eolp)
              (let ((min (save-excursion (beginning-of-line) (point))))
                (goto-char (if (re-search-backward "/\\(.\\)/[^/]*\\=" min t)
                             (1+ (point))
                             min))))
            (concat (regexp-quote
                     (buffer-substring (point)
                                       (save-excursion (end-of-line) (point))))
                    "[^/]*$")) ; hack: search only in file names
           (t eli-last-minibuf-string))))
    (setq eli-last-minibuf-string search-str)
    (no-errors-beep (next-matching-history-element search-str n))
    (goto-char (point-max))))

(defun eli-previous-history-contains (n)
  (interactive "p")
  (eli-next-history-contains (- n)))

(let ((keys '(([(up)]           eli-previous-history-element)
              ([(down)]         eli-next-history-element)
              ;; C-up/down searches for an initial string
              ([(control up)]   eli-previous-complete-history-element)
              ([(prior)]        eli-previous-complete-history-element)
              ([(control down)] eli-next-complete-history-element)
              ([(next)]         eli-next-complete-history-element)
              ;; M-up/down searches for a contained string
              ([(meta up)]      eli-previous-history-contains)
              ([(meta down)]    eli-next-history-contains)
              ;; misc
              ([(control tab)]  other-window) ; don't want file caching
              ))
      (keymaps (list minibuffer-local-map
                     minibuffer-local-completion-map
                     minibuffer-local-must-match-map
                     minibuffer-local-ns-map
                     minibuffer-local-filename-completion-map
                     minibuffer-local-filename-must-match-map
                     minibuffer-local-isearch-map
                     minibuffer-local-shell-command-map
                     read-expression-map)))
  (dolist (kmap keymaps)
    (when kmap
      (dolist (key+cmd keys)
        (apply 'define-key kmap key+cmd)))))

;;-----------------------------------------------------------------------------
;; Make some minibuffer keys electric - original idea stolen from XEmacs.

;; turn this off
(file-name-shadow-mode -1)

(defun eli-minibuffer-electric-slash ()
  (interactive)
  (save-restriction
    (narrow-to-region (field-beginning) (field-end))
    (if (and (= (point) (point-max)) (> (point) (point-min)))
      (let ((str (buffer-string)))
        (delete-region (point-min) (point-max))
        (insert
         (cond
           ;; permit `//hostname/path/to/file'
           ((equal "/" str) str)
           ;; permit `http://' but not `d://'
           ((string-match "\\([a-zA-Z][a-zA-Z]+:/\\)\\'" str)
            (match-string 0 str))
           ;; otherwise a second `/' starts a new absolute path
           ((string-match "/\\'" str) "")
           ;; make `../' go up, extend to any number of dots
           (t (catch 'done
                (while (string-match
                        "\\`\\(\\|.*/\\)\\([^/]*\\)/\\(\\.*\\)\\.\\'"
                        ;;  <---prfx---><--path---> <--dots-->
                        str)
                  (let ((prfx (match-string 1 str))
                        (path (match-string 2 str))
                        (dots (match-string 3 str)))
                    (setq str
                          (cond
                            ((equal dots "") (concat prfx path))
                            ((and (eq system-type 'windows-nt)
                                  (string-match "\\`.:\\'" path))
                             ;; don't know what this is (windows drive)
                             (throw 'done nil))
                            ((string-match "\\`\\.+\\'" path)
                             ;; more dots - combine them
                             (concat prfx path dots))
                            ((string-match "^~" path)
                             ;; expand a user home dir
                             (let ((str (expand-file-name path)))
                               (if (string-match (regexp-quote path) str)
                                 (concat prfx dots) ; not expanded, go up
                                 (concat str "/." dots)))) ; cont with expansn
                            ((string-match "\\`\\$" path)
                             ;; expand an environment variable
                             (let ((str (no-errors
                                         (substitute-in-file-name path))))
                               (if str
                                 (concat str "/." dots) ; cont with expansion
                                 (concat prfx dots)))) ; not expanded, go up
                            ((equal prfx "")
                             ;; can't go higher
                             prfx)
                            ;; normal cases - go up
                            (t (concat prfx dots)))))))
              str))
         "/"))
      (insert "/"))))
(put 'eli-minibuffer-electric-slash 'delete-selection t)

(defun eli-minibuffer-electric-key (&optional from to)
  (interactive)
  (let* ((from (or from "\\`.*/\\'"))
         (to   (or to ""))
         (from (if (eq system-type 'windows-nt)
                 (replace-regexp-in-string "/" "[/\\]" from nil t)
                 from)))
    (save-restriction
      (narrow-to-region (field-beginning) (field-end))
      (when (= (point) (point-max))
        (eli-replace-regexp from to))
      (when last-command-event (insert last-command-event)))))
(put 'eli-minibuffer-electric-key 'delete-selection t)

(defun eli-minibuffer-electric-bslash () ; convert to slashes on windows
  (interactive)
  (let ((last-command-event nil)) (eli-minibuffer-electric-key "\\\\" "/"))
  (eli-minibuffer-electric-slash))
(put 'eli-minibuffer-electric-bslash 'delete-selection t)

(defun eli-minibuffer-electric-colon () ; ".../x:" -> "x:" on windows
  (interactive) (eli-minibuffer-electric-key "\\`.*/\\([a-zA-Z]\\)\\'" "\\1"))
(put 'eli-minibuffer-electric-colon 'delete-selection t)

(defvar eli-electric-file-minibuffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-keys map
      '("/" eli-minibuffer-electric-slash)
      (and (eq system-type 'windows-nt) '("\\" eli-minibuffer-electric-bslash))
      '("~" eli-minibuffer-electric-key) ; user names
      '("$" eli-minibuffer-electric-key) ; variable at the beginning
      (and (eq system-type 'windows-nt) '(":" eli-minibuffer-electric-colon)))
    map))

;; Make it convenient to navigate between file names using sexp commands.
(defvar eli-electric-file-minibuffer-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\/ "." table)
    (when (eq system-type 'windows-nt)
      (modify-syntax-entry ?\\ "." table))
    (mapc (lambda (ch) (modify-syntax-entry ch "_" table))
          ",.? #^~':;@%!|")
    table))

(defun eli-electric-file-minibuffer-setup ()
  "Setup electric minibuffer for `find-file' etc."
  (when (eq minibuffer-completion-table 'read-file-name-internal)
    (when file-name-shadow-mode (file-name-shadow-mode -1)) ; no need for this
    (let* ((beg (minibuffer-prompt-end))
           (end (point-max))
           (cur (buffer-substring-no-properties beg end))
           (abbrev (abbreviate-file-name cur)))
      (unless (equal cur abbrev)
        (delete-region beg end)
        (insert abbrev)))
    (set-syntax-table eli-electric-file-minibuffer-syntax-table)
    ;; this is the only way I could get it to work right, using
    ;; minor-mode-alist made the bindings be in effect when going out of the
    ;; minibuffer with other-window...
    (set-keymap-parent eli-electric-file-minibuffer-mode-map
                       (current-local-map))
    (use-local-map eli-electric-file-minibuffer-mode-map)))
(add-hook 'minibuffer-setup-hook 'eli-electric-file-minibuffer-setup)

;;; minibuf.el ends here
