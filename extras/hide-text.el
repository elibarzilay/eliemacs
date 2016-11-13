;;; hide-text.el --- toggle text hiding using overlays

(defun hide-text-get-overlay-near-point (p)
  (let ((try (lambda (p)
               (let ((l (overlays-at p)))
                 (while (and l (not (eq 'hide-text
                                        (overlay-get (car l) 'invisible))))
                   (setq l (cdr l)))
                 (car l))))
        (whitespace (string-to-list " \t\r\n")))
    (or (funcall try p)
        (and (< (point-min) p) (funcall try (1- p)))
        (let ((o nil) (p p))
          (while (and (not o) (< p (point-max))
                      (memq (char-after p) whitespace))
            (setq p (1+ p) o (funcall try p)))
          o)
        (let ((o nil) (p (1- p)))
          (while (and (not o) (<= (point-min) p)
                      (memq (char-after p) whitespace))
            (setq p (1- p) o (funcall try p)))
          o))))

(defun hide-text-find-block-with-this-indentation (p1 p2)
  (let ((c (progn (beginning-of-line) (skip-chars-forward " \t")
                  (current-column))))
    (when (zerop c)
      (error "hide-region: cannot hide text at toplevel"))
    (forward-line 1)
    (while (and (not (eobp))
                (save-excursion (skip-chars-forward " \t")
                                (<= c (current-column))))
      (forward-line 1))
    (setq p2 (point))
    (goto-char p1)
    (beginning-of-line)
    (while (and (not (bobp))
                (save-excursion (forward-line -1)
                                (skip-chars-forward " \t")
                                (<= c (current-column))))
      (forward-line -1))
    (setq p1 (point))
    (cons c (cons p1 p2))))

(defun hide-text-find-block-or-overlay ()
  (let* ((r  (region-active-p))
         (p1 (point))
         (p2 (if r (mark) p1))
         (c  nil)
         (spaces (string-to-list " \t")))
    (if (< p2 p1) (let ((p p1)) (setq p1 p2 p2 p)))
    (or (and (not r) (hide-text-get-overlay-near-point p1))
        (save-excursion
          ;; get block to hide via indentation
          (unless r (setq r (hide-text-find-block-with-this-indentation p1 p2)
                          c (car r) p1 (cadr r) p2 (cddr r)))
          ;; minimize it to the non-space part (according to the minimal
          ;; column; and possibly drop a terminating newline too)
          (while (and (< p1 p2) (memq (char-after p1) spaces)
                      (or (not c) (progn (goto-char (1+ p1))
                                         (<= (current-column) c))))
            (setq p1 (1+ p1)))
          (while (and (< p1 p2) (memq (char-before p2) spaces))
            (setq p2 (1- p2)))
          (when (and (< p1 p2) (eq ?\n (char-before p2))) (setq p2 (1- p2)))
          (when (or (= p1 p2) (and (= p1 (point-min)) (= p2 (point-max))))
            (error "hide-region: cannot find text to hide"))
          (cons p1 p2)))))

;;;###autoload
(defun hide-text ()
  "Hides all neighboring lines with current indentation or active region,
or show a previously hidden text if we're next to one."
  (interactive)
  (let ((new '(hide-text . t)))
    (unless (member new buffer-invisibility-spec)
      (add-to-invisibility-spec new)))
  (let ((o (hide-text-find-block-or-overlay)))
    (if (overlayp o) (delete-overlay o)
        (let ((o (apply 'make-overlay (car o) (cdr o) (current-buffer) t nil)))
          (overlay-put o 'invisible 'hide-text)
          ;; (overlay-put o 'intangible t) ; not needed for invisible text
          (overlay-put o 'isearch-open-invisible t)
          (overlay-put o 'face ; make it look nice when opened by isearsh
                       '(:background "gray20"))
          ;; (overlay-put o 'help-echo
          ;;              (buffer-substring-no-properties
          ;;               (overlay-start o) (overlay-end o)))
          (deactivate-mark)
          (goto-char (overlay-start o))))))

(provide 'hide-text)

;;; hide-text.el ends here
