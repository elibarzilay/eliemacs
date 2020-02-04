;;; eli-marker.el --- Convenient marker keys.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(defvar eli-markers (make-hash-table :test 'equal))

(defun eli-marker-get-key (prompt)
  (let ((key (eli-get-key prompt t)))
    (and key (key-description key))))

(defun eli-current-restriction ()
  (let ((b (point-min)) (e (point-max)))
    (lambda () (widen) (narrow-to-region b (min e (point-max))))))

(defun eli-marker-set-marker ()
  (let ((key nil) (save '()) (what nil)
        (saves '(position restriction windows frames)))
    (while (not key)
      (when saves (setq save (cons (pop saves) save)))
      (setq what (replace-regexp-in-string
                  "[()]" ""
                  (replace-regexp-in-string
                   " " "+" (format "%s" (reverse save)) t t)))
      (setq key (eli-marker-get-key
                 (format "Key to save %s%s: " what
                         (if (not saves) ""
                             (format " (again to save %s)" (car saves)))))))
    (let ((data (mapcar (lambda (d)
                          (pcase d
                            ('position    (point-marker))
                            ('restriction (eli-current-restriction))
                            ('windows     (current-window-configuration))
                            ('frames      (current-frame-configuration))
                            (_ (error "internal error"))))
                        save)))
      (message "Setting marker key `%s' to this %s." key what)
      (puthash key data eli-markers))))

(defun eli-marker-copy (beg end)
  (interactive "r")
  (unless (use-region-p) (error "No region marked"))
  (let ((key (or (eli-marker-get-key "Key to save region: ")
                 (error "No key given"))))
    (message "Setting marker key `%s' to this text." key)
    (puthash key (buffer-substring beg end) eli-markers)))

(defun eli-delete-other-windows-or-restore ()
  "Like `delete-other-windows' and `widen',
or restores them if there is no restriction and no other windows."
  (interactive)
  (let ((key 'eli-delete-other-windows-or-restore))
    (if (and (zerop (window-child-count (frame-root-window)))
             (= (point-min) 1) (= (point-max) (save-restriction (point-max)))
             (gethash key eli-markers))
      (eli-marker-use key)
      (progn (puthash key
                      (list (current-window-configuration)
                            (eli-current-restriction)
                            (point-marker))
                      eli-markers)
             (widen)
             (delete-other-windows)))))

(defun eli-marker-use (key)
  (let ((data (gethash key eli-markers)) (more nil))
    (unless data (error "No `%s' marker set." key))
    (if (stringp data)
      (progn (when mark-active (delete-active-region))
             (insert data) (message "Inserted `%s'." key))
      (progn
        (dolist (d data)
          (cond ((markerp d)
                 (or (marker-buffer d)
                     (error "The `%s' marker's buffer no longer exists." key))
                 (switch-to-buffer (marker-buffer d))
                 (push-mark)
                 (goto-char d))
                ((functionp d) (funcall d)) ;TODO: sort out the `more' thing
                ((window-configuration-p d)
                 (set-window-configuration d)
                 (unless more (setq more " (restored windows)")))
                ((frame-configuration-p d)
                 (set-frame-configuration d)
                 (setq more " (restored windows/frames)"))
                (t (error "Bad data in `%s' marker: %S." key d))))
        (message "Jumped to `%s'%s." key (or more ""))))))

(defun eli-marker (&optional key)
  "Jump to a marker, possibly restoring window and/or frame configuration;
or set such a marker."
  (interactive (list (eli-marker-get-key "Key to use (again to set): ")))
  (if key (eli-marker-use key) (eli-marker-set-marker)))

;;; eli-marker.el ends here
