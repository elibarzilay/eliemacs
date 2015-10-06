;;; eli-marker.el --- Convenient marker keys.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(defvar eli-markers (make-hash-table :test 'equal))

(defun eli-marker-get-key (prompt)
  (let ((key (eli-get-key prompt t))) (and key (key-description key))))

(defun eli-marker-set-frameconf ()
  (let ((key nil))
    (while (not key)
      (setq key (eli-marker-get-key "Key to save marker+windows+frames: ")))
    (message "Setting marker key `%s' to this position+windows+frames." key)
    (puthash key (list (current-frame-configuration)) eli-markers)
    key))

(defun eli-marker-set-winconf ()
  (let ((key (eli-marker-get-key
              "Key to save marker+windows (again to save frames): ")))
    (if (not key)
      (setq key (eli-marker-set-frameconf))
      (progn (puthash key nil eli-markers)
             (message "Setting marker key `%s' to this position+windows."
                      key)))
    (puthash key
             (nconc (gethash key eli-markers '())
                    (list (current-window-configuration)))
             eli-markers)
    key))

(defun eli-marker-set-marker ()
  (let ((key (eli-marker-get-key
              "Key to save marker (again to save windows): ")))
    (if (not key)
      (setq key (eli-marker-set-winconf))
      (progn (puthash key nil eli-markers)
             (message "Setting marker key `%s' to this position." key)))
    (puthash key
             (nconc (gethash key eli-markers '()) (list (point-marker)))
             eli-markers)))

(defun eli-marker-copy (beg end)
  (interactive "r")
  (let ((key (eli-marker-get-key "Key to save region to: ")))
    (message "Setting marker key `%s' to this text." key)
    (puthash key (buffer-substring beg end) eli-markers)))

(defun eli-marker ()
  "Jump to a marker, possibly restoring window and/or frame configuration;
or set such a marker."
  (interactive)
  (let ((key (eli-marker-get-key
              "Marker to jump to (repeat to set a marker): ")))
    (if (not key)
      (eli-marker-set-marker)
      (let ((data (gethash key eli-markers)) (more nil))
        (unless data (error "No `%s' marker set." key))
        (if (stringp data)
          (progn (insert data) (message "Inserted `%s'." key))
          (progn
            (dolist (d data)
              (cond ((markerp d)
                     (or (marker-buffer d)
                         (error "The `%s' marker's buffer no longer exists."
                                key))
                     (switch-to-buffer (marker-buffer d))
                     (push-mark)
                     (goto-char d))
                    ((window-configuration-p d)
                     (set-window-configuration d)
                     (unless more (setq more " (restored windows)")))
                    ((frame-configuration-p d)
                     (set-frame-configuration d)
                     (setq more " (restored windows/frames)"))
                    (t (error "Bad data in `%s' marker: %S." key d))))
            (message "Jumped to `%s'%s." key (or more ""))))))))

;;; eli-marker.el ends here
