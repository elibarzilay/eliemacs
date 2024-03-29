(defvar idle-clock-start-hook nil)
(defvar idle-clock-end-hook   nil)
(defvar idle-clock-frames     nil) ; for use in hooks

(defvar-local idle-clock-seconds-p nil)
(defvar-local idle-clock-prev-conf nil)

(defvar idle-clock-buffer     nil)
(defvar idle-clock-timer      nil)
(defvar idle-clock-work-timer nil)

(defun idle-clock-display-string (&rest strs)
  (dolist (str strs)
    (let* ((spaces (make-string (/ (length str) 2) ?\ ))
           (str (concat spaces str spaces))
           (scale (/ (frame-width) 1.0 (length str))))
      (insert "\n\n" (propertize str 'display `(height ,scale))))))

(defun idle-clock-draw (now) ; called in the clock buffer
  (erase-buffer)
  (idle-clock-display-string
   (format (if idle-clock-seconds-p "%02d:%02d:%02d" "%02d:%02d")
           (nth 2 now)
           (+ (nth 1 now) (if (or idle-clock-seconds-p (< (car now) 45)) 0 1))
           (nth 0 now))
   (format "%04d-%02d-%02d" (nth 5 now) (nth 4 now) (nth 3 now))
   (format "%s <%s>" user-full-name user-mail-address))
  (goto-char (point-min)))

(defun idle-clock-work ()
  (with-current-buffer
      (or idle-clock-buffer (error "idle-clock-work: no clock"))
    (let* ((now  (current-time))
           (date (decode-time now))
           (next (apply 'encode-time
                        (if idle-clock-seconds-p (1+ (car date)) 60)
                        (cdr date))))
      (idle-clock-draw date)
      (setq idle-clock-work-timer (run-at-time next nil 'idle-clock-work)))))

(defun idle-clock-done ()
  (interactive)
  (when idle-clock-work-timer (cancel-timer idle-clock-work-timer))
  (when (and idle-clock-buffer (buffer-live-p idle-clock-buffer))
    (save-excursion
      (switch-to-buffer idle-clock-buffer)
      (let ((confs idle-clock-prev-conf))
        (kill-buffer idle-clock-buffer)
        (setq idle-clock-buffer nil)
        (dolist (conf confs)
          (with-selected-frame (car conf)
            (set-scroll-bar-mode (caddr conf))
            (set-window-configuration (cadr conf))))
        (let ((idle-clock-frames (mapcar #'car confs)))
          (run-hooks 'idle-clock-end-hook))
        (setq this-command '(lambda () nil))))))

(defun idle-clock (&optional arg)
  (interactive "p")
  (unless idle-clock-buffer
    (setq idle-clock-buffer (switch-to-buffer " *clock* "))
    (setq mode-name "Clock")
    (setq major-mode 'fundamental-mode)
    (setq-local idle-clock-seconds-p (< arg 0))
    (setq-local
     idle-clock-prev-conf
     (mapcar (lambda (f)
               (with-selected-frame f
                 (let ((c (list (current-window-configuration)
                                scroll-bar-mode)))
                   (switch-to-buffer idle-clock-buffer)
                   (delete-other-windows)
                   (cons f c))))
             (filter (lambda (f)
                       (not (equal "initial_terminal" (terminal-name f))))
                     (frame-list))))
    (let ((idle-clock-frames (mapcar #'car idle-clock-prev-conf)))
      (run-hooks 'idle-clock-start-hook))
    (message nil)
    ;; keep the cursor on, to indicate window focus
    ;; (setq-local cursor-type nil)
    (setq-local mode-line-format nil)
    (setq-local truncate-lines t)
    (set-window-fringes nil 1 1)
    (set-scroll-bar-mode nil)
    (use-local-map
     (let ((m (make-sparse-keymap))) (define-key m [t] 'idle-clock-done) m))
    (setq-local minor-mode-map-alist nil)
    (setq-local minor-mode-overriding-map-alist nil)
    (setq-local emulation-mode-map-alists nil)
    (when idle-clock-work-timer (cancel-timer idle-clock-work-timer))
    (idle-clock-work)))

(add-hook 'before-make-frame-hook 'idle-clock-done)

(defun idle-clock-mode (&optional arg)
  (interactive "P")
  (cond
    ((eq arg 'disable)
     (if idle-clock-timer
       (idle-clock-mode)
       (message "Clock already canceled")))
    (idle-clock-timer
     (cancel-timer idle-clock-timer)
     (setq idle-clock-timer nil)
     (message "Clock canceled"))
    (t (let* ((mins (if (not arg) 60 (prefix-numeric-value arg)))
              (arg  (cond ((> mins 0) +1) ((< mins 0) -1) (t 0)))
              (secs (* 60 (abs mins))))
         (setq idle-clock-timer
               (run-with-idle-timer secs t 'idle-clock arg))
         (message "Clock set to start after %s idle minutes" (abs mins))))))
