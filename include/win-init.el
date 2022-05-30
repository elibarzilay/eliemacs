;;; win-init.el --- Initializing Emacs windows.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;; Setup the Emacs window (frame) -- size, position, font, GUI stuff etc.  The
;; window configuration part is useful when there is no .Xdefaults around
;; (NTEmacs), and useful to have different settings for different displays in a
;; single process (eg, one frame in vnc, one on the console).

(defvar window-configurations nil
  "*A function to determine window configurations.

This function is called whenever a new frame is made (and on the initial
frame), and passed that frame as its first argument, and `t' as its second if
this is for a newly-made frame.  It should return a plist of
(option: value ...) that determine the configuration of this frame.

The options are:
- `size:' the value should be a list of width/height in characters to be used
  for the window (defaults to `eli-size')
- `w:' and `h:' can be used to set the width and height individually
- `pos:' the value should be an x/y position for the window (defaults to
  `eli-window-pos')
- `x:' and `y:' can be used to set the x and y position individually
- `fn:' the value should be a string naming a default font to use (defaults to
  `eli-font'), or `auto', in which case calculate a font size that makes
  `w:'-specified characters fit the width (see `eli-font-for-width').

If it is bound to nil (the default) or if `window-system' returns nil, then it
is not used.")

(defmacro win-init-merge-props (props var)
  `(progn (dolist (prop ,props) (setq ,var (assq-delete-all (car prop) ,var)))
          (setq ,var (append ,props ,var))))

(let ((frame (selected-frame)) (props nil))
  ;; background-mode
  (push `(background-mode . ,eli-color-style) props)
  (setq frame-background-mode eli-color-style)
  (frame-set-background-mode frame)
  ;; menubar
  (menu-bar-mode (if eli-use-menubar 1 -1))
  (push `(menu-bar-lines . ,(if eli-use-menubar 1 0)) props)
  ;; toolbar
  (when (functionp 'tool-bar-mode)
    (setq tool-bar-position 'top)
    (tool-bar-mode (if eli-use-toolbar 1 -1)))
  (push `(tool-bar-lines . ,(if eli-use-toolbar 1 0)) props)
  ;; make the toolbar as small as possible (if enabled)
  (setq tool-bar-button-relief 1)
  (setq tool-bar-button-margin 0)
  (set-face-attribute 'tool-bar nil :box nil)
  ;; scrollbar
  (when (functionp 'set-scroll-bar-mode)
    (set-scroll-bar-mode (if (memq eli-use-scrollbar '(left right nil))
                           eli-use-scrollbar 'left)))
  ;; modeline
  (unless eli-use-modeline (setq-default mode-line-format nil))
  ;; blink cursor
  (when (and window-system (not eli-blink-cursor)) ; the default is on...
    (blink-cursor-mode -1))
  ;; set frame properties
  (win-init-merge-props props default-frame-alist)
  (win-init-merge-props props initial-frame-alist)
  ;; Note: used to also push size, location, and font here:
  ;;   (push `(left . ,(car  eli-window-pos)) props)
  ;;   (push `(top  . ,(cadr eli-window-pos)) props)
  ;;   (push `(width  . ,width)  props)
  ;;   (push `(height . ,height) props)
  ;;   (push `(font . ,font) props)
  ;; but just do that in a hook now, so it can be different for each display
  ;; hack: we're going to do some interactions, so apply these settings now,
  ;; but make it so they're applied again after we're done loading .emacs
  (let ((frame-notice-user-settings frame-notice-user-settings)
        ;; ...but protecting this causes flicker
        ;; (frame-initial-frame frame-initial-frame)
        )
    (frame-notice-user-settings)))

(defvar current-antialias nil)

(defun win-init-apply-conf (&optional frame init)
  (interactive) ; make it easy to use manually
  (let* ((frame (or frame (selected-frame)))
         (conf  (and (window-system) window-configurations
                     (funcall window-configurations frame init)))
         ;; chosen configuration
         (w (car  eli-size))
         (h (cadr eli-size))
         (x (car  eli-window-pos))
         (y (cadr eli-window-pos))
         (fn eli-font))
    (when conf
      ;; set option vars
      (let ((p conf) (k nil) (v nil))
        (while p
          (setq k (car p) v (cadr p) p (cddr p))
          (pcase k
            ('size: (setq w (car v) h (cadr v)))
            ('w:    (setq w v))
            ('h:    (setq h v))
            ('pos:  (setq x (car v) y (cadr v)))
            ('x:    (setq x v))
            ('y:    (setq y v))
            ('fn:   (setq fn v)))))
      ;; redisplay after each setting to avoid oddities
      (when fn
        (setq current-antialias
              (or (not (stringp fn))
                  (not (string-match-p "antialias=0" fn))))
        (set-frame-font
         (if (and (eq fn 'auto) w)
           (progn (set-frame-parameter frame 'desired-width w)
                  (eli-font-for-width w))
           fn))
        (redisplay t))
      (cond ((and w h) (set-frame-size   frame w h) (redisplay t))
            (w         (set-frame-width  frame w)   (redisplay t))
            (h         (set-frame-height frame w)   (redisplay t)))
      (when (or x y)
        (modify-frame-parameters
         frame `((user-position . t)
                 (left . (+ ,(or x 0))) (top . (+ ,(or y 0)))))
        (redisplay t)))))

(push (lambda (f)
        ;; changing the font doesn't work immediately for some
        ;; reason
        (run-with-idle-timer 0.2 nil (lambda () (win-init-apply-conf f t))))
      after-make-frame-functions)

;; the font might not be available right now, so use a hook
;; (ignore-errors (win-init-apply-conf))
(add-hook 'emacs-startup-hook (lambda () (win-init-apply-conf nil t)))

;;-----------------------------------------------------------------------------
;; Some font tweaking functions

(defun eli-use-larger-font (n)
  (interactive "P")
  (let* ((n (cond ((eq n '-) -10) ((not n) 10) (t n)))
         (size1 (face-attribute 'default :height))
         (size2 (+ size1 n)))
    (set-face-attribute 'default nil :height size2)
    (message "Font size changed: %S --[%s%S]--> %S"
             size1 (if (> n 0) "+" "") n size2)))

(defun eli-use-smaller-font (n)
  (interactive "P")
  (let* ((n (cond ((eq n '-) -10) ((not n) 10) (t n))))
    (eli-use-larger-font (- n))))

(defun eli-flip-antialias ()
  (interactive)
  (set-frame-font
   (format ":antialias=%S"
           (if (setq current-antialias
                     (if current-prefix-arg
                       (<= 0 (prefix-numeric-value current-prefix-arg))
                       (not current-antialias)))
             1 0))))

;;-----------------------------------------------------------------------------
;; Font size utility

(defun eli-font-for-width (w &optional font-name)
  "Find a font that will fill the whole monitor width at W characters, when
including the fringe and scroll-bar widths.  The given font-name (defaults
to \"Consolas\") is used with a \"-NN\" suffix to set its size."
  (let* ((W (nth 3 (assq 'geometry (frame-monitor-attributes))))
         (W (- W (frame-scroll-bar-width) (frame-fringe-width)))
         (font-name (or font-name "Consolas"))
         (wanted (/ W 1.0 w))
         (lo 1) (hi 100) mid font font-width)
    (while (< lo hi)
      (setq mid (/ (+ lo hi) 2))
      (setq font (format "%s-%s" font-name mid))
      (setq font-width (let ((f (make-face 'temp)))
                         (set-face-font f font)
                         (window-font-width nil f)))
      (cond ((= mid lo) (setq hi mid))
            ((< font-width wanted) (setq lo mid))
            ((> font-width wanted) (setq hi mid))
            (t (setq lo mid hi mid))))
    font))

;;; win-init.el ends here
