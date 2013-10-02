;;; win-init.el --- Initializing Emacs windows.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;; Setup the Emacs window (frame) -- size, position, font, GUI stuff etc.  The
;; window configuration part is useful when there is no .Xdefaults around
;; (NTEmacs), and useful to have different settings for different displays in a
;; single process (eg, one frame in vnc, one on the console).

(defvar window-configurations
  ;;((* (OR linux gnu/linux) 640 400)
  ;; 81 28 "-*-courier-medium-r-normal-*-13-*-*-*-*-*-*-*")
  ;;((* (OR linux gnu/linux) 800 600)
  ;; 81 30 "-*-courier-medium-r-normal-*-15-*-*-*-*-*-*-*")
  ;;((* (OR linux gnu/linux) 1024 768)
  ;; 81 32 "-*-courier-medium-r-normal-*-19-*-*-*-*-*-*-*")
  ;; ((x * * *))
  ;; ((w32 * 640 *)
  ;;  81 28 "-*-Courier New-normal-r-*-*-13-*-*-*-*-*-*-*")
  ;; ((w32 * 800 600)
  ;;  81 30 "-*-Courier New-normal-r-*-*-15-*-*-*-*-*-*-*")
  ;; ((w32 * 1024 768)
  ;;  81 32 "-*-Courier New-normal-r-*-*-19-*-*-*-*-*-*-*")
  ;; ((w32 * 1152 864)
  ;;  81 32 "-*-Courier New-normal-r-*-*-21-*-*-*-*-*-*-*")
  ;; ((w32 * 1440 900)
  ;;  80 24 "Consolas 23")
  nil
  "*A function to determine window configurations.

This function is called whenever a new frame is made (and on the initial
frame), and passed that frame as its single input argument.  It should return
a plist of (option: value ...) that determine the configuration of this frame.

The known options are:
- `size:' the value should be a list of width/height in characters to be used
  for the window (defaults to `eli-size')
- `w:' and `h:' can be used to set the width and height individually
- `pos:' the value should be an x/y position for the window (defaults to
  `eli-window-pos')
- `x:' and `y:' can be used to set the x and y position individually
- `fn:' the value should be a string naming a default font to use (defaults to
  `eli-font')

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

(defun win-init-apply-conf (&rest frame)
  (interactive) ; make it easy to use manually
  (let* ((frame (or (car frame) (selected-frame)))
         (conf  (and (window-system) window-configurations
                     (funcall window-configurations frame)))
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
          (case k
            ((size:) (setq w (car v) h (cadr v)))
            ((w:)    (setq w v))
            ((h:)    (setq h v))
            ((pos:)  (setq x (car v) y (cadr v)))
            ((x:)    (setq x v))
            ((y:)    (setq y v))
            ((fn:)   (setq fn v)))))
      ;; redisplay after each setting to avoid oddities
      (when fn (set-frame-font fn) (redisplay t))
      (cond ((and w h) (set-frame-size   frame w h) (redisplay t))
            (w         (set-frame-width  frame w)   (redisplay t))
            (h         (set-frame-height frame w)   (redisplay t)))
      (when (or x y)
        (modify-frame-parameters
         frame `((left . (+ ,(or x 0))) (top . (+ ,(or y 0)))))
        (redisplay t)))))

(setq after-make-frame-functions
      (cons '(lambda (f)
               ;; changing the font doesn't work immediately for some
               ;; reason
               (run-with-idle-timer 0.2 nil
                 `(lambda () (win-init-apply-conf ,f))))
            after-make-frame-functions))

;; the font might not be available right now, so use a hook
(ignore-errors (win-init-apply-conf))
(add-hook 'emacs-startup-hook 'win-init-apply-conf)

;;; win-init.el ends here
