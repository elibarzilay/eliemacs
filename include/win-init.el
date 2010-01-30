;;; win-init.el --- Initializing Emacs windows.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;;; Commentary:
;; Setup the Emacs window when it starts - size, position, font, GUI stuff etc.
;; The window configuration part is useful when there is no .Xdefaults around
;; (NTEmacs), annoying otherwise.

;;; Code:

(defvar personal-window-configurations nil
  "*A user configurable list of window configurations.
Each element of the list holds a configuration that composed of a window
environment specification and the configuration options:
 (((Windows-Version Operating-System X-Res Y-Res) X-Size Y-Size Font-Name) ...)
- Windows-Version: windows symbol, e.g.: x, w32 (see the `window-system'
  variable)
- Operating-System: The OS used symbol used, e.g.: linux, windows-nt
  (see the `system-type' variable)
- X/Y-Res: The current screen size in pixels.
  (Get this values from the x-display-pixel-width/height functions)
- X/Y-Size: The size (in characters) of the Emacs window.  Also, `eli-size' can
  be used to override the chosen dimensions.
- Font-Name: The font to use.  Also, `eli-font' can be used to override the
  selected font.

The specification can hold the * symbol as a wildcard, and the font can be
dropped or all values (meaning don't change anything), so this:
will disable the whole thing:
  (setq personal-window-configurations '(((* * * *))))
and these two elements (in this order):
  .. ((x linux 1024 *) 81 32 \"font1\") ((x * 1024 *) 81 32) ...
will make Emacs on Linux X-Windows with a screen width of 1024 use a 81x32
screen and font1, and all other X system with the same width do the same except
for defining a font.")

(defconst window-configurations
  '(;;((* (OR linux gnu/linux) 640 400)
    ;; 81 28 "-*-courier-medium-r-normal-*-13-*-*-*-*-*-*-*")
    ;;((* (OR linux gnu/linux) 800 600)
    ;; 81 30 "-*-courier-medium-r-normal-*-15-*-*-*-*-*-*-*")
    ;;((* (OR linux gnu/linux) 1024 768)
    ;; 81 32 "-*-courier-medium-r-normal-*-19-*-*-*-*-*-*-*")
    ((x * * *))
    ;; ((w32 * 640 *)
    ;;  81 28 "-*-Courier New-normal-r-*-*-13-*-*-*-*-*-*-*")
    ;; ((w32 * 800 600)
    ;;  81 30 "-*-Courier New-normal-r-*-*-15-*-*-*-*-*-*-*")
    ;; ((w32 * 1024 768)
    ;;  81 32 "-*-Courier New-normal-r-*-*-19-*-*-*-*-*-*-*")
    ;; ((w32 * 1152 864)
    ;;  81 32 "-*-Courier New-normal-r-*-*-21-*-*-*-*-*-*-*")
    ((w32 * 1440 900)
     80 24 "Consolas 23"))
  "Default window confs, use `personal-window-configurations' to override it.")

;; This is used to search the list
(defun win-init-assoc-star (elt lst)
  "Association function that match '* with anything.
Get the value associated with ELT in LST, '* matches all."
  (let ((found nil))
    (while (and lst (not found))
      (when (win-init-equal-star elt (car (car lst)))
        (setq found (car lst)))
      (setq lst (cdr lst)))
    found))
(defun win-init-equal-star (x y)
  "Compare X and Y, non-nil if equal, or one is '*."
  (or (eq x '*) (eq y '*) (equal x y)
      (and (consp x) (eq (car x) 'OR) (memq y (cdr x)))
      (and (consp y) (eq (car y) 'OR) (memq x (cdr y)))
      (and (consp x) (consp y) (win-init-equal-star (car x) (car y))
           (win-init-equal-star (cdr x) (cdr y)))))

(defmacro win-init-merge-props (props var)
  `(progn (dolist (prop ,props) (setq ,var (assq-delete-all (car prop) ,var)))
          (setq ,var (append ,props ,var))))

(let* ((conf (and window-system
                  (cdr (win-init-assoc-star
                        (list window-system system-type
                              (x-display-pixel-width)
                              (x-display-pixel-height))
                        (append personal-window-configurations
                                window-configurations)))))
       (width  (or (car eli-size)  (car conf)))
       (height (or (cadr eli-size) (cadr conf)))
       (font   (or eli-font        (cadr (cdr conf))))
       (frame  (selected-frame))
       (props  nil))
  ;; background-mode
  (push `(background-mode . ,eli-color-style) props)
  (setq frame-background-mode eli-color-style)
  (frame-set-background-mode frame)
  ;; menubar
  (menu-bar-mode (if eli-use-menubar 1 -1))
  (push `(menu-bar-lines . ,(if eli-use-menubar 1 0)) props)
  ;; toolbar
  (tool-bar-mode (if eli-use-toolbar 1 -1))
  (push `(tool-bar-lines . ,(if eli-use-toolbar 1 0)) props)
  ;; make the toolbar as small as possible (if enabled)
  (setq tool-bar-button-relief 1)
  (setq tool-bar-button-margin 0)
  (set-face-attribute 'tool-bar nil :box nil)
  ;; scrollbar
  (set-scroll-bar-mode (if (memq eli-use-scrollbar '(left right nil))
                         eli-use-scrollbar 'left))
  ;; modeline
  (unless eli-use-modeline (setq-default mode-line-format nil))
  ;; blink cursor
  (when (and window-system (not eli-blink-cursor)) ; the default is on...
    (blink-cursor-mode -1))
  ;; window positon
  (when (and window-system eli-window-pos)
    (apply 'set-frame-position frame eli-window-pos)
    (push `(left . ,(car  eli-window-pos)) props)
    (push `(top  . ,(cadr eli-window-pos)) props))
  ;; window-size
  (when conf
    (set-frame-width  frame width)
    (set-frame-height frame height)
    (push `(width  . ,width)  props)
    (push `(height . ,height) props))
  ;; default font
  (when (and window-system font)
    (push `(font . ,font) props)
    (set-face-font 'default font))
  ;; set frame properties
  (win-init-merge-props props default-frame-alist)
  (win-init-merge-props props initial-frame-alist)
  ;; hack: we're going to do some interactions, so apply these settings now,
  ;; but make it so they're applied again after we're done loading .emacs
  (let ((frame-notice-user-settings frame-notice-user-settings)
        ;; ...but protecting this causes flicker
        ;; (frame-initial-frame frame-initial-frame)
        )
    (frame-notice-user-settings)))

;;; win-init.el ends here
