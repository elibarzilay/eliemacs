;;; modeline.el --- Modeline improvements.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

(defvar my-help-echo
  "Click left to select&resize, right: single window, left: delete it")

(setq display-time-default-load-average nil
      display-time-load-average nil
      display-time-use-mail-icon t
      display-time-day-and-date nil
      display-time-24hr-format t
      display-time-format ; ignores the above, used to add a dash in the end
      (propertize "%H:%M" 'face display-time-mail-face)
      )
;; The above doesn't get the face through (probably because of a
;; risky-local-variable property) , so dump the load & mail completely (it'll
;; be tedious to find the bit to change just the time, or to copy the whole
;; thing); just set the time with this code:
(setq display-time-string-forms
  '((propertize ">" 'help-echo my-help-echo)
    (propertize " " 'help-echo my-help-echo 'display
                '((space :align-to (- (+ right right-fringe right-margin) 5))))
    (propertize (format-time-string "%H:%M" now)
                'help-echo (format-time-string "%A, %Y-%m-%d %H:%M" now)
                'face display-time-mail-face)))
(display-time-mode 1)

;; copied from "bindings.el" and modified
(let* ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out")
       (dashes (propertize "-" 'help-echo my-help-echo)) ;ELI
       (format
        (list
         "%e"
         ;; 'mode-line-front-space
         dashes
         'mode-line-mule-info
         'mode-line-client
         'mode-line-modified
         ;; 'mode-line-remote
         ;; 'mode-line-frame-identification
         (propertize ">" 'help-echo my-help-echo)
         'mode-line-buffer-identification
         (propertize " " 'help-echo my-help-echo)
         'mode-line-position
         '(vc-mode vc-mode)
         (propertize " " 'help-echo my-help-echo)
         'mode-line-modes
         'mode-line-misc-info
         ;; 'mode-line-end-spaces
         ))
       (modes
        (list
         (propertize "%[" 'help-echo recursive-edit-help-echo)
         (propertize "(" 'help-echo my-help-echo)
         `(:propertize ("" mode-name)
                       help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                       mouse-face mode-line-highlight
                       local-map ,mode-line-major-mode-keymap)
         '("" mode-line-process)
         `(:propertize ("" minor-mode-alist)
                       mouse-face mode-line-highlight
                       help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
                       local-map ,mode-line-minor-mode-keymap)
         (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
                     'mouse-face 'mode-line-highlight
                     'local-map (make-mode-line-mouse-map
                                 'mouse-2 #'mode-line-widen))
         (propertize ")" 'help-echo my-help-echo)
         (propertize "%]" 'help-echo recursive-edit-help-echo)
         (propertize "-" 'help-echo my-help-echo))) ;ELI
       (position
        `((line-number-mode
           ((column-number-mode
             (0 ,(propertize ;ELI
                   "%l/%c" ;ELI
                   'local-map mode-line-column-line-number-mode-map
                   'mouse-face 'mode-line-highlight
                   'help-echo "Line number and Column number\n\
mouse-1: Display Line and Column Mode Menu"))
             (0 ,(propertize ;ELI
                  "L%l" ;ELI
                  'local-map mode-line-column-line-number-mode-map
                  'mouse-face 'mode-line-highlight
                  'help-echo "Line Number\n\
mouse-1: Display Line and Column Mode Menu"))))
           ((column-number-mode
             (0 ,(propertize ;ELI
                  "C%c" ;ELI
                  'local-map mode-line-column-line-number-mode-map
                  'mouse-face 'mode-line-highlight
                  'help-echo "Column number\n\
mouse-1: Display Line and Column Mode Menu")))))
          ,(propertize "-" 'help-echo my-help-echo)
          (0 ,(propertize
                "%p"
                'local-map mode-line-column-line-number-mode-map
                'mouse-face 'mode-line-highlight
                ;; XXX needs better description
                'help-echo "Location in buffer\n\
mouse-1: Display Line and Column Mode Menu"))
          (size-indication-mode
           (0 ,(propertize ;ELI
                "/%I" ;ELI
                'local-map mode-line-column-line-number-mode-map
                'mouse-face 'mode-line-highlight
                ;; XXX needs better description
                'help-echo "Size indication mode\n\
mouse-1: Display Line and Column Mode Menu"))))))
  (setq-default mode-line-format   format)
  (setq-default mode-line-modes    modes)
  (setq-default mode-line-position position))

;;; modeline.el ends here
