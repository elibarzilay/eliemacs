;;; modeline.el --- Modeline improvements.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

(setq display-time-default-load-average nil
      display-time-load-average nil
      display-time-use-mail-icon t
      display-time-day-and-date nil
      display-time-24hr-format t
      display-time-format "%H:%M-") ; add a dash (ignores the above two)
(display-time-mode 1)

;; copied from "bindings.el" and modified
(let* ((help-echo
	;; The multi-line message doesn't work terribly well on the
	;; bottom mode line...  Better ideas?
	;; 	  "\
	;; mouse-1: select window, mouse-2: delete others, mouse-3: delete,
	;; drag-mouse-1: resize, C-mouse-2: split horizontally"
	"mouse-1: Select (drag to resize)\n\
mouse-2: Make current window occupy the whole frame\n\
mouse-3: Remove current window from display")
       (recursive-edit-help-echo "Recursive edit, type C-M-c to get out")
       (dashes (propertize "-" 'help-echo help-echo)) ;ELI
       (standard-mode-line-format
	(list
	 "%e"
	 (propertize "-" 'help-echo help-echo)
	 'mode-line-mule-info
	 ;;ELI 'mode-line-client
	 'mode-line-modified
	 ;;ELI 'mode-line-remote
	 ;;ELI 'mode-line-frame-identification
         (propertize ">" 'help-echo help-echo) ;ELI
	 'mode-line-buffer-identification
	 (propertize " " 'help-echo help-echo) ;ELI
	 'mode-line-position
	 '(vc-mode vc-mode)
	 (propertize "  " 'help-echo help-echo)
	 'mode-line-modes
	 `(which-func-mode (,dashes which-func-format)) ;ELI
	 `(global-mode-string (,dashes global-mode-string))
	 (propertize "%-" 'help-echo help-echo))) ;ELI
       (standard-mode-line-modes
	(list
	 (propertize "%[" 'help-echo recursive-edit-help-echo)
	 (propertize "(" 'help-echo help-echo)
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
	 (propertize "%n" 'help-echo "mouse-2: Remove narrowing from the current buffer"
		     'mouse-face 'mode-line-highlight
		     'local-map (make-mode-line-mouse-map
				 'mouse-2 #'mode-line-widen))
	 (propertize ")" 'help-echo help-echo)
	 (propertize "%]" 'help-echo recursive-edit-help-echo)
	 (propertize "-" 'help-echo help-echo))) ;ELI

       (standard-mode-line-position
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
          ,(propertize "-" 'help-echo help-echo)
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
  (setq-default mode-line-format standard-mode-line-format)
  (setq-default mode-line-modes standard-mode-line-modes)
  (setq-default mode-line-position standard-mode-line-position))

;;; modeline.el ends here
