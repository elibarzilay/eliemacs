;;; eli-logo.el --- EliEmacs logo.
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(defvar eli-logo-delay
  (if (let ((d (getenv "DISPLAY")))
        (and d (string-match "^:[0-9]+\\(.[0-9]+\\)?$" d)))
    0.1
    0.5)
  "*The delay between steps in the display of the \"Maze!\" logo.")

(defconst MAZE-SCREEN
  `(yellow/red4-bold
    ""
    "      -----=====#####>>>   Eli Barzilay: Maze is Life!   <<<#####=====----- "
    ""
    */purple4
    ""
    "       MMM              MMM       AAA       ZZZZZZZZZZZZZZ  EEEEEEEEEEEEE  !!! "
    "       MMMM            MMMM      AAAAA      ZZZZZZZZZZZZZZ  EEEEEEEEEEEEE  !!! "
    */purple3
    "      MMMMMM        MMMMMM     AAA  AA                ZZZ  EEE            !!!  "
    "      MMMMMMM      MMMMMMM    AAA   AAA              ZZZ   EEE            !!!  "
    */purple2
    "     MMM MMMMM  MMMMM MMM   AAA     AAA            ZZZZ   EEE            !!!   "
    "     MMM   MMM MMMM   MMM  AAA       AA          ZZZZ     EEE            !!!   "
    */purple1
    "    MMM     MMMM     MMM  AAAAAAAAAAAAA        ZZZZ      EEEEEEEEEE     !!!    "
    "    MMM      MM      MMM  AAAAAAAAAAAAA      ZZZZ        EEEEEEEEEE     !!!    "
    white/violet
    "...MMM..............MMM..AAA.......AAA.....ZZZZ.........EEE............!!!....."
    white/pink
    "...MMM..............MMM..AAA.......AAA...ZZZZ...........EEE............!!!....."
    white/violet
    "..MMM..............MMM..AAA.......AAA...ZZZ............EEE....................."
    */purple1
    "  MMM              MMM  AAA       AAA  ZZZ             EEE                     "
    */purple2
    " MMM              MMM  AAA       AAA  ZZZZZZZZZZZZZZ  EEEEEEEEEEEEE  !!!       "
    */purple3
    " MMM              MMM  AAA       AAA  ZZZZZZZZZZZZZZ  EEEEEEEEEEEEE  !!!       "
    */purple4
    ""
    yellow/red4-bold
    ,(concat "                     Eli's Emacs Environment  "eli-version" ")
    "     -----=====#####>>>---------------------------------<<<#####=====----- "
    ""
    bold))

(defun eli-logo-draw (&optional msg)
  (let ((buf (get-buffer-create " *Welcome!* ")))
    (switch-to-buffer buf)
    (delete-other-windows)
    (setq mode-line-format "-%-"
          show-trailing-whitespace   nil
          indicate-empty-lines       nil
          indicate-buffer-boundaries nil
          truncate-lines             t)
    (erase-buffer)
    (let ((face nil)
          (lines MAZE-SCREEN)
          (w (max (window-width) 80)))
      (while lines
        (let* ((line (pop lines)))
          (if (symbolp line)
            (setq face (simple-make-face line))
            (let ((line (concat line
                                (make-string (- w (length line) 1)
                                             (if (equal line "")
                                               ?  (aref line (1- (length line)))))
                                "\n")))
              (insert (propertize line 'face face))))))
      (when msg (insert (propertize msg 'face face))))
    (set-regexp-face " ---.*--- " 'yellow/red2-bold)
    (set-regexp-face " Eli's.*Environment.*\\] "
                     'yellow/red2-bold-underline)
    (set-regexp-face "\\([ .]\\)\\(\\([MAZE!]\\)\\3*\\)\\(\\3\\)"
                     '((1 . yellow3/yellow3) (2 . yellow/yellow)
                       (4 . white/white)))
    (set-buffer-modified-p nil)
    (message nil)
    (goto-char (point-min))))

(defun eli-logo (&optional msg)
  "Maze is Life!"
  (interactive)
  (eli-logo-draw msg)
  (win-init-apply-conf)
  (let ((buf (current-buffer)) (key nil))
    (unwind-protect
        (while (not key)
          (let ((i 15) c)
            (goto-char (point-min)) (forward-line 4)
            (while (> (setq i (1- i)) 0)
              (let ((c (buffer-substring (point) (1+ (point)))))
                (delete-char 1) (end-of-line) (insert c) (forward-line)))
            (set-buffer-modified-p nil)
            (goto-char (point-min))
            (setq key (read-event nil nil eli-logo-delay))))
      (kill-buffer buf)
      (message nil))
    key))

;;; eli-logo.el ends here
