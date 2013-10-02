;;; eli-backup.el --- Backup settings
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

(cond
  ((not (stringp eli-backup-method)) nil)
  ((file-accessible-directory-p eli-backup-method) nil)
  ((file-exists-p eli-backup-method)
   (setq eli-backup-method nil)
   (warn "Inaccessible backups directory, disabling backups"))
  (t (make-directory eli-backup-method t)
     (if (file-accessible-directory-p eli-backup-method)
       (progn (set-file-modes eli-backup-method #o700)
              (message "Created backup directory: %S" eli-backup-method)
              (sit-for 2))
       (progn (setq eli-backup-method nil)
              (warn "Cannot create backups directory, disabling backups")))))

(setq-default make-backup-files (and eli-backup-method t))
(setq backup-by-copying t) ; safer this way
(setq break-hardlink-on-save nil) ; new in v23

;; Make backup files in ~/.backups/ (or whatever's in `eli-backup-method')
;; rather than scattered around all over the filesystem.
(when (stringp eli-backup-method)
  (setq backup-directory-alist (list (cons "." eli-backup-method))))

;; Disable backups for big files.
(defun eli-decide-if-backup-inhibited ()
  "Decide if backup for an opened file should be disabled.
Will decide this based on the size of the file, but will respect any
buffer-local setting.  See also `eli-max-backup-size'."
  (unless (local-variable-p 'backup-inhibited (current-buffer))
    (make-local-variable 'backup-inhibited)
    (setq backup-inhibited
          (not (or (eq eli-max-backup-size t)
                   (and (numberp eli-max-backup-size)
                        (<= buffer-saved-size eli-max-backup-size))))))
  (cond
    ((not eli-max-autosave-size) (auto-save-mode -1))
    ((numberp eli-max-autosave-size)
     (auto-save-mode (if (<= buffer-saved-size eli-max-autosave-size) 1 -1)))
    (t (auto-save-mode 1)))
  (let ((no-backup backup-inhibited)
        (no-auto (not buffer-auto-save-file-name)))
    (when (or no-backup no-auto)
      (message (concat "Note: "
                       (if no-backup "no backups" "")
                       (if (and no-backup no-auto) " and ")
                       (if no-auto "no autosaving" "")
                       " for this file.")))))
(add-hook 'find-file-hooks 'eli-decide-if-backup-inhibited)

;; Deal with autosave files too
(setq delete-auto-save-files t)
(setq auto-save-default t)
(unless (eq eli-backup-method 'safe)
  (setq auto-save-list-file-name nil
        auto-save-list-file-prefix nil))
(when (stringp eli-backup-method)
  (setq auto-save-file-name-transforms
        (list (list "\\`.*\\'" eli-backup-method t))))

;; Safe saving
(when (eq eli-backup-method 'safe)
  (setq eli-max-backup-size           t)  ; no size limit
  (setq-default make-backup-files     t   ; make backups
                version-control       t   ; different file versions
                vc-make-backup-files  t)  ; keep numbered versions
  (setq kept-old-versions             2   ; too many: keep oldest 2
        kept-new-versions             4   ;           and 4 newest
        delete-old-versions           t   ; delete olds, don't ask
        dired-kept-versions           2   ; spare 2 newest with dired's '.'
        backup-by-copying             nil ; backup by renaming, no copy
        backup-by-copying-when-linked t   ; backup by copy if links exist
        backup-by-copying-when-mismatch t ; backup by copy if different owner
        ))

;;; eli-backup.el ends here
