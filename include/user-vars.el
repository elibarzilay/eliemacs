;;; user-vars.el --- EliEmacs specific user variables
;;-----------------------------------------------------------------------------
;; Written by Eli Barzilay: Maze is Life!   (eli@barzilay.org)

;;; Code:

;; Colors, fonts, gui stuff
(defvar eli-color-style 'dark
  "*Default color style to use: 'dark or 'light.")
(defvar eli-size nil
  "*Default size for EliEmacs - a list of two numbers.
Setting this to nil leaves either the XDefault value, or the stuff from
`window-configurations' or `personal-window-configurations'.")
(defvar eli-font nil
  "*Default font for EliEmacs.
Setting this to nil leaves either the XDefault value, or the stuff from
`window-configurations' or `personal-window-configurations'.")
(defvar eli-blink-cursor nil
  "*If non-nil, use a blinking cursor.")
(defvar eli-window-pos nil
  "*Default position of EliEmacs (nil for default).
Note that this does not include window decorations.")
(defvar eli-use-menubar (and window-system t)
  "*Specifies whether EliEmacs uses a menubar.")
(defvar eli-use-toolbar nil
  "*Specifies whether EliEmacs uses a toolbar.")
(defvar eli-use-scrollbar 'left
  "*Specifies whether EliEmacs uses a scrollbar and on what side.
Value should be 'left, 'right, t for default side or nil for no scrollbar.")
(defvar eli-use-modeline t
  "*Specifies wether EliEmacs uses a modeline.")
(defvar eli-text-indications t
  "*Some visual indications for files.
This variable control some visual indicators - for files (you can set the
default values to get them everywhere).
Set this variable to a list containing 'trailing-whitespaces, or
'cursor-stretch to stretch the cursor over underlying character (eg, tabs).
Set it to t to get everything.")

;; Backup settings
(defvar eli-backup-method "~/.backups/"
  "*The way that backups are created in EliEmacs.
Possible values are:
  t    - normal backups
  nil  - no backups
  STR  - where STR names a directory, save backups there.  STR must end with
         a slash, and if no such directory or file exists, it will be created.
         Autosave files will be placed in that directory too.
  safe - normal backups, with multiple versions, ignore `eli-max-backup-size'
         etc - lots of backups.")
(defvar eli-max-backup-size (* 2 1024 1024)
  "*The maximum file-size that will get backed up.
If t always backup, if nil never backup.")
(defvar eli-max-autosave-size (* 10 1024 1024)
  "*The maximum file-size that will get autosaved.
If t always autosave, if nil never (like turning off `auto-save-mode').")
(defvar eli-auto-revert-on-change (not (eq 'safe eli-backup-method))
  "*If this is non-nil, redefine `ask-user-about-supersession-threat' so trying
to edit a buffer will automatically revert it.
Set before loading the environment.  Also, will automatically get set if
`eli-backup-method' is 'safe and no value given to it yet.")

;;; user-vars.el ends here
