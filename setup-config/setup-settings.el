;;; Settings

;;;; General Settings

;;;;; Custom File Location
;; Don't use persistent custom file (speeds up load time)
(use-package cus-edit
  :straight (:type built-in)
  :defer 1
  :config
  (setq custom-file (expand-file-name "custom.el" cpm-cache-dir))
  (when (not (file-exists-p custom-file))
    (write-file custom-file))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; :custom
;; (custom-file null-device "Don't store customizations"))
;; (custom-file (make-temp-file "emacs-custom"))

;; If using custom settings put them in a separate file
;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;;;; Private File
;; where to store private or "secret" info
(let ((private (expand-file-name "private.el" cpm-elisp-dir)))
  (if (file-exists-p private)
	  (load-file private)))

;;;;; Text settings
;; Make sure your text files end in a newline
(setq require-final-newline t)

;; Keep focus while navigating help buffers
(setq help-window-select 't)
;; big files
(setq large-file-warning-threshold 100000000)

;; Pretty symbols
(global-prettify-symbols-mode +1)
;; Show markup at point
(setq prettify-symbols-unprettify-at-point t)

;; Single space between sentences is more widespread than double
(setq-default sentence-end-double-space nil)

;; Iterate through CamelCase words
(global-subword-mode 1)

;; Allow visual lines
(global-visual-line-mode)
(setq line-move-visual t) ;; move via visual lines

;;;;; Indentation & Tabs
;; yes, both are needed!
(setq default-tab-width 4)
(setq tab-width 4)
(setq-default fill-column 78)
(setq fill-column 78)
(setq-default evil-indent-convert-tabs nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default evil-shift-round nil)
(setq-default tab-always-indent t)

;;;;; UTF 8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq-default buffer-file-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq locale-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;;;; Interface settings
;; (setq visible-bell nil) ;; The default
;; (setq ring-bell-function 'ignore)
;; Silence warnings generated by a function's being redefine by =defadvice=.
(setq ad-redefinition-action 'accept)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(setq show-paren-delay 0)
(blink-cursor-mode 0)

;;;;; Search and Replace
(use-package visual-regexp
  :commands (vr/query-replace)
  :config
  (use-package visual-regexp-steroids
    :commands (vr/select-query-replace)))


;;;;; Whitespace
;; Manage whitespace in prog modes
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;;;; Backups / Auto-Save
(use-package files
  :straight (:type built-in)
  :hook (after-init . auto-save-mode)
  :init
  ;; backups
  (let ((backup-dir (concat cpm-cache-dir "backup")))
    ;; Move backup file to `~/.emacs.d/.local/cache/backup'
    (setq backup-directory-alist `(("." . ,backup-dir)))
    ;; Makesure backup directory exist
    (when (not (file-exists-p backup-dir))
      (make-directory backup-dir t)))
  ;; auto save
  (setq auto-save-list-file-prefix
        (concat cpm-cache-dir "auto-save-list/.saves-"))
  (let ((auto-save-files-dir (concat cpm-cache-dir "auto-save-files/")))
    (setq auto-save-file-name-transforms
          `((".*" ,auto-save-files-dir t)))
    (when (not (file-exists-p auto-save-files-dir))
      (make-directory auto-save-files-dir t)))
  ;; auto-save every buffer that visits a file but not *scratch* etc
  ;; see https://emacs.stackexchange.com/q/7729/11934
  (setq-default auto-save-default t)
  (setq-default
   auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
   auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
   auto-save-visited-mode t
   delete-auto-save-files t
   create-lockfiles nil)
  :config
  (setq  make-backup-files t                ; backup of a file the first time it is saved.
         backup-by-copying t               ; don't clobber symlinks
         version-control t                 ; version numbers for backup files
         delete-old-versions t             ; delete excess backup files silently
         kept-old-versions 0               ; oldest versions to keep when a new numbered backup is made
         kept-new-versions 10              ; newest versions to keep when a new numbered backup is made
         vc-make-backup-files t            ; backup versioned files, which Emacs does not do by default
         )
  ;; save scratch buffer to auto-save dir
  (progn (set-buffer "*scratch*")
         (setq-local default-directory (concat cpm-cache-dir "auto-save-files/")))

  (defun cpm/full-auto-save ()
    (interactive)
    (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (if (and (buffer-file-name) (buffer-modified-p))
            (basic-save-buffer)))))

  (add-hook 'auto-save-hook 'cpm/full-auto-save)

  ;; Save all buffers after idle time
  (run-with-idle-timer 5 t (lambda () (cpm/full-auto-save)))
  ;; Save on exit from insert state
  ;; (add-hook 'evil-insert-state-exit-hook 'full-auto-save)
  )

(use-package backup-walker
  :commands backup-walker-start)


;;;; Save History
(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq-default savehist-file (concat cpm-cache-dir "savehist"))
  (when (not (file-exists-p savehist-file))
    (write-file savehist-file))
  (setq savehist-save-minibuffer-history t)
  (setq history-length 100)
  (put 'minibuffer-history 'history-length 50)
  (put 'evil-ex-history 'history-length 50)
  (put 'kill-ring 'history-length 25)
  (savehist-mode 1))

;;;; Desktop
(use-package desktop
  :defer
  :config
  (setq desktop-dirname             (concat cpm-cache-dir "desktops")
        desktop-base-file-name      "emacs.desktop"
        desktop-base-lock-name      "lock"
        desktop-path                (list desktop-dirname)
        desktop-save                'ask-if-new
        desktop-files-not-to-save   (concat "^$" ".*magit$")
        desktop-restore-eager 4
        desktop-load-locked-desktop t)
  (when (not (file-exists-p desktop-dirname))
    (make-directory desktop-dirname t))
  (setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                "\\)$"))
  (desktop-save-mode 0))

(defun cpm/my-desktop ()
  "Load the desktop and enable autosaving"
  (interactive)
  (let ((desktop-load-locked-desktop "ask"))
    (desktop-read)
    (desktop-save-mode 1)))

(defun cpm/save-desktop-save-buffers-kill-emacs ()
  "Save buffers and current desktop every time when quitting emacs."
  (interactive)
  (desktop-save-in-desktop-dir)
  (save-buffers-kill-emacs))

;;;; Time Stamps
(use-package time-stamp
  :straight (:type built-in)
  :commands (time-stamp cpm/time-stamp)
  :config
  (setq time-stamp-active t          ; do enable time-stamps
        time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp:
        time-stamp-format "Last modified on %Y-%02m%02d-%02H:%02M:%02S") ; date format
  (add-hook 'before-save-hook 'time-stamp) ; update when saving

  (defun cpm/time-stamp ()
    (interactive)
    (insert (concat  "Time-stamp: <"(format-time-string "%Y-%02m%02d-%02H:%02M:%02S")">"))))

;;;; Date & Time
(defun format-date (format)
  (let ((system-time-locale "en_US.UTF-8"))
    (insert (format-time-string format))))

(defun cpm/insert-date ()
  (interactive)
  (format-date "%A, %B %d %Y"))

(defun cpm/insert-date-and-time ()
  (interactive)
  (format-date "%m-%d-%Y %H:%M:%S"))

;;;; Universal Argument
(with-eval-after-load 'general
(general-define-key "M-u" 'universal-argument))

;;;; Emacs Profiling
;; might be worth checking this out more closely
;; https://github.com/raxod502/radian/blob/c4246176155873d3937ff997965279048dabbc01/emacs/radian.el#L4423-L4476
(use-package esup
  :commands esup
  :config
  (setq esup-depth 0))

;;;; Long Lines
;; Emacs has problems with reading files with long lines. This package helps with that
;; https://www.emacswiki.org/emacs?action=browse;oldid=OverLongLineMode;id=SoLong
;; Prior to 27.1, not included.
(use-package so-long
  ;; :straight (so-long :type git
  ;; :repo "https://git.savannah.gnu.org/git/so-long.git")
  :straight (:type built-in)
  :hook (after-init . global-so-long-mode)
  :config
  (global-so-long-mode))

;;;; Read Only
;;https://karthinks.com/software/batteries-included-with-emacs/
;; Use pager commands for read-only buffers
(setq view-read-only t)

;;;; Expand Region
(use-package expand-region
  :straight t
  :defer 1)

;;;; Safe Variables
(use-package files
  :straight (:type built-in)
  :config
  (setq safe-local-variable-values
        '((eval require 'org-roam-dev)
          (eval when
                (fboundp 'rainbow-mode)
                (rainbow-mode 1))
          (org-download-heading-lvl)
          (magit-todos-branch-list nil))))

;;;; Miscellaneous
(use-package remember
  :commands (remember remember-notes)
  :config
  (setq remember-data-dir (concat cpm-cache-dir "remember")
        remember-data-file (concat cpm-cache-dir "remember/notes"))
  (unless (file-directory-p remember-data-dir)
    (make-directory remember-data-dir t)))

(use-package restart-emacs :commands restart-emacs)
(setq confirm-kill-processes nil) ; don't object when quitting

(use-package autorevert
  :straight (:type built-in)
  :hook (after-init . global-auto-revert-mode)
  :init
  (setq auto-revert-interval .5)
  :config
  (setq auto-revert-verbose nil ; Shut up, please!
        revert-without-query '(".*") ;; disable revert query
        ;; Revert Dired buffers, too
        global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode))

;; (use-package request
;;   :defer t
;;   :config
;;   (setq request-storage-directory (concat cpm-cache-dir "request")))

;; Follow symlinks
(setq find-file-visit-truename t)


;;;; Mouse
;; Hide mouse cursor while typing. Why?
;; .. it can overlap characters we want to see.
(setq make-pointer-invisible t)

;;;; Undo
;; Don't group undo steps. Why?
;; .. without this is groups actions into a fixed number of steps which feels unpredictable.
(fset 'undo-auto-amalgamate 'ignore)

;; Increase undo limits. Why?
;; .. ability to go far back in history can be useful, modern systems have sufficient memory.
;; Limit of 64mb.
(setq undo-limit 6710886400)
;; Strong limit of 1.5x (96mb)
(setq undo-strong-limit 100663296)
;; Outer limit of 10x (960mb).
;; Note that the default is x100), but this seems too high.
(setq undo-outer-limit 1006632960)

;;;; Transient Popups
(use-package transient
  :defer 2
  :custom
  (transient-levels-file (concat cpm-cache-dir "transient/levels.el"))
  (transient-values-file (concat cpm-cache-dir "transient/values.el"))
  (transient-history-file (concat cpm-cache-dir "transient/history.el")))

;;;; Help Transient
;; A little more useful for calling help than just C-h (less info density)
;; see https://luca.cambiaghi.me/vanilla-emacs/readme.html#h:14F8ECDE-9E15-46F7-B903-ECE383251C48
(use-package transient
  :general
  (cpm/leader-keys
    "h" 'cpm/help-transient)
  :config
  (transient-define-prefix cpm/help-transient ()
    ["Help Commands"
     ["Mode & Bindings"
      ("m" "Mode" describe-mode)
      ("b" "Major Bindings" which-key-show-full-major-mode)
      ("B" "Minor Bindings" which-key-show-full-minor-mode-keymap)
      ("d" "Descbinds" describe-bindings)
      ]
     ["Describe"
      ("c" "Command" helpful-command)
      ("f" "Function" helpful-callable)
      ("o" "Symbol"  helpful-symbol)
      ("v" "Variable" helpful-variable)
      ("k" "Key" helpful-key)
      ]
     ["Info on"
      ("C-c" "Emacs Command" Info-goto-emacs-command-node)
      ("C-f" "Function" info-lookup-symbol)
      ("C-v" "Variable" info-lookup-symbol)
      ("C-k" "Emacs Key" Info-goto-emacs-key-command-node)
      ]
     ["Goto Source"
      ("L" "Library" find-library)
      ("F" "Function" find-function)
      ("V" "Variable" find-variable)
      ("K" "Key" find-function-on-key)
      ]
     ]
    [
     ["Internals"
      ("e" "Echo Messages" view-echo-area-messages)
      ("l" "Lossage" view-lossage)
      ]
     ["Describe"
      ("s" "Symbol" helpful-symbol)
      ("." "At Point   " helpful-at-point)
      ("C-f" "Face" describe-face)
      ("w" "Where Is" where-is)
      ("=" "Position" what-cursor-position)
      ]
     ["Info Manuals"
      ("C-i" "Info" info)
      ("C-4" "Other Window " info-other-window)
      ("C-e" "Emacs" completing-read-info-emacs-manual)
      ("C-l" "Elisp" completing-read-info-elisp-manual)
      ]
     ["Exit"
      ("q" "Quit" transient-quit-one)
      ("<escape>" "Quit" transient-quit-one)
      ]
     ]
    [
     ["External"
      ("W" "Dictionary" dictionary-lookup-definition)
      ]
     ]
    )
  )


;;; End Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-settings)
