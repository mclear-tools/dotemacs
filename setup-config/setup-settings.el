;;; Settings

;;;; General Settings

;;;;; Custom File Location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;; Private File
;; where to store private or "secret" info
(let ((private (expand-file-name "private.el" cpm-elisp-dir)))
  (if (file-exists-p private)
	  (load-file private)))


;;;;; Text settings
;; Make sure your text files end in a newline
(setq require-final-newline t)

;; unique buffers
(use-package uniquify
  :straight nil
  :defer 1
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

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

; Iterate through CamelCase words
(global-subword-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default indicate-empty-lines nil)
(setq-default fill-column 85)
(global-visual-line-mode)
(setq line-move-visual t) ;; move via visual lines

;;;;; Search and Replace
(use-package visual-regexp
  :commands (vr/query-replace)
  :config
  (use-package visual-regexp-steroids
    :commands (vr/select-query-replace)))

;;;;; UTF 8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;; Interface settings
(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)
;; Silence warnings generated by a function's being redefine by =defadvice=.
(setq ad-redefinition-action 'accept)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(setq show-paren-delay 0)
(blink-cursor-mode 0)

;;;;; Whitespace
;; Manage whitespace in prog modes
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;;;; Backups / Auto-Save
(let ((backup-dir (concat cpm-cache-dir "backup")))
  ;; Move backup file to `~/.emacs.d/.local/cache/backup'
  (setq backup-directory-alist `(("." . ,backup-dir)))
  ;; Makesure backup directory exist
  (when (not (file-exists-p backup-dir))
    (make-directory backup-dir t)))


(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      kept-old-versions 10               ; oldest versions to keep when a new numbered backup is made
      kept-new-versions 10               ; newest versions to keep when a new numbered backup is made
      )
(setq vc-make-backup-files t) ;;  backup versioned files, which Emacs does not do by default

(use-package backup-walker
  :commands backup-walker-start)

(use-package auto-save
  :straight nil
  :hook (after-init . auto-save-mode)
  :init
  (setq auto-save-list-file-prefix
        (concat cpm-cache-dir "auto-save-list/.saves-"))
  (let ((auto-save-files-dir (concat cpm-cache-dir "auto-save-files/")))
    (setq auto-save-file-name-transforms
          `((".*" ,auto-save-files-dir t)))
    (when (not (file-exists-p auto-save-files-dir))
      (make-directory auto-save-files-dir t)))
  (setq
   auto-save-default t               ; auto-save every buffer that visits a file
   auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
   auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
   auto-save-visited-mode t
   delete-auto-save-files t
   create-lockfiles nil))

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


;;;; Date & Time
(defun cpm/insert-date ()
  (interactive)
  (format-date "%A, %B %d %Y"))

(defun cpm/insert-date-and-time ()
  (interactive)
  (format-date "%m-%d-%Y %H:%M:%S"))

;;;; Time Stamps
(defun cpm/time-stamp ()
  (interactive)
  (insert (concat  "Time-stamp: <"(format-time-string "%Y-%02m%02d-%02H:%02M:%02S")">")))

(setq time-stamp-active t          ; do enable time-stamps
      time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp:
      time-stamp-format "Last modified on %Y-%02m%02d-%02H:%02M:%02S") ; date format
(add-hook 'before-save-hook 'time-stamp) ; update when saving

(defun format-date (format)
  (let ((system-time-locale "en_US.UTF-8"))
    (insert (format-time-string format))))

;;;; Universal Argument
(general-define-key "M-u" 'universal-argument)

;;;; Helpful Information
(use-package helpful
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'helpful-mode 'motion))
  :general
  ("C-h f" #'helpful-function)
  ("C-h k" #'helpful-key)
  ("C-h v" #'helpful-variable)
  ("C-c C-." #'helpful-at-point)
  ("C-h C-l" #'find-library))

(advice-add 'describe-package-1 :after #'cpm/describe-package--add-melpa-link)

;; Add melpa link to describe package info
(defun cpm/describe-package--add-melpa-link (pkg)
  (let* ((desc (if (package-desc-p pkg)
                   pkg
                 (cadr (assq pkg package-archive-contents))))
         (name (if desc (package-desc-name desc) pkg))
         (archive (if desc (package-desc-archive desc)))
         (melpa-link (format "https://melpa.org/#/%s" name)))
    (when (equal archive "melpa")
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "Summary:" nil t)
          (forward-line 1)
          (package--print-help-section "MELPA")
          (help-insert-xref-button melpa-link 'help-url melpa-link)
          (insert "\n"))))))

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
  :straight nil
  :hook (after-init . global-so-long-mode)
  :config
  (global-so-long-mode))

;;;; Highlight Lines
;; Highlight lines. You can toggle this off
(use-package hl-line-mode
  :straight nil
  :defer 1
  :config
  (global-hl-line-mode 1))

;;;; Read Only
;;https://karthinks.com/software/batteries-included-with-emacs/
;; Use pager commands for read-only buffers
(setq view-read-only t)

;;;; Expand Region
(use-package expand-region
  :straight t
  :defer 1)

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
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-interval .5)
  (global-auto-revert-mode)
  (setq auto-revert-verbose nil ; Shut up, please!
        revert-without-query '(".*") ;; disable revert query
        ;; Revert Dired buffers, too
        global-auto-revert-non-file-buffers t))

(use-package request
  :defer t
  :config
  (setq request-storage-directory (concat cpm-cache-dir "request")))

;; Follow symlinks
(setq find-file-visit-truename t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-settings)
