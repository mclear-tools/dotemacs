;;; setup-settings.el --- summary -*- lexical-binding: t -*-

;; Author: Colin McLear
;; Maintainer: Colin McLear

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Sane settings

;;; Code:

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
(setq-default fill-column 80)
(setq fill-column 80)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-always-indent 'complete)
;; TAB cycle if there are only few candidates
(setq-default completion-cycle-threshold 3)

;;;;; UTF 8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment "UTF-8")

(setq-default buffer-file-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq locale-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;;;; Interface settings
;; No audible bell/alert
(setq visible-bell t)
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
  ;; auto-save every file visiting buffer
  ;; see https://emacs.stackexchange.com/q/7729/11934
  (setq-default auto-save-default t)
  (setq-default
   auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30)
   auto-save-interval 300            ; number of keystrokes between auto-saves (default: 300)
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
  ;; (add-hook 'meow-insert-exit-hook #'cpm/full-auto-save)
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
  ;; (put 'evil-ex-history 'history-length 50)
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

(use-package restart-emacs :commands restart-emacs)
(setq confirm-kill-processes nil) ; don't object when quitting

;; Follow symlinks
(setq find-file-visit-truename t)


;;;; Mouse
;; Hide mouse cursor while typing. Why?
;; .. it can overlap characters we want to see.
(setq make-pointer-invisible t)

;;;; Undo
;; Don't group undo steps. Why?
;; .. without this it groups actions into a fixed number of steps which feels unpredictable.
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

;;;; Multisession

(use-package multisession
  :straight (:type built-in)
  :defer t
  :config
  (setq multisession-directory (concat cpm-cache-dir "multisession/")))

;;; End Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-settings)
;;; setup-settings.el ends here
