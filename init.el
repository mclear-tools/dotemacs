;; init.el  -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-

;;; Commentary:
;; Base init file to load config. Use "outshine-cycle-buffer" (<Tab> and <S-Tab>
;; in org style) to navigate through sections, and "imenu" to locate individual
;; use-package definition.

;;; Personal Information
;; Give emacs some personal info
(setq user-full-name "Colin McLear"
      user-mail-address "mclear@fastmail.com")

;;; Load Modules
;; Load all the setup modules

;;;; Core Modules
;; These are the "can't live without" modules
(require 'setup-libraries)
(require 'setup-keybindings)
(require 'setup-evil)
(require 'setup-evil-packages)
(require 'setup-dired)
(require 'setup-helm)
(require 'setup-helm-packages)
(require 'setup-ivy)
(require 'setup-core)

;;;; Other Modules
(require 'setup-ui)
(require 'setup-functions-macros)
(require 'setup-completion)
(require 'setup-modeline)
(require 'setup-theme)
(require 'setup-osx)
(require 'setup-navigation)
(require 'setup-windows)
(require 'setup-search)
(require 'setup-vc)
(require 'setup-shell)
(require 'setup-org)
(require 'setup-writing)
(require 'setup-projects)
(require 'setup-programming)
(require 'setup-pdf)
(require 'setup-calendars)
(require 'setup-dashboard)
(require 'setup-testing)

;;; Config Helper Functions

;; Function to navigate config files
(defun cpm/find-files-setup-config-directory ()
  (interactive)
  (helm-find-files-1 cpm-setup-dir))

;; Function to search config files
(defun cpm/search-setup-config-files ()
  (interactive)
  (helm-do-ag cpm-setup-dir))

;; Load init file
(defun cpm/load-init-file ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

;; reset file-name-handler-alist
(add-hook 'emacs-startup-hook (lambda ()
  (setq file-name-handler-alist cpm--file-name-handler-alist)))

;; Startup time
(message (format "Emacs ready in %.2f seconds with %d garbage collections."
                (float-time
                 (time-subtract after-init-time before-init-time)) gcs-done))
