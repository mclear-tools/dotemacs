;; init.el  -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-
;;; Commentary:
;; Base init file to load config. Use "bicycle-cycle" (<Tab> and <S-Tab>
;; in org style) to navigate through sections, and "imenu" to locate individual
;; use-package definition.

;;; Startup

;;;; Directory Variables
;;  We're going to define a number of directories that are used throughout this
;;  configuration to store different types of files.

(defconst cpm-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")

(defconst cpm-local-dir (concat cpm-emacs-dir ".local/")
  "Root directory for local Emacs files. Use this as permanent
  storage for files that are safe to share across systems (if
  this config is symlinked across several computers).")

(defconst cpm-temp-dir (concat cpm-local-dir "temp/")
  "Directory for non-essential file storage. Used by
  `cpm-etc-dir' and `cpm-cache-dir'.")

(defconst cpm-etc-dir (concat cpm-temp-dir "etc/")
  "Directory for non-volatile storage. These are not deleted or
  tampered with by emacs functions. Use this for dependencies
  like servers or config files that are stable (i.e. it should be
  unlikely that you need to delete them if something goes
  wrong).")

(defconst cpm-cache-dir (concat cpm-temp-dir "cache/")
  "Directory for volatile storage. Use this for transient files
  that are generated on the fly like caches and temporary files.
  Anything that may need to be cleared if there are problems.")

(defconst cpm-elisp-dir (concat cpm-local-dir "elisp/")
  "Where personal elisp packages and scripts are stored.")

(defconst cpm-setup-dir (concat cpm-emacs-dir "setup-config/")
  "Where the setup-init files are stored.")

;; dir for  natively compiled *.eln files
;; https://github.com/jimeh/build-emacs-for-macos#configuration
(when (boundp 'comp-eln-load-path)
  (setcar comp-eln-load-path
          (expand-file-name "cache/eln-cache/" cpm-cache-dir)))

;;;; System Variables
(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

;;;; Path Settings
;; Directory paths
(dolist (dir (list cpm-local-dir cpm-etc-dir cpm-cache-dir cpm-elisp-dir cpm-setup-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; (defconst cpm-local-bin (concat (getenv "HOME") "/bin") "Local execs.")
;; (defconst usr-local-bin "/usr/local/bin")
;; (defconst usr-local-sbin "/usr/local/sbin")
;; (setenv "PATH" (concat usr-local-bin ":" usr-local-sbin ":" (getenv "PATH") ":" cpm-local-bin))
;; (setq exec-path (append exec-path (list cpm-local-bin usr-local-sbin usr-local-bin)))

;;; Package Settings
;; I tell use-package to always defer loading packages unless explicitly told
;; otherwise. This speeds up initialization significantly as many packages are
;; only loaded later when they are explicitly used. But it can also cause
;; problems:
;; https://github.com/jwiegley/use-package#loading-packages-in-sequence. I also
;; put a lot of loading of packages off until after some number of seconds of idle. The
;; latter means package loading stays out of my way if I'm doing, e.g., a quick
;; restart-and-check of something in emacs.


;;;; Load Path
;; Add config files to load-path
(eval-and-compile
  (progn
    (push cpm-setup-dir load-path)))

(defconst cpm-local-bin (concat (getenv "HOME") "/bin") "Local execs.")
(defconst usr-local-bin "/usr/local/bin")
(defconst usr-local-sbin "/usr/local/sbin")
(setenv "PATH" (concat usr-local-bin ":" usr-local-sbin ":" (getenv "PATH") ":" cpm-local-bin))
(setq exec-path (append exec-path (list cpm-local-bin usr-local-sbin usr-local-bin)))

;;;; Straight
;;;;; Straight settings
;; use straight.el to install all packages
;; https://github.com/raxod502/straight.el
;; Don't check packages on startup
(setq straight-check-for-modifications '(check-on-save find-when-checking))
;; set branch
(setq straight-repository-branch "develop")
;; set dir
(setq straight-base-dir cpm-local-dir)
;; use use-package
(setq straight-use-package-by-default t)
;; Check updates manually
(setq straight-vc-git-auto-fast-forward nil)
;; see https://github.com/raxod502/straight.el/issues/757
(setq native-comp-deferred-compilation-deny-list nil)

;;;;; Bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;; Straight-X
;; use experimental straight commands
(require 'straight-x)
;; https://github.com/raxod502/straight.el#how-do-i-pin-package-versions-or-use-only-tagged-releases
(autoload #'straight-x-freeze-versions "straight-x")
;; package updates
;; use this workflow?
;; https://github.com/raxod502/straight.el/issues/354#issuecomment-465305063
(autoload #'straight-x-pull-all "straight-x")
;; async fetch
(autoload #'straight-x-fetch-all "straight-x")

;;;;; Straight Update Packages
;; ;; automatically update packages every week
;; (run-at-time "10:00pm" 604800 'straight-x-pull-all)

;; Update packages
(defun cpm--straight-update-packages ()
  "Wrapper for updating packages asynchronously with straight."
  (with-eval-after-load 'straight-x
    (straight-x-fetch-all)
    (switch-to-buffer "*straight*")
    (delete-other-windows)
    ;; this seems necessary to keep fetch from hanging/pausing
    (run-with-idle-timer 15 2 (lambda () (evil-next-line)))
    ;; run merge
    (run-with-idle-timer 30 2 (lambda () (straight-merge-all)))
    ;; build new packages
    (run-with-idle-timer 45 2 (lambda () (straight-check-all)))
    ;; kill session
    (run-with-idle-timer 240 nil (lambda () (kill-emacs)))))

(defun cpm/straight-update-packages-asynchronously ()
  (interactive)
  (async-shell-command-no-window "emacs -no-splash --eval '(cpm--straight-update-packages)'"))

;; Keybindings for use with updating packages interactively
(with-eval-after-load 'general
  (general-define-key :keymaps 'magit-log-mode-map
    "Q" #'exit-recursive-edit))


;;;; Use-Package
;; install use package
(straight-use-package 'use-package)
;; settings
(setq use-package-always-defer nil
      use-package-verbose t
      use-package-minimum-reported-time 0
      use-package-enable-imenu-support t
      use-package-expand-minimally nil
      use-package-always-ensure nil)

;;;; Security
;; Properly verify outgoing ssl connections.
;; See https://glyph.twistedmatrix.com/2015/11/editor-malware.html

(use-package gnutls
  :straight nil
  :defer 1
  :init
  (setq gnutls-verify-error t
        gnutls-min-prime-bits 3072))
;; (add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem"))

;; (setq gnutls-verify-error t
;;       tls-checktrust gnutls-verify-error
;;       tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
;;                         ;; compatibility fallbacks
;;                         "gnutls-cli -p %p %h"
;;                         "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")
;;       nsm-settings-file (expand-file-name "network-security.data" cpm-cache-dir))
;; ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")


;;;; Benchmark Init
(use-package benchmark-init
  ;; demand when using
  ;; :demand t
  :defer t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'emacs-startup-hook 'benchmark-init/deactivate))

;;;; Auto-compile
;; Automatically byte-recompile changed elisp libraries
(use-package auto-compile
  :defer 1
  :config
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter nil)
  (setq auto-compile-use-mode-line nil)
  (setq auto-compile-update-autoloads t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

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
(require 'setup-functions-macros)
(require 'setup-evil)
(require 'setup-settings)
(require 'setup-dired)
(require 'setup-completion)
(require 'setup-osx)

;;;; Other Modules
(require 'setup-splash)
(require 'setup-server)
(require 'setup-windows-buffers)
(require 'setup-ui)
(require 'setup-theme)
(require 'setup-childframe)
(require 'setup-navigation)
(require 'setup-search)
(require 'setup-vc)
(require 'setup-shell)
(require 'setup-org)
(require 'setup-org-extensions)
(require 'setup-writing)
(require 'setup-citation)
(require 'setup-notes)
(require 'setup-projects)
(require 'setup-programming)
(require 'setup-pdf)
(require 'setup-calendars)
(require 'setup-testing)

;; (require 'setup-ivy)
;; (require 'setup-nano)
;; (require 'setup-modeline)
;; (require 'setup-email)
;; (require 'setup-timer)
;; (require 'setup-dashboard)
;; (require 'setup-helm)


;;; Config Helper Functions

;;;; Config Navigation
;; Function to navigate config files

(defun cpm/find-files-setup-config-directory ()
  "find setup files"
  (interactive)
  (let ((default-directory cpm-setup-dir))
    (call-interactively 'find-file)))

;; Function to search in config files
(defun cpm/search-setup-config-files ()
  "async fuzzy search with ripgrep for all config files"
  (interactive)
  ;; (affe-grep cpm-setup-dir))
  (consult-ripgrep cpm-setup-dir))

;; Load init file
(defun cpm/load-init-file ()
  "load the base init file"
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

;;;; Outline Navigation
;; Packages to help with navigating
;; I used to use outshine.el but it was overkill -- these packages are much smaller/simpler

(use-package outline
  :straight (:type built-in)
  :hook (prog-mode . outline-minor-mode)
  :general
  (:keymaps 'outline-minor-mode-map :states '(normal motion)
   "<tab>" 'outline-cycle
   "S-<tab>" 'outline-cycle-buffer)
  (:keymaps 'outline-minor-mode-map :states '(normal motion)
   "gh"    'outline-previous-visible-heading
   "gj"    'outline-forward-same-level
   "gk"    'outline-backward-same-level
   "gl"    'outline-next-visible-heading
   "gu"    'outline-up-heading
   "M-j"   'outline-move-subtree-down
   "M-k"   'outline-move-subtree-up
   "M-h"   'outline-promote
   "M-l"   'outline-demote)
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              ;; prevent `outline-level' from being overwritten by `lispy'
              (setq-local outline-level #'outline-level)
              ;; setup heading regexp specific to `emacs-lisp-mode'
              (setq-local outline-regexp ";;;\\(;* \\)")
              ;; heading alist allows for subtree-like folding
              (setq-local outline-heading-alist
                          '((";;; " . 1)
                            (";;;; " . 2)
                            (";;;;; " . 3)
                            (";;;;;; " . 4)
                            (";;;;;;; " . 5))))))

;; Make outline faces look better
(use-package outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    'outline-minor-faces-add-font-lock-keywords))

;;;; Byte Compile Config Files
;; https://emacsredux.com/blog/2013/06/25/boost-performance-by-leveraging-byte-compilation/
(defun cpm/delete-byte-compiled-files ()
  "Delete byte-compiled files"
  (interactive)
  (shell-command-to-string "trash ~/.emacs.d/*.elc && trash ~/.emacs.d/setup-config/*.elc"))

(defun cpm/byte-compile-dotemacs ()
  "Byte compile all files in the .emacs.d base directory"
  (interactive)
  (cpm/delete-byte-compiled-files)
  (byte-recompile-directory user-emacs-directory 0 t))

;;;; After Startup

;; reset file-name-handler-alist
(add-hook 'emacs-startup-hook (lambda ()
                                (setq file-name-handler-alist cpm--file-name-handler-alist)
                                ;; reset garbage collection
                                (setq gc-cons-threshold 800000)
                                ;; Startup time
                                (message (format "Emacs ready in %.2f seconds with %d garbage collections."
                                                 (float-time
                                                  (time-subtract after-init-time before-init-time)) gcs-done)
                                         (put 'narrow-to-page 'disabled nil))))

