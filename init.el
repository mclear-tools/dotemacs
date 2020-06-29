;; init.el  -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-
;;; Commentary:
;; Base init file to load config. Use "bicycle-cycle" (<Tab> and <S-Tab>
;; in org style) to navigate through sections, and "imenu" to locate individual
;; use-package definition.

;;; Startup
;;;; Speed up startup
;; Help speed up emacs initialization
;; See https://blog.d46.us/advanced-emacs-startup/
;; and http://tvraman.github.io/emacspeak/blog/emacs-start-speed-up.html
;; and https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/

(defvar cpm--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;;;; Garbage collection
;; Adjust garbage collection thresholds during startup, and thereafter
;; see http://akrl.sdf.org
;; https://gitlab.com/koral/gcmh
;; NOTE: The system linked above generates too many GC pauses so I'm using my own mixed setup
;; https://github.com/purcell/emacs.d/blob/3b1302f2ce3ef2f69641176358a38fd88e89e664/init.el#L24

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; When idle for 15sec run the GC no matter what.
(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       (lambda ()
                         (message "Garbage Collector has run for %.06fsec"
                                  (k-time (garbage-collect))))))

;;;; Check Errors
;; Produce backtraces when errors occur
(setq debug-on-error nil)

;;;; When-let errors
;; https://github.com/alphapapa/frame-purpose.el/issues/3
;; https://github.com/alphapapa/frame-purpose.el/issues/3
(eval-and-compile
  (when (version< emacs-version "26")
    (with-no-warnings
      (defalias 'when-let* #'when-let)
      (function-put #'when-let* 'lisp-indent-function 1)
      (defalias 'if-let* #'if-let)
      (function-put #'if-let* 'lisp-indent-function 2))))

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

;;;; Clean View
;; Disable start-up screen
(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq frame-inhibit-implied-resize t)

;; UI - Disable visual cruft
(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Quick start scratch buffer
(setq initial-major-mode 'fundamental-mode)

;; echo buffer
;; Don't display any message
;; https://emacs.stackexchange.com/a/437/11934
(defun display-startup-echo-area-message ()
  (message ""))

;; And bury the scratch buffer, don't kill it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

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

;; Exec path -- Emacs won't know where to load things without this
(defconst cpm-local-bin (concat (getenv "HOME") "/bin") "Local execs.")
(defconst usr-local-bin "/usr/local/bin")
(defconst usr-local-sbin "/usr/local/sbin")
(setenv "PATH" (concat usr-local-bin ":" usr-local-sbin ":" (getenv "PATH") ":" cpm-local-bin))
(setq exec-path (append exec-path (list cpm-local-bin usr-local-sbin usr-local-bin)))


;;;; Security
;; Properly verify outgoing ssl connections.
;; See https://glyph.twistedmatrix.com/2015/11/editor-malware.html

(with-eval-after-load 'gnutls
  (setq gnutls-verify-error t)
  (setq gnutls-min-prime-bits 3072)
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem"))

;; (setq gnutls-verify-error t
;;       tls-checktrust gnutls-verify-error
;;       tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
;;                         ;; compatibility fallbacks
;;                         "gnutls-cli -p %p %h"
;;                         "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")
;;       nsm-settings-file (expand-file-name "network-security.data" cpm-cache-dir))
;; ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;;;; Byte Compile Warnings
;; Disable certain byte compiler warnings to cut down on the noise. This is a
;; personal choice and can be removed if you would like to see any and all byte
;; compiler warnings.
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

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
  (push cpm-setup-dir load-path))
;; prefer newer versions
(setq load-prefer-newer t)


;;;; Straight
;; use straight.el to install all packages
;; https://github.com/raxod502/straight.el
;; Don't check packages on startup
(setq straight-check-for-modifications nil)
;; set dir
(setq straight-base-dir cpm-local-dir)
;; use use-package
(setq straight-use-package-by-default t)
;; Check updates manually
(setq straight-vc-git-auto-fast-forward nil)

;; bootstrap straight
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

;; ;; automatic update packages every week
;; (run-at-time "10:00pm" 604800 'straight-x-pull-all)

;;;; Use-Package
;; install use package
(straight-use-package 'use-package)
;; settings
(setq use-package-always-defer t
      use-package-verbose t
      use-package-minimum-reported-time 0.01
      use-package-enable-imenu-support t)

;;;; Benchmark Init
(use-package benchmark-init
  ;; demand when using
  ;; :demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'emacs-startup-hook 'benchmark-init/deactivate))

;;;; Auto-compile
;; Automatically byte-recompile changed elisp libraries
(use-package auto-compile
  :defer 1
  :config
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
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
(require 'setup-ivy)
(require 'setup-helm)
(require 'setup-server)

;;;; Other Modules
(require 'setup-ui)
(require 'setup-modeline)
(require 'setup-theme)
(require 'setup-osx)
(require 'setup-windows)
(require 'setup-navigation)
(require 'setup-search)
(require 'setup-vc)
(require 'setup-shell)
(require 'setup-org)
(require 'setup-writing)
(require 'setup-projects)
(require 'setup-programming)
(require 'setup-pdf)
(require 'setup-calendars)
(require 'setup-completion)
(require 'setup-dashboard)
(require 'setup-posframe)
(require 'setup-testing)

;;; Config Helper Functions

;;;; Config Navigation
;; Function to navigate config files
(defun cpm/find-files-setup-config-directory ()
  "use counsel to find setup files"
  (interactive)
  (counsel-find-file cpm-setup-dir))
  ;; (helm-find-files-1 cpm-setup-dir))

;; Function to search config files
(defun cpm/search-setup-config-files ()
  "use counsel rg to search all config files"
  (interactive)
  (counsel-rg nil cpm-setup-dir))
;; (helm-do-ag cpm-setup-dir))

;; Load init file
(defun cpm/load-init-file ()
  "load the base init file"
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

;;;; Outline Navigation
;; Packages to help with navigating
;; I used to use outshine.el but it was overkill -- these packages are much smaller/simpler

(use-package outline
  :hook (prog-mode . outline-minor-mode))

(use-package bicycle
  :after outline
  :demand t
  :general
  (:keymaps 'outline-minor-mode-map :states '(normal motion)
   "<tab>" 'bicycle-cycle
   "S-<tab>" 'bicycle-cycle-global)
  (:keymaps 'outline-minor-mode-map :states '(normal motion)
   "gh" 'outline-up-heading
   "gj" 'outline-forward-same-level
   "gk" 'outline-backward-same-level
   "gl" 'outline-next-visible-heading
   "gu" 'outline-previous-visible-heading
   "M-j"   'outline-move-subtree-down
   "M-k"   'outline-move-subtree-up
   "M-h"   'outline-promote
   "M-l"   'outline-demote))

;; Make outline faces look better
(use-package outline-minor-faces
  :after outline
  :demand t
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

;; reset file-name-handler-alist
(add-hook 'emacs-startup-hook (lambda ()
                                (setq file-name-handler-alist cpm--file-name-handler-alist)))

;; Startup time
(message (format "Emacs ready in %.2f seconds with %d garbage collections."
                 (float-time
                  (time-subtract after-init-time before-init-time)) gcs-done))
(put 'narrow-to-page 'disabled nil)
