;; init.el  -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-
;;; Commentary:
;; Base init file to load config. Use "bicycle-cycle" (<Tab> and <S-Tab>
;; in org style) to navigate through sections, and "imenu" to locate individual
;; use-package definition.

;;; Personal Information
;; Give emacs some personal info
(setq user-full-name "Colin McLear"
      user-mail-address "mclear@fastmail.com")

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

;;;; Load Path
;; Add config files to load-path
(eval-and-compile
  (progn
    (push cpm-setup-dir load-path)))

;; Set PATH properly for emacs
(defconst cpm-local-bin (concat (getenv "HOME") "/bin") "Local execs.")
(defconst homebrew "/opt/homebrew/bin")
(defconst usr-local-bin "/usr/local/bin")
(defconst usr-local-sbin "/usr/local/sbin")
(setenv "PATH" (concat homebrew ":" usr-local-bin ":" usr-local-sbin ":" (getenv "PATH") ":" cpm-local-bin))
(setq exec-path (append exec-path (list homebrew cpm-local-bin usr-local-sbin usr-local-bin)))

;;; Package Settings
;; I use straight and use-package to manage settings.
;; I put a lot of loading of packages off until after some number of seconds of idle. The
;; latter means package loading stays out of my way if I'm doing, e.g., a quick
;; restart-and-check of something in emacs.

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
;; Tell straight.el about the profiles we are going to be using.
(setq straight-profiles
      '((nil . "default.el")
        ;; Packages which are pinned to a specific commit.
        (pinned . "pinned.el")))

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
  (with-eval-after-load 'straight
    (switch-to-buffer "*straight*")
    (erase-buffer)
    (delete-other-windows)
    (goto-char (point-max))
    (setq straight-vc-git-auto-fast-forward t)
    (straight-pull-all 1)
    (straight-check-all)
    (straight-freeze-versions)
    (straight-prune-build)
    (straight-remove-unused-repos t)
    ;; kill session
    (run-with-idle-timer 120 nil (lambda () (kill-emacs)))))

(defun cpm/straight-update-packages-asynchronously ()
  (interactive)
  (async-shell-command-no-window "/Applications/Emacs.app/Contents/MacOS/emacs --no-splash --eval '(cpm--straight-update-packages)'"))

;;;;; Straight Helper Functions
;; delete .DS_Store before prune
(advice-add 'straight-prune-build :before #'(lambda () (move-file-to-trash "/Users/roambot/.emacs.d/.local/straight/build/.DS_Store")))


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

;;;; El-Patch
;; Package for helping advise/modify features of other packages

(use-package el-patch
  :straight t
  :config
  (setq el-patch-enable-use-package-integration t))

;;;; Security
;; Properly verify outgoing ssl connections.
;; See https://glyph.twistedmatrix.com/2015/11/editor-malware.html

(use-package gnutls
  :straight nil
  :defer 1
  :init
  (setq gnutls-verify-error t
        gnutls-min-prime-bits 3072))

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

;;; Load Modules

;; Conditionally load modules
;; This allows startup with a clean emacs that still recognizes straight
;; Helpful for testing packages
;; Use (straight-use-package) command to selectively load packages
;; See https://emacs.stackexchange.com/a/34909/11934
;; For function switch see https://stackoverflow.com/a/4065412/6277148

(defun cpm--emacs-switches (switch)
  "depending on command line argument, load emacs with minimal settings & no modules; useful for running tests"
  (let ((found-switch (member switch command-line-args)))
    (setq command-line-args (delete switch command-line-args))
    found-switch))

(unless (cpm--emacs-switches "-clean")
  (message "Loading config modules")
  (require 'setup-modules))

;;;; Emacs Build Version
;; When built emacs has git-version patch
;; to include git sha1 in emacs-version string
(setq site-lisp "/Applications/Emacs.app/Contents/Resources/site-lisp/")
(when (file-exists-p (concat site-lisp "emacs-git-version.el"))
  (require 'emacs-git-version))

(defun cpm/emacs-version ()
  "Print emacs-version and put emacs-version string on the kill ring"
  (interactive)
  (let ((emacs (emacs-version)))
    (message (emacs-version))
    (kill-new emacs)))


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
  :bind (:map outline-minor-mode-map
         ("<tab>"   . outline-cycle)
         ("S-<tab>" . outline-cycle-buffer)
         ("M-j"     . outline-move-subtree-down)
         ("M-k"     . outline-move-subtree-up)
         ("M-h"     . outline-promote)
         ("M-l"     . outline-demote))
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
                                (message (format ";; ======================================================\n;; Emacs ready in %.2f seconds with %d garbage collections.\n;; ======================================================"
                                                 (float-time
                                                  (time-subtract after-init-time before-init-time)) gcs-done)
                                         (put 'narrow-to-page 'disabled nil))))

