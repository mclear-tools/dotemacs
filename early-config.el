;;; early-config.el --- User config for early init   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Colin McLear
;; Author: Colin McLear <mclear@unl.edu>
;; Keywords: tools, convenience

;;; Frame & Colors
;; ;; Set colors correctly so there is no flash at startup
(cond ((string= (shell-command-to-string "dark-mode status") "on\n")
       (push '(background-color . "#141414") initial-frame-alist)
       (setq active-theme 'dark-theme))
      (t
       (push '(background-color . "#FFFEFD") initial-frame-alist)
       (setq active-theme 'light-theme)))

(setq-default initial-frame-alist
              (append (list
                       '(internal-border-width . 12)
                       '(tool-bar-lines . 0)
                       '(vertical-scroll-bars . nil)
                       '(horizontal-scroll-bars . nil)
                       '(undecorated . nil))))

;; Resize pixel-wise to avoid gaps
(setq-default window-resize-pixelwise t)
(setq-default frame-resize-pixelwise t)

;; Don't show icon in frame
(setq-default ns-use-proxy-icon nil)

;; No modeline at startup
(setq mode-line-format nil)
(setq header-line-format nil)

;;; Package settings
;; The default package system is 'package.
;; This can be overridden in `early-config.el'.
(setq lem-package-system 'package)
(lem-package-bootstrap lem-package-system)
(setq lem-package-ensure-packages nil)

;;; Package List
;; Explicitly set packages for download/install or removal
;; Needs to be set before package initialization
;; https://www.olivertaylor.net/emacs/notes-on-package-el.html
(customize-set-variable 'package-selected-packages '(;; utility
                                                     async
                                                     dash
                                                     s
                                                     f
                                                     el-patch

                                                     ;; settings
                                                     visual-regexp
                                                     ws-butler
                                                     backup-walker
                                                     expand-region

                                                     ;; functions
                                                     crux

                                                     ;; macros
                                                     anaphora

                                                     ;; frames
                                                     ns-auto-titlebar

                                                     ;; windows
                                                     ace-window

                                                     ;; buffers
                                                     popper
                                                     revert-buffer-all

                                                     ;; fonts
                                                     all-the-icons
                                                     all-the-icons-dired
                                                     all-the-icons-completion

                                                     ;; faces
                                                     dimmer
                                                     goggles
                                                     highlight-numbers
                                                     hl-todo
                                                     outline-minor-faces
                                                     svg-tag-mode

                                                     ;; completion
                                                     cape
                                                     consult
                                                     consult-dir
                                                     corfu
                                                     embark
                                                     embark-consult
                                                     kind-icon
                                                     marginalia
                                                     orderless
                                                     vertico
                                                     yasnippet
                                                     yasnippet-snippets

                                                     ;; keybindings
                                                     which-key

                                                     ;; navigation
                                                     imenu-list
                                                     goto-last-change

                                                     ;; dired
                                                     dired-narrow
                                                     dired-quick-sort
                                                     diredfl
                                                     peep-dired
                                                     dired-ranger

                                                     ;; search
                                                     deadgrep
                                                     ripgrep
                                                     visual-regexp-steroids

                                                     ;; modal
                                                     meow

                                                     ;; vc
                                                     magit
                                                     git-commit
                                                     diff-hl
                                                     vdiff-magit

                                                     ;; tabs
                                                     tabspaces

                                                     ;; help
                                                     hydra
                                                     helpful
                                                     elisp-demos
                                                     info-colors

                                                     ;; colors
                                                     rainbow-mode

                                                     ;; modeline
                                                     hide-mode-line

                                                     ;; writing
                                                     binder
                                                     consult-flyspell
                                                     flyspell-correct
                                                     markdown-mode
                                                     markdown-toc
                                                     writeroom-mode
                                                     lorem-ipsum
                                                     palimpsest
                                                     auctex
                                                     define-word
                                                     osx-dictionary
                                                     visual-fill-column

                                                     ;; citation
                                                     citeproc
                                                     citar

                                                     ;; notes
                                                     denote
                                                     citar-denote
                                                     consult-notes

                                                     ;; programming
                                                     aggressive-indent
                                                     flymake-collection
                                                     elisp-def
                                                     embrace
                                                     highlight-indent-guides
                                                     iedit
                                                     multi-compile
                                                     package-lint
                                                     rainbow-delimiters
                                                     puni

                                                     ;; debug
                                                     bug-hunter
                                                     esup

                                                     ;; shell
                                                     exec-path-from-shell
                                                     eat

                                                     ;; eshell
                                                     pcmpl-homebrew
                                                     pcmpl-git
                                                     pcmpl-args
                                                     pcomplete-extension
                                                     esh-help
                                                     eshell-up
                                                     eshell-syntax-highlighting

                                                     ;; org extensions
                                                     htmlize
                                                     org-autolist
                                                     org-appear
                                                     org-contrib
                                                     org-download
                                                     org-modern
                                                     org-pomodoro
                                                     ox-pandoc
                                                     ox-hugo

                                                     ;; pdf
                                                     pdf-tools
                                                     org-noter

                                                     ;; elfeed
                                                     elfeed
                                                     elfeed-tube

                                                     ;; macos
                                                     reveal-in-osx-finder
                                                     grab-mac-link
                                                     osx-lib

                                                     ;; mail
                                                     mu4e-views
                                                     org-msg))

;; Auto install the required packages
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-packages.el
;; http://toumorokoshi.github.io/emacs-from-scratch-part-2-package-management.html
;; https://github.com/kaushalmodi/.emacs.d
(defvar lem-missing-packages '()
  "List populated at startup containing packages needing installation.")

(defun lem-check-and-install-packages ()
  "Check if packages are installed.
If missing, install packages."
  ;; Check packages
  (message "%s" "Checking for missing packages.")
  (dolist (p package-selected-packages)
    (unless (package-installed-p p)
      (add-to-list 'lem-missing-packages p 'append)))

  ;; Install packages
  (if lem-missing-packages
      (progn
        (message "Emacs is now refreshing its package database...")
        (package-refresh-contents)
        ;; Install the missing packages
        (dolist (p lem-missing-packages)
          (message "Installing `%s' .." p)
          (package-install p))
        (setq lem-missing-packages '()))
    (message "%s" "No missing packages.")))

;; Check for missing packages & install if necessary
(lem-check-and-install-packages)

;; Enable installed packages at startup
(setq package-enable-at-startup t)

;; Allow loading from the package cache
(setq package-quickstart t)


;;; early-config.el ends here
