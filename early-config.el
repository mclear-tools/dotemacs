;;; early-config.el --- User config for early init   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Colin McLear
;; Author: Colin McLear <mclear@unl.edu>
;; Keywords: tools, convenience

;;; Frame & Colors
;; ;; Set colors correctly so there is no flash at startup
;; (cond ((string= (shell-command-to-string "dark-mode status") "on\n")
;;        (push '(background-color . "#141414") initial-frame-alist)
;;        (setq active-theme 'dark-theme))
;;       (t
;;        (push '(background-color . "#FFFEFD") initial-frame-alist)
;;        (setq active-theme 'light-theme)))

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


;;; Package List
;; Explicitly set packages for download/install or removal
;; Needs to be set before package initialization
;; https://www.olivertaylor.net/emacs/notes-on-package-el.html
(customize-set-variable 'package-selected-packages '(ace-window
                                                     ag
                                                     aggressive-indent
                                                     all-the-icons
                                                     all-the-icons-completion
                                                     all-the-icons-dired
                                                     anaphora
                                                     applescript-mode
                                                     auctex
                                                     auto-compile
                                                     backup-walker
                                                     ;; bug-hunter
                                                     command-log-mode
                                                     citar
                                                     cape
                                                     consult-dir
                                                     consult-flyspell
                                                     ;; consult-notes
                                                     corfu
                                                     corfu-doc
                                                     crux
                                                     define-word
                                                     deadgrep
                                                     denote
                                                     diff-hl
                                                     dimmer
                                                     dired-narrow
                                                     dired-quick-sort
                                                     dired-ranger
                                                     diredfl
                                                     el-patch
                                                     elfeed-tube
                                                     elisp-def
                                                     elisp-demos
                                                     embark-consult
                                                     embrace
                                                     esh-help
                                                     eshell-syntax-highlighting
                                                     eshell-up
                                                     ;; esup
                                                     flymake-collection
                                                     flyspell-correct
                                                     ;; git-timemachine
                                                     gnutls
                                                     ;; gited
                                                     goggles
                                                     goto-last-change
                                                     grab-mac-link
                                                     haskell-mode
                                                     helpful
                                                     hide-mode-line
                                                     highlight-indent-guides
                                                     highlight-numbers
                                                     ibuffer-vc
                                                     iedit
                                                     imenu-list
                                                     info-colors
                                                     kind-icon
                                                     lorem-ipsum
                                                     lua-mode
                                                     macrostep
                                                     magit-todos
                                                     marginalia
                                                     markdown-toc
                                                     meow
                                                     mu4e-views
                                                     multi-compile
                                                     multi-vterm
                                                     ns-auto-titlebar
                                                     orderless
                                                     org-appear
                                                     org-autolist
                                                     org-contrib
                                                     org-download
                                                     org-modern
                                                     org-msg
                                                     org-noter
                                                     org-pomodoro
                                                     org-tree-slide
                                                     osx-dictionary
                                                     osx-lib
                                                     outline-minor-faces
                                                     ox-hugo
                                                     ox-pandoc
                                                     package-lint
                                                     palimpsest
                                                     pandoc-mode
                                                     paradox
                                                     pcmpl-args
                                                     pcmpl-git
                                                     pcmpl-homebrew
                                                     pcomplete-extension
                                                     ;; pdf-tools
                                                     peep-dired
                                                     php-mode
                                                     popper
                                                     puni
                                                     rainbow-delimiters
                                                     rainbow-identifiers
                                                     rainbow-mode
                                                     restart-emacs
                                                     reveal-in-osx-finder
                                                     revert-buffer-all
                                                     rg
                                                     svg-tag-mode
                                                     tabspaces
                                                     tldr
                                                     tramp-term
                                                     use-package
                                                     vdiff-magit
                                                     vertico
                                                     vimrc-mode
                                                     virtualenvwrapper
                                                     visual-regexp-steroids
                                                     web-mode
                                                     which-key
                                                     winum
                                                     writeroom-mode
                                                     ws-butler
                                                     yaml-mode
                                                     yasnippet-snippets))

;; Auto install the required packages
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-packages.el
;; http://toumorokoshi.github.io/emacs-from-scratch-part-2-package-management.html
;; https://github.com/kaushalmodi/.emacs.d
(defvar lem-missing-packages '()
  "List populated at startup containing packages needing installation.")

;; Check packages
(dolist (p package-selected-packages)
  (unless (package-installed-p p)
    (add-to-list 'lem-missing-packages p :append)))

;; Install packages
(when lem-missing-packages
  (message "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  ;; Install the missing packages
  (dolist (p lem-missing-packages)
    (message "Installing `%s' .." p)
    (package-install p))
  (setq lem-missing-packages '()))

;; Don't initialize installed packages at startup
(setq package-enable-at-startup t)

;; Allow loading from the package cache
(setq package-quickstart t)


;;; early-config.el ends here
