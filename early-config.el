;;; early-config.el --- User config for early init   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Colin McLear
;; Author: Colin McLear <mclear@unl.edu>
;; Keywords: tools, convenience

;;; Frame & Colors
;; ;; Set colors correctly so there is no flash at startup
(cond ((string= (shell-command-to-string "dark-mode status") "on\n")
       (setq active-theme 'dark-theme))
      (t
       (setq active-theme 'light-theme)))

(setq-default initial-frame-alist
              (append (list
                       '(fullscreen . maximized)
                       `(background-color . ,(if (eq active-theme 'light-theme) "#fffefd" "#141414"))
                       `(foreground-color . ,(if (eq active-theme 'light-theme) "#141414" "#A3A3A3"))
                       '(internal-border-width . 12)
                       '(tool-bar-lines . 0)
                       '(vertical-scroll-bars . nil)
                       '(horizontal-scroll-bars . nil)
                       '(undecorated . nil))))

;; No modeline at startup
(setopt mode-line-format nil)
(setopt header-line-format nil)

;;;; Package settings
;; Don't auto ensure packages with use-package
(setopt lem-package-ensure-packages nil)

;;;; Package List
;; Explicitly set packages for download/install or removal
;; Needs to be set before package initialization
;; https://www.olivertaylor.net/emacs/notes-on-package-el.html
(setopt package-selected-packages '(;; utility
                                    async
                                    dash
                                    s
                                    f
                                    el-patch

                                    ;; settings
                                    visual-regexp
                                    backup-walker
                                    expand-region

                                    ;; functions
                                    crux

                                    ;; macros
                                    anaphora
                                    macrostep

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
                                    rg
                                    wgrep
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
                                    obsidian

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
                                    treesit-auto
                                    vimrc-mode
                                    web-mode
                                    php-mode
                                    haskell-mode
                                    applescript-mode
                                    tldr
                                    lua-mode
                                    yaml-mode
                                    rainbow-identifiers
                                    auto-compile
                                    esxml
                                    kv

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
                                    org-tree-slide

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
                                    org-msg
                                    mu4e-column-faces

                                    ;; other
                                    command-log-mode
                                    xwidgets-reuse))

;; Package vc list
(setq package-vc-selected-packages '((zotxt-emacs
                                      :url "https://github.com/egh/zotxt-emacs.git"
                                      :branch "master")
                                     (bibtex-capf
                                      :url "https://github.com/mclear-tools/bibtex-capf.git"
                                      :branch "main")
                                     (pulsing-cursor
                                      :url "https://github.com/jasonjckn/pulsing-cursor"
                                      :branch "main")
                                     (org-devonthink
                                      :url "https://github.com/lasvice/org-devonthink"
                                      :branch "master")
                                     (org-modern-indent
                                      :url "https://github.com/jdtsmith/org-modern-indent")))

;; Auto install the required packages
;; Set missing package vars
(defvar lem-missing-packages '()
  "List populated at startup containing packages needing installation.")
(defvar lem-missing-vc-packages '()
  "List populated at startup containing vc packages requiring installation.")

;; Check for packages
(defun lem-check-missing-packages ()
  "Check for missing packages."
  (interactive)
  ;; Check packages
  (message "%s" "Checking for missing packages.")
  (dolist (p package-selected-packages)
    (unless (package-installed-p p)
      (add-to-list 'lem-missing-packages p 'append)))
  ;; Check vc installed packages (Emacs 29+)
  (when (version< "29" emacs-version)
    (message "%s" "Checking for missing vc packages.")
    (dolist (p package-vc-selected-packages)
      (unless (package-installed-p (car p))
        (add-to-list 'lem-missing-vc-packages (car p) 'append)))))

;; Install packages
(defun lem-install-missing-packages ()
  "Install missing packages from package & package-vc lists."
  (interactive)
  (lem-check-missing-packages)
  (cond ((or lem-missing-packages
             lem-missing-vc-packages)
         (message "Refreshing package database & installing missing packages...")
         (package-install-selected-packages t)
         (setq lem-missing-packages '())
         (package-vc-install-selected-packages)
         (setq lem-missing-vc-packages '()))
        (t
         (message "No missing packages."))))

;; Don't show warnings
(setq warning-minimum-level :emergency)
;; Enable installed packages at startup
(setopt package-enable-at-startup t
        ;; Allow loading from the package cache
        package-quickstart t)

;; Install packages if missing
(lem-install-missing-packages)




;;; early-config.el ends here
