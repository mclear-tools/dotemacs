(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "/Users/roambot/.emacs.d/.local/temp/cache/bookmarks")
 '(git-commit-summary-max-length 50)
 '(markdown-toc-header-toc-end "<!-- toc end -->")
 '(markdown-toc-header-toc-start "<!-- toc start -->")
 '(markdown-toc-header-toc-title " ")
 '(markdown-toc-list-item-marker "1.")
 '(org-image-actual-width '(100))
 '(package-selected-packages
   '(vterm helm-file-preview frog-jump-buffer helm-evil-markers centaur-tabs avy dired-quick-sort highlight-indent-guides aggressive-indent rg ws-butler magit-todos eterm-256color dired-narrow diredfl powerline frame-workflow frame-purpose nameframe-projectile dired-ranger magit company-bibtex markdown-toc no-littering bug-hunter esup company ob-python ob-plantuml ob-ditaa ob-tangle ob org-plus-contrib visual-regexp-steroids grab-mac-link org-projectile-helm org-projectile diff-hl reveal-in-osx-finder typo elisp-demos yasnippet-snippets yasnippet deft evil-org htmlize ox-hugo ox-pandoc org-pomodoro org-download helm-org-rifle org-randomnote ox-reveal org-bullets org-super-agenda org-pdfview pdf-tools tldr macrostep vimrc-mode yaml-mode php-mode lua-mode web-mode haskell-mode elisp-slime-nav applescript-mode rainbow-mode rainbow-identifiers rainbow-delimiters evil-lion eyebrowse deadgrep ag esh-autosuggest eshell-fringe-status shell-switcher tramp-term virtualenvwrapper shell-pop sane-term git-gutter-fringe git-gutter gited git-timemachine helm-gitignore evil-magit auctex palimpsest lorem-ipsum interleave writeroom-mode pandoc-mode markdown-mode helm-bibtex flyspell-correct-ivy multi-compile restart-emacs doom-modeline dashboard page-break-lines crux which-key helpful shackle kaolin-themes alect-themes color-theme-sanityinc-tomorrow nubox atom-one-dark-theme omtose-phellack-theme suscolors-theme jazz-theme darkokai-theme tao-theme darkmine-theme cyberpunk-theme afternoon-theme flatland-theme forest-blue-theme spacemacs-theme leuven-theme material-theme ample-theme gotham-theme darktooth-theme molokai-theme zenburn-theme nord-theme doom-themes gruvbox-theme ns-auto-titlebar centered-cursor-mode treemacs-projectile treemacs-evil treemacs window-numbering ace-window imenu-list counsel-projectile ivy helm-projectile helm-themes helm-dired-recent-dirs helm-hunks helm-ls-git helm-descbinds peep-dired dired+ bookmark+ evil-visualstar evil-numbers evil-mc evil-multiedit evil-commentary evil-embrace evil-surround evil-indent-textobject evil-collection backup-walker visual-regexp solarized-theme quelpa-use-package paradox outshine helm general f evil))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors '("#fdf4c1" . "#1d2021"))
 '(safe-local-variable-values
   '((eval setq org-export-initial-scope 'subtree)
     (eval add-hook 'after-save-hook #'org-hugo-export-wim-to-md-after-save :append :local)
     (org-download-heading-lvl))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(dashboard-heading ((t (:inherit font-lock-variable-name-face))))
 '(diredfl-compressed-file-name ((t (:foreground "#00629D"))))
 '(diredfl-compressed-file-suffix ((t (:foreground "#839496"))))
 '(diredfl-date-time ((t (:foreground "#9EA0E5"))))
 '(diredfl-deletion ((t (:background "Red" :foreground "Yellow"))))
 '(diredfl-dir-heading ((t (:background "#69B7F0" :foreground "#002b36"))))
 '(diredfl-dir-name ((t (:foreground "#69B7F0"))))
 '(diredfl-dir-priv ((t (:foreground "#268bd2"))))
 '(diredfl-exec-priv ((t (:foreground "#990A1b"))))
 '(diredfl-file-name ((t (:foreground "#2aa198"))))
 '(diredfl-file-suffix ((t (:foreground "#839496"))))
 '(diredfl-flag-mark-line ((t (:background "#dc322f"))))
 '(diredfl-no-priv ((t (:foreground "#b58900"))))
 '(diredfl-number ((t (:foreground "#DEB542"))))
 '(diredfl-rare-priv ((t (:background "#cb4b16" :foreground "#B4C342"))))
 '(diredfl-read-priv ((t (:foreground "#F2804F"))))
 '(diredfl-tagged-autofile-name ((t (:foreground "#328C0411328"))))
 '(diredfl-write-priv ((t (:foreground "#8b2C02"))))
 '(doom-modeline-bar ((t (:inherit highlight :inverse-video t :background "#268bd2"))))
 '(doom-modeline-eyebrowse ((t (:inherit highlight))))
 '(doom-modeline-inactive-bar ((t (:inherit highlight))))
 '(imenu-list-entry-face-0 ((t (:inherit imenu-list-entry-face :foreground "#269bd2"))))
 '(imenu-list-entry-face-1 ((t (:inherit imenu-list-entry-face :foreground "medium sea green"))))
 '(imenu-list-entry-face-2 ((t (:inherit imenu-list-entry-face :foreground "#cb4b16"))))
 '(imenu-list-entry-face-3 ((t (:inherit imenu-list-entry-face :foreground "#b58900")))))
