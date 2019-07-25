(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(bmkp-last-as-first-bookmark-file "/Users/roambot/.emacs.d/.local/temp/cache/bookmarks")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" default)))
 '(fci-rule-color "#073642")
 '(git-commit-summary-max-length 50)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(magit-diff-use-overlays nil)
 '(markdown-toc-header-toc-end "<!-- toc end -->")
 '(markdown-toc-header-toc-start "<!-- toc start -->")
 '(markdown-toc-header-toc-title " ")
 '(markdown-toc-list-item-marker "1.")
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-image-actual-width (quote (100)))
 '(package-selected-packages
   (quote
    (persp-projectile perspective vterm helm-file-preview frog-jump-buffer helm-evil-markers centaur-tabs avy dired-quick-sort highlight-indent-guides aggressive-indent rg ws-butler magit-todos eterm-256color dired-narrow diredfl powerline frame-workflow frame-purpose nameframe-projectile dired-ranger magit company-bibtex markdown-toc no-littering bug-hunter esup company ob-python ob-plantuml ob-ditaa ob-tangle ob org-plus-contrib visual-regexp-steroids grab-mac-link org-projectile-helm org-projectile diff-hl reveal-in-osx-finder typo elisp-demos yasnippet-snippets yasnippet deft evil-org htmlize ox-hugo ox-pandoc org-pomodoro org-download helm-org-rifle org-randomnote ox-reveal org-bullets org-super-agenda org-pdfview pdf-tools tldr macrostep vimrc-mode yaml-mode php-mode lua-mode web-mode haskell-mode elisp-slime-nav applescript-mode rainbow-mode rainbow-identifiers rainbow-delimiters evil-lion eyebrowse deadgrep ag esh-autosuggest eshell-fringe-status shell-switcher tramp-term virtualenvwrapper shell-pop sane-term git-gutter-fringe git-gutter gited git-timemachine helm-gitignore evil-magit auctex palimpsest lorem-ipsum interleave writeroom-mode pandoc-mode markdown-mode helm-bibtex flyspell-correct-ivy multi-compile restart-emacs doom-modeline dashboard page-break-lines crux which-key helpful shackle kaolin-themes alect-themes color-theme-sanityinc-tomorrow nubox atom-one-dark-theme omtose-phellack-theme suscolors-theme jazz-theme darkokai-theme tao-theme darkmine-theme cyberpunk-theme afternoon-theme flatland-theme forest-blue-theme spacemacs-theme leuven-theme material-theme ample-theme gotham-theme darktooth-theme molokai-theme zenburn-theme nord-theme doom-themes gruvbox-theme ns-auto-titlebar centered-cursor-mode treemacs-projectile treemacs-evil treemacs window-numbering ace-window imenu-list counsel-projectile ivy helm-projectile helm-themes helm-dired-recent-dirs helm-hunks helm-ls-git helm-descbinds peep-dired dired+ bookmark+ evil-visualstar evil-numbers evil-mc evil-multiedit evil-commentary evil-embrace evil-surround evil-indent-textobject evil-collection backup-walker visual-regexp solarized-theme quelpa-use-package paradox outshine helm general f evil)))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#1d2021")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(safe-local-variable-values
   (quote
    ((projectile-project-run-cmd . "mkdir -p build; cd build; cmake ..; make run")
     (projectile-project-compilation-cmd . "mkdir -p build; cd build; cmake ..; make")
     (eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))
     (eval setq org-export-initial-scope
           (quote subtree))
     (eval add-hook
           (quote after-save-hook)
           (function org-hugo-export-wim-to-md-after-save)
           :append :local)
     (org-download-heading-lvl))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c9485ddd1797")
     (60 . "#bf7e73b30bcb")
     (80 . "#b58900")
     (100 . "#a5a58ee30000")
     (120 . "#9d9d91910000")
     (140 . "#9595943e0000")
     (160 . "#8d8d96eb0000")
     (180 . "#859900")
     (200 . "#67119c4632dd")
     (220 . "#57d79d9d4c4c")
     (240 . "#489d9ef365ba")
     (260 . "#3963a04a7f29")
     (280 . "#2aa198")
     (300 . "#288e98cbafe2")
     (320 . "#27c19460bb87")
     (340 . "#26f38ff5c72c")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(vterm-install t t)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
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
