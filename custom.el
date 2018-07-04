(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "/Users/roambot/.emacs.d/.local/@roambot/cache/bookmarks")
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(exec-path-from-shell-check-startup-files nil t)
 '(flyspell-abbrev-p t)
 '(flyspell-correct-interface (quote flyspell-correct-ivy) t)
 '(flyspell-issue-message-flag nil)
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-use-global-abbrev-table-p t)
 '(golden-ratio-exclude-buffer-regexp (quote ("iedit")))
 '(helm-follow-mode-persistent t)
 '(imenu-list-position (quote left))
 '(package-selected-packages
   (quote
    (peep-dired dired+ evil-collection imenu-list smartparens doom-modeline flyspell-correct-ivy org-brain treemacs-projectile treemacs-evil treemacs neotree ox-hugo exec-path-from-shell mixed-pitch quelpa-use-package quelpa applescript-mode eterm-256color projectile nameframe-projectile org-projectile counsel-projectile helm-projectile yasnippet-snippets tldr company-bibtex auctex htmlize toc-org org-download helm-org-rifle ox-pandoc org-randomnote org-super-agenda ox-reveal org-bullets magit esh-autosuggest bookmark-plus evil-org bookmark+ all-the-icons font-lock+ org-pomodoro org-plus-contrib dashboard page-break-lines epkg restart-emacs zotxt org-pdfview pdf-tools palimpsest lorem-ipsum interleave deft writeroom-mode pandoc-mode markdown-mode helm-bibtex git-gutter-fringe git-gutter gited git-timemachine evil-magit macrostep vimrc-mode yaml-mode php-mode lua-mode web-mode haskell-mode elisp-slime-nav rainbow-mode rainbow-identifiers rainbow-delimiters evil-lion aggressive-indent yasnippet eshell-fringe-status shell-switcher eshell-prompt-extras tramp-term virtualenvwrapper shell-pop sane-term evil-anzu anzu rg ag ranger window-numbering ace-window ivy helm-themes helm-dired-recent-dirs helm-flyspell helm-swoop helm-hunks helm-ls-git helm-descbinds helm-ag helm helpful shackle which-key centered-cursor-mode diminish beacon all-the-icons-dired hl-todo highlight-numbers nlinum powerline kaolin-themes alect-themes color-theme-sanityinc-tomorrow nubox atom-one-dark-theme omtose-phellack-theme suscolors-theme jazz-theme darkokai-theme tao-theme darkmine-theme cyberpunk-theme afternoon-theme flatland-theme forest-blue-theme spacemacs-theme leuven-theme material-theme ample-theme gotham-theme darktooth-theme molokai-theme zenburn-theme nord-theme doom-themes gruvbox-theme evil-numbers evil-commentary evil-embrace embrace evil-surround evil-indent-textobject evil general osx-location backup-walker visual-regexp crux git f s dash async use-package)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((eval setq org-export-initial-scope
           (quote subtree))
     (eval setq org-export-initial-scope
           (quote subtree)
           :append :local)
     (eval add-hook
           (quote after-save-hook)
           (function org-hugo-export-wim-to-md-after-save)
           :append :local)
     (eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))
     (eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))
     (org-list-allow-alphabetical . t)
     (org-download-heading-lvl))))
 '(send-mail-function (quote mailclient-send-it))
 '(sp-base-key-bindings (quote sp))
 '(sp-override-key-bindings
   (quote
    (("C-S-<left>" . sp-backward-slurp-sexp)
     ("C-S-<right>" . sp-backward-barf-sexp)
     ("C-M-t" . sp-transpose-sexp)
     ("C-S-k" . sp-kill-hybrid-sexp)
     ("C-c C-<right>" . sp-slurp-hybrid-sexp)
     ("C-(" . sp-rewrap-sexp)
     ("C-M-<backspace>" . sp-splice-sexp-killing-around)
     ("C-S-<backspace>")
     ("M-<backspace>")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(doom-modeline-bar ((t (:inherit highlight :inverse-video t :background "#268bd2"))))
 '(doom-modeline-eyebrowse ((t (:inherit highlight))))
 '(doom-modeline-inactive-bar ((t (:inherit highlight))))
 '(imenu-list-entry-face-0 ((t (:inherit imenu-list-entry-face :foreground "#269bd2"))))
 '(imenu-list-entry-face-1 ((t (:inherit imenu-list-entry-face :foreground "medium sea green"))))
 '(imenu-list-entry-face-2 ((t (:inherit imenu-list-entry-face :foreground "#cb4b16"))))
 '(imenu-list-entry-face-3 ((t (:inherit imenu-list-entry-face :foreground "#b58900")))))
