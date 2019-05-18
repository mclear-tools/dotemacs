;;; Avy
(use-package avy
  :commands (avy-goto-char))

;;; Imenu list outline
(use-package imenu-list
  :ensure t
  :commands (imenu-list-smart-toggle imenu-list-minor-mode)
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t
        imenu-list-position 'left)
  :custom-face
  (imenu-list-entry-face-0 ((t (:inherit imenu-list-entry-face :foreground "#269bd2"))))
  (imenu-list-entry-face-1 ((t (:inherit imenu-list-entry-face :foreground "medium sea green"))))
  (imenu-list-entry-face-2 ((t (:inherit imenu-list-entry-face :foreground "#cb4b16"))))
  (imenu-list-entry-face-3 ((t (:inherit imenu-list-entry-face :foreground "#b58900")))))

;;; Historian history keeper
(use-package historian
  :defer 5
  :load-path "~/.emacs.d/.local/elisp/historian"
  :config
  (setq historian-save-file (concat cpm-cache-dir ".historian"))
  (historian-mode 1))
  (use-package saveplace
    :init
    (save-place-mode 1)
    :config
    (setq save-place-file (concat cpm-cache-dir "saved-places")
    ;; (setq save-place-forget-unreadable-files nil)
  ))


;;; Treemacs
(use-package treemacs
  :ensure t
  :commands treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-file-event-delay           5000
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (concat cpm-cache-dir "treemacs-persist")
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple)))))
  ;; :bind
  ;; (:map global-map
  ;;       ("M-0"       . treemacs-select-window)
  ;;       ("C-x t 1"   . treemacs-delete-other-windows)
  ;;       ("C-x t t"   . treemacs)
  ;;       ("C-x t B"   . treemacs-bookmark)
  ;;       ("C-x t C-t" . treemacs-find-file)
  ;;       ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;;; Centered Cursor
(use-package centered-cursor-mode
  :diminish centered-cursor-mode
  :hook ((prog-mode markdown-mode org-mode) . centered-cursor-mode)
  :commands (centered-cursor-mode
             global-centered-cursor-mode)
  :config
  (progn
    (setq ccm-recenter-at-end-of-file t
          ccm-ignored-commands '(mouse-drag-region
                                 mouse-set-point
                                 widget-button-click
                                 scroll-bar-toolkit-scroll
                                 evil-mouse-drag-region))))

;;; Hydra
(use-package hydra :defer 2)

  ;; hydra for TODOs
  (with-eval-after-load 'hydra
  (defhydra cpm/hydra-todo
             (:pre
              (hl-todo-mode 1)
              :post
              (hl-todo-mode -1))
  "Todo"
  ("n" hl-todo-next "Next")
  ("p" hl-todo-previous "Previous")
  ("o" hl-todo-occur "Occur")
  ("q" nil "Quit" :color blue :exit t)))

;;; Recent files
(use-package recentf
  :ensure nil
  :commands (helm-recentf)
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-save-file (concat cpm-etc-dir "recentf"))
  ;; remove agenda files from list.
  (setq recentf-exclude '("projects.org"
                          "inbox.org"
                          "someday.org"
                          "bookmark")
        recentf-max-saved-items 300
        recentf-max-menu-items 10))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-navigation)