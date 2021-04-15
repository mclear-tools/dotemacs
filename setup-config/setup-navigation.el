;;; Imenu list outline
(use-package imenu-list
  :commands (imenu-list-smart-toggle imenu-list-minor-mode)
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t
        imenu-list-position 'left))

;;; Save place
(use-package saveplace
  :hook (after-init . save-place-mode)
  :config
  (setq save-place-file (concat cpm-cache-dir "saved-places"))
  (setq save-place-forget-unreadable-files nil))

;;; Go To Change
(use-package goto-chg
  :commands goto-last-change goto-last-change-reverse)

;;; Treemacs
(use-package treemacs
  :commands treemacs
  :general
  (:states '(normal insert motion emacs)
   :keymaps 'winum-keymap
   "M-0" #'treemacs-select-window)
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-file-event-delay           5000
          treemacs-follow-after-init          t
          treemacs-recenter-distance   0.1
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
  )

(use-package treemacs-projectile
  :after treemacs projectile
  )

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
(use-package hydra :defer 1)

;;; Recent files
(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-save-file (concat cpm-cache-dir "recentf"))
  ;; remove agenda files from list.
  (setq recentf-exclude '("writing.org"
                          "inbox.org"
                          "todo.org"
                          "teaching.org"
                          "someday.org"
                          "bookmark"
                          "elpa"
                          "cache")
        recentf-max-saved-items 300
        recentf-max-menu-items 10))

;;;; Goto Address
;; This package allows you to click or hit a key sequence while on a
;; URL or e-mail address, and either load the URL into a browser of
;; your choice using the browse-url package, or if it's an e-mail
;; address, to send an e-mail to that address.
(use-package goto-addr
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)
         (eshell-mode . goto-address-mode)
         (text-mode . goto-address-mode)
         (shell-mode . goto-address-mode))
  :general (:states '(normal insert emacs motion)
            :keymaps 'goto-address-highlight-keymap
            "<RET>"  'goto-address-at-point
            "M-<RET>" 'newline)
  :commands (goto-address-prog-mode
             goto-address-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-navigation)
