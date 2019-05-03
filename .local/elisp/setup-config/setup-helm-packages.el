;;; Helm  Packages

(use-package helm-ag
  :load-path "~/.emacs.d/.local/elisp/helm-ag/"
  :after helm
  :demand t
  :commands (helm-ag helm-ag-buffers helm-ag-this-file helm-do-ag helm-ag-project-root cpm/helm-files-do-ag cpm/helm-files-search-current-directory)
  :config
  (setq helm-follow-mode-persistent t)
  (setq helm-ag-base-command "rg --no-heading")
  (setq helm-ag-fuzzy-match t))

(use-package helm-descbinds
  :commands helm-descbinds
  :config
  (setq helm-descbinds-window-style 'same-window)
  (add-hook 'helm-mode-hook 'helm-descbinds-mode))

(use-package helm-ls-git :commands helm-ls-git-ls)

(use-package helm-hunks :commands helm-hunks)

(use-package helm-swoop
  :commands (helm-swoop-without-pre-input helm-swoop-back-to-last-point helm-multi-swoop helm-multi-swoop-all)
  :load-path "~/.emacs.d/.local/elisp/helm-swoop/"
  :config
  (setq helm-swoop-use-fuzzy-match t)
  (setq helm-swoop-split-with-multiple-windows t))

(use-package helm-dired-recent-dirs
    :commands helm-dired-recent-dirs-view)

(use-package helm-files
  :ensure nil
  :defer t
  :config
  (setq helm-ff-skip-boring-files t)
  (setq helm-idle-delay 0.05)
  (setq helm-input-idle-delay 0.05)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-boring-file-regexp-list
  '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "~$"
    "\\.so$" "\\.a$" "\\.elc$" "\\.fas$" "\\.fasl$" "\\.pyc$" "\\.pyo$")))
(use-package helm-themes
  :commands helm-themes)

(defadvice helm-themes--load-theme (after helm-themes--load-theme-after activate) (require 'powerline) (powerline-reset))
  ;; (ad-unadvise 'helm-themes--load-theme)

(use-package helm-projectile
 :commands (helm-projectile-switch-to-buffer
            helm-projectile-find-dir
            helm-projectile-dired-find-dir
            helm-projectile-recentf
            helm-projectile-find-file
            helm-projectile-grep
            helm-projectile
            helm-projectile-switch-project)
 :init
 (setq projectile-switch-project-action 'helm-projectile)
 :config
 (helm-projectile-on))

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

(use-package ace-window
  :commands (ace-window ace-swap-window aw-flip-window cpm/swap-windows))

(use-package avy
  :commands (avy-goto-char))

(use-package golden-ratio
  :load-path "~/.emacs.d/.local/elisp/golden-ratio.el/"
  :ensure nil
  ;; :after (:any perspective helm nameframe projectile)
  ;; :demand t
  :defer 3
  :config
  (setq golden-ratio-exclude-buffer-names '("*Ilist*" "*Deft*"))
  (setq golden-ratio-exclude-buffer-regexp '("Ilist"))
  (setq golden-ratio-exclude-modes '("dired" "peep-dired"))
  ;; inhibit in helm windows
  (defun cpm--helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))
      (add-to-list 'golden-ratio-inhibit-functions 'cpm--helm-alive-p)
  ;;fix for ispell
  (defun cpm--ispell-alive-p ()
    (get-buffer ispell-choices-buffer))
  (add-to-list 'golden-ratio-inhibit-functions 'cpm--ispell-alive-p)
  ;; use golden ratio for the following
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-up
                  evil-window-down
                  buf-move-left
                  buf-move-right
                  buf-move-up
                  buf-move-down
                  window-number-select
                  select-window
                  select-window-1
                  select-window-2
                  select-window-3
                  select-window-4
                  select-window-5
                  select-window-6
                  select-window-7
                  select-window-8
                  select-window-9
                  previous-multiframe-window
                  magit-status)))
  (golden-ratio-mode 1))
   (use-package window-numbering
     :defer 1
     :config
     (defun window-numbering-install-mode-line (&optional position)
     "Do nothing, the display is handled by the powerline.")
     (setq window-numbering-auto-assign-0-to-minibuffer nil)

     (window-numbering-mode 1)

;; make sure neotree is always 0
(defun spacemacs//window-numbering-assign ()
  "Custom number assignment for neotree."
   (when (and (boundp 'neo-buffer-name)
              (string= (buffer-name) neo-buffer-name)
              ;; in case there are two neotree windows. Example: when
              ;; invoking a transient state from neotree window, the new
              ;; window will show neotree briefly before displaying the TS,
              ;; causing an error message. the error is eliminated by
              ;; assigning 0 only to the top-left window
              (eq (selected-window) (window-at 0 0)))
     0))

 ;; using lambda to work-around a bug in window-numbering, see
 ;; https://github.com/nschum/window-numbering.el/issues/10
 (setq window-numbering-assign-func
       (lambda () (spacemacs//window-numbering-assign))))
;; unset C- and M- digit keys
(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n))))

(use-package windmove
  :commands (windmove-up windmove-down windmove-left windmove-right)
  :config
  (defun cpm/split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right))
  (defun cpm/split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down))
  ;; add edit mode keybindings
  (global-set-key (kbd "<H-up>")     'windmove-up)
  (global-set-key (kbd "<H-down>")   'windmove-down)
  (global-set-key (kbd "<H-left>")   'windmove-left)
  (global-set-key (kbd "<H-right>")  'windmove-right))

(use-package winner
 :ensure nil
 :commands (winner-undo winner-redo winner-mode)
 :config
 (winner-mode 1))

;; (general-define-key :states '(normal motion visual insert)
;;   "C-c o" 'other-window)

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


(provide 'setup-helm-packages)
