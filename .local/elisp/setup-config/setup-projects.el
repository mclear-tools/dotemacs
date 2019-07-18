;; Project Management
;; This project workflow primarily uses a single frame with
;; different workspaces, made possible by projectile and eyebrowse.

;;; Projectile
(use-package projectile
  :ensure t
  ;; :load-path (lambda () (concat cpm-elisp-dir "projectile-2.0.0"))
  :defer 1
  :init
  ;; save projectile-known-projects-file in cache folder
  (setq projectile-known-projects-file
        (concat cpm-cache-dir "projectile-bookmarks.eld"))
  (setq projectile-cache-file
        (concat cpm-cache-dir "projectile.cache"))
  (setq projectile-enable-caching t
        projectile-files-cache-expire 60)
  :config
  (setq projectile-git-submodule-command nil)
  (projectile-global-mode))

;;; Project Functions
;; Single frame project functions
(defun cpm/open-agenda-in-workspace ()
  (interactive)
  (eyebrowse-mode)
  (eyebrowse-switch-to-window-config-1)
  (cpm/jump-to-org-super-agenda)
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) "Org-agenda"))

(defun cpm/open-project-and-workspace ()
  (interactive)
  (eyebrowse-create-window-config)
  (helm-projectile-switch-project)
  (setq frame-title-format
    '(""
      "%b"
      (:eval
       (let ((project-name (projectile-project-name)))
         (unless (string= "-" project-name)
           (format " in [%s]" project-name))))))
  (let ((project-name (projectile-project-name)))
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) project-name))
  (magit-status))

;; Some useful functions for opening projects in new frames
(defun cpm/open-project-and-frame ()
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil)))
  (crux-create-scratch-buffer)
  (helm-projectile-switch-project)
  (toggle-frame-maximized)
  (setq frame-title-format
        '(""
          "%b"
          (:eval
           (let ((project-name (projectile-project-name)))
             (unless (string= "-" project-name)
               (format " in [%s]" project-name))))))
  (split-window-right)
  (magit-status))

;;; Eyebrowse
(use-package eyebrowse
  :commands (eyebrowse-create-window-config eyebrowse-switch-to-window-config-1 eyebrowse-switch-to-window-config-2)
  :general
  (:states '(insert normal motion emacs) :keymaps 'override
           "s-1" 'eyebrowse-switch-to-window-config-1
           "s-2" 'cpm/open-agenda-in-workspace
           "s-3" 'eyebrowse-switch-to-window-config-3
           "s-4" 'eyebrowse-switch-to-window-config-4
           "s-5" 'eyebrowse-switch-to-window-config-5
           "s-p" 'eyebrowse-switch-to-window-config
           "s-]" 'eyebrowse-next-window-config
           "s-[" 'eyebrowse-last-window-config)
  :config
  (setq eyebrowse-new-workspace 'dired-jump
        eyebrowse-mode-line-style 'hide
        eyebrowse-wrap-around t
        eyebrowse-switch-back-and-forth t)

  ;; Define a tabedit command (a la Vim) to create new tabs with
  ;; optional file name or directory name. When no filename is passed on
  ;; it calls the default eyebrowse function.
  (evil-define-command cpm/eyebrowse-create-window-config-with-file (file)
    :repeat nil
    (interactive "<f>")
    (if (and file (f-exists? file))
        (progn
          (eyebrowse-create-window-config)
          (find-file file))
      (eyebrowse-create-window-config)))

  (evil-ex-define-cmd "tabc[lose]" 'eyebrowse-close-window-config)
  (evil-ex-define-cmd "tabe[dit]"  'cpm/eyebrowse-create-window-config-with-file)
  (evil-ex-define-cmd "tabfirst"   'eyebrowse-switch-to-window-config-0)
  (evil-ex-define-cmd "tablast"    'eyebrowse-last-window-config)
  (evil-ex-define-cmd "tabn"       'eyebrowse-next-window-config)
  (evil-ex-define-cmd "tabp"       'eyebrowse-prev-window-config)
  (evil-ex-define-cmd "tabs"       'eyebrowse-switch-to-window-config)

  ;; This one doesn't exist in Vim, but it's useful if you'd like to use
  ;; tabs like Tmux, where it's very common to rename tabs.
  (evil-ex-define-cmd "tabr[ename]" 'eyebrowse-rename-window-config)

  (eyebrowse-setup-evil-keys)
  (eyebrowse-mode t))

;;; Bookmarks
(use-package bookmark
  :defer 2
  :config
  (setq bookmark-default-file (concat cpm-cache-dir "bookmarks")))

(use-package bookmark+
  :commands (bmkp-switch-bookmark-file-create bmkp-set-desktop-bookmark)
  :config
  (setq bmkp-last-as-first-bookmark-file (concat cpm-cache-dir "bookmarks")))

;;; Org and Projectile
(use-package org-projectile
  :ensure t
  :defer 3
  :config
  (setq org-projectile-projects-file "~/Dropbox/org-files/projects.org"))

 (use-package org-projectile-helm
  :ensure t
  :after org-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-projects)
