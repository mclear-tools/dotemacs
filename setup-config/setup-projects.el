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
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :defer 1
  :config
  ;; see https://github.com/bbatsov/persp-projectile/issues/23#issuecomment-463625961
  (define-key projectile-mode-map [remap projectile-find-other-file] #'helm-projectile-find-other-file)
  (define-key projectile-mode-map [remap projectile-find-file] #'helm-projectile-find-file)
  (define-key projectile-mode-map [remap projectile-find-file-in-known-projects] #'helm-projectile-find-file-in-known-projects)
  (define-key projectile-mode-map [remap projectile-find-file-dwim] #'helm-projectile-find-file-dwim)
  (define-key projectile-mode-map [remap projectile-find-dir] #'helm-projectile-find-dir)
  (define-key projectile-mode-map [remap projectile-recentf] #'helm-projectile-recentf)
  (define-key projectile-mode-map [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer)
  (define-key projectile-mode-map [remap projectile-grep] #'helm-projectile-grep)
  (define-key projectile-mode-map [remap projectile-ack] #'helm-projectile-ack)
  (define-key projectile-mode-map [remap projectile-ag] #'helm-projectile-ag)
  (define-key projectile-mode-map [remap projectile-ripgrep] #'helm-projectile-rg)
  (define-key projectile-mode-map [remap projectile-browse-dirty-projects] #'helm-projectile-browse-dirty-projects)
  (helm-projectile-commander-bindings))

;;; Eyebrowse
(use-package eyebrowse
  :commands (eyebrowse-mode eyebrowse-create-window-config eyebrowse-switch-to-window-config-1 eyebrowse-switch-to-window-config-2)
  :config
  (setq eyebrowse-new-workspace 'dired-jump
        eyebrowse-mode-line-style 'hide
        eyebrowse-wrap-around t
        eyebrowse-switch-back-and-forth t)
  (eyebrowse-mode t))

;;; Perspectives
;; I use this to isolate buffers in the different eyebrowse workspaces
(use-package perspective
  :ensure t
  :defer 1
  :commands (persp-mode persp-switch persp-next persp-prev persp-add-buffer persp-rename persp-kill)
  :general
  (:states '(insert normal motion emacs) :keymaps 'override
           "s-p" 'persp-switch
           "s-]" 'persp-next
           "s-[" 'persp-prev)
  :config
  (setq persp-show-modestring nil)
  (persp-mode 1)
  (add-hook 'persp-switch-hook 'cpm/eyebrowse-persp-switch))

(use-package persp-projectile
  :ensure t
  :after perspective)

;;; Project Functions

;;;; Switch Eyebrowse after Change of Perspective
(defun cpm/eyebrowse-persp-switch ()
  "set eyebrowse when switching perspectives"
  (interactive)
  (let* ((persp-name (persp-curr))
         (eyebrowse-switch-to-window-config persp-name))))


;;;; Open agenda as Workspace
(defun cpm/open-agenda-in-workspace ()
  "open agenda in its own workspace"
  (interactive)
  (eyebrowse-mode)
  (eyebrowse-switch-to-window-config-1)
  (persp-switch "agenda")
  (setq frame-title-format '("" "%b"))
  (require 'org-super-agenda)
  (cpm/jump-to-org-super-agenda)
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) "agenda")
  (persp-add-buffer "*dashboard*")
  (persp-kill "main"))
(general-define-key
 :states '(insert normal motion emacs)
 :keymaps 'override
 "s-1" 'cpm/open-agenda-in-workspace)

;;;; Open emacs.d in workspace
(defun cpm/open-emacsd-in-workspace ()
  "open emacsd in workspace"
  (interactive)
  (eyebrowse-mode)
  (eyebrowse-switch-to-window-config-1)
  (persp-switch "emacs.d")
  (setq frame-title-format
        '(""
          "%b"
          (:eval
           (let ((project-name (projectile-project-name)))
             (unless (string= "-" project-name)
               (format " in [%s]" project-name))))))
  (require 'crux)
  (crux-find-user-init-file)
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) "emacs.d")
  (persp-kill "main")
  (require 'magit)
  (magit-status-setup-buffer))
(general-define-key
 :states '(insert normal motion emacs)
 :keymaps 'override
 "s-2" 'cpm/open-emacsd-in-workspace)


;;;; Open New Project in Workspace
(defun cpm/open-project-and-workspace ()
  "open a new project as its own workspace -- i.e. in its own perspective and eyebrowse slot"
  (interactive)
  (eyebrowse-create-window-config)
  ;; create a temp scratch buffer for persp switch
  ;; see https://github.com/bbatsov/helm-projectile/issues/4#issuecomment-280949497
  (persp-switch (let ((temp-charset "1234567890abcdefghijklmnopqrstuvwxyz")
                      (random-string ""))
                  (dotimes (i 6 random-string)
                    (setq random-string
                          (concat
                           random-string
                           (char-to-string (elt temp-charset (random (length temp-charset)))))))))
  (helm-projectile-switch-project)
  (setq frame-title-format
        '(""
          "%b"
          (:eval
           (let ((project-name (projectile-project-name)))
             (unless (string= "-" project-name)
               (format " in [%s]" project-name))))))
  (let ((project-name (projectile-project-name)))
    (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) project-name)
    (persp-rename project-name)
    (persp-kill "main")
    (kill-matching-buffers "\*scratch\*" nil t)
    (persp-add-buffer (generate-new-buffer (concat "*scratch* " "("project-name")"))))
  (require 'magit)
  (magit-status-setup-buffer))

;;;; Open a Project in a New Frame
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
  (require 'magit)
  (magit-status-setup-buffer))

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
