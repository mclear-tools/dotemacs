;; Project Management

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

;;; Perspective
(use-package perspective
  :defer 1
  :config
  (setq persp-show-modestring nil)
  (persp-mode))

(use-package persp-projectile
  :after (projectile perspective)
  :demand t)

(with-eval-after-load 'persp-projectile
    (defhydra hydra-persp (:columns 4
                           :color blue)
      "Perspective"
      ("a" persp-add-buffer "Add Buffer")
      ("i" persp-import "Import")
      ("c" persp-kill "Close")
      ("n" persp-next "Next")
      ("p" persp-prev "Prev")
      ("k" persp-remove-buffer "Kill Buffer")
      ("r" persp-rename "Rename")
      ("A" persp-set-buffer "Set Buffer")
      ("s" persp-switch "Switch")
      ("C-x" persp-switch-last "Switch Last")
      ("b" persp-switch-to-buffer "Switch to Buffer")
      ("P" projectile-persp-switch-project "Switch Project")
      ("q" nil "Quit")))

;;; Frame Purpose & Workflow
;; (use-package frame-purpose
;;   :ensure t
;;   :defer 1
;;   :config
;;   (setq persp-show-modestring nil))

  ;; (frame-purpose-mode 1)

;; (use-package frame-workflow
;;   :quelpa (frame-workflow :fetcher github :repo "akirak/frame-workflow")
;;   :general
;;   ("s-p" 'frame-workflow-switch-frame)
;;   :config
;;   (frame-workflow-mode 1)
;;   ;; (autoload 'helm-frame-workflow "helm-frame-workflow")
;;   (setq projectile-switch-project-action #'frame-workflow-switch-directory-frame))

;; (defun frame-workflow-set-projectile-frame-name (frame-action)
;;   "Set the frame's name and call FRAME-ACTION."
;;   (set-frame-parameter nil 'name (projectile-project-name))
;;   (funcall frame-action))

;; (setq frame-workflow-directory-frame-action
;;       (lambda ()
;;         (frame-workflow-set-projectile-frame-name #'projectile-dired)))


;;; Eyebrowse
(use-package eyebrowse
  :commands (eyebrowse-switch-to-window-config-1 eyebrowse-switch-to-window-config-2)
  :general
  (:states '(insert normal motion emacs) :keymaps 'override
    "s-1" 'eyebrowse-switch-to-window-config-1
    "s-2" 'eyebrowse-switch-to-window-config-2
    "s-3" 'eyebrowse-switch-to-window-config-3
    "s-4" 'eyebrowse-switch-to-window-config-4
    "s-5" 'eyebrowse-switch-to-window-config-5)
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


;;; Project Functions
;; Some useful functions for opening projects in new frames
(defun cpm/open-project-and-frame ()
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
  (set-buffer-major-mode buffer)
  (display-buffer buffer '(display-buffer-pop-up-frame . nil)))
  (crux-create-scratch-buffer)
  (helm-projectile-switch-project)
  (setq frame-title-format
    '(""
      "%b"
      (:eval
       (let ((project-name (projectile-project-name)))
         (unless (string= "-" project-name)
           (format " in [%s]" project-name)))))))

;;; Nameframe
(use-package nameframe
  :commands (nameframe-create-frame)
  :after projectile
  :demand t
  :general
  ("s-p" 'nameframe-switch-frame))

(use-package namefram-projectile
  :ensure t
  :disabled t
  :config
  (nameframe-projectile-mode t))

(use-package nameframe-perspective
  :ensure t
  :after nameframe
  :demand t
  :config
  (nameframe-perspective-mode t))

;;; Nameframe Project Functions
;; functions for named work frames
(defun cpm/load-website ()
  (interactive)
  (persp-mode 1)
  (nameframe-create-frame "Website")
  (toggle-frame-maximized)
  (find-file "~/Dropbox/Work/projects/website/website.org")
  (magit-status))
(defun cpm/load-org-agenda-todo ()
  (interactive)
  (persp-mode 1)
  (nameframe-create-frame "Org Agenda")
  (toggle-frame-maximized)
  (cpm/jump-to-org-super-agenda)
  (split-window-right)
  (find-file "~/Dropbox/org-files/todo.org"))
 (defun cpm/load-phil101 ()
   (interactive)
   (persp-mode 1)
   (nameframe-create-frame "PHIL 101")
   (toggle-frame-maximized)
   (find-file "~/Dropbox/Work/projects/phil101/content/slides/lecture_outline.org")
   (split-window-right)
   (find-file "~/Dropbox/Work/projects/phil101/content/pages/schedule.org"))
 (defun cpm/load-phil105 ()
   (interactive)
   (persp-mode 1)
   (nameframe-create-frame "PHIL 105")
   (toggle-frame-maximized)
   (find-file "~/Dropbox/Work/projects/phil105/phil105-classplan.org")
   (magit-status))
 (defun cpm/load-phil232 ()
   (interactive)
   (persp-mode 1)
   (nameframe-create-frame "PHIL 232")
   (toggle-frame-maximized)
   (find-file "~/Dropbox/Work/projects/phil232/phil232_lecture_outline.org")
   (split-window-right)
   (find-file "~/Dropbox/Work/projects/phil232/content/pages/schedule.org"))
  (defun cpm/load-phil871 ()
    (interactive)
    (persp-mode 1)
    (nameframe-create-frame "PHIL 871")
    (toggle-frame-maximized)
    (find-file "~/Dropbox/Work/projects/phil871-kant-survey/phil871-kant-survey.org"))
 (defun cpm/load-kant-apperception-substance ()
   (interactive)
   (persp-mode 1)
   (nameframe-create-frame "Apperception & Substance")
   (toggle-frame-maximized)
   ;; (org-open-link-from-string "[[file:~/Dropbox/org-files/todo.org::*Apperception%20&%20Substance][Apperception & Substance]]")
   (find-file "~/Dropbox/Work/projects/KantApperception/Kantsubstance.md")
   (split-window-right)
   (find-file "~/Dropbox/Work/projects/KantApperception/phil-imprint-submission/revision-notes.org"))
   ;; (magit-status)
 (defun cpm/load-kant-reflection ()
   (interactive)
   (persp-mode 1)
   (nameframe-create-frame "Kant on Reflection")
   (toggle-frame-maximized)
   (find-file "~/Dropbox/Work/projects/KantReflection/Kant-reflection-notes.org")
   (magit-status))
 (defun cpm/load-kant-agency-book ()
   (interactive)
   (persp-mode 1)
   (nameframe-create-frame "Kant on Rational Agency")
   (toggle-frame-maximized)
   (find-file "~/Dropbox/Work/projects/Book-Projects/Kant-Agency-Book/Kant-Rational-Agency-Notes.org")
   (magit-status))
 (defun cpm/load-kant-rationality-book ()
   (interactive)
   (persp-mode 1)
   (nameframe-create-frame "Kant on Rationality")
   (toggle-frame-maximized)
   (find-file "~/Dropbox/notes/zettel/2019-0119-1607-book-notes.md")
   (split-window-right)
   (find-file "~/Dropbox/Work/projects/Book-Projects/rationality-book/chapters")
   (split-window-below)
   (magit-status))
 (defun cpm/load-emacs-config ()
   (interactive)
   (persp-mode 1)
   (nameframe-create-frame "Emacs Config")
   (toggle-frame-maximized)
   (find-file "~/.emacs.d/config.org"))
 (defun cpm/load-kant-free-thought ()
   (interactive)
   (persp-mode 1)
   (nameframe-create-frame "Kant on Free Intellect")
   (toggle-frame-maximized)
   (find-file "~/Dropbox/Work/projects/KantFreeThought/KantFreeThought.md")
   (split-window-right)
   (org-open-link-from-string "[[file:~/Dropbox/org-files/projects.org::*Kant%20Free%20Intellect][Kant Free Intellect]]"))
 (defun cpm/load-zettelkasten ()
     (interactive)
     (persp-mode 1)
     (nameframe-create-frame "Zettelkasten")
     (toggle-frame-maximized)
     (zd-deft-new-search))

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



(provide 'setup-projects)
