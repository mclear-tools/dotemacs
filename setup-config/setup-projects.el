;; Project Management This project workflow primarily uses a single frame with
;; different workspaces, made possible by projectile, persp-mode, and eyebrowse.
;; Workflow is: open a project (see `cpm/open-project-and-workspace`) and use as many
;; workspaces (via eyebrowse) as you need in that project See also
;; https://raw.githubusercontent.com/seagle0128/.emacs.d/master/lisp/init-persp.el
;; and https://github.com/yanghaoxie/emacs.d#workspaces

;;; Projectile
(use-package projectile
  :ensure t
  :hook (after-init . projectile-mode)
  :init
  ;; save projectile-known-projects-file in cache folder
  (setq projectile-known-projects-file
        (concat cpm-cache-dir "projectile-bookmarks.eld"))
  (setq projectile-cache-file
        (concat cpm-cache-dir "projectile.cache"))
  (setq projectile-enable-caching t
        projectile-files-cache-expire 60)
  :config
  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))
  (setq projectile-git-submodule-command nil))


;;; Eyebrowse
(use-package eyebrowse
  :hook (after-init . eyebrowse-mode)
  :general
  (:keymaps 'override
   :states '(normal visual motion)
   "gt" 'eyebrowse-next-window-config
   "gT" 'eyebrowse-prev-window-config
   "gc" 'eyebrowse-create-window-config
   "gd" 'eyebrowse-close-window-config
   "gl" 'eyebrowse-last-window-config
   "g0" 'eyebrowse-switch-to-window-config-0
   "g1" 'eyebrowse-switch-to-window-config-1
   "g2" 'eyebrowse-switch-to-window-config-2
   "g3" 'eyebrowse-switch-to-window-config-3
   "g4" 'eyebrowse-switch-to-window-config-4
   "g5" 'eyebrowse-switch-to-window-config-5
   "g6" 'eyebrowse-switch-to-window-config-6
   "g7" 'eyebrowse-switch-to-window-config-7
   "g8" 'eyebrowse-switch-to-window-config-8
   "g9" 'eyebrowse-switch-to-window-config-9)
  (my/leader-keys
    "w." 'hydra-eyebrowse/body
    "ww" 'eyebrowse-switch-to-window-config
    "wr" 'eyebrowse-rename-window-config)
  :config
  (setq eyebrowse-new-workspace 'dired-jump
        eyebrowse-mode-line-style 'hide
        eyebrowse-wrap-around t
        eyebrowse-switch-back-and-forth t)
  (custom-set-faces '(eyebrowse-mode-line-active ((nil))))
  (eyebrowse-mode))

(defhydra hydra-eyebrowse (:hint nil)
  "
 Go to^^^^^^                         Actions^^
 [_0_.._9_]^^     nth/new workspace  [_d_] close current workspace
 [_C-0_.._C-9_]^^ nth/new workspace
 [_<tab>_]^^^^    last workspace     [_q_] quit
 [_c_/_C_]^^      create workspace
 [_n_/_C-l_]^^    next workspace
 [_N_/_p_/_C-h_]  prev workspace
 [_w_]^^^^        workspace w/helm/ivy\n"
  ("0" eyebrowse-switch-to-window-config-0 :exit t)
  ("1" eyebrowse-switch-to-window-config-1 :exit t)
  ("2" eyebrowse-switch-to-window-config-2 :exit t)
  ("3" eyebrowse-switch-to-window-config-3 :exit t)
  ("4" eyebrowse-switch-to-window-config-4 :exit t)
  ("5" eyebrowse-switch-to-window-config-5 :exit t)
  ("6" eyebrowse-switch-to-window-config-6 :exit t)
  ("7" eyebrowse-switch-to-window-config-7 :exit t)
  ("8" eyebrowse-switch-to-window-config-8 :exit t)
  ("9" eyebrowse-switch-to-window-config-9 :exit t)
  ("C-0" eyebrowse-switch-to-window-config-0)
  ("C-1" eyebrowse-switch-to-window-config-1)
  ("C-2" eyebrowse-switch-to-window-config-2)
  ("C-3" eyebrowse-switch-to-window-config-3)
  ("C-4" eyebrowse-switch-to-window-config-4)
  ("C-5" eyebrowse-switch-to-window-config-5)
  ("C-6" eyebrowse-switch-to-window-config-6)
  ("C-7" eyebrowse-switch-to-window-config-7)
  ("C-8" eyebrowse-switch-to-window-config-8)
  ("C-9" eyebrowse-switch-to-window-config-9)
  ("<tab>" eyebrowse-last-window-config)
  ("<return>" nil :exit t)
  ("TAB" eyebrowse-last-window-config)
  ("RET" nil :exit t)
  ("c" eyebrowse-create-window-config :exit t)
  ("C" eyebrowse-create-window-config)
  ("C-h" eyebrowse-prev-window-config)
  ("C-l" eyebrowse-next-window-config)
  ("d" eyebrowse-close-window-config)
  ;; ("l" hydra-persp/body :exit t)
  ("n" eyebrowse-next-window-config)
  ("N" eyebrowse-prev-window-config)
  ("p" eyebrowse-prev-window-config)
  ;; ("R" spacemacs/workspaces-ts-rename :exit t)
  ("w" eyebrowse-switch-to-window-config :exit t)
  ("q" nil))


;;; Perspectives
(use-package persp-mode
  :hook (after-init . persp-mode)
  :general
  (:states '(insert normal motion emacs)
   :keymaps 'override
   "s-p" 'persp-switch
   "s-]" 'persp-next
   "s-[" 'persp-prev)
  :config
  (setq persp-reset-windows-on-nil-window-conf t
        persp-set-last-persp-for-new-frames nil
        persp-auto-resume-time -1
        persp-add-buffer-on-after-change-major-mode nil
        persp-nil-name "default"
        persp-autokill-buffer-on-remove t
        persp-save-dir (expand-file-name "persp-confs/" cpm-cache-dir)
        persp-common-buffer-filter-functions
        (list #'(lambda (b)
                  "Ignore temporary buffers."
                  (or (string-prefix-p " " (buffer-name b))
                      (and (string-prefix-p "*" (buffer-name b))
                           (not (string-equal "*scratch*" (buffer-name b))))
                      (string-prefix-p "magit" (buffer-name b))
                      (string-prefix-p "Pfuture-Callback" (buffer-name b))
                      (eq (buffer-local-value 'major-mode b) 'nov-mode)
                      (eq (buffer-local-value 'major-mode b) 'vterm-mode)))))

  ;; fix for (void-function make-persp-internal) error
  ;; NOTE: Redefine `persp-add-new' to address.
  ;; Issue: Unable to create/handle persp-mode
  ;; https://github.com/Bad-ptr/persp-mode.el/issues/96
  ;; https://github.com/Bad-ptr/persp-mode-projectile-bridge.el/issues/4
  ;; https://emacs-china.org/t/topic/6416/7
  (defun* persp-add-new (name &optional (phash *persp-hash*))
    "Create a new perspective with the given `NAME'. Add it to `PHASH'.
   Return the created perspective."
    (interactive "sA name for the new perspective: ")
    (if (and name (not (equal "" name)))
        (destructuring-bind (e . p)
            (persp-by-name-and-exists name phash)
          (if e p
            (setq p (if (equal persp-nil-name name)
                        nil (make-persp :name name)))
            (persp-add p phash)
            (run-hook-with-args 'persp-created-functions p phash)
            p))
      (message "[persp-mode] Error: Can't create a perspective with empty name.")
      nil))

  ;; Integrate Ivy
  ;; https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-ivy-el
  (with-eval-after-load 'ivy
    (add-to-list 'ivy-ignore-buffers
                 #'(lambda (b)
                     (when persp-mode
                       (let ((persp (get-current-persp)))
                         (if persp
                             (not (persp-contain-buffer-p b persp))
                           nil)))))))

(use-package persp-mode-projectile-bridge
  :ensure t
  :after (persp-mode projectile-mode)
  :hook (persp-mode . persp-mode-projectile-bridge-mode)
  :functions (persp-add-new
              persp-add-buffer
              set-persp-parameter)
  :commands (persp-mode-projectile-bridge-find-perspectives-for-all-buffers
             persp-mode-projectile-bridge-kill-perspectives
             persp-mode-projectile-bridge-add-new-persp
             projectile-project-buffers))


;;; Project Functions
;;;; Open agenda as perspective
(defun cpm/open-agenda-in-workspace ()
  "open agenda in its own perspective"
  (interactive)
  (persp-switch "agenda")
  (setq frame-title-format '("" "%b"))
  (require 'org-super-agenda)
  (cpm/jump-to-org-super-agenda)
  (persp-add-buffer "*Org Agenda*"))

(general-define-key
 :states '(insert normal motion emacs)
 :keymaps 'override
 "s-1" 'cpm/open-agenda-in-workspace)

;;;; Open emacs.d in workspace
(defun cpm/open-emacsd-in-workspace ()
  "open emacsd in its own perspective"
  (interactive)
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
  (require 'magit)
  (magit-status-setup-buffer))

(general-define-key
 :states '(insert normal motion emacs)
 :keymaps 'override
 "s-2" 'cpm/open-emacsd-in-workspace)

;;;; Open New Project in Workspace
(defun cpm/open-project-and-workspace ()
  "open a new project as its own perspective"
  (interactive)
  (persp-switch "new-persp")
  (counsel-projectile-switch-project)
  (setq frame-title-format
        '(""
          "%b"
          (:eval
           (let ((project-name (projectile-project-name)))
             (unless (string= "-" project-name)
               (format " in [%s]" project-name))))))
  (require 'magit)
  (magit-status-setup-buffer)
  (persp-rename (projectile-project-name)))

;;; Bookmarks
(use-package bookmark
  :defer 2
  :config
  (setq bookmark-default-file (concat cpm-cache-dir "bookmarks")))

(use-package bookmark+
  :commands (bmkp-switch-bookmark-file-create bmkp-set-desktop-bookmark)
  :config
  (setq bmkp-last-as-first-bookmark-file (concat cpm-cache-dir "bookmarks")))


;;; End Projects.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-projects)
