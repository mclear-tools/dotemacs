;; Project Management This project workflow primarily uses a single frame with
;; different workspaces, made possible by projectile, persp-mode, and eyebrowse.
;; Workflow is: open a project (see `cpm/open-existing-project-and-workspace`) and
;; use as many workspaces (via eyebrowse) as you need in that project. You can also
;; create a new buffer in a new perspective using a dummy git project and create a
;; new project entirely
;; See also https://raw.githubusercontent.com/seagle0128/.emacs.d/master/lisp/init-persp.el
;; and https://github.com/yanghaoxie/emacs.d#workspaces

;;; Projectile
(use-package projectile
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
  (setq projectile-git-submodule-command nil
        projectile-current-project-on-switch 'move-to-end))
  ;; (setq projectile-mode-line-prefix " P:")
  ;; (setq projectile-dynamic-mode-line t))

  (add-hook 'projectile-after-switch-project-hook (lambda ()
                                                    (projectile-invalidate-cache nil)))


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
  (cpm/leader-keys
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
  ("r" eyebrowse-rename-window-config)
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
        persp-add-buffer-on-after-change-major-mode t
        persp-nil-name "default"
        persp-kill-foreign-buffer-behaviour 'kill
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-autokill-persp-when-removed-last-buffer t
        persp-autokill-buffer-on-remove 'kill
        persp-save-dir (expand-file-name "persp-confs/" cpm-cache-dir))
  ;; persp-common-buffer-filter-functions
  ;; (list #'(lambda (b)
  ;;           "Ignore temporary buffers."
  ;;           (or (string-prefix-p " " (buffer-name b))
  ;;               (and (string-prefix-p "*" (buffer-name b))
  ;;                    (not (string-equal "*scratch*" (buffer-name b))))
  ;;               (string-prefix-p "magit" (buffer-name b))
  ;;               (string-prefix-p "Pfuture-Callback" (buffer-name b))
  ;;               (eq (buffer-local-value 'major-mode b) 'nov-mode)
  ;;               (eq (buffer-local-value 'major-mode b) 'vterm-mode)))))

  ;; fix for (void-function make-persp-internal) error
  ;; NOTE: Redefine `persp-add-new' to address.
  ;; Issue: Unable to create/handle persp-mode
  ;; https://github.com/Bad-ptr/persp-mode.el/issues/96
  ;; https://github.com/Bad-ptr/persp-mode-projectile-bridge.el/issues/4
  ;; https://emacs-china.org/t/topic/6416/7
  (cl-defun persp-add-new (name &optional (phash *persp-hash*))
    "Create a new perspective with the given `NAME'. Add it to `PHASH'.
   Return the created perspective."
    (interactive "sA name for the new perspective: ")
    (if (and name (not (equal "" name)))
        (cl-destructuring-bind (e . p)
            (persp-by-name-and-exists name phash)
          (if e p
            (setq p (if (equal persp-nil-name name)
                        nil (make-persp :name name)))
            (persp-add p phash)
            (run-hook-with-args 'persp-created-functions p phash)
            p))
      (message "[persp-mode] Error: Can't create a perspective with empty name.")
      nil)))

;; ;; Integrate Ivy
;; ;; https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-ivy-el
;; (with-eval-after-load 'ivy
;;   (add-to-list 'ivy-ignore-buffers
;;                #'(lambda (b)
;;                    (when persp-mode
;;                      (let ((persp (get-current-persp)))
;;                        (if persp
;;                            (not (persp-contain-buffer-p b persp))
;;                          nil)))))

;;   (setq ivy-sort-functions-alist
;;         (append ivy-sort-functions-alist
;;                 '((persp-kill-buffer   . nil)
;;                   (persp-remove-buffer . nil)
;;                   (persp-add-buffer    . nil)
;;                   (persp-switch        . nil)
;;                   (persp-window-switch . nil))))))

(use-package persp-mode-projectile-bridge
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
;;;; Open agenda as workspace
(defun cpm/open-agenda-in-workspace ()
  "open agenda in its own perspective"
  (interactive)
  (if (get-buffer "*Org Agenda*")
      (progn
        (persp-switch "agenda")
        ;; (eyebrowse-switch-to-window-config-1)
        (switch-to-buffer "*Org Agenda*")
        (org-agenda-redo)
        (delete-other-windows))
    (progn
      (persp-switch "agenda")
      ;; (setq frame-title-format '("" "%b"))
      (require 'org)
      (require 'org-super-agenda)
      (cpm/jump-to-org-super-agenda)
      (persp-add-buffer "*Org Agenda*"))))

(general-define-key
 :states '(insert normal motion emacs)
 :keymaps 'override
 "s-1" 'cpm/open-agenda-in-workspace)

;;;; Open emacs.d in workspace
(defun cpm/open-emacsd-in-workspace ()
  "open emacsd in its own perspective"
  (interactive)
  (if (get-buffer "init.el")
      (persp-switch "emacs.d")
    (persp-switch "emacs.d")
    ;; (setq frame-title-format
    ;;       '(""
    ;;         "%b"
    ;;         (:eval
    ;;          (let ((project-name (projectile-project-name)))
    ;;            (unless (string= "-" project-name)
    ;;              (format " in [%s]" project-name))))))
    (find-file-other-window user-init-file)
    (require 'magit)
    (magit-status-setup-buffer)))

(general-define-key
 :states '(insert normal motion emacs)
 :keymaps 'override
 "s-2" 'cpm/open-emacsd-in-workspace)

;;;; Open Notes in workspace
(defun cpm/open-notes-in-workspace ()
  "open notes dir in its own perspective"
  (interactive)
  (if (get-buffer "*Deft*")
      (persp-switch "Notes")
    (persp-switch "Notes")
    ;; (setq frame-title-format
    ;;       '(""
    ;;         "%b"
    ;;         (:eval
    ;;          (let ((project-name (projectile-project-name)))
    ;;            (unless (string= "-" project-name)
    ;;              (format " in [%s]" project-name))))))
    (cpm/notebook))
  (persp-add-buffer "*Deft*"))

(general-define-key
 :states '(insert normal motion emacs)
 :keymaps 'override
 "s-3" 'cpm/open-notes-in-workspace)

;;;; Terminal Workspace
(defun cpm/vterm-home ()
  (interactive)
  (let ((default-directory "~/"))
    (require 'multi-vterm)
    (multi-vterm-next)))

(defun cpm/open-new-terminal-and-workspace ()
  "open an empty buffer in its own perspective"
  (interactive)
  (if (get-buffer "*vterminal<1>*")
      (persp-switch "Terminal")
    (persp-switch "Terminal"))
  (evil-set-initial-state 'vterm-mode 'insert)
  (cpm/vterm-home)
  (delete-other-windows)
  (persp-add-buffer "*vterminal<1>*")
  ;; (setq frame-title-format '("" "%b"))
  )

(general-define-key
 :states '(insert normal motion emacs)
 :keymaps 'override
 "s-4" 'cpm/open-new-terminal-and-workspace)

;;;; Open Mu4e Email in Workspace
(defun cpm/open-email-in-workspace ()
  "open agenda in its own perspective"
  (interactive)
  (if (get-buffer "*mu4e-main*")
      (progn
        (persp-switch "Email")
        ;; (eyebrowse-switch-to-window-config-1)
        (switch-to-buffer "*mu4e-main*")
        (delete-other-windows))
    (progn
      (persp-switch "Email")
      ;; (setq frame-title-format '("" "%b"))
      (mu4e)
      (persp-add-buffer "*mu4e-main*"))))

(general-define-key
 :states '(insert normal motion emacs)
 :keymaps 'override
 "s-5" 'cpm/open-email-in-workspace)


;;;; Open New Buffer in Workspace
;; This function is a bit weird; It creates a new buffer in a new workspace with a
;; dummy git project to give the isolation of buffers typical with a git project
;; I'm sure there is a more elegant way to do this but I don't know how :)
(defun cpm/open-new-buffer-and-workspace ()
  "open an empty buffer in its own perspective"
  (interactive)
  (eyebrowse-switch-to-window-config-0)
  (persp-switch "new-persp")
  (let ((cpm-project-temp-dir "/tmp/temp-projects/"))
    (progn
      (when (not (file-exists-p cpm-project-temp-dir))
        (make-directory cpm-project-temp-dir t))
      (when (not (file-exists-p (concat cpm-project-temp-dir ".git/")))
        (magit-init cpm-project-temp-dir))
      (when (not (file-exists-p (concat cpm-project-temp-dir "temp")))
        (with-temp-buffer (write-file (concat cpm-project-temp-dir "temp")))))
    (setq default-directory cpm-project-temp-dir)
    (find-file (concat cpm-project-temp-dir "temp"))
    (persp-add-buffer "*scratch*")
    ;; (setq frame-title-format '("" "%b")))
    ))


;;;; Open Project in New Workspace
(defun cpm/open-existing-project-and-workspace ()
  "open a project as its own perspective"
  (interactive)
  (persp-switch "new-persp")
  (projectile-switch-project)
  ;; (setq frame-title-format
  ;;       '(""
  ;;         "%b"
  ;;         (:eval
  ;;          (let ((project-name (projectile-project-name)))
  ;;            (unless (string= "-" project-name)
  ;;              (format " in [%s]" project-name))))))
  ;; (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) (projectile-project-name))
  (require 'magit)
  (magit-status-setup-buffer)
  (persp-rename (projectile-project-name)))

;;;; Open & Create New Project in New Workspace
;; Create a new git project in its own perspective & workspace and create some useful
;; files
(defun cpm/create-new-project-and-workspace ()
  "create & open a project as its own perspective"
  (interactive)
  ;; (eyebrowse-switch-to-window-config-1)
  (persp-switch "new-project")
  (cpm/git-new-project)
  ;; (setq frame-title-format
  ;;       '(""
  ;;         "%b"
  ;;         (:eval
  ;;          (let ((project-name (projectile-project-name)))
  ;;            (unless (string= "-" project-name)
  ;;              (format " in [%s]" project-name))))))
  ;; (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) (projectile-project-name))
  (delete-other-windows)
  (find-file ".gitignore")
  (find-file "project-todo.org")
  (magit-status-setup-buffer))


;;;; Eyebrowse & Perspectives
;; courtesy of Spacemacs
;; Eyebrowse - allow perspective-local eyebrowse workspaces --------------------------
;; See https://github.com/syl20bnr/spacemacs/issues/3733
;; and https://github.com/syl20bnr/spacemacs/pull/5874

(defun cpm/load-eyebrowse-for-perspective (type &optional frame)
  "Load an eyebrowse workspace according to a perspective's parameters.
 FRAME's perspective is the perspective that is considered, defaulting to
 the current frame's perspective.
 If the perspective doesn't have a workspace, create one."
  (when (eq type 'frame)
    (let* ((persp (get-current-persp frame))
           (window-configs (persp-parameter 'eyebrowse-window-configs persp))
           (current-slot (persp-parameter 'eyebrowse-current-slot persp))
           (last-slot (persp-parameter 'eyebrowse-last-slot persp)))
      (if window-configs
          (progn
            (eyebrowse--set 'window-configs window-configs frame)
            (eyebrowse--set 'current-slot current-slot frame)
            (eyebrowse--set 'last-slot last-slot frame)
            (eyebrowse--load-window-config current-slot))
        (eyebrowse--set 'window-configs nil frame)
        (eyebrowse-init frame)
        (cpm/save-eyebrowse-for-perspective frame)))))

(defun cpm/update-eyebrowse-for-perspective (_new-persp-name _frame)
  "Update and save current frame's eyebrowse workspace to its perspective.
 Parameter _NEW-PERSP-NAME is ignored, and exists only for compatibility with
 `persp-before-switch-functions'."
  (eyebrowse--update-window-config-element
   (eyebrowse--current-window-config (eyebrowse--get 'current-slot)
                                     (eyebrowse--get 'current-tag)))
  (cpm/save-eyebrowse-for-perspective))

(defun cpm/save-eyebrowse-for-perspective (&optional frame)
  "Save FRAME's eyebrowse workspace to FRAME's perspective.
 FRAME defaults to the current frame."
  (let ((persp (get-current-persp frame)))
    (set-persp-parameter
     'eyebrowse-window-configs (eyebrowse--get 'window-configs frame) persp)
    (set-persp-parameter
     'eyebrowse-current-slot (eyebrowse--get 'current-slot frame) persp)
    (set-persp-parameter
     'eyebrowse-last-slot (eyebrowse--get 'last-slot frame) persp)))

(add-hook 'persp-before-switch-functions #'cpm/update-eyebrowse-for-perspective)
(add-hook 'eyebrowse-post-window-switch-hook #'cpm/save-eyebrowse-for-perspective)
(add-hook 'persp-activated-functions #'cpm/load-eyebrowse-for-perspective)


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
