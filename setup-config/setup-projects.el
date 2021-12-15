;; Project Management This project workflow uses a single frame with
;; different workspaces, using primarily only built-in packages such as project.el and tab-bar.el

;;; Project
;; Use project to switch to, and search in, projects (replaces projectile)
(use-package project
  :straight (:type built-in)
  :commands (project-find-file project-switch-to-buffer project-switch-project)
  :config
  (setq project-list-file (concat cpm-cache-dir "projects")))

(defun cpm-project-name ()
  "return name of project without path"
  (file-name-nondirectory (directory-file-name (if (vc-root-dir) (vc-root-dir) "-"))))

(defun project-magit-dir ()
  "Run magit in the current project's root"
  (interactive)
  (magit-status))

;; Add to keymap
(define-key (current-global-map) (kbd "C-x p G") #'project-magit-dir)

;;; Tab Bar
;; Use tab-bar for window grouping and configuration within a project (replaces eyebrowse)
(use-package tab-bar
  :straight (:type built-in)
  :commands (tab-bar-switch-to-tab tab-bar-new-tab)
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-show nil)
  :config
  (setq tab-bar-tab-name-function #'cpm/name-tab-by-project-or-default)
  (setq tab-bar-mode t))

;; See https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/
;; I've modified it to use project instead of projectile
(defun cpm/name-tab-by-project-or-default ()
  "Return project name if in a project, or default tab-bar name if not.
The default tab-bar name uses the buffer name."
  (if (string= "-" (cpm-project-name))
      (tab-bar-tab-name-current)
    (cpm-project-name)))

;; Get the current tab name for use in some other display
(defun cpm-current-tab-name ()
  (alist-get 'name (tab-bar--current-tab)))

;;; Project Functions
;;;; Open agenda as workspace
(defun cpm/open-agenda-in-workspace ()
  "open agenda in its own tab"
  (interactive)
  (if (get-buffer "*Org Agenda*")
      (progn
        (tab-bar-switch-to-tab "Agenda")
        (switch-to-buffer "*Org Agenda*")
        (org-agenda-redo)
        (delete-other-windows))
    (progn
      (tab-bar-switch-to-tab "Agenda")
      (require 'org)
      (require 'org-super-agenda)
      (cpm/jump-to-org-super-agenda))))

(general-define-key
 ;; :states '(emacs)
 :keymaps 'override
 "s-1" 'cpm/open-agenda-in-workspace)

;;;; Open emacs.d in workspace
(defun cpm/open-emacsd-in-workspace ()
  "open emacsd in its own tab"
  (interactive)
  (if (get-buffer "init.el")
      (tab-bar-switch-to-tab "emacs.d")
    (progn
      (tab-bar-switch-to-tab "emacs.d")
      (find-file-other-window user-init-file)
      (project-magit-dir))))

(general-define-key
 :keymaps 'override
 "s-2" 'cpm/open-emacsd-in-workspace)

;;;; Open Notes in workspace
(defun cpm/open-notes-in-workspace ()
  "open notes dir in its own tab"
  (interactive)
  (if (get-buffer "content-org")
      (tab-bar-switch-to-tab "Notes")
    (progn
      (tab-bar-switch-to-tab "Notes")
      (cpm/notebook))))

(general-define-key
 :keymaps 'override
 "s-3" 'cpm/open-notes-in-workspace)

;;;; Terminal Workspace
(defun cpm/vterm-home ()
  (interactive)
  (let ((default-directory "~/"))
    (require 'multi-vterm)
    (multi-vterm-next)))

(defun cpm/open-new-terminal-and-workspace ()
  "open an empty buffer in its own tab"
  (interactive)
  (if (get-buffer "*vterminal<1>*")
      (tab-bar-switch-to-tab "Terminal")
    (progn
      (tab-bar-switch-to-tab "Terminal")
      (cpm/vterm-home)
      (delete-other-windows))))

(general-define-key
 :keymaps 'override
 "s-4" 'cpm/open-new-terminal-and-workspace)

;;;; Open New Buffer & Workspace
;; This function is a bit weird; It creates a new buffer in a new workspace with a
;; dummy git project to give the isolation of buffers typical with a git project
;; I'm sure there is a more elegant way to do this but I don't know how :)
(defun cpm/open-new-buffer-and-workspace ()
  "open an empty buffer in its own tab"
  (interactive)
  (tab-bar-switch-to-tab "New project")
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
    ))


;;;; Open Project in New Workspace
(defun cpm/open-existing-project-and-workspace ()
  "open a project as its own tab"
  (interactive)
  (progn
    (tab-bar-new-tab)
    (call-interactively 'project-switch-project)
    (tab-bar-rename-tab (cpm-project-name))
    (project-magit-dir)))


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

;;; Bookmarks
(use-package bookmark
  :straight (:type built-in)
  :defer 2
  :config
  (setq bookmark-default-file (concat cpm-cache-dir "bookmarks")))

(use-package bookmark+
  :commands (bmkp-switch-bookmark-file-create bmkp-set-desktop-bookmark)
  :config
  (setq bmkp-last-as-first-bookmark-file (concat cpm-cache-dir "bookmarks")))

;;; New Git Project
(defun cpm/git-new-project ()
  "Initializes a new git repo and adds it to projectile's known projects."
  (interactive)
  (let ((project-dir (expand-file-name
                      (read-directory-name "New project root:"))))
    (magit-init project-dir)
    (projectile-add-known-project project-dir)
    (setq default-directory project-dir)))

;;; Goto Projects
(defun cpm/goto-projects ()
  "Open projects dir"
  (interactive)
  (find-file "~/Dropbox/Work/projects"))

;;; End Projects.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-projects)
