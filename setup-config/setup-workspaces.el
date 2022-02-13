;; Workspaces
;; Leverage tab-bar.el and project.el to create workspaces

;;; Buffer Workspaces
(defvar cpm-workspace-create-permitted-buffer-names
  '("*scratch*")
  "List of buffer names kept by `cpm/workspace-create'.")

(defun cpm/workspace-create (&optional arg)
  "Create a new tab/workspace with cleaned buffer lists.

ARG is directly passed to `tab-bar-new-tab'.
Only buffers in `cpm--workspace-create-permitted-buffer-names'
are kept kept in the `buffer-list' and `buried-buffer-list'.
This is similar to `elscreen-create'."
  (interactive)
  (tab-bar-new-tab arg)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html
  ;; The current-tab uses `buffer-list' and `buried-buffer-list'.
  ;; A hidden tab keeps these as `wc-bl' and `wc-bbl'.
  (set-frame-parameter nil
                       'buffer-list
                       (seq-filter (lambda (buffer)
                                     (member (buffer-name buffer)
                                             cpm-workspace-create-permitted-buffer-names))
                                   (frame-parameter nil 'buffer-list)))
  (set-frame-parameter nil
                       'buried-buffer-list
                       (seq-filter (lambda (buffer)
                                     (member (buffer-name buffer)
                                             cpm-workspace-create-permitted-buffer-names))
                                   (frame-parameter nil 'buried-buffer-list))))

;; NOTE: to clone tab/workspace with all buffers use tab-bar-duplicate-tab

;;; Filter Workspace Buffers
;; filter buffers for switch-to-buffer
(advice-add #'internal-complete-buffer :filter-return #'cpm--tab-bar-buffer-name-filter)

;; filter buffers for consult-buffer
(with-eval-after-load 'consult
  ;; hide full buffer list (available with "b")
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda ()
                      (cpm--tab-bar-buffer-name-filter ((lambda () (consult--buffer-query :sort 'visibility
                                                                                     :as #'buffer-name))))))

    "Set workspace buffer list for consult-buffer.")
  (push consult--source-workspace consult-buffer-sources))


;;; Workspace Functions

;;;; Startup Workspaces
(defun cpm--workspace-setup ()
  "Set up worksapce at startup."
  ;; Add *Messages* to Tab 1 to keep it in all tab
  ;; through `my-workspace-create-permitted-buffer-names'.
  (progn
    (cpm/workspace-create)
    (tab-bar-rename-tab "Default")
    (when (get-buffer "*Messages*")
      (set-frame-parameter nil
                           'buffer-list
                           (cons (get-buffer "*Messages*")
                                 (frame-parameter nil 'buffer-list))))
    (when (get-buffer "*splash*")
      (set-frame-parameter nil
                           'buffer-list
                           (cons (get-buffer "*splash*")
                                 (frame-parameter nil 'buffer-list))))))

(add-hook 'after-init-hook #'cpm--workspace-setup)

;;;; Open Project in New Workspace
(defun cpm/open-existing-project-and-workspace ()
  "Open a project as its own workspace"
  (interactive)
  (progn
    (cpm/workspace-create)
    (call-interactively 'project-switch-project-open-file)
    (tab-bar-rename-tab (cpm/name-tab-by-project-or-default))
    (project-magit-dir)))

;;;; Open agenda as workspace
(defun cpm/open-agenda-in-workspace ()
  "Open agenda in its own workspace"
  (interactive)
  (if (get-buffer "*Org Agenda*")
      (progn
        (tab-bar-switch-to-tab "Agenda")
        (switch-to-buffer "*Org Agenda*")
        (org-agenda-redo)
        (delete-other-windows))
    (progn
      (cpm/workspace-create)
      (tab-bar-rename-tab "Agenda")
      (require 'org)
      (require 'org-super-agenda)
      (cpm/jump-to-org-super-agenda))))

(bind-key* "s-1" 'cpm/open-agenda-in-workspace)

;;;; Open emacs.d in workspace
(defun cpm/open-emacsd-in-workspace ()
  "Open emacs.d in its own workspace"
  (interactive)
  (if (get-buffer "init.el")
      (tab-bar-switch-to-tab "emacs.d")
    (progn
      (cpm/workspace-create)
      (tab-bar-rename-tab "emacs.d")
      (find-file-other-window user-init-file)
      (project-magit-dir))))

(bind-key* "s-2" 'cpm/open-emacsd-in-workspace)

;;;; Open Notes in workspace
(defun cpm/open-notes-in-workspace ()
  "Open notes dir in its own workspace"
  (interactive)
  (if (get-buffer "content-org")
      (tab-bar-switch-to-tab "Notes")
    (progn
      (cpm/workspace-create)
      (tab-bar-rename-tab "Notes")
      (cpm/notebook))))

(bind-key* "s-3" 'cpm/open-notes-in-workspace)

;;;; Terminal Workspace
(defun cpm/vterm-home ()
  "Open vterm in its own workspace"
  (interactive)
  (let ((default-directory "~/"))
    (require 'multi-vterm)
    (multi-vterm-next)))

(defun cpm/open-new-terminal-and-workspace ()
  "Open an empty buffer in its own workspace"
  (interactive)
  (if (get-buffer "*vterminal<1>*")
      (tab-bar-switch-to-tab "Terminal")
    (progn
      (cpm/workspace-create)
      (tab-bar-rename-tab "Terminal")
      (cpm/vterm-home)
      (delete-other-windows))))

(bind-key* "s-4" 'cpm/open-new-terminal-and-workspace)

;;;; Open New Buffer & Workspace
;; This function is a bit weird; It creates a new buffer in a new workspace with a
;; dummy git project to give the isolation of buffers typical with a git project
;; I'm sure there is a more elegant way to do this but I don't know how :)
(defun cpm/open-new-buffer-and-workspace ()
  "Open an empty buffer in its own workspace"
  (interactive)
  (cpm/workspace-create)
  (tab-bar-rename-tab-tab "New project")
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


;;;; Open & Create New Project in New Workspace
;; Create a new git project in its own workspace and create some useful
;; files
(defun cpm/create-new-project-and-workspace ()
  "create & open a project as its own workspace"
  (interactive)
  (cpm/workspace-create)
  (tab-bar-rename-tab "New-project")
  (cpm/git-new-project)
  (delete-other-windows)
  (find-file ".gitignore")
  (find-file "project-todo.org")
  (magit-status-setup-buffer))


;;; Provide workspaces
(provide 'setup-workspaces)
