;; Workspaces -*- lexical-binding: t; -*-
;; Leverage tab-bar.el and project.el to create workspaces
;; NOTE that much of this code is from or is inspired by
;; https://github.com/kaz-yos/emacs and
;; https://github.com/wamei/elscreen-separate-buffer-list/issues/8 and
;; https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/ and
;; https://github.com/minad/consult#multiple-sources

;;; Buffer Workspaces
(defvar cpm-workspace-create-permitted-buffer-names
  '("*scratch*")
  "List of buffer names kept by `cpm/workspace-create'.")

(defun cpm/workspace-create (&optional arg)
  "Create a new tab/workspace with cleaned buffer lists.

ARG is directly passed to `tab-bar-new-tab'.
Only buffers in `cpm--workspace-create-permitted-buffer-names'
are kept in the `buffer-list' and `buried-buffer-list'.
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

;;;; Group Buffers By Tab
;; tab-bar version of separate buffer list filter
;; See https://github.com/wamei/elscreen-separate-buffer-list/issues/8
;; https://github.com/kaz-yos/emacs/blob/master/init.d/200_tab-related.el#L74-L87

(defun cpm--tab-bar-buffer-name-filter (buffer-names)
  "Filter BUFFER-NAMES by the current tab's buffer list
It should be used to filter a list of buffer names created by
other functions, such as `helm-buffer-list'."
  (let ((buffer-names-to-keep
         ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-List.html
         (append (mapcar #'buffer-name (alist-get 'wc-bl (tab-bar--tab)))
                 (mapcar #'buffer-name (alist-get 'wc-bbl (tab-bar--tab))))))
    (seq-filter (lambda (elt)
                  (member elt buffer-names-to-keep))
                buffer-names)))


;;;; Filter Buffers for Switch-to-Buffer

(advice-add #'internal-complete-buffer :filter-return #'cpm--tab-bar-buffer-name-filter)

;;;; Filter Buffers for Consult-Buffer

(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b")
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
  ;; Add *Messages* and *splash* to Tab \`Home\'
  (progn
    (cpm/workspace-create)
    (tab-bar-rename-tab "Home")
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

;;;; Open Agenda as Workspace
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

;;;; Open emacs.d in Workspace
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

;;;; Open Notes in Workspace

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
(defun cpm/vterm-workspace ()
  "Open vterm in home dir in its own workspace"
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
      (cpm/vterm-workspace)
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
