;; Workspaces leveraging tab-bar and project -*- lexical-binding: t; -*-

;;; Workspaces

(use-package emacs-workspaces
  :straight (:local-repo "/Users/roambot/.emacs.d/.local/elisp/emacs-workspaces/")
  :bind (:map project-prefix-map
         ("p" . emacs-workspaces/open-existing-project-and-workspace)
         ("n" . emacs-workspaces/create-new-project-and-workspace))
  :commands (emacs-workspaces/create-workspace
             emacs-workspaces/create-new-project-and-workspace
             emacs-workspaces/open-existing-project-and-workspace)
  :config
  (setq emacs-workspaces-use-consult-project t))

;;;; Consult Buffer Filtering
;; Filter Buffers for Consult-Buffer

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
                      (emacs-workspaces--tab-bar-buffer-name-filter ((lambda () (consult--buffer-query :sort 'visibility
                                                                                                  :as #'buffer-name))))))

    "Set workspace buffer list for consult-buffer.")
  (push consult--source-workspace consult-buffer-sources))

;; This uses consult-project to open project workspace instead
;; (defun emacs-workspaces/consult-open-existing-project-and-workspace ()
;;   "Using consult, open an existing project as its own workspace"
;;   (interactive)
;;   (progn
;;     (emacs-workspaces/create-workspace)
;;     (consult-project)
;;     (tab-bar-rename-tab (emacs-workspaces--name-tab-by-project-or-default))))

;;; Workspace Functions

;;;; Startup Workspaces
(defun cpm--workspace-setup ()
  "Set up worksapce at startup."
  ;; Add *Messages* and *splash* to Tab \`Home\'
  (progn
    (emacs-workspaces/create-workspace)
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
    (emacs-workspaces/create-workspace)
    (call-interactively 'project-switch-project-open-file)
    (tab-bar-rename-tab (emacs-workspaces--name-tab-by-project-or-default))
    (project-magit-dir)))

;;;; Open Agenda as Workspace
(defun cpm/open-agenda-in-workspace ()
  "Open agenda in its own workspace"
  (interactive)
  (if (member "Agenda" (emacs-workspaces--list-workspaces))
      (progn
        (tab-bar-switch-to-tab "Agenda")
        (switch-to-buffer "*Org Agenda*")
        (org-agenda-redo)
        (delete-other-windows))
    (progn
      (emacs-workspaces/create-workspace)
      (tab-bar-rename-tab "Agenda")
      (require 'org)
      (require 'org-super-agenda)
      (cpm/jump-to-org-super-agenda))))

(bind-key* "s-1" 'cpm/open-agenda-in-workspace)

;;;; Open emacs.d in Workspace
(defun cpm/open-emacsd-in-workspace ()
  "Open emacs.d in its own workspace"
  (interactive)
  (if (member "emacs.d" (emacs-workspaces--list-workspaces))
      (tab-bar-switch-to-tab "emacs.d")
    (progn
      (emacs-workspaces/create-workspace)
      (tab-bar-rename-tab "emacs.d")
      (find-file-other-window user-init-file)
      (project-magit-dir))))

(bind-key* "s-2" 'cpm/open-emacsd-in-workspace)

;;;; Open Notes in Workspace

(defun cpm/open-notes-in-workspace ()
  "Open notes dir in its own workspace"
  (interactive)
  (if (member "Notes" (emacs-workspaces--list-workspaces))
      (tab-bar-switch-to-tab "Notes")
    (progn
      (emacs-workspaces/create-workspace)
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
  (if (member "Terminal" (emacs-workspaces--list-workspaces))
      (tab-bar-switch-to-tab "Terminal")
    (progn
      (emacs-workspaces/create-workspace)
      (tab-bar-rename-tab "Terminal")
      (cpm/vterm-workspace)
      (delete-other-windows))))

(bind-key* "s-4" 'cpm/open-new-terminal-and-workspace)

;;;; Open Mu4e Email in Workspace
(defun cpm/open-email-in-workspace ()
  "Open mu4e email in its own perspective"
  (interactive)
  (if (member "Email" (emacs-workspaces--list-workspaces))
      (progn
        (tab-bar-switch-to-tab "Email")
        (cond ((get-buffer " *mu4e-main*")
               (switch-to-buffer " *mu4e-main*"))
              ((get-buffer "*mu4e-headers*")
               (switch-to-buffer "*mu4e-headers*"))
              (t (mu4e))))
    (progn
      (emacs-workspaces/create-workspace)
      (tab-bar-rename-tab "Email")
      (find-file (concat org-directory "mail.org"))
      (mu4e)
      (switch-to-buffer " *mu4e-main*"))))

(bind-key* "s-5" 'cpm/open-email-in-workspace)

;;;; Open New Buffer & Workspace
;; This function is a bit weird; It creates a new buffer in a new workspace with a
;; dummy git project to give the isolation of buffers typical with a git project
;; I'm sure there is a more elegant way to do this but I don't know how :)
(defun cpm/open-new-buffer-and-workspace ()
  "Open an empty buffer in its own workspace"
  (interactive)
  (emacs-workspaces/create-workspace)
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
    (find-file (concat cpm-project-temp-dir "temp"))))

(bind-key "N" #'cpm/open-new-buffer-and-workspace 'project-prefix-map)

;;; Provide
(provide 'setup-workspaces)