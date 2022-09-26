;;;; Workspace Functions

;;;;; Startup Workspaces
(defun cpm--workspace-setup ()
  "Set up workspace at startup."
  ;; Add *Messages* and *splash* to Tab \`Home\'
  (progn
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

(add-hook 'emacs-startup-hook #'cpm--workspace-setup)

(defun cpm-go-home ()
  "Go to home tab."
  (interactive)
  (tab-bar-switch-to-tab "Home"))

;;;;; Open Project in New Workspace
(defun cpm-open-existing-project-and-workspace ()
  "Open a project as its own workspace"
  (interactive)
  (progn
    (tab-bar-new-tab)
    (call-interactively 'project-switch-project-open-file)
    (tab-bar-rename-tab (tabspaces--name-tab-by-project-or-default))
    (project-magit-dir)))

;;;;; Open Agenda as Workspace
(defun cpm-open-agenda-in-workspace ()
  "Open agenda in its own workspace"
  (interactive)
  (if (member "Agenda" (tabspaces--list-tabspaces))
      (progn
        (tab-bar-switch-to-tab "Agenda")
        (switch-to-buffer "*Org Agenda*")
        (org-agenda-redo)
        (delete-other-windows))
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "Agenda")
      (require 'org)
      (require 'mu4e)
      (lem-jump-to-org-dashboard))))

;;;;; Open emacs.d in Workspace
(defun cpm-open-emacsd-in-workspace ()
  "Open emacs.d in its own workspace"
  (interactive)
  (if (member "emacs.d" (tabspaces--list-tabspaces))
      (tab-bar-switch-to-tab "emacs.d")
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "emacs.d")
      (find-file lem-config-file)
      (split-window-right)
      (other-window 1)
      (project-magit-dir))))

;;;;; Open Notes in Workspace

(defun cpm-open-notes-in-workspace ()
  "Open notes dir in its own workspace"
  (interactive)
  (if (member "Notes" (tabspaces--list-tabspaces))
      (tab-bar-switch-to-tab "Notes")
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "Notes")
      (dired lem-notes-dir)
      (lem-notebook))))

;;;;; Elfeed Workspace
(defun cpm-open-elfeed-in-workspace ()
  "Open Elfeed in its own workspace."
  (interactive)
  (cond ((member "Elfeed" (tabspaces--list-tabspaces))
         (tab-bar-switch-to-tab "Elfeed"))
        (t
         (tab-bar-new-tab)
         (tab-bar-rename-tab "Elfeed")
         (elfeed)
         (elfeed-update))))

;;;;; Terminal Workspace
(defun cpm-vterm-workspace ()
  "Open vterm in home dir in its own workspace"
  (interactive)
  (let ((default-directory "~/"))
    (require 'multi-vterm)
    (multi-vterm-next)))

(defun cpm-open-new-terminal-and-workspace ()
  "Open an empty buffer in its own workspace"
  (interactive)
  (if (member "Terminal" (tabspaces--list-tabspaces))
      (tab-bar-switch-to-tab "Terminal")
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "Terminal")
      (cpm-vterm-workspace)
      (delete-other-windows))))

;;;;; Eshell Workspace
(defun cpm-open-new-eshell-and-workspace ()
  "Open an empty buffer in its own workspace"
  (interactive)
  (let ((popper--reference-modes '(vterm-mode term-mode shell-mode compilation-mode help-mode))
        (popper--reference-names '("^\\*vterm.*\\*$" "^\\*term.*\\*$" "^\\*shell.*\\*$" "\\*Async Shell Command\\*" "Output\\*$" "\\*Messages\\*")))
    (if (member "Eshell" (tabspaces--list-tabspaces))
        (tab-bar-switch-to-tab "Eshell")
      (progn
        (tab-bar-new-tab)
        (tab-bar-rename-tab "Eshell")
        (lem-eshell-home)
        (rename-buffer "eshell-workspace")
        (delete-other-windows)))))

;;;;; Open Mu4e Email in Workspace
(defun cpm-open-email-in-workspace ()
  "Open mu4e email in its own workspace"
  (interactive)
  (cond ((member "Email" (tabspaces--list-tabspaces))
         (tab-bar-switch-to-tab "Email")
         (cond ((get-buffer "*mu4e-headers*")
                (switch-to-buffer "*mu4e-headers*"))
               ((get-buffer " *mu4e-main*")
                (switch-to-buffer " *mu4e-main*")
                (delete-other-windows))
               (t (mu4e))))
        (t
         (tab-bar-new-tab)
         (tab-bar-rename-tab "Email")
         (require 'org) ; need this for loading?
         (find-file (concat org-directory "mail.org"))
         (mu4e)
         (switch-to-buffer " *mu4e-main*"))))

;;;;; Open New Buffer & Workspace
;; This function is a bit weird; It creates a new buffer in a new workspace with a
;; dummy git project to give the isolation of buffers typical with a git project
;; I'm sure there is a more elegant way to do this but I don't know how :)
(defun cpm-open-new-buffer-and-workspace ()
  "Open an empty buffer in its own workspace"
  (interactive)
  (tab-bar-new-tab)
  (tab-bar-rename-tab-tab "New project")
  (let ((lem-project-temp-dir "/tmp/temp-projects/"))
    (progn
      (when (not (file-exists-p lem-project-temp-dir))
        (make-directory lem-project-temp-dir t))
      (when (not (file-exists-p (concat lem-project-temp-dir ".git/")))
        (magit-init lem-project-temp-dir))
      (when (not (file-exists-p (concat lem-project-temp-dir "temp")))
        (with-temp-buffer (write-file (concat lem-project-temp-dir "temp")))))
    (setq default-directory lem-project-temp-dir)
    (find-file (concat lem-project-temp-dir "temp"))))

;;;; Workspace Hooks
;; (advice-add 'tab-bar-rename-tab :after #'lem-tabs--create-scratch-tab)
(defun lem-tabs--create-scratch-tab ()
  "Create a scratch buffer for every tab workspace."
  (get-buffer-create (concat "*scratch-" (string-trim (tabspaces--name-tab-by-project-or-default) "[\\*]" "[\\*]") "*")))

;;;; Workspace Shortcuts (Splash)
;; Rebind splash keys to personal functions
(with-eval-after-load 'lem-setup-splash
  (bind-keys :map lem-splash-mode-map
    ("a" . cpm-open-agenda-in-workspace)
    ("c" . cpm-open-emacsd-in-workspace)
    ("e" . cpm-open-elfeed-in-workspace)
    ("m" . cpm-open-email-in-workspace)
    ("n" . cpm-open-notes-in-workspace)
    ("p" . cpm-open-existing-project-and-workspace)
    ("q" . lem-splash-screen-bury)
    ("[esc]" . lem-splash-screen-bury)
    ("k" . lem-splash-screen-kill)))

;;;; Workspace Keybindings

;; See user config for keybinds
(bind-key "N" #'cpm-open-new-buffer-and-workspace 'project-prefix-map)

;;;;; Mail Keybindings
(bind-keys :prefix-map lem+mail-keys
           :prefix (concat lem-prefix " m")
           ("a" . mu4e-view-save-attachment-multi)
           ("c" . mu4e-compose-new           )
           ("e" . lem-email-save-and-kill    )
           ("i" . lem-go-to-mail-inbox       )
           ("k" . lem-mu-kill-server         )
           ("m" . cpm-open-email-in-workspace)
           ("s" . mu4e-update-mail-and-index )
           ("S" . lem-swiftbar-email-update  )
           ("u" . lem-go-to-mail-unread      ))



(provide 'cpm-setup-workspaces)
