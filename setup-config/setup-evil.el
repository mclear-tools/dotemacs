;;; Evil Mode

(use-package evil
  :defer .1 ;; don't block emacs when starting, load evil immediately after startup
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (progn
  ;; Cursor shape and color
    (defcustom dotemacs-evil/emacs-cursor
    "red"
    "The color of the cursor when in Emacs state."
    :type 'color
    :group 'dotemacs-evil)

    (defcustom dotemacs-evil/emacs-insert-mode
    nil
    "If non-nil, insert mode will act as Emacs state."
    :type 'boolean
    :group 'dotemacs-evil)

    ;; move over visual lines like normal lines
    (general-define-key :states '(motion normal)
           "j"   #'evil-next-visual-line
           "k"   #'evil-previous-visual-line)

    (setq evil-search-module 'evil-search)
    (setq evil-magic 'very-magic)
    ;; (setq evil-want-C-i-jump nil)
    ;; Set colors for cursor states
    (setq evil-emacs-state-cursor '("SkyBlue2" box))
    (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
    (setq evil-visual-state-cursor '("gray" box))
    (setq evil-insert-state-cursor '("chartreuse3" (bar . 2)))
    (setq evil-replace-state-cursor '("red" hbar))
    (setq evil-motion-state-cursor  '("plum3" box))
    (setq evil-operator-state-cursor '("red" hollow))
    ;; (setq evil-visual-state-tag "VISUAL")
    ;use insert in commits automatically
    (add-hook 'git-commit-mode-hook 'evil-insert-state)
    (evil-set-initial-state 'dashboard-mode 'motion)
    (evil-set-initial-state 'messages-buffer-mode 'motion)
    (evil-set-initial-state 'magit-log-edit-mode 'insert)
    (evil-set-initial-state 'org-agenda-mode 'motion)
    (evil-set-initial-state 'org-export-dispatch 'motion)
    ;; evil-normal-state is preferred, so revert when idle
    (run-with-idle-timer 60 t 'evil-normal-state)
    ;; don't echo evil state
    (setq evil-echo-state nil)
    ;; don't move cursor back when exiting insert state
    (setq evil-move-cursor-back nil)
    ;; evil everywhere
    (evil-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-evil)

