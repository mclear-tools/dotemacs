;;; Evil Mode

(use-package evil
  :defer .1 ;; don't block emacs when starting, load evil immediately after startup
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :general
  (:states '(normal motion)
   "gb" #'evil-jump-backward
   "gf" #'evil-jump-forward)
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
    ;; use insert in commits automatically
    (add-hook 'git-commit-mode-hook 'evil-insert-state)
    (evil-set-initial-state 'dashboard-mode 'motion)
    (evil-set-initial-state 'messages-buffer-mode 'motion)
    (evil-set-initial-state 'magit-log-edit-mode 'insert)
    (evil-set-initial-state 'org-agenda-mode 'motion)
    (evil-set-initial-state 'org-export-dispatch 'motion)
    (evil-set-initial-state 'paradox-menu-mode 'motion)
    ;; evil-normal-state is preferred, so revert when idle
    (run-with-idle-timer 60 t 'evil-normal-state)
    ;; don't echo evil state
    (setq evil-echo-state nil)
    ;; don't move cursor back when exiting insert state
    (setq evil-move-cursor-back nil)
    ;; edit by visual lines
    (setq evil-respect-visual-line-mode nil)
    ;; whether to allow evil-char move across lines
    (setq evil-cross-lines nil)
    ;; Use counsel to provide :ls
    (evil-ex-define-cmd "buffers" 'counsel-ibuffer)
    ;; fine-grained undo
    (setq evil-want-fine-undo t)
    ;; disable undo-tree
    (global-undo-tree-mode -1)
    ;; evil everywhere
    (evil-mode 1)))

;;; Evil Collection
(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init)
  :custom
  (evil-collection-company-use-tng nil))


;;; Evil Surround
(use-package evil-surround
  :after evil
  ;; :commands (evil-surround-region evil-surround-change evil-surround-delete)
  :general
  (:states '(visual)
   "s" 'evil-surround-region
   "S" 'evil-substitute)
  :config (global-evil-surround-mode 1))

(use-package evil-embrace
  :after evil-surround
  :after evil
  :hook ((markdown org) . embrace-org-mode-hook)
  :config
  (with-eval-after-load 'evil-surround
    (evil-embrace-enable-evil-surround-integration))
  (setq evil-embrace-show-help-p nil)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (defun embrace-markdown-mode-hook ()
    (dolist (lst '((?* "*" . "*")
                   (?\ "\\" . "\\")
                   (?$ "$" . "$")
                   (?/ "/" . "/")))
      (embrace-add-pair (car lst) (cadr lst) (cddr lst))))
  (add-hook 'markdown-mode-hook 'embrace-markdown-mode-hook))

;;; Evil Comments
(use-package evil-commentary
  :commands (evil-commentary evil-commentary-line)
  :config
  (evil-commentary-mode))

;;; Evil Undo
;; there are problems but there doesn't seem to be a workaround
;; https://github.com/emacs-evil/evil/issues/1074 and
;; http://ergoemacs.org/emacs/emacs_best_redo_mode.html
;; So disable undo-tree; have to do this manually b/c it is automatically loaded by evil

;; so many problems with this package...
;; https://github.com/emacs-evil/evil/issues/1074
(use-package undo-tree
  :after evil
  :commands (undo-tree-undo undo-tree-redo undo-tree-visualize)
  ;; :disabled
  :general
  (:states '(normal)
   "u" 'undo-tree-undo)
  (:states '(normal insert motion emacs)
   "s-z" 'undo-tree-undo
   "s-Z" 'undo-tree-redo)
  :init
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-enable-undo-in-region nil)
  ;; supposedly causes errors in undo read
  ;; see https://emacs.stackexchange.com/a/34214/11934
  (setq undo-tree-enable-undo-in-region nil)
  ;; stop littering - set undo directory
  (let ((undo-dir (concat cpm-cache-dir "undo")))
    (setq undo-tree-history-directory-alist `(("." . ,undo-dir)))
    (unless (file-directory-p undo-dir)
      (make-directory undo-dir t)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode 1))


;;; End Evil-Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-evil)

