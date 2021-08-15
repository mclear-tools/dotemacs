;;; Evil Mode

(use-package evil
  ;; start as soon as possible but don't block loading
  :defer .1
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :general
  (:states '(normal motion)
   "gb" #'evil-jump-backward
   "gf" #'evil-jump-forward)
  :custom
  ;; Set undo system
  (evil-undo-system 'undo-redo)
  :config
  (progn
    ;; move over visual lines like normal lines
    (general-define-key :states '(motion normal)
      "j"   #'evil-next-visual-line
      "k"   #'evil-previous-visual-line)

    (setq evil-search-module 'evil-search)
    (setq evil-magic 'very-magic)
    (setq evil-want-C-i-jump nil)
    ;; Set colors for cursor states
    ;; (setq evil-emacs-state-cursor    '("#649bce" box))
    ;; (setq evil-normal-state-cursor   '("#ebcb8b" box))
    ;; (setq evil-visual-state-cursor   '("#677691" box))
    ;; (setq evil-insert-state-cursor   '("#ed683e" (bar . 2)))
    ;; (setq evil-replace-state-cursor  '("#a9444e" hbar))
    ;; (setq evil-motion-state-cursor   '("#93004d" box))
    ;; (setq evil-operator-state-cursor '("#cc1f24" hollow))
    (setq evil-visual-state-tag "VISUAL")
    ;; use insert in commits automatically
    (add-hook 'git-commit-mode-hook 'evil-insert-state)
    ;; (evil-set-initial-state 'dashboard-mode 'motion)
    (evil-set-initial-state 'messages-buffer-mode 'motion)
    (evil-set-initial-state 'magit-mode 'normal)
    (evil-set-initial-state 'magit-log-edit-mode 'insert)
    (evil-set-initial-state 'org-agenda-mode 'motion)
    (evil-set-initial-state 'org-export-dispatch 'motion)
    ;; evil-normal-state is preferred, so revert when idle
    (run-with-idle-timer 60 t 'evil-normal-state)
    ;; don't echo evil state
    (setq evil-echo-state t)
    ;; Make evil cursor behavior more emacsy
    (setq evil-move-cursor-back nil) ;; don't move cursor back when exiting insert state
    (setq evil-move-beyond-eol t) ;; allow end of line movement
    ;; highlight closing bracket like vim not emacs
    (setq evil-highlight-closing-paren-at-point-states '(not emacs insert replace))
    ;; edit by visual lines
    (setq evil-respect-visual-line-mode nil)
    ;; whether to allow evil-char move across lines
    (setq evil-cross-lines nil)
    ;; Use consult to provide :ls
    (evil-ex-define-cmd "buffers" 'consult-buffer)
    ;; fine-grained undo
    (setq evil-want-fine-undo t)
    ;; evil everywhere
    (evil-mode 1)
    ))

;;; Evil Cursor (In Terminal)
(use-package term-cursor
  :if (not (display-graphic-p))
  :straight (term-cursor :host github :repo "h0d/term-cursor.el")
  :after evil
  :init
  ;; For all buffers
  (global-term-cursor-mode))

;;; Evil Collection
(use-package evil-collection
  :defer 2
  :config
  (evil-collection-init)
  :custom
  (evil-collection-company-use-tng nil))

;;; Evil Surround
(use-package evil-surround
  ;; :after evil
  :general
  (:states '(visual)
   "s" 'evil-surround-region
   "S" 'evil-substitute)
  :config
  (global-evil-surround-mode))

(use-package evil-embrace
  :after evil-surround
  :demand t
  :config
  (evil-embrace-enable-evil-surround-integration)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (setq evil-embrace-show-help-p nil)
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
;; emacs 28 has undo-redo so disable undo-tree
;; so many problems with this package...
;; https://github.com/emacs-evil/evil/issues/1074
(use-package undo-tree
  :disabled t
  :after evil
  :straight (:type git :host gitlab :repo "tsc25/undo-tree")
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

;;; Evil Lion
;; Left-align with gl MOTION CHAR or right-align with gL MOTION CHAR. If the align
;; separator is / you will be prompted for a regular expression instead of a plain
;; character. If the align separator is RET alignment will be performed with
;; align.el's rules specific for the major mode. You can pass count 1 to align on the
;; first occurrence of CHAR. To pass count, use: COUNT gl MOTION CHAR. Example, left
;; align gl: After pressing glip= (gl is the operator, ip text object paragraph, =
;; separator)
(use-package evil-lion
  :after evil
  :straight (:host github :repo "edkolev/evil-lion")
  :config
  (evil-lion-mode))

;;; Evil Matchit
(use-package evil-matchit
  :straight t
  :hook (after-init . global-evil-matchit-mode))

;;; End Evil-Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-evil)

