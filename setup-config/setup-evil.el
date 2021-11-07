
;;; Evil Mode
(use-package evil
  :straight t
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
      "k"   #'evil-previous-visual-line))
  (setq evil-search-module 'evil-search)
  (setq evil-magic 'very-magic)
  (setq evil-want-C-i-jump nil)
  (setq evil-visual-state-tag "VISUAL")
  ;; use insert in commits automatically
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
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
  (setq evil-want-fine-undo nil)

  ;; evil everywhere
  (evil-mode 1))

;; (progn
;;   (straight-use-package 'evil)
;;   (defvar use-package--warning87
;;     #'(lambda
;;         (keyword err)
;;         (let
;;             ((msg
;;               (format "%s/%s: %s" 'evil keyword
;;                       (error-message-string err))))
;;           (display-warning 'use-package msg :error))))
;;   (condition-case-unless-debug err
;;       (progn
;;         (let
;;             ((custom--inhibit-theme-enable nil))
;;           (unless
;;               (memq 'use-package custom-known-themes)
;;             (deftheme use-package)
;;             (enable-theme 'use-package)
;;             (setq custom-enabled-themes
;;                   (remq 'use-package custom-enabled-themes)))
;;           (custom-theme-set-variables 'use-package
;;                                       '(evil-undo-system 'undo-redo nil nil "Customized with use-package evil")))
;;         (unless
;;             (fboundp 'evil-jump-backward)
;;           (autoload #'evil-jump-backward "evil" nil t))
;;         (unless
;;             (fboundp 'evil-jump-forward)
;;           (autoload #'evil-jump-forward "evil" nil t))
;;         (condition-case-unless-debug err
;;             (setq evil-want-integration t evil-want-keybinding nil)
;;           (error
;;            (funcall use-package--warning87 :init err)))
;;            (eval-after-load 'evil
;;           '(let
;;                ((now
;;                  (current-time)))
;;              (message "%s..." "Configuring package evil")
;;              (prog1
;;                  (condition-case-unless-debug err
;;                      (progn
;;                        (progn
;;                          (general-define-key :states:
;;                                              '(motion normal)
;;                                              "j" #'evil-next-visual-line "k" #'evil-previous-visual-line)
;;                          (setq evil-search-module 'evil-search)
;;                          (setq evil-magic 'very-magic)
;;                          (setq evil-want-C-i-jump nil)
;;                          (setq evil-visual-state-tag "VISUAL")
;;                          (add-hook 'git-commit-mode-hook 'evil-insert-state)
;;                          (evil-set-initial-state 'messages-buffer-mode 'motion)
;;                          (evil-set-initial-state 'magit-mode 'normal)
;;                          (evil-set-initial-state 'magit-log-edit-mode 'insert)
;;                          (evil-set-initial-state 'org-agenda-mode 'motion)
;;                          (evil-set-initial-state 'org-export-dispatch 'motion)
;;                          (run-with-idle-timer 60 t 'evil-normal-state)
;;                          (setq evil-echo-state t)
;;                          (setq evil-move-cursor-back nil)
;;                          (setq evil-move-beyond-eol t)
;;                          (setq evil-highlight-closing-paren-at-point-states
;;                                '(not emacs insert replace))
;;                          (setq evil-respect-visual-line-mode nil)
;;                          (setq evil-cross-lines nil)
;;                          (evil-ex-define-cmd "buffers" 'consult-buffer)
;;                          (setq evil-want-fine-undo t)
;;                          (evil-mode 1))
;;                        t)
;;                    (error
;;                     (funcall use-package--warning87 :config err)))
;;                (let
;;                    ((elapsed
;;                      (float-time
;;                       (time-subtract
;;                        (current-time)
;;                        now))))
;;                  (if
;;                      (> elapsed 0)
;;                      (message "%s...done (%.3fs)" "Configuring package evil" elapsed)
;;                    (message "%s...done" "Configuring package evil"))))))
;;         (general-def :states
;;           '(normal motion)
;;           "gb" #'evil-jump-backward "gf" #'evil-jump-forward :package 'evil))
;;     (error
;;      (funcall use-package--warning87 :catch err))))

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
  :commands (evil-lion-left evil-lion-right))

;;; Evil Matchit
(use-package evil-matchit
  :straight t
  :hook (after-init . global-evil-matchit-mode))

;;; End Evil-Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-evil)

