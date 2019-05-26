;;; Version Control

;;;; Magit
(use-package magit
  :commands
  (magit-blame-mode
   magit-commit
   magit-diff
   magit-log
   magit-status)
  :hook ((git-commit-mode . turn-on-flyspell)
         (evil-magit-init . magit-mode))
  :init
  (setq vc-follow-symlinks t)
  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  :config
  ;; make magit go fullscreen
  ;; (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-diff-refine-hunk t)
  (global-git-commit-mode t) ; use emacs as editor for git commits
  (setq magit-push-always-verify nil))

;; settings for committing using magit
(use-package git-commit
  :ensure t
  :after magit
  :hook (git-commit-mode . cpm/git-commit-auto-fill-everywhere)
  :custom (git-commit-summary-max-length 50)
  :preface
  (defun cpm/git-commit-auto-fill-everywhere ()
    "Ensures that the commit body does not exceed 72 characters."
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil)))

;; transient popups for magit
(use-package transient
  :after magit
  :demand t
  :ensure t
  :init
  (setq transient-levels-file (concat cpm-cache-dir "transient/levels.el"))
  (setq transient-values-file (concat cpm-cache-dir "transient/values.el"))
  (setq transient-history-file (concat cpm-cache-dir "transient/history.el")))

;; add todos in magit
(use-package magit-todos
  ;; :load-path "~/.emacs.d/.local/elisp/magit-todos"
  :ensure t
  :after magit
  :demand t
  :config
  (magit-todos-mode))

;; generate ignore files with helm
(use-package helm-gitignore
  :ensure t
  :commands helm-gitignore)

;;;; Evil-Magit
(use-package evil-magit
  :after magit
  :demand t
  :config
  (setq evil-magit-use-y-for-yank t
        evil-magit-use-z-for-folds t))

;;;; Git Navigation
(use-package git-timemachine            ; Go back in Git time
  :commands git-timemachine)

(use-package gited
  :commands (gited-list gited-list-branches))

;;;; Git Gutter
(use-package git-gutter
  :defer t
  :hook ((markdown-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode)
         (conf-mode . git-gutter-mode))
  :init
  :config
  (setq git-gutter:disabled-modes '(org-mode asm-mode image-mode)
        git-gutter:update-interval 1
        git-gutter:window-width 2
        git-gutter:ask-p nil)
  (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                         :hint nil)
   "
 Git gutter:
   _j_: next hunk        _s_tage hunk     _q_uit
   _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
   ^ ^                   _p_opup hunk
   _h_: first hunk
   _l_: last hunk        set start _R_evision
 "
 ("j" git-gutter:next-hunk)
 ("k" git-gutter:previous-hunk)
 ("h" (progn (goto-char (point-min))
             (git-gutter:next-hunk 1)))
 ("l" (progn (goto-char (point-min))
             (git-gutter:previous-hunk 1)))
 ("s" git-gutter:stage-hunk)
 ("r" git-gutter:revert-hunk)
 ("p" git-gutter:popup-hunk)
 ("R" git-gutter:set-start-revision)
 ("q" nil :color blue)
 ("Q" (progn (git-gutter-mode -1)
             ;; git-gutter-fringe doesn't seem to
             ;; clear the markup right away
             (sit-for 0.1)
             (git-gutter:clear))
             :color blue)))

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :after git-gutter
  :demand fringe-helper
  :config
  ;; subtle diff indicators in the fringe
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added
  [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
  nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
  [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
  nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
  [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
  nil nil 'center))

   (defun quick-commit ()
   "make a quick commit from the mini-buffer"
   (interactive)
   (evil-ex '"!Git add % && Git commit -m '" ))

;;;; Show Git Status in Dired
  (use-package diff-hl
    :ensure t
    :defer 1
    :config
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode))


(provide 'setup-vc)
