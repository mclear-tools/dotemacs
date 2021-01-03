;;;; Ivy
(use-package ivy
  :general
  (:keymaps 'ivy-mode-map
   "M-o" 'ivy-dispatching-done
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line
   "C-<SPC>" 'cpm/ivy-toggle-mark)
  (:keymaps 'ivy-switch-buffer-map
   "C-d" 'ivy-switch-buffer-kill)
  :config
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (setq ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
        ivy-height 15  ; Number of result lines to display
        ivy-count-format "(%d/%d) " ;; Display count displayed and total
        ivy-on-del-error-function nil
        ivy-initial-inputs-alist nil ; No regexp by default
        ivy-wrap t ; wrap candidates
        )
  (defun cpm/ivy-toggle-mark ()
    "Toggle mark for current candidate and move forwards."
    (interactive)
    (if (ivy--marked-p)
        (ivy-unmark)
      (ivy-mark))))

(use-package ivy-hydra
  :requires (ivy)
  :after ivy
  :demand t)

;;;; Counsel
(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :defines
  (projectile-completion-system magit-completing-read-function)
  :commands (counsel-projectile counsel-org-goto jump-in-buffer counsel-org-tag)
  :general
  (:keymaps 'counsel-mode-map
   "C-j" #'ivy-next-line
   "C-k" #'ivy-previous-line)
  ("C-h l" #'counsel-find-library
   "C-h C-l" #'view-lossage)
  :init
  ;; Use faster search tool: ripgrep (rg)
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S -M 120 -L -i --line-number --no-heading --color never %s %s")
    (setq counsel-rg-base-command "rg -S -M 120 -L -i --with-filename --no-heading --line-number --color never %s"))
  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n────────\n")
  :config
  (setq counsel-find-file-ignore-regexp "\\.elc\\'")
  (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  (add-to-list 'ivy-format-functions-alist '(counsel-describe-face . counsel--faces-format-function))
  ;; Add action to open file literally
  (ivy-add-actions 'counsel-find-file
                   `(("l" find-file-literally "Open literally")))
  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))
  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read)))



;;;; Supporting Packages
;; Counsel integration for Projectile
(use-package counsel-projectile
  :hook (projectile . counsel-projectile-mode)
  :commands (counsel-projectile-mode
             counsel-projectile
             counsel-projectile-switch-project
             counsel-projectile-find-file
             counsel-projectile-find-file-dwim
             counsel-projectile-find-dir
             counsel-projectile-switch-to-buffer
             counsel-projectile-bookmark)
  :init
  (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
  :config
  (setq projectile-known-projects-file
        (concat cpm-cache-dir "projectile-bookmarks.eld")))


;; Integrate yasnippet
(use-package ivy-yasnippet
  :commands ivy-yasnippet--preview
  :general ("C-c C-y"  'ivy-yasnippet)
  :config (advice-add #'ivy-yasnippet--preview :override #'ignore))


;; Correcting words with flyspell via Ivy
(use-package flyspell-correct-ivy
  :after flyspell
  :general (:keymaps 'flyspell-mode-map
            [remap flyspell-correct-word-before-point]  'flyspell-correct-previous-word-generic))


;;;; End Ivy Config
(provide 'setup-ivy)
