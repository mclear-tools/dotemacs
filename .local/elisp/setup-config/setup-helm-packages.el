;;; Helm  Packages

(use-package helm-ag
  :load-path "~/.emacs.d/.local/elisp/helm-ag/"
  :after helm
  :demand t
  :commands (helm-ag helm-ag-buffers helm-ag-this-file helm-do-ag helm-ag-project-root cpm/helm-files-do-ag cpm/helm-files-search-current-directory)
  :config
  (setq helm-follow-mode-persistent t)
  (setq helm-ag-base-command "rg --no-heading")
  (setq helm-ag-fuzzy-match t))

(use-package helm-descbinds
  :commands helm-descbinds
  :config
  (setq helm-descbinds-window-style 'same-window)
  (add-hook 'helm-mode-hook 'helm-descbinds-mode))

(use-package helm-ls-git :commands helm-ls-git-ls)

(use-package helm-hunks :commands helm-hunks)

(use-package helm-swoop
  :commands (helm-swoop-without-pre-input helm-swoop-back-to-last-point helm-multi-swoop helm-multi-swoop-all)
  :load-path "~/.emacs.d/.local/elisp/helm-swoop/"
  :config
  (setq helm-swoop-use-fuzzy-match t)
  (setq helm-swoop-split-with-multiple-windows t))

(use-package helm-dired-recent-dirs
    :commands helm-dired-recent-dirs-view)

(use-package helm-files
  :ensure nil
  :defer t
  :config
  (setq helm-ff-skip-boring-files t)
  (setq helm-idle-delay 0.05)
  (setq helm-input-idle-delay 0.05)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-boring-file-regexp-list
  '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "~$"
    "\\.so$" "\\.a$" "\\.elc$" "\\.fas$" "\\.fasl$" "\\.pyc$" "\\.pyo$")))
(use-package helm-themes
  :commands helm-themes)

(defadvice helm-themes--load-theme (after helm-themes--load-theme-after activate) (require 'powerline) (powerline-reset))
  ;; (ad-unadvise 'helm-themes--load-theme)

(use-package helm-projectile
 :commands (helm-projectile-switch-to-buffer
            helm-projectile-find-dir
            helm-projectile-dired-find-dir
            helm-projectile-recentf
            helm-projectile-find-file
            helm-projectile-grep
            helm-projectile
            helm-projectile-switch-project)
 :init
 (setq projectile-switch-project-action 'helm-projectile)
 :config
 (helm-projectile-on))

;;;; Search info manuals with helm
;;   Search the Emacs, Emacs Lisp, and Emacs Common Lisp manuals, all at the same time
;; Courtesy of alphapapa and reddit:
;; https://www.reddit.com/r/emacs/comments/bl3dsb/search_entire_info_manual_with_swiper/emmxsxg?utm_source=share&utm_medium=web2x
(defun cpm/helm-info-emacs-elisp-cl ()
  "Helm for Emacs, Elisp, and CL-library info pages."
  (interactive)
  (helm :sources '(helm-source-info-emacs
                   helm-source-info-eintr
                   helm-source-info-cl)))

;;;; Helm-markers
(use-package helm-evil-markers
  :ensure t
  :after (evil helm)
  :demand t
  :config
  ;; enable helm-evil-markers
  (helm-evil-markers-toggle))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-helm-packages)
