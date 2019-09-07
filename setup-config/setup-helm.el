;;; Helm
(use-package helm
  :ensure t
  :defer
  :init
  ;; ;; fix errors on byte compile
  (require 'helm-config)
  :general
  ;; ("M-x"   'helm-M-x)
  ("C-h i" 'helm-info)
  ;; helm vim-bindings in buffer
  (:keymaps 'helm-map
   "C-z"   'helm-select-action ; list actions using C-z
   "C-j"   'helm-next-line
   "C-k"   'helm-previous-line
   "C-h"   'helm-next-source
   "C-l"   'helm-previous-source
   "TAB"   'helm-execute-persistent-action ; rebind tab to do persistent action
   "C-i"   'helm-execute-persistent-action ; make TAB works in terminal
   "C-S-h" 'describe-key)
  :commands (helm-mode helm-find-files-1 helm-mini helm-M-x helm-find-files helm-find)
  :config
  (progn
    ;; Use helm to provide :ls, unless ibuffer is used
    (with-eval-after-load 'evil
      (evil-ex-define-cmd "buffers" 'helm-buffers-list))
    (set-face-attribute 'helm-source-header nil
                        :height 180)
    (setq helm-locate-fuzzy-match nil
          helm-locate-command "mdfind -interpret -name %s %s")
    (setq helm-M-x-fuzzy-match t  ;; Use fuzzy match in helm
          helm-apropos-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-imenu-fuzzy-match t
          helm-recentf-fuzzy-match t
          helm-adaptive-mode 1 ; learn from selections
          helm-prevent-escaping-from-minibuffer t
          helm-bookmark-show-location t
          helm-ff-file-name-history-use-recentf t
          helm-find-files-sort-directories t
          helm-display-header-line nil
          helm-move-to-line-cycle-in-source t
          helm-always-two-windows t
          helm-split-window-inside-p nil
          ;; helm-split-window-default-side 'other
          helm-echo-input-in-header-line t
          helm-occur-auto-update-on-resume 'noask)
    (setq helm-boring-buffer-regexp-list
          (quote
           ("\\Minibuf.+\\*"
            "\\` "
            "\\*.+\\*"
            )))
    (setq helm-white-buffer-regexp-list
          (quote
           ("\\*magit:"
            "\\*eshell"
            "\\*scratch*"
            "\\*ansi-term"
            "\\*Pandoc Output*"
            "\\*compilation*"
            "\\*dashboard*"
            )))
    (helm-autoresize-mode 1)
    (setq helm-autoresize-max-height 40)
    (setq helm-autoresize-min-height 35))
  ;; (define-key helm-map (kbd "C-a") (kbd "RET"))
  )

(defvar my-helm-bottom-buffers nil
  "List of bottom buffers before helm session.
    Its element is a pair of `buffer-name' and `mode-line-format'.")

(defun my-helm-bottom-buffers-init ()
  (setq-local mode-line-format (default-value 'mode-line-format))
  (setq my-helm-bottom-buffers
        (cl-loop for w in (window-list)
                 when (window-at-side-p w 'bottom)
                 collect (with-current-buffer (window-buffer w)
                           (cons (buffer-name) mode-line-format)))))

(defun my-helm-bottom-buffers-hide-mode-line ()
  (setq-default cursor-in-non-selected-windows nil)
  (mapc (lambda (elt)
          (with-current-buffer (car elt)
            (setq-local mode-line-format nil)))
        my-helm-bottom-buffers))

(defun my-helm-bottom-buffers-show-mode-line ()
  (setq-default cursor-in-non-selected-windows t)
  (when my-helm-bottom-buffers
    (mapc (lambda (elt)
            (with-current-buffer (car elt)
              (setq-local mode-line-format (cdr elt))))
          my-helm-bottom-buffers)
    (setq my-helm-bottom-buffers nil)))

(defun my-helm-keyboard-quit-advice (orig-func &rest args)
  (my-helm-bottom-buffers-show-mode-line)
  (apply orig-func args))

(add-hook 'helm-before-initialize-hook #'my-helm-bottom-buffers-init)
(add-hook 'helm-after-initialize-hook #'my-helm-bottom-buffers-hide-mode-line)
(add-hook 'helm-exit-minibuffer-hook #'my-helm-bottom-buffers-show-mode-line)
(add-hook 'helm-cleanup-hook #'my-helm-bottom-buffers-show-mode-line)
(advice-add 'helm-keyboard-quit :around #'my-helm-keyboard-quit-advice)
(defun my-helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))
(add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)

;;; Helm  Packages

(use-package helm-org
  :ensure t
  :after helm
  :commands (helm-org-agenda-files-headings
             helm-org-in-buffer-headings
             helm-org-parent-headings
             helm-org-capture-templates))

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

;;;; Helm-preview
;; Preview files selected in helm
(use-package helm-file-preview
  :ensure t
  :after helm
  :demand t
  :config
  ;; turn off to have preview no matter what
  (setq helm-file-preview-only-when-line-numbers t)
  ;; set to nil to leave file open
  (setq helm-file-preview-preview-only t)
  (helm-file-preview-mode t))

;;; End Helm Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-helm)
