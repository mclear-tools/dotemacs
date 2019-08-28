;;;; Helm
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


(provide 'setup-helm)
