;; Programming
;;; Alignment
;; https://github.com/edkolev/evil-lion : This package provides gl and gL align operators: gl MOTION CHAR and right-align gL MOTION CHAR
(use-package evil-lion
 :defer t
 :config
 (general-define-key :states '(normal) :keymaps 'prog-mode-map
 "g l" 'evil-lion-left
 "g L" 'evil-lion-right)

 (general-define-key :states '(visual) :keymaps 'prog-mode-map
 "g l" 'evil-lion-left
 "g L" 'evil-lion-right))

;;; Delimiters & Identifiers
;; https://github.com/Fanael/rainbow-delimiters Useful package that will highlight
;; delimiters such as parentheses, brackets or braces according to their depth. Each
;; successive level is highlighted in a different color. This makes it easy to spot
;; matching delimiters, orient yourself in the code, and tell which statements are at
;; a given depth.
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
         :foreground "red"
         :inherit 'error
         :box t))

;; https://github.com/Fanael/rainbow-identifiers Rainbow identifiers mode is an Emacs
;; minor mode providing highlighting of identifiers based on their names. Each
;; identifier gets a color based on a hash of its name.
(use-package rainbow-identifiers
  :commands rainbow-identifiers-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

;;; Colors
;; https://github.com/emacsmirror/rainbow-mode Colorize color names in buffers
(use-package rainbow-mode
  :commands rainbow-mode)

;;; Languages
;;;; Applescript
(use-package applescript-mode
  :ensure t
  :mode (("\\.scpt\\'" . applescript-mode)
         ("\\.applescript\\'"       . applescript-mode))
  :commands (applescript-mode))

;;;; Elisp
(use-package elisp-slime-nav
  :commands elisp-slime-nav-mode
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode
  :config
  ;; Show ElDoc messages in the echo area immediately, instead of after 1/2 a second.
  (setq eldoc-idle-delay 0))
  ;; Elisp hook
  (add-hook 'emacs-lisp-mode-hook (lambda ()
              (setq show-trailing-whitespace t)
              (prettify-symbols-mode)
              (eldoc-mode)
              (yas-minor-mode)
              (company-mode)
              (rainbow-delimiters-mode)))

;;;; Haskell
(use-package haskell-mode
  :commands haskell-mode)

;;;; HTML
(use-package web-mode
  :commands (web-mode)
  :mode ("\\.html$" . web-mode)
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-expanding t
        web-mode-enable-css-colorization t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t))

;;;; Lua
(use-package lua-mode
  :commands lua-mode
  :init
  (dolist (pattern '("\\.lua\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'lua-mode))))

;;;; PHP
(use-package php-mode
  :commands php-mode
  :init
  (dolist (pattern '("\\.php\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'php-mode))))

;;;; Shell Scripts
(use-package sh-script
  :commands sh-script-mode
  :init
  (progn
    ;; Use sh-mode when opening `.zsh' files, and when opening Prezto runcoms.
    (dolist (pattern '("\\.zsh\\'"
                       "zlogin\\'"
                       "zlogout\\'"
                       "zpreztorc\\'"
                       "zprofile\\'"
                       "zshenv\\'"
                       "zshrc\\'"))
      (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))))

  (defun spacemacs//setup-shell ()
      (when (and buffer-file-name
                 (string-match-p "\\.zsh\\'" buffer-file-name))
        (sh-set-shell "zsh")))
    (add-hook 'sh-mode-hook 'spacemacs//setup-shell)

;;;; YAML
(use-package yaml-mode
  :commands yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
  (add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

;;;; Plist
(use-package plist-mode
  :ensure nil
  :load-path cpm-elisp-dir)

;;;; Vim
(use-package vimrc-mode
  :commands vimrc-mode)

;;; Macrostep
;; https://github.com/joddie/macrostep Interactive macro expander for emacs
(use-package macrostep :commands macrostep-expand)

;;; Documentation
(use-package tldr
  :commands (tldr tldr-update-docs)
  :init
  (with-eval-after-load 'evil
  (evil-set-initial-state 'tldr-mode 'emacs))
  :config
  (setq tldr-directory-path (expand-file-name "tldr/" cpm-etc-dir)))

;;; Indentation
(use-package aggressive-indent
  :preface
  (defun cpm/aggressive-indent-mode-off ()
    (aggressive-indent-mode 0))
  :hook
  ((css-mode . aggressive-indent-mode)
   (emacs-lisp-mode . aggressive-indent-mode)
   (js-mode . aggressive-indent-mode)
   (lisp-mode . aggressive-indent-mode)
   (sgml-mode . aggressive-indent-mode))
  :config
  (setq-default aggressive-indent-comments-too t)
  (add-to-list 'aggressive-indent-protected-commands 'comment-dwim))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq-default highlight-indent-guides-method 'character
                highlight-indent-guides-responsive 'top))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-programming)
