;;; Programming
(use-package goto-addr
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)
         (eshell-mode . goto-address-mode)
         (shell-mode . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
              ("<RET>" . goto-address-at-point)
              ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
             goto-address-mode))
(use-package evil-lion
  :defer t
  :config
  (general-define-key :states '(normal) :keymaps 'prog-mode-map
  "g l" 'evil-lion-left
  "g L" 'evil-lion-right)

  (general-define-key :states '(visual) :keymaps 'prog-mode-map
  "g l" 'evil-lion-left
  "g L" 'evil-lion-right)
  )
   (use-package rainbow-delimiters
     :commands rainbow-delimiters-mode
     :init
     (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
     :config
     (set-face-attribute 'rainbow-delimiters-unmatched-face nil
            :foreground "red"
            :inherit 'error
            :box t))
(use-package rainbow-identifiers
  :commands rainbow-identifiers-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))
(use-package rainbow-mode
  :commands rainbow-mode)
    (use-package applescript-mode
      :ensure t
      :mode (("\\.scpt\\'" . applescript-mode)
             ("\\.applescript\\'"       . applescript-mode))
      :commands (applescript-mode))
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

  (use-package haskell-mode
    :commands haskell-mode)
(use-package web-mode
  :commands (web-mode)
  :mode ("\\.html$" . web-mode)
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-expanding t
        web-mode-enable-css-colorization t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t))
(use-package lua-mode
  :commands lua-mode
  :init
  (dolist (pattern '("\\.lua\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'lua-mode))))
(use-package php-mode
  :commands php-mode
  :init
  (dolist (pattern '("\\.php\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'php-mode))))
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
  (use-package yaml-mode
    :commands yaml-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
    (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
    (add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
  )
(use-package vimrc-mode
  :commands vimrc-mode)
(use-package macrostep
  :commands macrostep-expand
)

(use-package tldr
  :commands (tldr tldr-update-docs)
  :init
  (with-eval-after-load 'evil
  (evil-set-initial-state 'tldr-mode 'emacs))
  :config
  (setq tldr-directory-path (expand-file-name "tldr/" cpm-etc-dir)))


;;;; plist-mode.el --- Plist major mode

;; Author: wiml <wiml@omnigroup.com>
;; Maintainer: wiml


;; This file may be distributed under the terms of the GNU General Public
;; License. You will find a copy of the General Public License in your
;; emacs distribution. If you don't have emacs this file won't be very
;; useful to you.

;; History:
;;   November, 1998 (wiml):
;;      Created. Does paren-balancing and indentation.
;;

;; Things to do:
;;
;; Font lock isn't very useful with this mode because plist semantics are
;; so arbitrary. Perhaps minor modes should define the keys that are
;; useful and/or valid in each dictionary (i.e., provide a schema).
;; Or (less complicated) plist-mode could understand that a string
;; to the left of an equals sign is probably a dictionary key, and
;; therefore should have a different face, and possibly be matched against
;; a list of known dictionary keys for a given kind of plist (a simpler
;; sort of schema).
;;
;; Font-lock doesn't understand that barewords are strings too.
;;
;; Would it be useful to automatically add and remove doublequotes around
;; strings when the user puts an invalid character in the middle? This is
;; the second most common cause of unparsable plists (after unmatched
;; braces and incorrect semicolons --- okay, make that the third most
;; common cause).
;;
;; Indentation doesn't understand multiline comments.


(defvar plist-mode-syntax-table nil
  "syntax table used in plist-mode")
(if plist-mode-syntax-table
    ()
  (setq plist-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?{ "(}" plist-mode-syntax-table)
  (modify-syntax-entry ?} "){" plist-mode-syntax-table)
  (modify-syntax-entry ?/ "  14" plist-mode-syntax-table)
  (modify-syntax-entry ?* "  23" plist-mode-syntax-table))

(defvar plist-mode-map nil "Keymap for plist-mode.")
(if plist-mode-map
    ()
  (setq plist-mode-map (make-sparse-keymap))
  (define-key plist-mode-map "\t" 'plist-indent-line)
)

(defvar plist-indentation 4 "Amount to indent nested items in plist mode.")
(defvar plist-mode-hook nil "Hooks to run on entry to plist mode.")

(defun plist-mode ()
  "Major mode for editing plist files as used by OpenStep and its
descendants (Rhapsody, MacOS X, etc.).

A plist consists of one toplevel item. Items are strings, arrays, and
dictionaries. A string is a sequence of characters between
double-quotes. A string containing no special characters need not be
quoted. An array consists of a number of items, separated by commas,
surrounded by parentheses. A dictionary is delimited by curly braces
and has entries of the form STRING = ITEM ;.

The following keys are active in plist-mode:
\\{plist-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map plist-mode-map)
  (set-syntax-table plist-mode-syntax-table)
  (setq mode-name "Plist")
  (setq major-mode 'plist-mode)
  (run-hooks 'plist-mode-hook))

(defun calculate-plist-indent ()
  (save-excursion
    (beginning-of-line)
    (if (< (point) 2)
    0
      (skip-chars-forward " \t")
      (let ((indent-above (if (eq (char-syntax (following-char)) ?\) )
                  0
                plist-indentation)))
    (up-list -1)
    (+ (current-indentation) indent-above)))))

(defun plist-indent-line ()
  (interactive)
  (let ((level (calculate-plist-indent))
    (pos (- (point-max) (point)))
    beg)
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (equal level (current-column))
    nil
      (delete-region beg (point))
      (indent-to level)
      ;; ???
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-programming)
