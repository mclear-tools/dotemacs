;; UI & Appearance

;;; Scrolling
(setq auto-window-vscroll nil)
;; the text cursor moves off-screen. Instead, only scroll the minimum amount
;; necessary to show the new line. (A number of 101+ disables re-centering.)
(setq scroll-conservatively 101)

;; Optimize mouse wheel scrolling for smooth-scrolling trackpad use.
;; Trackpads send a lot more scroll events than regular mouse wheels,
;; so the scroll amount and acceleration must be tuned to smooth it out.
(setq
 ;; If the frame contains multiple windows, scroll the one under the cursor
 ;; instead of the one that currently has keyboard focus.
 mouse-wheel-follow-mouse 't
 ;; Completely disable mouse wheel acceleration to avoid speeding away.
 mouse-wheel-progressive-speed nil
 ;; The most important setting of all! Make each scroll-event move 2 lines at
 ;; a time (instead of 5 at default). Simply hold down shift to move twice as
 ;; fast, or hold down control to move 3x as fast. Perfect for trackpads.
 mouse-wheel-scroll-amount '(2 ((shift) . 4) ((control) . 6)))

;;; Frames

;;;; Frame defaults
(setq default-frame-alist
      (append (list
	           ;; '(font . "Roboto Mono:style=Light:size=15")
               '(internal-border-width . 20)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               )))

;; maximize frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq window-resize-pixelwise t)

;;;; Frame titlebar
;; Theme transparent titlebar
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;;;; UI Elements
(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))


;;;; Fix titlebar titling colors
;; see also https://github.com/d12frosted/homebrew-emacs-plus/issues/55
(use-package ns-auto-titlebar
  :commands ns-auto-titlebar-mode
  :if (eq system-type 'darwin)
  :init (ns-auto-titlebar-mode))


;;;; Miniframe
(use-package mini-frame
  :straight (:type git :host github :repo "muffinmad/emacs-mini-frame")
  ;; :hook (after-init . mini-frame-mode)
  :commands (mini-frame-mode)
  :config
  (custom-set-variables
   '(mini-frame-show-parameters
     '((top . 60)
       (width . 0.7)
       (left . 0.5))))
  (setq mini-frame-resize t))

;;; Fonts

(setq-default line-spacing 0.10)

(use-package faces
  :straight (:type built-in)
  :defer t
  :custom
  (face-font-family-alternatives
   '(("Monospace" "Roboto Mono" "Hasklug Nerd Font" "InconsolataLGC Nerd Font" "SauceCodePro Nerd Font" "Consolas" "Monaco" "PT Mono")
     ("Monospace Serif" "Roboto" "Roboto Slab" "Courier 10 Pitch" "Monospace")
     ("Serif" "Avenir" "Avenir Next" "Helvetica Neue" "Georgia" "Cambria" "Times New Roman" "DejaVu Serif" "serif")))
  :custom-face
  (variable-pitch ((t (:family "Serif" :height 200))))
  (fixed-pitch ((t (:family "Monospace Serif" :height 150))))
  (default ((t (:family "Monospace" :height 150))))
  :config
  ;; (set-face-attribute 'default nil :font "Roboto Mono Light" :height 150)
  ;; (set-face-attribute 'fixed-pitch nil :font "Roboto Mono" :height 150)
  ;; (set-face-attribute 'variable-pitch nil :font "Avenir Next" :height 200)
  (set-fontset-font t 'unicode "Symbola" nil 'prepend)

  ;; Fall back font for glyph missing in Roboto
  (defface fallback '((t :family "Fira Code"
                         :inherit fringe)) "Fallback")
  (set-display-table-slot standard-display-table 'truncation
                          (make-glyph-code ?… 'fallback))
  (set-display-table-slot standard-display-table 'wrap
                          (make-glyph-code ?↩ 'fallback)))

;;; Scale Text
;; Set default line spacing (in pixels)
(setq-default line-spacing nil)
;; When using `text-scale-incraese', this sets each 'step' to about one point size.
(setq text-scale-mode-step 1.08)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") 'text-scale-adjust)

;;; Line Numbers
(use-package display-line-numbers
  ;; :hook (markdown-mode prog-mode)
  :commands display-line-numbers-mode
  :init
  (setq-default display-line-numbers-type 'visual)
  (setq-default display-line-numbers-width-start t))


;;; Dialogs and popups
;; No file dialog
(setq use-file-dialog nil)
;; No dialog box
(setq use-dialog-box nil)
;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)
;; Set popup windows
(setq-default pop-up-windows t)
;; Set popup frames
(setq-default pop-up-frames nil)


;;; Highlight
(use-package highlight-numbers
  :defer t
  :commands highlight-numbers-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package hl-todo
  :defer t
  :commands hl-todo-mode
  :init
  ;; (add-hook 'org-mode-hook #'hl-todo-mode)
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'markdown-mode-hook #'hl-todo-mode))

;; hydra for TODOs
(with-eval-after-load 'hydra
  (defhydra cpm/hydra-todo
    (:pre
     (hl-todo-mode 1)
     :post
     (hl-todo-mode -1))
    "Todo"
    ("n" hl-todo-next "Next")
    ("p" hl-todo-previous "Previous")
    ("o" hl-todo-occur "Occur")
    ("q" nil "Quit" :color blue :exit t)))

;; ;;https://github.com/erickgnavar/dotfiles/tree/master/.emacs.d#highlight-todo-fixme-etc
;; (defun cpm/highlight-todo-like-words ()
;;   (font-lock-add-keywords
;;    nil `(("\\<\\(FIXME\\|TODO\\|NOTE\\)"
;;           1 font-lock-warning-face t))))


;; (add-hook 'prog-mode-hook 'my/highlight-todo-like-words)


;;; Icons
(use-package all-the-icons
  :defer t)
;;dependency
;; (quelpa
;;  '(font-lock+ :fetcher wiki))
(use-package font-lock+
  :defer 1)
;; icons for dired
(use-package all-the-icons-dired
  :defer t
  :commands all-the-icons-dired-mode
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; No ugly button for checkboxes
(setq widget-image-enable nil)


;;; Highlight Cursor Line with Pulse
;; From https://karthinks.com/software/batteries-included-with-emacs/
;; Replace external package with internal command

(defun pulse-line (&rest _)
  "Pulse the current line."
  (interactive)
  (pulse-momentary-highlight-one-line (point)))

(add-hook 'window-state-change-hook #'pulse-line)

(dolist (command '(scroll-up-command scroll-down-command
                                     recenter-top-bottom other-window select-window-by-number))
  (advice-add command :after #'pulse-line))
(defadvice other-window (after other-window-pulse activate) (pulse-line))
(defadvice delete-window (after delete-window-pulse activate) (pulse-line))
(defadvice recenter-top-bottom (after recenter-top-bottom-pulse activate))

;;; Emoji
(use-package emojify
  :commands (emojify-mode emojify-apropos-emoji)
  ;; :hook ((prog-mode markdown-mode) . emojify-mode)
  :config
  (setq emojify-emojis-dir (concat cpm-etc-dir "emojis")))

;;; Underline
(setq x-underline-at-descent-line t)


;;; Delight
(use-package delight
  :straight t
  :defer 1)

;;; Helpful Information
;; Much better lookup both in details and headings/aesthetics
(use-package helpful
  :init
  (setq evil-lookup-func #'helpful-at-point)
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'helpful-mode 'motion))
  :general
  ("C-h f" #'helpful-function)
  ("C-h k" #'helpful-key)
  ("C-h o" #'helpful-symbol)
  ("C-h v" #'helpful-variable)
  ("C-c C-." #'helpful-at-point)
  ("C-h C-l" #'find-library))

(advice-add 'describe-package-1 :after #'cpm/describe-package--add-melpa-link)

;; Add melpa link to describe package info
(defun cpm/describe-package--add-melpa-link (pkg)
  (let* ((desc (if (package-desc-p pkg)
                   pkg
                 (cadr (assq pkg package-archive-contents))))
         (name (if desc (package-desc-name desc) pkg))
         (archive (if desc (package-desc-archive desc)))
         (melpa-link (format "https://melpa.org/#/%s" name)))
    (when (equal archive "melpa")
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "Summary:" nil t)
          (forward-line 1)
          (package--print-help-section "MELPA")
          (help-insert-xref-button melpa-link 'help-url melpa-link)
          (insert "\n"))))))

;;;; Helpful Demos
(use-package elisp-demos
  :defer 1
  :config
  ;; inject demos into helpful
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))


;;; Better Info
;; Better looking info pages
(use-package info-colors
  :straight (:host github :repo "ubolonton/info-colors")
  :defer t
  :config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

;;; Xwidget Browser
(use-package xwwp-follow-link
  :straight (:host github :repo "canatella/xwwp")
  :custom
  (xwwp-follow-link-completion-system 'default)
  :general
  (:keymaps 'xwidget-webkit-mode-map
   "v"  'xwwp-follow-link))

;;; Mode line
;;;; Hide Modeline
(use-package emacs-hide-mode-line
  :straight (:type git :host github :repo "hlissner/emacs-hide-mode-line")
  :commands hide-mode-line-mode)

;;; End UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-ui)
