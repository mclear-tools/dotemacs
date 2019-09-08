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

;;; Fonts
(defvar cpm-font1 (font-spec :family "InconsolataLGC Nerd Font" :size 13))
(defvar cpm-font2 (font-spec :family "Hasklug Nerd Font" :size 13))
(defvar cpm-font3 (font-spec :family "DejaVuSansMono Nerd Font" :size 13))
(defvar cpm-font4 (font-spec :family "SauceCodePro Nerd Font" :size 13))
(defvar cpm-ligatures t)
(defvar cpm-vari-font (font-spec :family "Avenir"))
(defvar cpm-unicode-font (font-spec :family "Symbola"))
(set-face-attribute 'default nil :font cpm-font2)
(set-face-attribute 'variable-pitch nil :font cpm-vari-font)
(set-fontset-font t 'unicode cpm-unicode-font nil 'prepend)
(setq-default line-spacing 0.10)

;;; Frames
;;;; Frame formatting
(setq frame-title-format "\n")
;; (setq frame-title-format '('nil))
;; (setq frame-title-format
;;       '((buffer-file-name "%f" "%b")))

(if (display-graphic-p)
    (progn
      ;; start frame of emacs maximized
      (add-to-list 'initial-frame-alist '(fullscreen . maximized))
      ;; new frames
      (setq default-frame-alist
            '(
              (top . 25)
              (left . 275)
              (width . 106) ;; chars
              (height . 60) ;; lines
              ))))

;;;; Frame titlebar
;; Theme transparent titlebar
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Fix titlebar titling colors
;; see also https://github.com/d12frosted/homebrew-emacs-plus/issues/55
(use-package ns-auto-titlebar
  :commands ns-auto-titlebar-mode
  :if (eq system-type 'darwin)
  :init (ns-auto-titlebar-mode))

;; no border title
;; (setq default-frame-alist '((undecorated . t)))

;;;; No frame scroll bars
(defun cpm/disable-scroll-bars (frame)
  "Disable scroll bars on new frames"
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'cpm/disable-scroll-bars)

;;; Scale Text
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") 'text-scale-adjust)

;;; Line Numbers
(use-package display-line-numbers
  :ensure nil
  ;; :hook (markdown-mode prog-mode)
  :commands display-line-numbers-mode
  :init
  (setq-default display-line-numbers-type 'visual))

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

;;; Icons
(use-package all-the-icons
  :after dashboard)
;;dependency
(use-package font-lock+
  :defer 1)
;; icons for dired
(use-package all-the-icons-dired
  :defer t
  :commands all-the-icons-dired-mode
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;;; Beacon
(use-package beacon
 :defer 5
 :config
 (beacon-mode 1)
 (add-to-list 'beacon-dont-blink-major-modes 'eshell-mode))

;;; Emoji
(use-package emojify
  :commands (emojify-mode emojify-apropos-emoji)
  ;; :hook ((prog-mode markdown-mode) . emojify-mode)
  :config
  (setq emojify-emojis-dir (concat cpm-etc-dir "emojis")))


;;; Tabs
;; FIXME ;; cool but needs to mature
(eval-when-compile
  (quelpa '(centaur-tabs :fetcher github :repo "ema2159/centaur-tabs")))
(use-package centaur-tabs
  :disabled
  :defer .5
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (dired-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (magit-log-mode . centaur-tabs-local-mode)
  (magit-diff-mode . centaur-tabs-local-mode)
  (magit-status-mode . centaur-tabs-local-mode)
  (magit-process-mode . centaur-tabs-local-mode)
  (magit-stashes-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  (help-mode . centaur-tabs-local-mode)
  (fundamental-mode . centaur-tabs-local-mode)
  (lisp-interaction-mode . centaur-tabs-local-mode)
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-background-color (face-background 'default))
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar t)
  (setq centaur-tabs-set-modified-marker t)
  (centaur-tabs-inherit-tabbar-faces)
  :bind
  ("s-[" . centaur-tabs-backward)
  ("s-]" . centaur-tabs-forward))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-ui)
