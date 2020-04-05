;; Themes
;; I use solarized except in terminal, where Gruvbox seems to work better

;;; Gruvbox Theme
(use-package gruvbox-theme
  :if (not (display-graphic-p))
  :demand t
  :config
  (load-theme 'gruvbox t))

;;; Solarized Theme
(use-package solarized-theme
  :ensure t
  :if (display-graphic-p)
  :demand t
  ;; :custom
  ;; (custom-enabled-themes (quote (solarized-dark solarized-light)))
  :init
;;;; Solarized Settings
  ;; don't make the fringe stand out from the background
  (setq solarized-distinct-fringe-background nil)
  ;; change the font for some headings and titles
  (setq solarized-use-variable-pitch t)
  ;; make the modeline high contrast
  (setq solarized-high-contrast-mode-line nil)
  ;; use this setting without hi contrast modeline
  (setq x-underline-at-descent-line t)
  ;; Use bolding
  (setq solarized-use-less-bold nil)
  ;; Use more italics
  (setq solarized-use-more-italic t)
  ;; Use colors for indicators such as git:gutter, flycheck and similar
  (setq solarized-emphasize-indicators t)
  ;; Set to nil of you don't want to change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines t)
  :config
;;;; Hooks for Custom Theme
  (defvar after-load-theme-hook nil
    "Hook run after a color theme is loaded using `load-theme'.")
  (defadvice load-theme (after run-after-load-theme-hook activate)
    "Run `after-load-theme-hook'."
    (run-hooks 'after-load-theme-hook))

;;;; Solarized Dark
  (defun customize-solarized-dark ()
    "Customize solarized theme"
    ;; see https://emacs.stackexchange.com/a/52804/11934
    ;; (let ((custom--inhibit-theme-enable nil))
    (custom-theme-set-faces
     'solarized-dark

     ;; make bg darker for higher contrast & foreground slightly lighter
     `(default ((t (:foreground "#8f9ea0" :background "#002833"))))

     ;; matching fringe
     `(fringe ((t (:background "#002833" :foreground "#586e75"))))

     ;; fix modeline underline
     `(mode-line ((t (:background "#073642" :foreground "#839496" :box (:line-width 1 :color "#002833" :style unspecified) :overline "#002833" :underline "#002833"))))

     ;; terminal
     `(term ((t (:background "#002833" :foreground "#839496"))))

     ;; org faces
     `(org-block ((t (:foreground "#2E8B57"))))
     `(org-block-begin-line ((t (:foreground "#74a8a4" :weight bold :slant normal))))
     `(org-block-end-line ((t (:foreground "#74a8a4" :weight bold :slant normal))))
     `(org-level-1 ((t (:inherit variable-pitch :foreground "#268bd2" :height 1.5))))
     `(org-level-2 ((t (:inherit variable-pitch :foreground "medium sea green" :height 1.3))))
     `(org-level-3 ((t (:inherit variable-pitch :foreground "#cb4b16" :height 1.2))))
     `(org-level-4 ((t (:inherit variable-pitch :foreground "#6c71c4" :height 1.15))))
     `(org-level-8 ((t (:inherit variable-pitch :foreground "#9e1e86" :height 1.1))))
     `(org-quote ((t (:inherit org-block :slant normal :weight normal))))
     `(org-agenda-date ((t (:background "#002833" :foreground "dark cyan" :inverse-video nil :box (:line-width 5 :color "#002833") :overline nil :underline t :slant normal :weight normal :height 1.5 :family "Avenir Next"))))
     `(org-agenda-date-today ((t (:inherit org-agenda-date :background "#268bd2" :foreground "#002833" :inverse-video t :box nil :overline nil :weight bold))))
     `(org-tag ((t (:inherit font-lock-comment-face :weight bold :height 0.9))))

     ;; markdown faces
     `(markdown-comment-face ((t (:weight normal :slant italic :strike-through nil))))
     `(markdown-header-face-1 ((t (:inherit variable-pitch :foreground "#268bd2" :height 1.75))))
     `(markdown-header-face-2 ((t (:inherit variable-pitch :foreground "medium sea green" :height 1.45))))
     `(markdown-header-face-3 ((t (:inherit variable-pitch :foreground "#cb4b16" :height 1.2))))

     ;; ivy faces
     `(ivy-confirm-face ((t (:foreground "#859900"))))
     `(ivy-current-match ((t (:weight bold :foreground "goldenrod1" :background "#1f4a54" :underline nil))))
     `(ivy-match-required-face ((t (:foreground "#dc322f"))))
     `(ivy-minibuffer-match-face-1 ((t (:foreground "#8f9ea0"))))
     `(ivy-minibuffer-match-face-2 ((t (:foreground "goldenrod1"))))
     `(ivy-minibuffer-match-face-3 ((t (:foreground "goldenrod1"))))
     `(ivy-minibuffer-match-face-4 ((t (:foreground "goldenrod1"))))
     `(ivy-remote ((t (:foreground "#268bd2"))))
     `(swiper-line-face ((t (:weight bold :background "#1f4a54" :underline nil))))
     `(swiper-match-face-1 ((t (:foreground "#8f9ea0"))))
     `(swiper-match-face-2 ((t (:foreground "goldenrod1"))))
     `(swiper-match-face-3 ((t (:foreground "goldenrod1"))))
     `(swiper-match-face-4 ((t (:foreground "goldenrod1"))))


     ;; posframe faces
     `(hydra-posframe-face ((t (:background "#073642"))))
     `(ivy-posframe ((t (:background "#073642"))))
     `(which-key-posframe ((t (:background "#073642"))))
     `(helm-posframe-face ((t (:background "#073642"))))

     ;; helm faces
     `(helm-selection ((t (:background "#1f4a54" :foreground "goldenrod1" :underline nil))))
     `(helm-match ((t (:foreground "#b58900"))))

     ;; line number highlighting
     `(line-number-current-line ((t (:inherit default :foreground "goldenrod1"))))
     ;; '(nlinum-current-line ((t (:inherit default :foreground "goldenrod1"))))
     `(linum-highlight-face ((t (:inherit default :foreground "goldenrod1"))))
     ;; '(nlinum-hl-face ((t (:inherit default :foreground "goldenrod1"))))

     ;; battery faces
     `(fancy-battery-charging ((t (:foreground "dark blue" :weight bold))))
     `(fancy-battery-critical ((t (:foreground "dark red" :weight bold))))
     `(fancy-battery-discharging ((t (:foreground "dark magenta" :weight bold))))))
  (add-hook 'after-load-theme-hook 'customize-solarized-dark)

;;;; Load Theme
  (load-theme 'solarized-dark t))

;;;; Solarized Light
(with-eval-after-load 'solarized-theme
  (defun customize-solarized-light ()
    "Customize solarized theme"
    ;; (let ((custom--inhibit-theme-enable nil))
    (custom-theme-set-faces
     'solarized-light
     ;; increase text contrast
     ;; `(default ((t (:background "#fdf6e3" :foreground "#727e80"))))
     `(default ((t (:background "#fdf6e3" :foreground "#667173"))))

     ;; fix modeline underline
     `(mode-line ((t (:background "#eee8d5" :foreground "#657b83" :box (:line-width 1 :color "#fdf6e3" :style unspecified) :overline "#fdf6e3" :underline "#fdf6e3"))))
     ;; org faces
     `(org-block ((t (:foreground "#2E8B57"))))
     `(org-block-begin-line ((t (:foreground "#74a8a4" :weight bold :slant normal))))
     `(org-block-end-line ((t (:foreground "#74a8a4" :weight bold :slant normal))))
     `(org-level-1 ((t (:inherit variable-pitch :foreground "#268bd2" :height 1.3))))
     `(org-level-2 ((t (:inherit variable-pitch :foreground "medium sea green" :height 1.2))))
     `(org-level-3 ((t (:inherit variable-pitch :foreground "#cb4b16" :height 1.15))))
     `(org-level-4 ((t (:inherit variable-pitch :foreground "#6c71c4" :height 1.15))))
     `(org-level-8 ((t (:inherit variable-pitch :foreground "#9e1e86" :height 1.1))))
     `(org-quote ((t (:inherit org-block :slant normal :weight normal))))
     `(org-agenda-date ((t (:background "#fdf6e3" :foreground "dark cyan" :inverse-video nil :box (:line-width 5 :color "#002833") :overline nil :underline t :slant normal :weight normal :height 1.5 :family "Avenir Next"))))
     `(org-agenda-date-today ((t (:inherit org-agenda-date :background "#268bd2" :foreground "#fdf6e3" :inverse-video t :box nil :overline nil :weight bold))))
     `(org-tag ((t (:inherit font-lock-comment-face :weight bold :height 0.9))))

     ;; markdown faces
     `(markdown-comment-face ((t (:weight normal :slant italic :strike-through nil))))
     `(markdown-header-face-1 ((t (:inherit variable-pitch :foreground "#268bd2" :height 1.75))))
     `(markdown-header-face-2 ((t (:inherit variable-pitch :foreground "medium sea green" :height 1.45))))
     `(markdown-header-face-3 ((t (:inherit variable-pitch :foreground "#cb4b16" :height 1.2))))

     ;; ivy faces
     `(ivy-confirm-face ((t (:foreground "#859900"))))
     `(ivy-current-match ((t (:weight bold :foreground "#268bd2" :background "#fdf6e3" :underline nil))))
     `(ivy-match-required-face ((t (:foreground "#dc322f"))))
     `(ivy-minibuffer-match-face-1 ((t (:foreground "#8f9ea0"))))
     `(ivy-minibuffer-match-face-2 ((t (:foreground "goldenrod1"))))
     `(ivy-minibuffer-match-face-3 ((t (:foreground "goldenrod1"))))
     `(ivy-minibuffer-match-face-4 ((t (:foreground "goldenrod1"))))
     `(ivy-remote ((t (:foreground "#268bd2"))))
     `(swiper-line-face ((t (:weight bold :background "#fdf6e3" :underline nil))))
     `(swiper-match-face-1 ((t (:foreground "#8f9ea0"))))
     `(swiper-match-face-2 ((t (:foreground "goldenrod1"))))
     `(swiper-match-face-3 ((t (:foreground "goldenrod1"))))
     `(swiper-match-face-4 ((t (:foreground "goldenrod1"))))


     ;; posframe faces
     `(hydra-posframe-face ((t (:background "#eee8d5"))))
     `(ivy-posframe ((t (:background "#eee8d5"))))
     `(which-key-posframe ((t (:background "#eee8d5"))))
     `(helm-posframe ((t (:background "#eee8d5"))))


     ;; helm faces
     `(helm-selection ((t (:background "#fdf6e3" :foreground "#268bd2" :underline nil :weight bold))))
     `(helm-match ((t (:foreground "#cb4b16" :weight bold))))

     ;; '(helm-selection ((t (:foreground "#f7f438" :background "#64b5ea" :underline nil :weight bold))))
     ;; line size
     `(set-face-attribute 'linum nil :inherit 'fixed-pitch)
     ;; line highlighting
     `(linum-highlight-face ((t (:inherit default :foreground "#002b36"))))
     ;; '(nlinum-hl-face ((t (:inherit default :foreground "#002b36"))))
     `(line-number-current-line ((t (:inherit default :foreground "#002b36"))))
     ;; '(nlinum-current-line ((t (:inherit default :foreground "#002b36"))))
     ;; battery faces
     `(fancy-battery-charging ((t (:foreground "dark blue" :weight bold))))
     `(fancy-battery-critical ((t (:foreground "dark red" :weight bold))))
     `(fancy-battery-discharging ((t (:foreground "dark magenta" :weight bold))))))
  (add-hook 'after-load-theme-hook 'customize-solarized-light))


;;; Toggle Menubar
(defun cpm/osx-toggle-menubar-theme ()
  (interactive)
  (shell-command "dark-mode"))
(defun cpm/osx-menubar-theme-light ()
  (interactive)
  (shell-command "dark-mode off"))
(defun cpm/osx-menubar-theme-dark ()
  (interactive)
  (shell-command "dark-mode on"))

;;; Theme & menubar toggle
(setq active-theme 'solarized-dark)
(defun toggle-dark-light-theme ()
  (interactive)
  (if (eq active-theme 'solarized-light)
      (progn (setq active-theme 'solarized-dark)
             (cpm/osx-menubar-theme-dark)
             (force-mode-line-update))
    (progn (setq active-theme 'solarized-light)
           (cpm/osx-menubar-theme-light)
           (force-mode-line-update)))
  (load-theme active-theme))

;;; Packaging Themes

;; (defvar packages-appearance '(doom-themes
;;                               nord-theme
;;                               solarized-theme
;;                               zenburn-theme
;;                               molokai-theme
;;                               darktooth-theme
;;                               gotham-theme
;;                               ample-theme
;;                               material-theme
;;                               leuven-theme
;;                               spacemacs-theme
;;                               gruvbox-theme
;;                               forest-blue-theme
;;                               flatland-theme
;;                               afternoon-theme
;;                               cyberpunk-theme
;;                               darkmine-theme
;;                               tao-theme
;;                               darkokai-theme
;;                               jazz-theme
;;                               suscolors-theme
;;                               omtose-phellack-theme
;;                               atom-one-dark-theme
;;                               nubox
;;                               color-theme-sanityinc-tomorrow
;;                               alect-themes
;;                               kaolin-themes
;;                               srcery-theme)
;;   "A list of themes to ensure are installed at launch.")

;; (defun appearance-packages-installed-p ()
;;   (loop for p in packages-appearance
;;         when (not (package-installed-p p)) do (return nil)
;;         finally (return t)))

;; (unless (appearance-packages-installed-p)
;;   ;; check for new packages (package versions)
;;   (message "%s" "Emacs is now refreshing its package themes...")
;;   (package-refresh-contents)
;;   (message "%s" " done.")
;;   ;; install the missing packages
;;   (dolist (p packages-appearance)
;;     (when (not (package-installed-p p))
;;       (package-install p))))

;; (provide 'packages-appearance)


;;; End setup-theme.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-theme)
