;; Mods and functions for Nano config
;;https://emacs.stackexchange.com/a/52804/11934
(setq custom--inhibit-theme-enable nil)

;;; Nano
(use-package nano
  :straight (:type git :host github :repo "rougier/nano-emacs"
             :fork (:host github :repo "mclear-tools/nano-emacs" :branch "test-new-stuff"))
  :config
  (require 'nano-base-colors)
  (require 'nano-colors)
  (require 'nano-faces)
  (require 'nano-theme)
  (require 'nano-theme-dark)
  (require 'nano-theme-light)
  (require 'nano-splash)
  (require 'nano-modeline))



;;; Nano Themes
;;;; Define Nano Themes

(defun nano-theme-dark ()
  "Enable dark Nano theme and customizations."
  (interactive)
  (nano-theme-set-dark)
  (nano-faces)
  (nano-theme)
  ;; Fall back font for glyph missing in Roboto
  (defface fallback '((t :family "Fira Code"
                         :inherit 'nano-face-faded)) "Fallback")
  (set-display-table-slot standard-display-table 'truncation
                          (make-glyph-code ?… 'fallback))
  (set-display-table-slot standard-display-table 'wrap
                          (make-glyph-code ?↩ 'fallback))
  )

(defun nano-theme-light ()
  "Enable light Nano theme and customizations."
  (interactive)
  (nano-theme-set-light)
  (nano-faces)
  (nano-theme)
  ;; Fall back font for glyph missing in Roboto
  (defface fallback '((t :family "Fira Code"
                         :inherit 'nano-face-faded)) "Fallback")
  (set-display-table-slot standard-display-table 'truncation
                          (make-glyph-code ?… 'fallback))
  (set-display-table-slot standard-display-table 'wrap
                          (make-glyph-code ?↩ 'fallback))
  )

;;;; Custom Faces for Nano Themes


;; pulled from nano-themes to use independently
;; This is material to get the mode line to run separately of the theme;
;; as well as attempts as custom functions
(defun nano-theme--mode-line ()
  "Derive mode-line and header-line faces from nano-faces."
  (set-face-attribute 'mode-line nil
                      :height 0.1
                      :foreground (face-background 'nano-face-default)
                      :background (face-background 'nano-face-default)
                      :overline (face-background 'nano-face-subtle)
                      :underline nil
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :height 0.1
                      :foreground (face-background 'nano-face-default)
                      :background (face-background 'nano-face-default)
                      :overline (face-background 'nano-face-subtle)
                      :underline nil
                      :inherit nil
                      :box nil)
  ;;(when (display-graphic-p)
  (set-face-attribute 'header-line nil
                      :weight 'light
                      :foreground (face-foreground 'nano-face-default)
                      :background (face-background 'nano-face-default)

                      :overline nil
                      :underline nil
                      :box nil
                      :box `(:line-width 1
                             :color ,(face-background 'nano-face-default)
                             :style nil)
                      :inherit nil)

  (set-face-attribute 'internal-border nil
                      :background (face-background 'nano-face-default)))


(defun nano-theme--minibuffer ()
  "Derive minibuffer / echo area faces from nano faces."
  ;; Minibuffer / echo area
  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'nano-face-faded)))))




;; These don't work
(defun cpm/nano-custom-faces ()
  "custom faces for nano theme"
  (set-face 'italic                                     'nano-face-salient))

;; these seem unnecessary
(defun nano-theme--magit ()
  ;; Inherit theme for  Magit
  (with-eval-after-load 'magit
    (set-face 'magit-branch 'nano-face-strong)
    (set-face 'magit-diff-context-highlight 'nano-face-subtle)
    (set-face 'magit-diff-file-header 'nano-face-subtle)
    (set-face 'magit-diffstat-added 'nano-face-critical)
    (set-face 'magit-diffstat-removed 'nano-face-popout)
    (set-face 'magit-hash 'nano-face-background)
    (set-face 'magit-hunk-heading 'nano-face-salient)
    (set-face 'magit-hunk-heading-highlight 'nano-face-popout)
    (set-face 'magit-item-highlight 'nano-face-critical)
    (set-face 'magit-log-author 'nano-face-subtle)
    (set-face 'magit-process-ng 'nano-face-bold)
    (set-face 'magit-process-ok 'nano-face-salient)
    (set-face 'magit-section-heading 'nano-face-subtle)
    (set-face 'magit-section-highlight 'nano-face-critical)))


;;; Gruvbox Theme
(use-package gruvbox-theme
  :straight t
  :if (not (display-graphic-p))
  :demand t
  :config
  (load-theme 'gruvbox t))

;;; Doom Themes
(use-package doom-themes
  :straight t
  :defer 1
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable custom treemacs theme (all-the-icons must be installed!)
  (setq doom-themes-treemacs-theme "doom-colors")) ; use the colorful treemacs theme

;; I get errors if I don't load these functions separately
(with-eval-after-load 'doom-themes
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))



;;; Solarized Theme
;;;; Solarized Package Settings
(use-package solarized-theme
  ;; :if (display-graphic-p)
  ;; :demand t
  :defer t
  :init
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
  (setq solarized-scale-org-headlines t))

;;;; Customized Solarized Faces
;;;;;   Solarized Dark
(defun cpm/solarized-dark ()
  "My customized solarized dark theme"
  ;; for issues with emacs 27 see https://emacs.stackexchange.com/a/52804/11934
  ;; (let ((custom--inhibit-theme-enable nil))
  (interactive)
  (mapc #'disable-theme custom-enabled-themes) ; clear any existing themes
  (load-theme 'solarized-dark t)
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
   `(ivy-posframe-border ((t (:background "#073642"))))
   `(which-key-posframe ((t (:background "#073642"))))
   `(which-key-posframe-border ((t (:background "#073642"))))
   `(helm-posframe ((t (:background "#073642"))))
   `(helm-posframe-border ((t (:background "#073642"))))
   `(frog-menu-posframe-background-face ((t (:background "#073642"))))

   ;; helm faces
   `(helm-selection ((t (:background "#1f4a54" :foreground "goldenrod1" :underline nil))))
   `(helm-match ((t (:foreground "#b58900"))))

   ;; line number highlighting
   `(line-number-current-line ((t (:background "#002833" :foreground "goldenrod1"))))
   `(line-number ((t (:background "#002833" :foreground "#586e75" :weight thin))))
   ;; '(nlinum-current-line ((t (:inherit default :foreground "goldenrod1"))))
   `(linum-highlight-face ((t (:foreground "goldenrod1"))))
   ;; '(nlinum-hl-face ((t (:inherit default :foreground "goldenrod1"))))

   ;; battery faces
   `(fancy-battery-charging ((t (:foreground "dark blue" :weight bold))))
   `(fancy-battery-critical ((t (:foreground "dark red" :weight bold))))
   `(fancy-battery-discharging ((t (:foreground "dark magenta" :weight bold))))))

;;;;;   Solarized Light
(defun cpm/solarized-light ()
  "My customized solarized-light theme"
  (interactive)
  (mapc #'disable-theme custom-enabled-themes) ; clear any existing themes
  (load-theme 'solarized-light t)
  ;; for issues with emacs 27 see https://emacs.stackexchange.com/a/52804/11934
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
   `(ivy-minibuffer-match-face-1 ((t (:foreground "#64b5ea"))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground "#64b5ea"))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground "#64b5ea"))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground "#64b5ea"))))
   `(ivy-remote ((t (:foreground "#268bd2"))))
   `(swiper-line-face ((t (:weight bold :background "#fdf6e3" :underline nil))))
   `(swiper-match-face-1 ((t (:foreground "#64b5ea"))))
   `(swiper-match-face-2 ((t (:foreground "#64b5ea"))))
   `(swiper-match-face-3 ((t (:foreground "#64b5ea"))))
   `(swiper-match-face-4 ((t (:foreground "#64b5ea"))))

   ;; posframe faces
   `(hydra-posframe-face ((t (:background "#eee8d5"))))
   `(ivy-posframe ((t (:background "#eee8d5"))))
   `(ivy-posframe-border ((t (:background "#eee8d5"))))
   `(which-key-posframe ((t (:background "#eee8d5"))))
   `(which-key-posframe-border ((t (:background "#eee8d5"))))
   `(helm-posframe ((t (:background "#eee8d5"))))
   `(helm-posframe-border ((t (:background "#eee8d5"))))
   `(frog-menu-posframe-background-face ((t (:background "#eee8d5"))))

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



;;; Modus Operandi Theme
(use-package modus-themes
  :disabled
  :straight t
  :init
  (modus-themes-load-themes)
  :config
  (setq modus-themes-bold-constructs t
        modus-themes-slanted-constructs t
        modus-themes-syntax "yellow-comments-green-strings"
        modus-themes-prompts "subtle-accented"
        modus-themes-mode-line "borderless-moody"
        modus-themes-completions "moderate"
        modus-themes-paren-match "subtle-bold"
        modus-themes-scale-headings t
        modus-themes-variable-pitch-headings t)
  (modus-themes-load-vivendi))


;;; Disable All Custom Themes
;; function to disable all themes
(defun cpm/disable-all-themes ()
  "disable all active themes."
  (interactive)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;;; Toggle Menubar
;; toggle menubar to light or dark
(defun cpm/osx-toggle-menubar-theme ()
  "toggle menubar to dark or light using shell command"
  (interactive)
  (shell-command "dark-mode"))
(defun cpm/osx-menubar-theme-light ()
  "turn dark mode off"
  (interactive)
  (shell-command "dark-mode off"))
(defun cpm/osx-menubar-theme-dark ()
  "turn dark mode on"
  (interactive)
  (shell-command "dark-mode on"))

;;; Theme & menubar toggle
;; Coordinate setting of theme with os theme
;; (setq active-theme 'nano-theme-light)
(defun toggle-dark-light-theme ()
  (interactive)
  (if (eq active-theme 'nano-theme-light)
      (progn (cpm/osx-menubar-theme-dark)
             ;; (cpm/disable-all-themes)
             (nano-theme-dark)
             ;; (force-mode-line-update)
             (setq active-theme 'nano-theme-dark))
    (progn (cpm/osx-menubar-theme-light)
           ;; (cpm/disable-all-themes)
           (nano-theme-light)
           ;; (force-mode-line-update)
           (setq active-theme 'nano-theme-light))))

;;; Set Theme by Timer
;; Inspired by https://github.com/hmatheisen/theme-switcher
;; When emacs is launched in the evening automatically load the dark theme
;; set to dark theme after 6pm
;; (defvar day-hour 08
;;   "The hour when the theme goes from dark to light in the morning. Default is 8am. ")

;; (defvar night-hour 18
;;   "The hour when the theme goes from light to dark in the evening. Default is 6pm.")

;; (let ((now (string-to-number (format-time-string "%H"))))
;;   (if (and (>= now day-hour) (< now night-hour))
;;       (progn
;;         (cpm/osx-menubar-theme-light)
;;         (nano-theme-light))
;;     (progn
;;       (cpm/osx-menubar-theme-dark)
;;       (nano-theme-dark))))

;;; System Appearance Hook
;; See https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
(defun cpm/system-apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (progn (nano-theme-light) (setq active-theme 'nano-theme-light)))
    ('dark (progn (nano-theme-dark) (setq active-theme 'nano-theme-dark)))))

(add-hook 'ns-system-appearance-change-functions #'cpm/system-apply-theme)

;;; End Provide Nano Personal
(provide 'setup-theme)
