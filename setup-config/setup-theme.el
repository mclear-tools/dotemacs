;; Mods and functions for Nano config
;;https://emacs.stackexchange.com/a/52804/11934
(setq custom--inhibit-theme-enable nil)

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




(defun cpm/nano-custom-faces ()
  "custom faces for nano theme"
  (set-face 'italic                                     'nano-face-salient))

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


;;;; Disable All Custom Themes
(defun cpm/disable-all-themes ()
  "disable all active themes."
  (interactive)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;;;; Toggle Menubar
(defun cpm/osx-toggle-menubar-theme ()
  (interactive)
  (shell-command "dark-mode"))
(defun cpm/osx-menubar-theme-light ()
  (interactive)
  (shell-command "dark-mode off"))
(defun cpm/osx-menubar-theme-dark ()
  (interactive)
  (shell-command "dark-mode on"))

;;;; Theme & menubar toggle
(setq active-theme 'nano-theme-dark)
(defun toggle-dark-light-theme ()
  (interactive)
  (if (eq active-theme 'nano-theme-light)
      (progn (cpm/osx-menubar-theme-dark)
             (cpm/disable-all-themes)
             (nano-theme-dark)
             (force-mode-line-update)
             (setq active-theme 'nano-theme-dark))
    (progn (cpm/osx-menubar-theme-light)
           (cpm/disable-all-themes)
           (nano-theme-light)
           (force-mode-line-update)
           (setq active-theme 'nano-theme-light))))

;;;; Night Timer
;; Got the idea from https://github.com/hmatheisen/theme-switcher
;; When emacs is launched in the evening automatically load the dark theme
(defvar day-hour 08
  "The hour when the theme goes from dark to light in the morning. Default is 8am. ")

(defvar night-hour 18
  "The hour when the theme goes from light to dark in the evening. Default is 6pm.")

;; (let ((now (string-to-number (format-time-string "%H"))))
;;   (if (and (>= now day-hour) (< now night-hour))
;;       (nano-theme-light)
;;     (progn
;;       (cpm/osx-menubar-theme-dark)
;;       (nano-theme-dark))))



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
  (require 'nano-modeline)
  )



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
  ;; :defer 1
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



;;; Set Theme

(nano-theme-dark)
;; (nano-theme--mode-line)

;;; End Provide Nano Personal
(provide 'setup-theme)
