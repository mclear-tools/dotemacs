;; Theming functions

;;; Theme Custom Folder
(setq-default custom-theme-directory (concat cpm-local-dir "custom-themes/"))
(add-to-list 'custom-theme-load-path (concat custom-theme-directory "bespoke-themes/"))

;;; What Face?
;; https://stackoverflow.com/a/66287459/6277148
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;; Autothemer

(use-package autothemer
  :straight t)

;;; Gruvbox Theme
(use-package gruvbox-theme
  ;; :if (not (display-graphic-p))
  :defer t)

;;; Doom Themes
(use-package doom-themes
  :defer 2
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable custom treemacs theme (all-the-icons must be installed!)
  (setq doom-themes-treemacs-theme "doom-colors")) ; use the colorful treemacs theme

;; I get errors if I don't load these functions separately
;; (with-eval-after-load 'doom-themes
;;   ;; Enable flashing mode-line on errors
;; (doom-themes-visual-bell-config)
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;;; Bespoke Theme
(use-package bespoke-themes
  :straight (:type built-in)
  :load-path ".local/custom-themes/bespoke-themes"
  ;; :custom
  ;; (set-bespoke-header-line nil)
  :config
  ;; (bespoke-header-line)
  ;; (setq-default header-line-format mode-line-format)
  ;; use mode line visual bell
  (bespoke-themes-visual-bell-config))

;;; Disable All Custom Themes
(defun cpm/disable-all-themes ()
  "disable all active themes."
  (interactive)
  (dolist (i custom-enabled-themes)
    (disable-theme i))
  (setq-default header-line-format nil)
  (setq-default mode-line-format cpm--mode-line-format)
  (setq x-underline-at-descent-line t)
  (force-mode-line-update)
  (cpm/revert-all-file-buffers))

;;; Load Theme Wrapper
(defun cpm/load-theme ()
  (interactive)
  (progn
    (cpm/disable-all-themes)
    (call-interactively 'load-theme)))

;;; Toggle Menubar
;; toggle menubar to light or dark
(defun cpm/osx-toggle-menubar-theme ()
  "toggle menubar to dark or light using shell command"
  (interactive)
  (shell-command "dark-mode"))
(defun cpm/osx-menubar-theme-light ()
  "turn dark mode off"
  (interactive)
  (shell-command "dark-mode off")
  (load-theme 'bespoke-light t))
(defun cpm/osx-menubar-theme-dark ()
  "turn dark mode on"
  (interactive)
  (shell-command "dark-mode on")
  (load-theme 'bespoke-dark t))


;;; Theme & menubar toggle
(defun toggle-dark-light-theme ()
  "Coordinate setting of theme with os theme and toggle"
  (interactive)
  (if (eq active-theme 'light-theme)
      (progn (cpm/osx-menubar-theme-dark)
             (setq active-theme 'dark-theme))
    (progn (cpm/osx-menubar-theme-light)
           (setq active-theme 'light-theme))))


;;; Minibuffer & Echo
(defun bespoke-theme--minibuffer ()
  "Derive minibuffer / echo area faces from bespoke faces."
  ;; Minibuffer / echo area
  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'fringe)))))
(bespoke-theme--minibuffer)

;;; System Appearance Hook
;; See https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
(defun cpm/system-apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (progn (load-theme 'bespoke-light t)
                   (setq active-theme 'light-theme)))
    ('dark (progn (load-theme 'bespoke-dark t)
                  (setq active-theme 'dark-theme)))))

(add-hook 'ns-system-appearance-change-functions #'cpm/system-apply-theme)

;;; Reload Active Theme
(defun cpm/reload-active-theme ()
  "reload current bespoke theme"
  (interactive)
  (if (eq active-theme 'light-theme)
      (load-theme 'bespoke-light t)
    (load-theme 'bespoke-dark t)))

;;; End setup-theme

(provide 'setup-theme)
