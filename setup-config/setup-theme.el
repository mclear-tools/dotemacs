;; Theming functions

;;; No-confirm themes
(setq custom-safe-themes t)

;;; Custom Theme Folder
(setq-default custom-theme-directory (concat cpm-local-dir "custom-themes/"))
;; find all themes recursively in custom-theme-folder
(let ((basedir custom-theme-directory))
  (dolist (f (directory-files basedir))
    (if (and (not (or (equal f ".") (equal f "..")))
             (file-directory-p (concat basedir f)))
        (add-to-list 'custom-theme-load-path (concat basedir f)))))

;;; What Face?
;; https://stackoverflow.com/a/66287459/6277148
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;; Doom Themes
(use-package doom-themes
  :defer 2
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable custom treemacs theme (all-the-icons must be installed!)
  (setq doom-themes-treemacs-theme "doom-colors")) ; use the colorful treemacs theme

;;; Bespoke Theme
(use-package bespoke-themes
  :straight (:type built-in)
  :load-path ".local/custom-themes/bespoke-themes"
  :config
  ;; Set mode line as header line
  (setq bespoke-set-mode-line 'header)
  ;; Set mode line height
  (setq bespoke-set-mode-line-height 3)
  ;; Set up evil cursor colors
  (setq bespoke-set-evil-cursors t)
  ;; Use mode line visual bell
  (setq bespoke-set-visual-bell t)
  ;; Set use of italics
  (setq bespoke-set-italic-comments t
        bespoke-set-italic-keywords t)
  ;; Set variable pitch
  (setq bespoke-set-variable-pitch t)
  ;; Set initial theme variant
  (setq bespoke-set-theme 'dark)
  ;; Load theme
  (load-theme 'bespoke t))

;;; Nano Theme
(use-package nano-theme
  :straight (:host github :repo "rougier/nano-theme"))

(use-package nano-modeline
  :straight (:host github :repo "rougier/nano-modeline")
  :commands (nano-modeline))

;;; Disable All Custom Themes
(defun cpm/disable-all-themes ()
  "disable all active themes."
  (interactive)
  (progn
    (dolist (i custom-enabled-themes)
      (disable-theme i))
    ;; disable window-divider mode
    (window-divider-mode -1)
    ;; revert to mode line
    (setq-default header-line-format nil)
    (setq-default mode-line-format
                  '((:eval
                     (list
                      evil-mode-line-tag
                      "| "
                      "%b "
                      "%m "
                      (cond ((and buffer-file-name (buffer-modified-p))
                             (propertize "(**)" 'face `(:foreground "#f08290")))
                            (buffer-read-only "(RO)" ))
                      " %l:%c %0"
                      " "
                      ))))
    (force-mode-line-update)))

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
  (shell-command "dark-mode off"))
(defun cpm/osx-menubar-theme-dark ()
  "turn dark mode on"
  (interactive)
  (shell-command "dark-mode on"))

;;; Theme & menubar toggle
(setq active-theme 'light-theme)
(defun toggle-dark-light-theme ()
  "Coordinate setting of theme with os theme and toggle"
  (interactive)
  (if (eq active-theme 'light-theme)
      (progn (cpm/osx-menubar-theme-dark)
             (setq active-theme 'dark-theme))
    (progn (cpm/osx-menubar-theme-light)
           (setq active-theme 'light-theme))))

;;; After Load Theme Hook
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))


;;; System Appearance Hook
;; See https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
(defun cpm/system-apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (progn
              (setq bespoke-set-theme 'light)
              (load-theme 'bespoke t)
              (setq active-theme 'light-theme)
              ))
    ('dark (progn
             (setq bespoke-set-theme 'dark)
             (load-theme 'bespoke t)
             (setq active-theme 'dark-theme)
             ))))

(add-hook 'ns-system-appearance-change-functions #'cpm/system-apply-theme)

;;; Reload Active Theme
(defun cpm/bespoke-reload-theme ()
  "reload current bespoke theme"
  (interactive)
  (load-theme 'bespoke t))


;;; End setup-theme

(provide 'setup-theme)
