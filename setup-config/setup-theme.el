;; Themes
;; For help on custom themeing see https://emacs.stackexchange.com/questions/17431/how-do-i-change-portions-of-a-custom-theme

;;https://emacs.stackexchange.com/a/52804/11934
(setq custom--inhibit-theme-enable nil)


;;; Disable All Custom Themes
(defun cpm/disable-all-themes ()
  "disable all active themes."
  (interactive)
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

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
(setq active-theme 'nano-theme-light)
(defun toggle-dark-light-theme ()
  (interactive)
  (if (eq active-theme 'nano-theme-light)
      (progn (cpm/osx-menubar-theme-dark)
             (cpm/disable-all-themes)
             (nano-theme-set-dark)
             (force-mode-line-update)
             (setq active-theme 'nano-theme-dark))
    (progn (cpm/osx-menubar-theme-light)
           (cpm/disable-all-themes)
           (nano-theme-set-light)
           (force-mode-line-update)
           (setq active-theme 'nano-theme-light))))

;;; Night Timer
;; Got the idea from https://github.com/hmatheisen/theme-switcher
;; When emacs is launched in the evening automatically load the dark theme
(defvar day-hour 08
  "The hour when the theme goes from dark to light in the morning. Default is 8am. ")

(defvar night-hour 18
  "The hour when the theme goes from light to dark in the evening. Default is 6pm.")

;; (let ((now (string-to-number (format-time-string "%H"))))
;;   (if (and (>= now day-hour) (< now night-hour))
;;       (nano-theme-set-light)
;;     (progn
;;       (setq active-theme 'nano-theme-dark)
;;       (cpm/osx-menubar-theme-dark)
;;       (nano-theme-set-dark))))

;;; Load Theme with System Mode
;; See https://www.reddit.com/r/emacs/comments/hejsqm/is_there_a_way_to_detect_lightdark_mode_on_mac/fvrr382?utm_source=share&utm_medium=web2x

(defun cpm/set-system-theme-mode ()
  (interactive)
  (if (string= (shell-command-to-string "printf %s \"$( osascript -e \'tell application \"System Events\" to tell appearance preferences to return dark mode\' )\"") "true")
      (progn
        (cpm/disable-all-themes)
        ;; (shell-command-to-string "defaults write org.gnu.Emacs TransparentTitleBar DARK")
        (cpm/solarized-dark))
    (progn
      (cpm/disable-all-themes)
      ;; (shell-command-to-string "defaults write org.gnu.Emacs TransparentTitleBar LIGHT")
      (cpm/solarized-light))))

;; (cpm/set-system-theme-mode)

;;; Packaging Themes
;; I don't really use any other themes so I've disabled this
;; I'm keeping it mainly as a list of themes I like
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
