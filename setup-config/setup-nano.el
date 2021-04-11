;;; Nano
;; Mods and functions for Nano config
(use-package nano
  :disabled t
  ;; :defer 2
  :straight (:type git :host github :repo "rougier/nano-emacs"
             :fork (:host github :repo "mclear-tools/nano-emacs" :branch "test-new-stuff"))
  :config
  ;; (require 'nano-base-colors)
  ;; (require 'nano-colors)
  ;; (require 'nano-faces)
  ;; (require 'nano-theme)
  ;; (require 'nano-theme-dark)
  ;; (require 'nano-theme-light)
  ;; (require 'nano-splash)
  ;; (require 'nano-modeline)
  )



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


;;; Theme & menubar toggle
;; Coordinate setting of theme with os theme
;; (setq active-theme 'nano-theme-light)
;; (defun toggle-dark-light-theme ()
;;   (interactive)
;;   (if (eq active-theme 'nano-theme-light)
;;       (progn (cpm/osx-menubar-theme-dark)
;;              ;; (cpm/disable-all-themes)
;;              (nano-theme-dark)
;;              ;; (force-mode-line-update)
;;              (setq active-theme 'nano-theme-dark))
;;     (progn (cpm/osx-menubar-theme-light)
;;            ;; (cpm/disable-all-themes)
;;            (nano-theme-light)
;;            ;; (force-mode-line-update)
;;            (setq active-theme 'nano-theme-light))))

(provide 'setup-nano)
