;; Open a child frame.

;;; Posframe
(use-package posframe)

;;; Vertico Posframe
(use-package vertico-posframe
  :disabled
  :straight (:type git :host github :repo "tumashu/vertico-posframe")
  :if (and (window-system) (version<= "26.1" emacs-version))
  :custom-face
  (vertico-posframe ((t (:background ,bespoke-subtle))))
  (vertico-posframe-border ((t (:background ,bespoke-subtle))))
  (child-frame-border ((t (:background ,bespoke-subtle))))

  :config
  (setq vertico-posframe-border-width 15
        vertico-posframe-height 40
        vertico-posframe-width 70
        vertico-posframe-parameters '((left-fringe . 0)
                                      (right-fringe . 0)
                                      (internal-border-width . 15))
        vertico-posframe-size-function 'cpm/vertico-posframe-size
        vertico-posframe-poshandler 'posframe-poshandler-frame-top-center)
  (vertico-posframe-mode 1))

(defun cpm/vertico-posframe-size ()
  (list
   :min-height (round (* (frame-height) 0.25))
   :min-width (round (* (frame-width) 0.52))))


;;; Transient Posframe
(use-package transient-posframe
  :straight (:host github :repo "yanghaoxie/transient-posframe")
  :if (and (window-system) (version<= "26.1" emacs-version))
  :commands (transient-posframe-mode)
  :hook (magit-mode . transient-posframe-mode)
  :config
  (setq transient-posframe-border-width 20
        transient-posframe-min-height (round (* (frame-height) 0.5))
        transient-posframe-min-width (round (* (frame-width) 0.5))
        transient-posframe-poshandler 'posframe-poshandler-frame-top-center))

;;; Which-Key Posframe
;; this never works reliably -- spacing gets wonky
;; or colors don't change correctly when theme changesd
;; For spacing issue see
;; https://github.com/yanghaoxie/which-key-posframe/issues/10
(use-package which-key-posframe
  :disabled
  :if (and (window-system) (version<= "26.1" emacs-version))
  :hook (after-init . which-key-posframe-mode)
  :custom-face
  (which-key-posframe-border ((t (:background ,bespoke-subtle))))
  (which-key-posframe ((t (:background ,bespoke-subtle))))
  :config
  ;; see https://github.com/yanghaoxie/which-key-posframe/issues/5#issuecomment-527528759
  ;; (defun cpm/posframe-arghandler (buffer-or-name arg-name value)
  ;;   (let ((info '(:width (round (* (frame-width) 1)) :height 85)))
  ;;     (or (plist-get info arg-name) value)))
  ;; (setq posframe-arghandler #'cpm/posframe-arghandler)
  (setq which-key-posframe-border-width 15)
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-top-center))

;; (setq which-key-posframe-font nil)
;; (setq which-key-posframe-border-width 20)
;; (setq which-key-posframe-poshandler 'posframe-poshandler-frame-top-center))

;;; Company Posframe
  (use-package company-posframe
    :if (and (window-system) (version<= "26.1" emacs-version))
    :hook (company-mode . company-posframe-mode))

;;; Mini Popup
;; Pop up for vertico
(use-package mini-popup
  :disabled
  :straight (:host github :repo "minad/mini-popup")
  :if (display-graphic-p)
  ;; ;; Need to set keybindings for vertico
  :general
  (:states '(normal insert motion emacs) :keymaps 'vertico-map
   "<escape>" #'minibuffer-keyboard-quit
   "C-n"      #'vertico-next-group
   "C-p"      #'vertico-previous-group
   "C-j"      #'vertico-next
   "C-k"      #'vertico-previous
   "M-RET"    #'vertico-exit)
  :custom-face
  (mini-popup-default ((t (:background ,bespoke-subtle))))
  (mini-popup-border ((t (:background ,bespoke-subtle))))
  (child-frame-border ((t (:background ,bespoke-subtle))))
  :hook (after-init . mini-popup-mode)
  :config
  (setq  mini-popup--frame-parameters
         '((no-accept-focus . t)
           (min-width . t)
           (min-height . t)
           ;; (top . 0.023)
           (top . 0.0)
           (left . 0.5)
           (width . .855)
           (height . 10)
           (child-frame-border-width . 15)
           (left-fringe . 20)
           (right-fringe . 20)
           (vertical-scroll-bars . nil)
           (horizontal-scroll-bars . nil)
           (menu-bar-lines . 0)
           (tool-bar-lines . 0)
           (tab-bar-lines . 0)
           (no-other-frame . t)
           (unsplittable . t)
           (undecorated . t)
           (cursor-type . t)
           (minibuffer . nil)
           (visibility . nil)
           (no-special-glyphs . t)
           (desktop-dont-save . t)))
  ;; Configure a height function (Example for Vertico)
  (defun mini-popup-height-resize ()
    (* (1+ (min vertico--total vertico-count)) (default-line-height)))
  (defun mini-popup-height-fixed ()
    (* (1+ (if vertico--input vertico-count 0)) (default-line-height)))
  (setq mini-popup--height-function #'mini-popup-height-fixed)

  ;; Disable the minibuffer resizing of Vertico (HACK)
  (advice-add #'vertico--resize-window :around
              (lambda (&rest args)
                (unless mini-popup-mode
                  (apply args))))

  ;; Ensure that the popup is updated after refresh (Consult-specific)
  (add-hook 'consult--completion-refresh-hook
            (lambda (&rest _) (mini-popup--setup)) 99))


;;; Mini-Frame
;; Provides a great ui for completion, similar to posframe
(use-package mini-frame
  :disabled
  :straight (:type git :host github :repo "muffinmad/emacs-mini-frame")
  :hook (after-init . mini-frame-mode)
  :commands (mini-frame-mode)
  :custom
  (mini-frame-show-parameters
   `((top    . 0.0)
     (left   . 0.5)
     (width  . 0.90)
     (height . 11)
     (child-frame-border-width . 15)
     (internal-border-width . 0)
     (left-fringe . 20)
     (right-fringe . 20)
     ))
  (mini-frame-color-shift-step 7)
  (mini-frame-advice-functions '(read-from-minibuffer
                                 read-string
                                 completing-read))
  (mini-frame-resize nil)
  :config
  (setq mini-frame-ignore-commands
        '("edebug-eval-expression" debugger-eval-expression "vertico" vertico-mode))
  (setq mini-frame-resize 'not-set))


;;; Reload Child-Frame for theme
;; Child frames tend to have wonky colors after theme change. This tries to fix that.
(defun cpm/reload-child-frame ()
  "Function to reload child-frame color settings on theme change."
  (interactive)
  (posframe-delete-all)
  (load-library "setup-childframe")
  (custom-reevaluate-setting 'child-frame-border)
  ;; (custom-reevaluate-setting 'mini-popup-background)
  ;; (custom-reevaluate-setting 'mini-popup-border)
  (custom-reevaluate-setting 'which-key-posframe-border)
  (custom-reevaluate-setting 'which-key-posframe))

;; Add hook to reload child-frame colors on theme change
;; (add-hook 'after-load-theme-hook 'cpm/reload-child-frame)


;;; End childframe
(provide 'setup-childframe)
