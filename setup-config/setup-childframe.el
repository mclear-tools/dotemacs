;; Open a child frame.

;;; Mini Popup
;; Pop up for vertico
(use-package mini-popup
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
  (mini-popup-background ((t (:background ,bespoke-subtle))))
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


;;; Reload Child-Frame for theme
(defun cpm/reload-child-frame ()
  "function to reload child-frame settings"
  (interactive)
  (load-library "setup-childframe"))

;; add hook to reload child-frame colors on bespoke-theme change
(add-hook 'after-load-theme-hook 'cpm/reload-child-frame)

;;; Posframe
(use-package posframe)
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
(use-package which-key-posframe
  :if (and (window-system) (version<= "26.1" emacs-version))
  :hook (after-init . which-key-posframe-mode)
  :custom-face
  (which-key-posframe-border ((t (:background ,bespoke-subtle))))
  (which-key-posframe ((t (:background ,bespoke-subtle))))
  :config
  (setq posframe-arghandler #'cpm/posframe-arghandler)
  ;; see https://github.com/yanghaoxie/which-key-posframe/issues/5#issuecomment-527528759
  (defun cpm/posframe-arghandler (buffer-or-name arg-name value)
    (let ((info '(:width (round (* (frame-width) 0.72)) :height 75)))
      (or (plist-get info arg-name) value)))
  (setq which-key-posframe-border-width 20)
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-top-center))

;;; Company Posframe
(use-package company-posframe
  :if (and (window-system) (version<= "26.1" emacs-version))
  :hook (company-mode . company-posframe-mode))

;;; End posframe
(provide 'setup-childframe)
