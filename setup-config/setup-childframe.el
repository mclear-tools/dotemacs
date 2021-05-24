;; Open a child frame.

;;; Miniframe
;; Provides a great ui for completion, similar to posframe
(use-package mini-frame
  :straight (:type git :host github :repo "muffinmad/emacs-mini-frame")
  :hook (after-init . mini-frame-mode)
  :commands (mini-frame-mode)
  :custom
  (mini-frame-show-parameters `((top    . 0.023)
                                (width  . 0.98)
                                (left   . 0.5)
                                (height . 11)
                                (child-frame-border-width . 15)
                                (internal-border-width . 0)
                                (left-fringe . 20)
                                (right-fringe . 20)
                                ;; set colors for bespoke theme
                                (foreground-color . ,bespoke-strong)
                                (background-color . ,bespoke-subtle)
                                ))
  ;; (mini-frame-color-shift-step 7)
  (mini-frame-advice-functions '(read-from-minibuffer
                                 read-string
                                 completing-read))
  (mini-frame-resize nil)
  :config
  (defun cpm/reload-child-frame ()
    "function to reload child-frame settings"
    (interactive)
    (load-library "setup-childframe"))
  ;; add hook to reload mini-frame colors on bespoke-theme change
  (add-hook 'after-load-theme-hook 'cpm/reload-child-frame))

;;; Posframe
(use-package posframe)
;;; Transient Posframe
(use-package transient-posframe
  :straight (:host github :repo "yanghaoxie/transient-posframe")
  :if (and (window-system) (version<= "26.1" emacs-version))
  :hook (after-init . transient-posframe-mode)
  :custom-face
  (transient-posframe-border ((t (:inherit default :background ,bespoke-subtle))))
  (transient-posframe ((t (:foreground ,bespoke-strong :background ,bespoke-subtle))))
  :config
  (setq transient-posframe-border-width 20
        transient-posframe-min-height (round (* (frame-height) 0.4))
        transient-posframe-min-width (round (* (frame-width) 0.4))
        transient-posframe-poshandler 'posframe-poshandler-frame-top-center
        ))

;;; Which-Key Posframe
(use-package which-key-posframe
  :if (and (window-system) (version<= "26.1" emacs-version))
  :hook (after-init . which-key-posframe-mode)
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
