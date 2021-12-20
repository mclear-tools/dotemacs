;; Open a child frame. I used to use child frames more extensively but they
;; always tend to have issues, especially with theme colors, so now I use them
;; only for transient and company

;;; Posframe
(use-package posframe)

;;; Transient Posframe
(use-package transient-posframe
  :straight (:host github :repo "yanghaoxie/transient-posframe")
  :if (and (window-system) (version<= "26.1" emacs-version))
  :hook (after-init . transient-posframe-mode)
  :config
  (setq transient-posframe-border-width 20
        transient-posframe-min-height (round (* (frame-height) 0.5))
        transient-posframe-min-width (round (* (frame-width) 0.5))
        transient-posframe-poshandler 'posframe-poshandler-frame-top-center))

;;; Company Posframe
(use-package company-posframe
  :if (and (window-system) (version<= "26.1" emacs-version))
  :hook (company-mode . company-posframe-mode))


;;; End childframe
(provide 'setup-childframe)
