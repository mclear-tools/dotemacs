;; Open a child frame.

;;; Miniframe
;; NOTE: provides a great ui for completion, similar to posframe
(use-package mini-frame
  :straight (:type git :host github :repo "muffinmad/emacs-mini-frame")
  :hook (after-init . mini-frame-mode)
  :commands (mini-frame-mode)
  :custom
  (mini-frame-show-parameters `((top    . 0.023)
                                (width  . 0.98)
                                (left   . 0.5)
                                (height . 10)
                                (child-frame-border-width . 15)
                                (internal-border-width . 0)
                                (left-fringe . 20)
                                (right-fringe . 20)
                                ;; set colors for bespoke theme
                                (foreground-color . ,bespoke-strong)
                                (background-color . ,bespoke-subtle)
                                ))
  (mini-frame-color-shift-step 7)
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
  (setq which-key-posframe-border-width 15)
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-top-center))

;;; Selectrum Posframe
;; (setq selectrum-display-action '(display-buffer-show-in-posframe))

(defun display-buffer-show-in-posframe (buffer _alist)
  (frame-root-window
   (posframe-show buffer
                  :min-height 10
                  :min-width (round (* (frame-width) 0.75))
                  :internal-border-width 15
                  :left-fringe 0
                  :right-fringe 0
                  :poshandler 'posframe-poshandler-frame-center)))

;; (add-hook 'minibuffer-exit-hook 'posframe-delete-all)

;;; Helm posframe
(use-package helm-posframe
  :disabled
  :if (and (window-system) (version<= "26.1" emacs-version))
  :hook (after-init . helm-posframe-enable)
  :init
  (add-hook 'helm-org-rifle-after-command-hook 'helm-posframe-cleanup)
  :custom
  (helm-posframe-height 15)
  (helm-posframe-width (round (* (frame-width) 1.15)))
  (helm-posframe-parameters '((left-fringe . 0)
                              (right-fringe . 0)
                              (internal-border-width . 12)))
  :config
  (setq helm-posframe-poshandler 'posframe-poshandler-frame-center))

;;; Ivy Posframe
(use-package ivy-posframe
  :disabled
  :if (and (window-system) (version<= "26.1" emacs-version))
  :hook
  (ivy-mode . ivy-posframe-mode)
  :custom
  (ivy-posframe-size-function 'cpm/ivy-posframe-size)
  (ivy-posframe-height 50)
  (ivy-posframe-width 70)
  (ivy-posframe-parameters '((left-fringe . 0)
                             (right-fringe . 0)
                             (internal-border-width . 15)))
  (ivy-posframe-display-functions-alist
   '((swiper          . ivy-posframe-display-at-frame-top-center)
     (swiper-isearch  . ivy-posframe-display-at-frame-top-center)
     (complete-symbol . ivy-posframe-display-at-point)
     (counsel-M-x     . ivy-posframe-display-at-frame-center)
     (t               . ivy-posframe-display-at-frame-center)))
  :custom-face
  (ivy-posframe-cursor ((t (:background "#268bd2"))))
  :config
  (ivy-posframe-mode))

(defun cpm/ivy-posframe-size ()
  (list
   :min-height ivy-height
   :min-width (round (* (frame-width) 0.52))))

;;; Company Posframe
;; (eval-when-compile
;;   (quelpa
;;    '(company-posframe :fetcher github :repo "tumashu/company-posframe")))
(use-package company-posframe
  :disabled
  :if (and (window-system) (version<= "26.1" emacs-version))
  :hook (company-mode . company-posframe-mode))


;;; Hydra Posframe
;; doesn't work well with ivy hydras
;; (eval-when-compile
;;   (quelpa
;;    '(hydra-posframe :fetcher github :repo "Ladicle/hydra-posframe")))
(use-package hydra-posframe
  :disabled
  :if (and (window-system) (version<= "26.1" emacs-version))
  :hook (after-init . hydra-posframe-enable)
  :config
  (setq hydra-posframe-border-width 15))

;;; End posframe
(provide 'setup-childframe)
