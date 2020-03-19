;; Open a child frame.

;;; Posframe
(eval-when-compile
  (quelpa
   '(posframe :fetcher github :repo "tumashu/posframe")))
(use-package posframe
  :ensure nil)

;;; Helm posframe
;; FIXME: helm-posframe forces minimize window
(eval-when-compile
  (quelpa
   '(helm-posframe :fetcher github :repo "tumashu/helm-posframe")))
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
(eval-when-compile
  (quelpa
   '(ivy-posframe :fetcher github :repo "tumashu/ivy-posframe")))
(use-package ivy-posframe
  :if (and (window-system) (version<= "26.1" emacs-version))
  :hook
  (ivy-mode . ivy-posframe-mode)
  :custom
  (ivy-posframe-size-function 'cpm/ivy-posframe-size)
  (ivy-posframe-parameters '((left-fringe . 0)
                             (right-fringe . 0)
                             (internal-border-width . 12)))
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
   :min-width (round (* (frame-width) 0.72))))

;;; Company Posframe
(eval-when-compile
  (quelpa
   '(company-posframe :fetcher github :repo "tumashu/company-posframe")))
(use-package company-posframe
  :ensure nil
  :disabled
  :if (and (window-system) (version<= "26.1" emacs-version))
  :hook (company-mode . company-posframe-mode))


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
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-center))

;;; Hydra Posframe
;; doesn't work well with ivy hydras
(eval-when-compile
  (quelpa
   '(hydra-posframe :fetcher github :repo "Ladicle/hydra-posframe")))
(use-package hydra-posframe
  :disabled
  :ensure nil
  :if (and (window-system) (version<= "26.1" emacs-version))
  :hook (after-init . hydra-posframe-enable)
  :config
  (setq hydra-posframe-border-width 15))


;;; End posframe
(provide 'setup-posframe)
