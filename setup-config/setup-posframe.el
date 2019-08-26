;; Open a child frame.

;;; Posframe
(eval-when-compile
  (quelpa
   '(posframe :fetcher github :repo "tumashu/posframe")))
(use-package posframe
  :ensure nil)

;;; Helm posframe
(eval-when-compile
  (quelpa
   '(helm-posframe :fetcher github :repo "tumashu/helm-posframe")))
(use-package helm-posframe
  :if (and (window-system) (version<= "26.1" emacs-version))
  :init
  (add-hook 'helm-org-rifle-after-command-hook 'helm-posframe-cleanup)
  :config
  (setq helm-posframe-poshandler 'posframe-poshandler-frame-center
        helm-posframe-height 10
        helm-posframe-width (round (* (frame-width) 0.49))
        helm-posframe-parameters '((internal-border-width . 10)))
  (helm-posframe-enable))

;;; Ivy Posframe
(eval-when-compile
  (quelpa
   '(ivy-posframe :fetcher github :repo "tumashu/ivy-posframe")))
(use-package ivy-posframe
  :if window-system
  :hook
  (ivy-mode . ivy-posframe-mode)
  :custom
  (ivy-posframe-size-function 'cpm/ivy-posframe-size)
  (ivy-posframe-parameters '((left-fringe . 0)
                             (right-fringe . 0)
                             (internal-border-width . 12)))
  (ivy-posframe-display-functions-alist
   '((swiper          . nil)
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
  :after posframe
  :config
  (company-posframe-mode 1))


;;; Which-Key Posframe

(use-package which-key-posframe
  :if (and (window-system) (version<= "26.1" emacs-version))
  :hook (after-init . which-key-posframe-mode)
  :config
  (setq which-key-posframe-border-width 15)
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-center))



;;; Hydra Posframe

(eval-when-compile
  (quelpa
   '(hydra-posframe :fetcher github :repo "Ladicle/hydra-posframe")))
(use-package hydra-posframe
  :hook (after-init . hydra-posframe-enable)
  :config
  (setq hydra-posframe-border-width 15))


;;; End posframe
(provide 'setup-posframe)
