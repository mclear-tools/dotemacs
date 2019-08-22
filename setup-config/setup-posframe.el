;; Open a child frame. Not quite ready for use as it conflicts with perspective
;; see https://github.com/tumashu/posframe/issues/29
;; see also https://github.com/nex3/perspective-el/issues/88

(use-package posframe
  :ensure t
  :defer 1)

(eval-when-compile
  (quelpa
   '(helm-posframe :fetcher github :repo "tumashu/helm-posframe")))
(use-package helm-posframe
  :ensure nil
  :after posframe
  :config
  (helm-posframe-enable))

(eval-when-compile
  (quelpa
   '(helm-posframe :fetcher github :repo "tumashu/company-posframe")))
(use-package company-posframe
  :ensure nil
  :after posframe
  :config
  (company-posframe-mode 1))
