;;; Modeline
;;;; Bespoke Mode line
(use-package bespoke-modeline
  :straight (:local-repo "/Users/roambot/.emacs.d/.local/custom-themes/bespoke-modeline/")
  :init
  ;; Set header-line
  (setq bespoke-modeline-position 'top)
  ;; Set mode-line height
  (setq bespoke-modeline-size 2)
  ;; Show diff lines in mode-line
  (setq bespoke-modeline-git-diff-mode-line t)
  ;; Set mode-line cleaner
  (setq bespoke-modeline-cleaner t)
  ;; Use mode-line visual bell
  (setq bespoke-modeline-visual-bell t)
  ;; Set vc symbol
  (setq bespoke-modeline-vc-symbol "") ;; 
  :config
  (bespoke-modeline-mode))



;;; End Modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-modeline)
