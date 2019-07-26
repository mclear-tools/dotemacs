(use-package ivy
  :diminish ivy-mode
  :general
  (:keymaps 'ivy-minibuffer-map
    "C-j" 'ivy-next-line
    "C-k" 'ivy-previous-line)
  :config
  (setq ivy-use-virtual-buffers t
        ;; number of result lines to display
        ivy-height 10
        ;; no regexp by default
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist
        ;; allow input not in order
        '((t   . ivy--regex-ignore-order))
        ivy-count-format "%d/%d "))

(use-package counsel-projectile :commands counsel-projectile-bookmark)

(use-package counsel
  :commands (council-org-goto jump-in-buffer counsel-org-tag)
  :config
  (map! (:map counsel-mode-map
         :ni "C-j" #'ivy-next-line
         :ni "C-k" #'ivy-previous-line)))

(use-package swiper
  :commands (swiper swiper-all))


(provide 'setup-ivy)
