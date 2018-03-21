;;; suscolors-theme.el --- Colorful theme, inspired by Gruvbox.

;; Copyright (c) 2016 Tomas Vojtisek
;; URL: https://github.com/TheSuspiciousWombat/SusColors-emacs
;; Package-Version: 20161109.1215

;;; Commentary:

;; Colorful theme, inspired by Gruvbox.

;;; Code:

(deftheme suscolors
  "Colorful theme, inspired by Gruvbox.")

(let (
      (bg "#262626")
      (fg "#d7af5f")
      (orange "#d78700")
      (orange2 "#d75f00")
      (grey "#949494")
      (grey2 "#303030")
      (grey3 "#7f7f7f")
      (green "#5faf5f")
      (green2 "#87ff5f")
      (brown "#875f5f")
      (yellow "#ffff5f")
      (yellow2 "ffff87")
      (blue "#5fafd7")
      (blue2 "#0087af")
      (pink "#D54AB6")
      (pink2 "#ff5fd7")
      (cyan "#008787")
      (cyan2 "#00afaf")
      (red "#D8553B")
      (red2 "#EF3935")
      )
  (custom-theme-set-faces
   'suscolors
   `(default ((t (:foreground ,fg :background ,bg))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-builtin-face ((t (:foreground ,blue))))
   `(region ((t (:background ,grey3))))
   `(font-lock-variable-name-face ((t (:foreground ,orange))))
   `(font-lock-keyword-face ((t (:foreground ,red))))
   `(font-lock-comment-face ((t (:foreground ,grey))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-type-face ((t (:foreground ,pink))))
   `(font-lock-constant-face ((t (:foreground ,pink))))
   `(button ((t (:foreground ,blue2 :weight bold :underline t))))
   `(link ((t (:foreground ,blue2 :weight bold))))
   `(menu ((t (:foreground ,fg :background ,grey2))))
   `(mode-line ((t (:background ,grey2 :foreground ,fg))))
   `(font-lock-warning-face ((t (:foreground ,red2))))
   `(compilation-warning ((t (:foreground ,red2))))
   `(highlight ((t (:background ,grey2 :foreground ,blue))))
   `(linum ((t (:foreground ,grey3 :weight bold))))
   `(widget-field ((t (:foreground ,fg :background ,grey3))))
   `(message-header-name-face ((t (:foreground ,green))))
   `(message-header-subject ((t (:foreground ,red :weight bold))))
   `(message-header-other ((t (:foreground ,pink))))
   ;; Highlight quoted mode-line
   `(highlight-quoted-symbol ((t (:foreground ,pink))))
   ;; hl-line and hlinum-activate
   `(linum-highlight-face ((t (:foreground ,grey :background ,grey2 :weight bold))))
   `(hl-line ((t (:background ,grey2))))
   ;; magit
   ;;`(magit-diff-added-highlight ((t (:background ,"#35B82C" :foreground ,fg))))
   ;;`(magit-diff-removed-highlight ((t (:background "yellow"))))
   ;;`(magit-diff-context-highlight ((t (:background ,bg))))
   ;; Org
   `(org-level-1 ((t (:foreground ,green))))
   `(org-level-2 ((t (:foreground ,red))))
   `(org-level-3 ((t (:foreground ,blue))))
   `(org-level-4 ((t (:foreground ,orange))))
   `(org-level-5 ((t (:foreground ,pink))))
   ;; Helm
   `(helm-selection ((t (:foreground ,orange :background ,grey2 :weight bold))))
   `(helm-ff-directory ((t (:foreground ,green :weight bold))))
   `(helm-ff-dotted-directory ((t (:background "nil" :foreground ,fg))))
   `(helm-ff-dotted-symlink-directory ((t (:background "nil" :foreground ,red))))
   `(helm-ff-executable ((t (:background "nil" :foreground ,blue))))
   `(helm-ff-invalid-symlink ((t (:foreground ,red2 :background "nil"))))
   `(helm-ff-symlink ((t (:foreground ,pink))))
   `(helm-source-header ((t (:foreground ,grey :background ,grey2 :weight bold))))
   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,red))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,pink))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,blue2))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,red))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,orange))))
   ;; Company-mode
   `(company-tooltip ((t (:foreground ,fg :background ,bg))))
   `(company-tooltip-selection ((t (:foreground ,fg :background ,grey2))))
   `(company-scrollbar-fg ((t (:background ,grey3))))
   `(company-scrollbar-bg ((t (:background ,grey2))))
   `(company-tooltip-common ((t (:foreground ,orange))))
   `(company-preview ((t (:background ,grey3))))
   `(company-preview-common ((t (:background ,grey3 :foreground ,red))))
   `(company-mouse ((t (:background ,grey2))))
   ;; Flycheck
   `(flycheck-warning ((t (:foreground ,red2 :weight bold :underline t))))
   ;; js2-mode
   `(js2-function-param ((t (:foreground ,orange))))
   ;; erc
   `(erc-timestamp-face ((t (:foreground ,red))))
   `(erc-prompt-face ((t (:foreground ,green :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,blue))))
   `(erc-notice-face ((t (:foreground ,pink))))
   `(erc-button ((t (:foreground ,blue2))))
   `(erc-current-nick-face ((t (:foreground ,red))))
   ;; elfeed
   `(elfeed-search-feed-face ((t (:foreground ,orange))))
   `(elfeed-search-tag-face ((t (:foreground ,green))))
   `(elfeed-search-date-face ((t (:foreground ,red))))
   `(elfeed-search-title-face ((t (:foreground ,fg))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
  	       (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'suscolors)
;;; suscolors-theme.el ends here
