;;; kaolin-valley-dark-theme.el --- Colorful Kaolin theme with brown background.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme valley-dark  "Colorful Kaolin theme with brown background."

  ;; Palette modification
  (
   ;; Colors
   (spring-green6 "#3e594e")
   (aquamarine4   "#518270")
   (orange0       "#d1832e")
   ;; (cerulean4     "#536a9d")
   (cerulean4     "#47629E")

   ;; Color vars
   ;; TODO: (??) make more bright
   (bg0 "#1C1616")
   (bg1 "#211D1D")
   ;; (bg1 "#211f1d")
   (bg2 "#262121")
   (bg3 "#2E2828")
   (bg4 "#352D2D")
   (pane "#262122")

   (fg1 amber9)

   (keyword     teal0)
   (second-key  keyword)
   (builtin     aquamarine1)

   (var         crimson3)
   (const       crimson3)
   (functions   cyan3)
   (type        amber3)

   (comment     brown2)
   ;; TODO:
   (comment-alt teal2)

   (str         magenta3)
   (str-alt     cerise4)
   (doc         str-alt)

   (prep        vermilion3)
   (num         harlequin3)
   (bool        num)
   (warning     orange1)
   (err         red3)

   (dim-buffer white0)
   (hl         capri3)
   ;; TODO: add colored
   (hl-line    (if kaolin-themes-hl-line-colored bg3 bg3))
   (hl-indent  gray0)
   ;; TODO:
   (selection bg4)
   ;; TODO:
   (pulse bg4)

   (todo red3)

   (tooltip-hl-bg bg3)
   (tooltip-hl-fg hl)

   (rb1 teal1)
   (rb2 aquamarine1)
   (rb3 violet4)
   (rb4 cyan1)
   (rb5 spring-green1)
   (rb6 amber3)
   (rb7 magenta3)
   (rb8 brown3)
   (rb9 crimson3)

   (diff-add spring-green3)
   (diff-mod vermilion3)
   (diff-rem red3)

    ;; Mode-line
   (line-fg           fg4)
   (line-color1       functions)
   (line-color2       str)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       bg3)

   (prompt aquamarine1)

   ;; Telephone-line
   (segment-active    gray2)
   (segment-inactive  gray2)
   (evil-normal       brown3)
   (evil-insert       harlequin3)
   (evil-visual       orange3)
   (evil-replace      red3)
   (evil-motion       amber3)
   (evil-operator     evil-normal)
   (evil-emacs        cyan3)

   (win-border    bg3)
   (line-num-fg   brown2)
   (line-num-hl   amber3)

   (cursor        fg1)

   (ivy1          gray9)
   (ivy2          cerulean3)
   (ivy3          yellow0)
   (ivy4          red3))

  (
   (highlight-quoted-symbol  (:foreground num))

   (org-document-title     (:foreground orange3 :bold bold))
   ;; (org-document-info      (:foreground brown3))

   ;; TODO:
   (org-level-1            (:foreground keyword :bold bold :height 1.1))
   (org-level-2            (:foreground functions  :bold nil))
   (org-level-3            (:foreground str :bold nil))
   (org-level-4            (:foreground builtin :bold nil))
   (org-date               (:foreground aquamarine3 :underline underline))
   (org-code               (:foreground num))
   (org-verbatim           (:foreground orange1)))

  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-valley-dark
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))

;;; kaolin-valley-dark-theme.el ends here
