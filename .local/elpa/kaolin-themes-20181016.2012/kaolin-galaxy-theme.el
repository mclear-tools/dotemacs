;;; kaolin-galaxy-theme.el --- Bright theme based on one of the Sebastian Andaur arts.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(defgroup kaolin-galaxy nil
  "Kaolin galaxy theme options."
  :group 'kaolin-themes)

(defcustom kaolin-galaxy-alt-bg nil
  "Use alternative darker background color."
  :type 'boolean
  :group 'kaolin-themes)

(define-kaolin-theme galaxy "Bright theme based on one of the Sebastian Andaur arts."
  ;; Palette modification
  (
   (azure2        "#232c35")
   (azure1        "#2a57cc")
   (chartreuse1   "#73c66c")
   (spring-green1 "#6dd797")
   (ultramarine7  "#615B75")

                               ;; dark      bright   terminal
   (bg0 (if kaolin-galaxy-alt-bg "#19181C" "#1C1B21") black0)
   (bg1 (if kaolin-galaxy-alt-bg "#1d1c21" "#212026") black1)
   (bg2 (if kaolin-galaxy-alt-bg "#26252c" "#2a2931") black2)
   (bg3 (if kaolin-galaxy-alt-bg "#302e36" "#31303A") black3)
   (bg4 (if kaolin-galaxy-alt-bg "#393741" "#3d3b46") black4)

   ;; TODO: add other fg vars
   ;; (fg1 blue9)

   (keyword     cyan3)
   (second-key  keyword)
   (builtin     teal0)
   (functions   magenta3)
   (var         amber3)
   (const       orange3)
   (type        crimson3)
   (prep        capri3)
   ;; TODO: change to type or str color
   (num         capri3)
   (bool        num)

   ;; TODO: a bit more darken
   (comment     ultramarine7)
   ;; (comment-alt vermilion7)
   (comment-alt azure4)
   (str         spring-green3)
   (str-alt     harlequin3)
   (doc         str-alt)
   (warning     orange3)
   (err         crimson0)

   (dim-buffer "#140E14")
   ;; TODO: (??) change
   (hl         vermilion3)
   (hl-line    (if kaolin-themes-hl-line-colored bg2 bg2))
   (hl-indent  bg4)
   ;; TODO: (??) less bright
   (selection  bg4)
   (pulse      ultramarine6)

   (todo crimson1)
   (done spring-green1)

   ;; Tooltip
   (tooltip-hl-bg bg4)
   (tooltip-hl-fg cyan0)

   (ivy2 cerulean3)
   (ivy3 red3)
   (ivy4 violet1)

   (rb1 violet4)
   (rb2 teal4)
   (rb3 violet3)
   (rb4 blue4)
   (rb5 capri4)
   (rb6 cerulean7)
   (rb7 orange8)
   (rb8 magenta4)
   (rb9 crimson3)

   (diff-add spring-green1)
   (diff-mod yellow3)
   (diff-rem red3)

   ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-color2       keyword)
   (line-border       bg3)

   (segment-active    gray3)
   (segment-inactive  gray3)
   (evil-normal       capri4)
   (evil-insert       spring-green1)
   (evil-visual       orange1)
   (evil-replace      red1)
   (evil-motion       yellow1)
   (evil-operator     evil-normal)
   (evil-emacs        yellow3)

   (win-border    bg3)
   (line-num-fg   ultramarine7 black4)
   (line-num-hl   capri3)

   (cursor        "#c3c8e0"))

  (
   ;; Custom theme set faces
   (show-paren-mismatch (:background bg2 :foreground red0))

   (org-level-3         (:foreground magenta3))

   (highlight-quoted-quote   (:foreground functions))
   (highlight-quoted-symbol  (:foreground type))

   (org-document-title  (:foreground amber3 :bold bold))
   (org-document-info   (:foreground type))
   (org-date            (:foreground spring-green3 :underline underline))
   (org-code            (:foreground erin3))
   (org-verbatim        (:foreground aquamarine3))
   (org-quote           (:foreground blue9)))

  ;; Set custom vars
  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-galaxy
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))


;;; kaolin-galaxy-theme.el ends here
