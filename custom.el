(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(battery-update-interval 10)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/.cache/bookmarks")
 '(compilation-message-face (quote default))
 '(counsel-locate-cmd (quote counsel-locate-cmd-mdfind))
 '(custom-safe-themes
   (quote
    ("5d3e0746023fc5e246eb3e0e48c1ccb5ce0387fc4273896c6cf02ee349c2eba8" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(desktop-modes-not-to-save
   (quote
    (tags-table-mode dired-mode Info-mode info-lookup-mode fundamental-mode)))
 '(fancy-battery-mode t)
 '(fancy-battery-show-percentage t)
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-buffer-not-found helm-source-bookmarks)))
 '(hl-sexp-background-color "#1c1f26")
 '(magit-diff-use-overlays nil)
 '(markdown-command "pandoc" t)
 '(markdown-enable-math t t)
 '(markdown-footnote-location (quote end))
 '(markdown-nested-imenu-heading-index t t)
 '(markdown-open-command "/Users/Roambot/bin/scripts/mark.sh" t)
 '(org-confirm-babel-evaluate nil)
 '(org-ellipsis "↴")
 '(org-fontify-quote-and-verse-blocks t)
 '(org-footnote-auto-adjust t)
 '(org-footnote-section "Footnotes")
 '(org-from-is-user-regexp "\\<Roambot\\>")
 '(org-hide-emphasis-markers t)
 '(org-imenu-depth 4)
 '(org-link-translation-function (quote toc-org-unhrefify))
 '(org-pandoc-epub-rights "Copyright 2017 Roambot <>")
 '(org-pandoc-options (quote ((standalone . t))))
 '(org-pandoc-options-for-beamer-pdf (quote ((latex-engine . "xelatex"))))
 '(org-pandoc-options-for-docx (quote ((standalone))))
 '(org-pandoc-options-for-latex-pdf (quote ((latex-engine . "xelatex"))))
 '(org-pretty-entities t)
 '(org-return-follows-link t)
 '(org-reveal-default-frag-style "roll-in")
 '(org-reveal-hlevel 2)
 '(org-reveal-root "file:///Users/roambot/bin/reveal.js")
 '(org-startup-indented t)
 '(org-structure-template-alist
   (quote
    (("s" "#+BEGIN_SRC ?

#+END_SRC")
     ("e" "#+BEGIN_EXAMPLE
?
#+END_EXAMPLE")
     ("q" "#+BEGIN_QUOTE
?
#+END_QUOTE")
     ("v" "#+BEGIN_VERSE
?
#+END_VERSE")
     ("V" "#+BEGIN_VERBATIM
?
#+END_VERBATIM")
     ("c" "#+BEGIN_CENTER
?
#+END_CENTER")
     ("l" "#+BEGIN_LaTeX
?
#+END_LaTeX")
     ("L" "#+LaTeX: ")
     ("h" "#+BEGIN_HTML
?
#+END_HTML")
     ("H" "#+HTML: ")
     ("a" "#+BEGIN_ASCII
?
#+END_ASCII")
     ("A" "#+ASCII: ")
     ("i" "#+INDEX: ?")
     ("I" "#+INCLUDE: %file ?")
     ("E" "#+BEGIN_SRC emacs-lisp
?
#+END_SRC")
     ("t" "#+BEGIN_COMMENT TODO: ?  #+END_COMMENT")
     ("f" "#+ATTR_REVEAL: :frag (appear)?")
     ("b" "#+REVEAL: split?")
     ("n" "#+BEGIN_NOTES
?
#+END_NOTES"))))
 '(org-wiki-location "~/Dropbox/Work/wiki")
 '(package-selected-packages
   (quote
    (guess-language pdf-tools zweilight-theme simpleclip evil-embrace embrace lua-mode evil-surround evil-numbers evil-indent-textobject general use-package solarized-theme org-plus-contrib material-theme gruvbox-theme evil-escape)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((org-list-allow-alphabetical . t)
     (org-list-allow-alphabetical)
     (org-footnote-section)
     (eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))
     (kill-buffer-hook org-publish-current-file))))
 '(save-place-forget-unreadable-files nil)
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(shell-pop-term-shell "/usr/local/bin/zsh")
 '(toc-org-max-depth 10)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(fancy-battery-charging ((t (:foreground "dark blue" :weight bold))))
 '(fancy-battery-critical ((t (:foreground "dark red" :weight bold))))
 '(fancy-battery-discharging ((t (:foreground "dark magenta" :weight bold))))
 '(helm-selection ((t (:foreground "#f7f438" :background "#64b5ea" :underline nil :weight bold))))
 '(linum-highlight-face ((t (:inherit default :foreground "goldenrod1"))))
 '(markdown-comment-face ((t (:weight bold :strike-through nil))))
 '(org-block ((t (:foreground "#2E8B57"))))
 '(org-block-begin-line ((t (:foreground "#74a8a4" :weight bold :slant normal))))
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#268bd2" :height 1.3))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "medium sea green" :height 1.2))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#cb4b16" :height 1.15))))
 '(org-quote ((t (:inherit org-block :slant normal :weight normal)))))
