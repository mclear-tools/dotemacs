(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(back-button-mode t)
 '(battery-update-interval 10)
 '(compilation-message-face (quote default))
 '(counsel-ag-base-command "ag --color --group %s")
 '(counsel-locate-cmd (quote counsel-locate-cmd-mdfind))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(desktop-path (quote ("~/.emacs.d/" "~" "~/.emacs.d/.cache/desktop")))
 '(doc-view-continuous t)
 '(doc-view-resolution 300)
 '(fancy-battery-mode t)
 '(fancy-battery-show-percentage t)
 '(fill-column 72)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-sexp-background-color "#1c1f26")
 '(interleave-org-notes-dir-list (quote ("~/projects/notebook/content/org_notes" ".")))
 '(ivy-count-format "(%d/%d) ")
 '(ivy-minibuffer-faces
   (quote
    (ivy-minibuffer-match-face-1 ivy-minibuffer-match-face-2 ivy-minibuffer-match-face-3 ivy-minibuffer-match-face-4)))
 '(ivy-mode t)
 '(ivy-use-virtual-buffers t)
 '(ivy-wrap t)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files nil)
 '(org-footnote-auto-adjust t)
 '(org-footnote-section "Footnotes")
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
#+END_NOTES")
     )))
 '(package-selected-packages
   (quote
    (dired swiper ivy avy helm-files uniquify em-cmpl counsel general workgroups2 tabbar shell-switcher eshell-prompt-extras virtualenvwrapper better-shell auto-compile haskell-mode lorem-ipsum dired-k helm-hunks crux dired-async lispy ox-reveal lispyville ox-clip palimpsest palimpsest-mode smooth-scrolling init-open-recentf magit smart-mode-line-powerline-theme smart-mode-line color-theme-solarized emacs-color-theme-solarized org-journal bind-map interleave wttrin org-ref helm-projectile projectile hc-zenburn-theme zenburn-theme badger-theme vimrc-mode yaml-mode pandoc-mode markdown-mode web-mode elisp-slime-nav company-math company-auctex company yasnippet window-numbering which-key visual-regexp sunshine evil-smartparens smartparens reveal-in-osx-finder restart-emacs ranger rainbow-mode rainbow-identifiers rainbow-delimiters persp-mode org-pdfview pdf-tools paradox hl-todo highlight-numbers info+ iedit centered-cursor-mode ag ace-jump-mode deft centered-window-mode back-button dash-at-point sr-speedbar helm-bibtex helm-dired-recent-dirs helm-flyspell helm-swoop helm-ls-git helm-descbinds helm-ag helm git-gutter-fringe+ git-gutter+ git-timemachine evil-magit shell-pop sane-term multi-term fancy-battery spaceline toc-org htmlize org-bullets ox-pandoc evil-org helm-themes solarized-theme material-theme darktooth-theme gruvbox-theme evil-commentary evil-surround evil-args evil-terminal-cursor-changer evil-numbers evil-leader evil-indent-textobject evil-escape evil s dash async use-package org-plus-contrib)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(safe-local-variable-values
   (quote
    ((org-footnote-section)
     (eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))
     (kill-buffer-hook org-publish-current-file))))
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-term-shell "/usr/local/bin/zsh")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#839496" 0.2))
 '(sml/shorten-modes nil)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-follow-symlinks t)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
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
 '(markdown-comment-face ((t (:weight bold :strike-through nil))))
 '(org-block ((t (:foreground "#2E8B57" :slant italic))))
 '(org-block-begin-line ((t (:foreground "#74a8a4" :weight bold :slant normal)))))

