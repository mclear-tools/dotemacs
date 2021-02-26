;; Programming
;;; Alignment
;; https://github.com/edkolev/evil-lion : This package provides gl and gL align operators: gl MOTION CHAR and right-align gL MOTION CHAR
(use-package evil-lion
 :defer t
 :config
 (general-define-key :states '(normal) :keymaps 'prog-mode-map
 "g l" 'evil-lion-left
 "g L" 'evil-lion-right)

 (general-define-key :states '(visual) :keymaps 'prog-mode-map
 "g l" 'evil-lion-left
 "g L" 'evil-lion-right))

;;; Delimiters & Identifiers
;; https://github.com/Fanael/rainbow-delimiters Useful package that will highlight
;; delimiters such as parentheses, brackets or braces according to their depth. Each
;; successive level is highlighted in a different color. This makes it easy to spot
;; matching delimiters, orient yourself in the code, and tell which statements are at
;; a given depth.
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
         :foreground "red"
         :inherit 'error
         :box t))

;; https://github.com/Fanael/rainbow-identifiers Rainbow identifiers mode is an Emacs
;; minor mode providing highlighting of identifiers based on their names. Each
;; identifier gets a color based on a hash of its name.
(use-package rainbow-identifiers
  :commands rainbow-identifiers-mode)



;;; Colors
;; https://github.com/emacsmirror/rainbow-mode Colorize color names in buffers
(use-package rainbow-mode
  :commands rainbow-mode)

;;; Languages
;;;; Applescript
(use-package applescript-mode
  :mode (("\\.scpt\\'" . applescript-mode)
         ("\\.applescript\\'"       . applescript-mode))
  :commands (applescript-mode))

;;;; Elisp
;;;;; Lisp Packages
(use-package lisp-mode
  :commands lisp-mode
  :straight nil)

(use-package emacs-lisp-mode
  :straight nil
  :mode (("\\.el$" . emacs-lisp-mode))
  :interpreter (("emacs" . emacs-lisp-mode))
  )

(use-package elisp-slime-nav
  :straight nil
  :commands elisp-slime-nav-mode
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

(use-package eldoc
  :straight nil
  :commands eldoc-mode
  :hook (emacs-lisp-mode . turn-on-eldoc-mode)
  :diminish eldoc-mode
  :config
  ;; Show ElDoc messages in the echo area immediately, instead of after 1/2 a second.
  (setq eldoc-idle-delay 0))

;; Elisp hook
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq show-trailing-whitespace t)
                                  (prettify-symbols-mode)
                                  (eldoc-mode)
                                  (yas-minor-mode)
                                  (company-mode)
                                  (rainbow-delimiters-mode)))

;;;;; Elisp indentation
;; Fix the indentation of keyword lists in Emacs Lisp. See [1] and [2].
;;
;; Before:
;;  (:foo bar
;;        :baz quux)
;;
;; After:
;;  (:foo bar
;;   :bar quux)
;;
;; [1]: https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L12-L94
;; [2]: http://emacs.stackexchange.com/q/10230/12534
;; Package for helping advise other packages

(use-package el-patch
  :defer 1
  :config
  (setq el-patch-enable-use-package-integration t))
(with-eval-after-load 'el-patch
  (el-patch-defun lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  (this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (el-patch-let (($cond (and (elt state 2)
                               (el-patch-wrap 1 1
                                 (or (not (looking-at "\\sw\\|\\s_"))
                                     (looking-at ":")))))
                   ($then (progn
                            (if (not (> (save-excursion (forward-line 1) (point))
                                        calculate-lisp-indent-last-sexp))
                                (progn (goto-char calculate-lisp-indent-last-sexp)
                                       (beginning-of-line)
                                       (parse-partial-sexp (point)
                                                           calculate-lisp-indent-last-sexp 0 t)))
                            ;; Indent under the list or under the first sexp on the same
                            ;; line as calculate-lisp-indent-last-sexp.  Note that first
                            ;; thing on that line has to be complete sexp since we are
                            ;; inside the innermost containing sexp.
                            (backward-prefix-chars)
                            (current-column)))
                   ($else (let ((function (buffer-substring (point)
                                                            (progn (forward-sexp 1) (point))))
                                method)
                            (setq method (or (function-get (intern-soft function)
                                                           'lisp-indent-function)
                                             (get (intern-soft function) 'lisp-indent-hook)))
                            (cond ((or (eq method 'defun)
                                       (and (null method)
                                            (> (length function) 3)
                                            (string-match "\\`def" function)))
                                   (lisp-indent-defform state indent-point))
                                  ((integerp method)
                                   (lisp-indent-specform method state
                                                         indent-point normal-indent))
                                  (method
                                   (funcall method indent-point state))))))
      (let ((normal-indent (current-column))
            (el-patch-add
              (orig-point (point))))
        (goto-char (1+ (elt state 1)))
        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
        (el-patch-swap
          (if $cond
              ;; car of form doesn't seem to be a symbol
              $then
            $else)
          (cond
           ;; car of form doesn't seem to be a symbol, or is a keyword
           ($cond $then)
           ((and (save-excursion
                   (goto-char indent-point)
                   (skip-syntax-forward " ")
                   (not (looking-at ":")))
                 (save-excursion
                   (goto-char orig-point)
                   (looking-at ":")))
            (save-excursion
              (goto-char (+ 2 (elt state 1)))
              (current-column)))
           (t $else)))))))

;;;; Haskell
(use-package haskell-mode
  :commands haskell-mode)

;;;; HTML
(use-package web-mode
  :commands (web-mode)
  :mode ("\\.html$" . web-mode)
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-expanding t
        web-mode-enable-css-colorization t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t))

;;;; Lua
(use-package lua-mode
  :commands lua-mode
  :init
  (dolist (pattern '("\\.lua\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'lua-mode))))

;;;; PHP
(use-package php-mode
  :commands php-mode
  :init
  (dolist (pattern '("\\.php\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'php-mode))))

;;;; Shell Scripts
(use-package sh-script
  :commands sh-script-mode
  :init
  (progn
    ;; Use sh-mode when opening `.zsh' files, and when opening Prezto runcoms.
    (dolist (pattern '("\\.zsh\\'"
                       "zlogin\\'"
                       "zlogout\\'"
                       "zpreztorc\\'"
                       "zprofile\\'"
                       "zshenv\\'"
                       "zshrc\\'"))
      (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))))

  (defun spacemacs//setup-shell ()
      (when (and buffer-file-name
                 (string-match-p "\\.zsh\\'" buffer-file-name))
        (sh-set-shell "zsh")))
    (add-hook 'sh-mode-hook 'spacemacs//setup-shell)

;;;; YAML
(use-package yaml-mode
  :commands yaml-mode
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode))
  :config
  (add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

;;;; Plist
(use-package plist-mode
  :straight nil
  :load-path "~/.emacs.d/.local/elisp/plist-mode"
  :commands (plist-mode))

;;;; Vim
(use-package vimrc-mode
  :commands vimrc-mode)

;;; Macrostep
;; https://github.com/joddie/macrostep Interactive macro expander for emacs
(use-package macrostep :commands macrostep-expand)

;;; Documentation
(use-package tldr
  :commands (tldr tldr-update-docs)
  :init
  (with-eval-after-load 'evil
  (evil-set-initial-state 'tldr-mode 'emacs))
  :config
  (setq tldr-directory-path (expand-file-name "tldr/" cpm-etc-dir)))

;;; Indentation
(use-package aggressive-indent
  :preface
  (defun cpm/aggressive-indent-mode-off ()
    (aggressive-indent-mode 0))
  :hook
  ((css-mode . aggressive-indent-mode)
   (emacs-lisp-mode . aggressive-indent-mode)
   (js-mode . aggressive-indent-mode)
   (lisp-mode . aggressive-indent-mode)
   (sgml-mode . aggressive-indent-mode))
  :config
  (setq-default aggressive-indent-comments-too t)
  (add-to-list 'aggressive-indent-protected-commands 'comment-dwim))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq-default highlight-indent-guides-method 'character
                highlight-indent-guides-character ?\│
                highlight-indent-guides-responsive 'top
                highlight-indent-guides-auto-enabled t))


;;; Linting/Error Checking
(use-package flycheck
  :defer 2
  :init
  (setq global-flycheck-mode 1)
  :config
  (setq-default flycheck-emacs-lisp-initialize-packages t
                flycheck-highlighting-mode 'symbols
                flycheck-check-syntax-automatically '(idle-change)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Compiling

;;;; Multi-Compile
(use-package multi-compile
  :commands (compile multi-compile-run)
  :config
  (setq multi-compile-history-file (concat cpm-cache-dir "multi-compile.cache"))
  (setq multi-compile-completion-system 'default)
  (setq multi-compile-alist '(
                              (org-mode . (
                                           ("pandoc-docx & Open" . "pandoc -s -C -f org+smart --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --reference-doc=/Users/Roambot/.pandoc/custom-reference.docx   --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.docx %file-name && open %file-sans.docx")
                                           ("pandoc-pdf & Open" . "pandoc -s -C -f org+smart --pdf-engine=xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")))

                              ;; commands for pandoc
                              (markdown-mode . (
                                                ("pandoc-normalize" . "pandoc -f markdown -t markdown -s --id-prefix=%file-sans: --atx-headers --columns=85 --wrap=auto --reference-location=block -o %file-name %file-name")
                                                ("pandoc-sep-html & Open" . "pandoc -f markdown -t html4 -s --base-header-level=1 --number-sections --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/.pandoc/pandoc-templates/sep.html4 --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.html %file-name && open %file-sans.html")
                                                ("pandoc-pdf & Open" . "pandoc -s -C --pdf-engine=xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                ("pandoc-beamer-compile-presentation" . "pandoc -C -i --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.beamer --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --metadata-file=/Users/roambot/dotfiles/pandoc/presentation-meta.yml --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -t beamer %file-name -o %file-sans.pdf && pandoc --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/pres-handout-meta.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -o %file-sans-handout.pdf %file-name")
                                                ("pandoc-beamer & Open" . "pandoc -C -i --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.beamer --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --metadata-file=/Users/roambot/dotfiles/pandoc/presentation-meta.yml --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -t beamer %file-name -o %file-sans.pdf && open %file-sans.pdf")
                                                ("pandoc-beamer-handout & Open" . "pandoc -C --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/pres-handout-meta.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -o %file-sans-handout.pdf %file-name && open %file-sans-handout.pdf")
                                                ("pandoc-handout & Open" . "pandoc -s -C --pdf-engine=xelatex  --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --template=/Users/Roambot/.pandoc/pandoc-templates/tufte.tex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                ("pandoc-docx & Open" . "pandoc -s -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --reference-doc=/Users/Roambot/.pandoc/custom-reference.docx --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.docx %file-name && open %file-sans.docx")
                                                ("pandoc-html & Open" . "pandoc -C -f markdown -t html5 -s --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/.pandoc/pandoc-templates/default.html5 --css=/Users/roambot/.pandoc/pandoc.css --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.html %file-name && open %file-sans.html")
                                                ("pandoc-pdf" . "pandoc -s -C --pdf-engine=xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name")
                                                ("pandoc-docx" . "pandoc -s -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --reference-doc=/Users/Roambot/.pandoc/custom-reference.docx --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.docx %file-name")
                                                ("pandoc-html" . "pandoc -f markdown -t html5 -s -C --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/.pandoc/pandoc-templates/default.html5 --css=/Users/roambot/.pandoc/pandoc.css --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.html %file-name")
                                                ("pandoc-handout" . "pandoc -s --pdf-engine=xelatex -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --template=/Users/Roambot/.pandoc/pandoc-templates/tufte.tex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name")
                                                ("test pdf" . "pandoc -s --pdf-engine=xelatex -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                ("pandoc-letter-pdf & Open" . "pandoc -s --pdf-engine=xelatex -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --template=/Users/Roambot/dotfiles/pandoc/pandoc-templates/letter.tex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                ("pandoc-book-chapter-pdf & Open" . "pandoc -s -N --pdf-engine=xelatex --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --citeproc --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/promote-headers.lua --filter pandoc-latex-color --metadata=reference-section-title:'References' --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                ((string/starts-with buffer-file-name "/Users/roambot/Dropbox/Work/projects/Book-Projects/rationality-book/") . (("compile rationality book" . "cd %make-dir && make -k && open %make-dirbuild/pdf/kant-rationality-book.pdf"))))))))

;; I think this goes with multi-compile
(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (stringp string) (string-match (rx-to-string `(: bos ,prefix) t) string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-programming)
