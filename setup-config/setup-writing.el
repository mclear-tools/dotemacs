;; Writing

;;; Spelling
(use-package ispell
  :commands (ispell-word ispell-region ispell-buffer)
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell")
    ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))
  (setq ispell-choices-win-default-height 6))
  ;; (when (executable-find "hunspell")
  ;;   (setq-default ispell-program-name "hunspell")
  ;;   (setq ispell-extra-args   '("-d en_US"))
  ;;   (setq ispell-really-hunspell t)))

  ;; Save a new word to personal dictionary without asking
  ;; (setq ispell-silently-savep nil)

  ;; (setq-default ispell-program-name "/usr/local/bin/aspell")
  ;; (setq ispell-extra-args
  ;;     (list "--sug-mode=fast" ;; ultra|fast|normal|bad-spellers
  ;;           "--lang=en_US"
  ;;           "--ignore=3")))

(use-package flyspell
  ;; :general
  ;; (:states '(normal insert emacs) :keymaps 'flyspell-mode-map
  ;;  "C-;" 'flyspell-auto-correct-previous-word
  ;;  "C-:" 'flyspell-correct-wrapper)
  :config
  (setq flyspell-abbrev-p t
        flyspell-use-global-abbrev-table-p t
        flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  :hook ((markdown-mode . flyspell-mode)
         (org-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package flyspell-correct
  :straight t
  :after flyspell
  :general
  (:states '(normal insert emacs) :keymaps 'flyspell-mode-map
   "C-;" 'flyspell-correct-wrapper
   "C-:" 'flyspell-correct-at-point))

(with-eval-after-load 'hydra
  ;; keybinding is SPC-S
  (defhydra hydra-spelling (:color blue)
    "
  ^
  ^Spelling^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^───────
  _q_ quit            _<_ previous        _c_ correction
  ^^                  _>_ next            _d_ dictionary
  ^^                  _f_ check           _m_ mode
  ^^                  ^^                  ^^
  "
    ("q" nil)
    ("<" flyspell-correct-previous :color pink)
    (">" flyspell-correct-next :color pink)
    ("c" ispell)
    ("d" ispell-change-dictionary)
    ("f" flyspell-buffer :color pink)
    ("m" flyspell-mode)))



;;; Abbrev
(use-package abbrev
  :straight nil
  :defer 2
  :config
  ;; (add-hook 'text-mode-hook #'abbrev-mode)
  (setq abbrev-file-name (concat cpm-local-dir "abbrev/.abbrev_defs")
        save-abbrevs 'nil)
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;;; Bibtex-Actions
;; Use completing read to select actions for bibtex
(use-package bibtex-actions
  :straight (:host github :repo "bdarcus/bibtex-actions")
  :commands (bibtex-actions-open
             bibtex-actions-open-pdf
             bibtex-actions-open-link
             bibtex-actions-insert-citation
             bibtex-actions-insert-reference
             bibtex-actions-insert-key
             bibtex-actions-insert-bibtex
             bibtex-actions-add-pdf-attachment
             bibtex-actions-open-notes
             bibtex-actions-open-entry
             bibtex-actions-add-pdf-to-library)
  :config
  ;; Set insert citekey with markdown citekeys for org-mode
  (setq bibtex-completion-format-citation-functions
        '((org-mode    . bibtex-completion-format-citation-pandoc-citeproc)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default)))
  (setq bibtex-completion-display-formats
        '((t . "${author:16} ${title:36} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")))
  ;; Set default action for ivy-bibtex to edit notes file
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")
  (setq bibtex-completion-notes-template-one-file "* ${author} (${date}): ${title} \n :PROPERTIES:\n :INTERLEAVE_PDF: ${file}\n :Custom_ID: ${=key=}\n :END:\n [[pdfview:${file}][file link]]")
  (setq bibtex-completion-notes-template-multiple-files "#+TITLE: ${author-or-editor} (${year}): ${title}\n#+ROAM_KEY: cite:${=key=}\n#+SETUPFILE: ./hugo_setup.org\n#+HUGO_SECTION: reading-notes\n\n- Tags :: \n- Bookends link :: bookends://sonnysoftware.com/${beref}\n- PDF :: [[${file}][PDF Link]]\n\n#+BEGIN_SRC bibtex\n (insert (org-ref-get-bibtex-entry \"${=key=}\"))\n#+END_SRC")
  (setq bibtex-completion-bibliography "~/Dropbox/Work/bibfile.bib"
        bibtex-completion-library-path "~/Dropbox/Work/be-library/"
        bibtex-completion-pdf-field nil
        bibtex-completion-notes-path "~/Dropbox/Work/projects/notebook/content-org"
        bibtex-completion-notes-extension ".org"
        )
  ;; use w/embark-act
  (with-eval-after-load 'embark
    (setf (alist-get 'bibtex embark-keymap-alist) 'bibtex-actions-map))
  )

;;; Helm Bibtex
(use-package helm-bibtex
  :disabled
  :straight t
  :commands helm-bibtex
  :after helm
  :config
  ;; Set insert citekey with markdown citekeys for org-mode
  (setq bibtex-completion-format-citation-functions
        '((org-mode    . bibtex-completion-format-citation-pandoc-citeproc)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default)))
  (setq bibtex-completion-display-formats
        '((t . "${author:16} ${title:36} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")))
  ;; Set default action for ivy-bibtex to edit notes file
  (setq helm-bibtex-default-action 'helm-bibtex-edit-notes)
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")
  (setq bibtex-completion-notes-template-one-file "* ${author} (${date}): ${title} \n :PROPERTIES:\n :INTERLEAVE_PDF: ${file}\n :Custom_ID: ${=key=}\n :END:\n [[pdfview:${file}][file link]]")
  (setq bibtex-completion-notes-template-multiple-files "#+TITLE: ${author-or-editor} (${year}): ${title}\n#+ROAM_KEY: cite:${=key=}\n#+SETUPFILE: ./hugo_setup.org\n#+HUGO_SECTION: reading-notes\n\n- Tags :: \n- Bookends link :: bookends://sonnysoftware.com/${beref}\n- PDF :: [[${file}][PDF Link]]\n\n#+BEGIN_SRC bibtex\n (insert (org-ref-get-bibtex-entry \"${=key=}\"))\n#+END_SRC")
  (setq bibtex-completion-bibliography "~/Dropbox/Work/bibfile.bib"
        bibtex-completion-library-path "~/Dropbox/Work/be-library/"
        bibtex-completion-pdf-field nil
        bibtex-completion-notes-path "~/Dropbox/Work/projects/notebook/content-org"
        bibtex-completion-notes-extension ".org"
        helm-bibtex-full-frame nil))

;;; Org Ref
(use-package org-ref
  :commands (org-ref org-ref-get-bibtex-entry)
  :after org
  :init
  (setq reftex-default-bibliography (concat (getenv "HOME") "/Dropbox/Work/bibfile.bib"))
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  (setq org-ref-default-bibliography '("~/Dropbox/Work/bibfile.bib")
        org-ref-pdf-directory (concat (getenv "HOME") "/Library/Mobile Documents/iCloud~com~sonnysoftware~bot/Documents/be-library/")
        org-ref-notes-directory (concat (getenv "HOME") "/Users/roambot/Dropbox/Work/projects/notebook/content-org")
        bibtex-completion-notes-path "~/Dropbox/Work/projects/notebook/content-org"
        org-ref-notes-function 'org-ref-notes-function-many-files)
  :config
  (setf (cdr (assoc 'org-mode bibtex-completion-format-citation-functions)) 'org-ref-format-citation)
  (setq doi-utils-download-pdf nil))

;; workaround for bibtex timer issue described here:
;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2018-01/msg00472.html
(cancel-function-timers 'bibtex-parse-buffers-stealthily)


(use-package org-ref-ox-hugo
  :disabled
  :load-path "~/.emacs.d/.local/elisp/org-ref-ox-hugo-20200315/"
  :after org-ref
  :demand t
  :init
  (add-to-list 'org-ref-formatted-citation-formats
               '("md"
                 ("article" . "${author}, *${title}*, ${journal}, *${volume}(${number})*, ${pages} (${year}). ${doi}")
                 ("inproceedings" . "${author}, *${title}*, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("book" . "${author}, *${title}* (${year}), ${address}: ${publisher}.")
                 ("phdthesis" . "${author}, *${title}* (Doctoral dissertation) (${year}). ${school}, ${address}.")
                 ("inbook" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("incollection" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("proceedings" . "${editor} (Eds.), _${booktitle}_ (${year}). ${address}: ${publisher}.")
                 ("unpublished" . "${author}, *${title}* (${year}). Unpublished manuscript.")
                 ("misc" . "${author} (${year}). *${title}*. Retrieved from [${howpublished}](${howpublished}). ${note}.")
                 (nil . "${author}, *${title}* (${year})."))))


;;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :config
  (setq markdown-command
        (concat
         "/usr/local/bin/pandoc"
         " --from=markdown --to=html"
         " --standalone --mathjax --highlight-style=pygments"
         " --css=/Users/roambot/.pandoc/pandoc.css"
         " --quiet"
         " --number-sections"
         " --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua"
         " --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua"
         ;; " --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml"
         " --metadata=reference-section-title:References"
         " --citeproc"
         " --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib"
         ))

  (setq markdown-enable-math nil
        markdown-enable-wiki-links t
        markdown-nested-imenu-heading-index t
        markdown-open-command "~/bin/mark.sh"
        markdown-footnote-location 'immediately
        markdown-unordered-list-item-prefix "-   "
        markdown-header-scaling t
        markdown-use-pandoc-style-yaml-metadata t)
  (setq markdown-live-preview-window-function 'cpm--markdown-live-preview-window-xwidget)

  (defun cpm--markdown-live-preview-window-xwidget (file)
    "Preview file with xwidget browser"
    (xwidget-webkit-browse-url (concat "file://" file))
    (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
      (when (buffer-live-p buf)
        (and (eq buf (current-buffer)) (quit-window))
        (pop-to-buffer buf))))

  (defun cpm--markdown-settings ()
    "settings for markdown mode"
    (progn
      (turn-on-flyspell)
      (auto-fill-mode)
      (centered-cursor-mode 1)
      (git-gutter-mode 1)
      (hl-todo-mode)))

  ;; markdown hooks
  (add-hook 'markdown-mode-hook 'cpm--markdown-settings)

  )
;; remove strikout comment face
;; (set-face-attribute 'markdown-comment-face nil :weight 'bold :strike-through nil)

;; macro: delete backslashes in paragraph to cleanup markdown conversion
(fset 'cpm/md-delete-backslash
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("\361\361f\\x" 0 "%d")) arg)))


;;; Markdown TOC
(use-package markdown-toc
  :after markdown
  :hook (markdown-mode . markdown-toc))

;;; Pandoc
(use-package pandoc-mode
  :commands (cpm/pandoc-convert-to-pdf run-pandoc pandoc-convert-to-pdf)
  :config
  (setq pandoc-use-async t)
  ;; stop pandoc from just hanging forever and not completing conversion
  ;; see https://github.com/joostkremers/pandoc-mode/issues/44
  (setq pandoc-process-connection-type nil)
  (progn
    (defun run-pandoc ()
      "Start pandoc for the buffer and open the menu"
      (interactive)
      (pandoc-mode)
      (pandoc-main-hydra/body))
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

  (defun cpm/pandoc-convert-to-pdf ()
   (interactive)
   (cond
   ((eq major-mode 'org-mode)
    (call-interactively 'org-pandoc-export-to-latex-pdf-and-open))
   (t
    (call-interactively 'pandoc-convert-to-pdf) (cpm/pandoc-pdf-open) (evil-window-prev 1))))

  (defun cpm/pandoc-command-line-convert-to-pdf ()
   "convert to pdf"
   (interactive)
   (evil-ex "!pandoc -s -N -V mainfont=Optima --pdf-engine=xelatex --bibliography=~/Dropbox/Work/bibfile.bib --template=~/.pandoc/pandoc-templates/default.latex -o '%.pdf' '%'"))

  (defun cpm/pandoc-pdf-open ()
   "Open created PDF file"
   (interactive)
   (find-file-other-window (concat (file-name-sans-extension buffer-file-name) ".pdf"))))
  :init
  (progn
    (setq pandoc-data-dir (concat cpm-local-dir "pandoc-mode/"))
    ;; help pandoc find xelatex
    (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))))

;;; Writeroom
(use-package writeroom-mode
  :commands (writeroom-mode)
  :config
  (setq writeroom-fullscreen-effect 'maximized)
  (setq writeroom-width 95)
  (setq writeroom-mode-line t)
  (setq writeroom-bottom-divider-width 0))

;; Set up a distraction free space
(defun distraction-free ()
  "distraction free writing"
  (interactive)
  (git-gutter-mode 0)
  (linum-mode 0)
  (centered-cursor-mode)
  (writeroom-mode)
  )

;;; Interleave (Notes)
(use-package interleave
  :commands interleave-mode)
(use-package lorem-ipsum
  :commands (Lorem-ipsum-insert-sentences Lorem-ipsum-insert-list Lorem-ipsum-insert-paragraphs)
  :config
  (lorem-ipsum-use-default-bindings)
  )

;;; Palimpsest (make archive)
(use-package palimpsest
  :diminish palimpsest-mode
  :hook ((markdown-mode org-mode) . palimpsest-mode)
  :custom
  (palimpsest-send-bottom "C-c C-z")
  (palimpsest-send-top "C-c C-a")
  :config
  (setq palimpsest-trash-file-suffix ".archive"))

;;; Latex Packages
;; Basic settings
(use-package auctex
  :mode (("\\.tex\\'" . latex-mode)
         ("\\.latex\\'" . latex-mode))
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
    (setq-default TeX-engine 'xetex)
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil
          TeX-PDF-mode t)
    (setq-default TeX-master nil)))

(use-package preview
  :straight nil
  :after auctex
  :commands LaTeX-preview-setup
  :init
  (progn
    (setq-default preview-scale 1.4
                  preview-scale-function '(lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))))

(use-package reftex
  :commands turn-on-reftex
  :init
  (progn
    (setq reftex-plug-into-AUCTeX t)))

(use-package bibtex
  :defer t
  :mode ("\\.bib" . bibtex-mode)
  :init
  (progn
    (setq bibtex-align-at-equal-sign t)
    (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120)))))


;; Auto-fill for LaTeX
(defun schnouki/latex-auto-fill ()
  "Turn on auto-fill for LaTeX mode."
  (turn-on-auto-fill)
  (set-fill-column 80)
  (setq default-justification 'left))
(add-hook 'LaTeX-mode-hook #'schnouki/latex-auto-fill)

;; Compilation command
(add-hook 'LaTeX-mode-hook (lambda () (setq compile-command "latexmk -pdflatex=xelatex -f -pdf %f")))

;; Prevent ispell from verifying some LaTeX commands
;; http://stat.genopole.cnrs.fr/dw/~jchiquet/fr/latex/emacslatex
(defvar schnouki/ispell-tex-skip-alists
      '("cite" "nocite"
  "includegraphics"
  "author" "affil"
  "ref" "eqref" "pageref"
  "label"))
(setq ispell-tex-skip-alists
      (list
       (append (car ispell-tex-skip-alists)
         (mapcar #'(lambda (cmd) (list (concat "\\\\" cmd) 'ispell-tex-arg-end)) schnouki/ispell-tex-skip-alists))
       (cadr ispell-tex-skip-alists)))

;; Indentation with align-current in LaTeX environments
(defvar schnouki/LaTeX-align-environments '("tabular" "tabular*"))
(add-hook 'LaTeX-mode-hook
    (lambda ()
      (require 'align)
      (setq LaTeX-indent-environment-list
      ;; For each item in the list...
      (mapcar (lambda (item)
          ;; The car is an environment
          (let ((env (car item)))
            ;; If this environment is in our list...
            (if (member env schnouki/LaTeX-align-environments)
          ;; ...then replace this item with a correct one
          (list env 'align-current)
        ;; else leave it alone
        item)))
        LaTeX-indent-environment-list))))

;; Use dvipdfmx to convert DVI files to PDF in AUCTeX
(eval-after-load 'tex
  '(add-to-list 'TeX-command-list
                '("DVI to PDF" "dvipdfmx %d" TeX-run-command t t) t))

;; SyncTeX (http://www.emacswiki.org/emacs/AUCTeX#toc19)
(defun synctex/un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

;;; Typography
(use-package typo
  :defer 2
  ;; :hook (org-mode . typo-mode)
  :config
  (typo-global-mode))

;;; Dictionary
(use-package define-word
  :commands (define-word define-word-at-point))
;;; Writegood Mode
(use-package writegood-mode
  :hook (markdown-mode . writegood-mode)
  :config
  (setq cpm/weasel-words
        '("actually"
          "basically"
          "easily"
          "easy"
          "specifically"
          "simple"
          "simply"))
  (setq writegood-weasel-words
        (-concat writegood-weasel-words cpm/weasel-words)))



;;; Notes / Deft
(use-package deft
  :commands (deft deft-open-file-other-window cpm/notebook deft-new-file-named)
  :general
  (:keymaps 'deft-mode-map :states '(normal motion)
   "o" 'cpm/deft-open
   "p" 'cpm/deft-open-preview
   "q" 'kill-this-buffer)
  (:keymaps 'deft-mode-map :states '(insert)
   "C-j" 'evil-next-line
   "C-k" 'evil-previous-line
   "C-o" 'cpm/deft-open
   "C-p" 'cpm/deft-open-preview)
  :config
  ;; https://github.com/jrblevin/deft/issues/100
  (defun deft-parse-summary (contents title)
    "Parse the file CONTENTS, given the TITLE, and extract a summary.
The summary is a string extracted from the contents following the
title."
    (let* ((summary (let ((case-fold-search nil))
                      (replace-regexp-in-string deft-strip-summary-regexp " " contents)))
           (summary-processed (deft-chomp
                                (if (and title
                                         (not deft-use-filename-as-title)
                                         (string-match (regexp-quote
                                                        (if deft-org-mode-title-prefix
                                                            (concat "^#+TITLE: " title)
                                                          title))
                                                       summary))
                                    (substring summary (match-end 0) nil)
                                  summary))))
      (substring summary-processed 0 (min 512 (string-width summary-processed)))))
  (with-eval-after-load 'evil
    (add-to-list 'evil-insert-state-modes 'deft-mode))
  ;; basic settings for use with zettel
  (setq deft-directory (concat (getenv "HOME") "/Dropbox/work/projects/notebook/content-org")
        deft-recursive t
        deft-use-filename-as-title t
        deft-separator " "
        deft-extensions '("org" "txt" "md")
        deft-default-extension "org")
  ;; file renaming rules
  (setq deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))
  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[a-zA-Z_]+:.*$" ;;org-mode metadata
                ;;yaml metadata
                "\\|^\\-\\{3\\}$"
                "\\|^[a-zA-Z_]+:.*$"
                "\\|@[a-zA-Z_].*$"
                ;; line beginning with markdown links
                "\\|^\\[.*$"
                "\\|^# .*$" ;; md titles
                "\\)"))

  ;;function to run deft in specified directory
  (defun any-deft (dir)
    "Run deft in directory DIR"
    (setq deft-directory dir)
    (switch-to-buffer "*Deft*")
    (kill-this-buffer)
    (deft))
  (defun cpm/notebook ()
    "Goto main notes with deft"
    (interactive)
    (any-deft "~/Dropbox/Work/projects/notebook/content-org")
    (kill-this-buffer)
    (any-deft "~/Dropbox/Work/projects/notebook/content-org"))
  (defun cpm/deft-open ()
    (interactive)
    (deft-open-file-other-window t))
  (defun cpm/deft-open-preview ()
    (interactive)
    (deft-open-file-other-window)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-writing)
