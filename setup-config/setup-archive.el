;;; Old Notes Search
(defvar cpm-notes-dir "~/Dropbox/Work/projects/notebook/content-org/")
(defun cpm/search-all-notes ()
  (interactive)
  (cd cpm-notes-dir)
  (call-interactively #'deadgrep))

;;; Company Org Roam
(use-package company-org-roam
  :disabled t
  :after (org company)
  :demand t
  :config
  (push 'company-org-roam company-backends))


;;; Deft
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

  (setq deft-strip-summary-regexp "\\`\\(.+\n\\)")


  ;; (setq deft-strip-summary-regexp
  ;;       (concat "\\("
  ;;               "[\n\t]" ;; blank
  ;;               "\\|^#\\+[a-zA-Z_]+:.*$" ;;org-mode metadata
  ;;               ":PROPERTIES:\n\\(.+\n\\)+:END:\n" ;; strip org property drawer
  ;;               ;;yaml metadata
  ;;               "\\|^\\-\\{3\\}$"
  ;;               "\\|^[a-zA-Z_]+:.*$"
  ;;               "\\|@[a-zA-Z_].*$"
  ;;               ;; line beginning with markdown links
  ;;               "\\|^\\[.*$"
  ;;               "\\|^# .*$" ;; md titles
  ;;               "\\)"))

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

;;; Selectrum
;; Good completion package -- much more sane and organized than ivy. I'd prefer to use vertico but that doesn't work well with mini-frame-mode
(use-package selectrum
  :disabled
  :straight (:host github :repo "raxod502/selectrum")
  :hook (after-init . selectrum-mode)
  :general
  (:keymaps 'selectrum-minibuffer-map
   ;; "RET"    'icomplete-force-complete-and-exit
   "C-M-i"  'minibuffer-complete
   "M-RET"  'exit-minibuffer
   "<down>" 'selectrum-next-candidate
   "C-j"    'selectrum-next-candidate
   "<up>"   'selectrum-previous-candidate
   "C-k"    'selectrum-previous-candidate)
  :config
  (setq selectrum-num-candidates-displayed 10)
  (setq selectrum-fix-vertical-window-height t)
  (setq selectrum-extend-current-candidate-highlight t)
  (setq selectrum-count-style 'current/matches)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (setq selectrum-refine-candidates-function #'orderless-filter))

;; history
(use-package selectrum-prescient
  :disabled
  :straight t
  :after selectrum
  :config
  (setq selectrum-prescient-enable-filtering nil)
  (setq prescient-save-file (concat cpm-cache-dir "prescient-save.el"))
  (prescient-persist-mode)
  (selectrum-prescient-mode +1))

;; (use-package orderless
;;   :straight t
;;   :after selectrum
;;   :config
;;   (setq completion-styles '(orderless))
;;   (setq completion-category-defaults nil)
;;   (setq orderless-skip-highlighting (lambda () selectrum-is-active)))

;;; Miniframe
;; Provides a great ui for completion, similar to posframe
(use-package mini-frame
  :straight (:type git :host github :repo "muffinmad/emacs-mini-frame")
  :hook (after-init . mini-frame-mode)
  :commands (mini-frame-mode)
  :custom
  (mini-frame-show-parameters
   `((top    . 0.0)
     (left   . 0.5)
     (width  . 0.855)
     (height . 11)
     (child-frame-border-width . 15)
     (internal-border-width . 0)
     (left-fringe . 20)
     (right-fringe . 20)
     ;; set colors for bespoke theme
     (foreground-color . ,bespoke-strong)
     (background-color . ,bespoke-subtle)
     ))
  ;; (mini-frame-color-shift-step 7)
  (mini-frame-advice-functions '(read-from-minibuffer
                                 read-string
                                 completing-read))
  (mini-frame-resize nil)
  :config
  (setq mini-frame-ignore-commands
        '("edebug-eval-expression" debugger-eval-expression "vertico" vertico-mode))
  (setq mini-frame-resize 'not-set))


;;; Icomplete
(use-package icomplete-vertical
  :disabled
  :straight (:type built-in)
  :hook (after-init . icomplete-vertical-mode)
  :general
  (:keymaps 'icomplete-minibuffer-map
   "C-v"    'icomplete-vertical-toggle
   "RET"    'icomplete-force-complete-and-exit
   "TAB"    'icomplete-force-complete-and-exit
   "C-M-i"  'minibuffer-complete
   "M-RET"  'exit-minibuffer
   "<down>" 'icomplete-forward-completions
   "C-j"    'icomplete-forward-completions
   "<up>"   'icomplete-backward-completions
   "C-k"    'icomplete-backward-completions)
  :custom
  (icomplete-show-matches-on-no-input t)
  (icomplete-hide-common-prefix nil)
  (icomplete-compute-delay 0.0)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (icomplete-scroll t)
  :config
  (icomplete-mode)
  (icomplete-vertical-mode))

;;; Sticky Buffer
;; Stick/Lock buffer to window, courtesy of ShingoFukuyama.
;; https://gist.github.com/ShingoFukuyama/8797743

;; (defvar sticky-buffer-previous-header-line-format)
;; (define-minor-mode sticky-buffer-mode
;;   "Make the current window always display this buffer."
;;   nil " sticky" nil
;;   (if sticky-buffer-mode
;;       (progn
;;         (set (make-local-variable 'sticky-buffer-previous-header-line-format)
;;              header-line-format)
;;         (set-window-dedicated-p (selected-window) sticky-buffer-mode))
;;     (set-window-dedicated-p (selected-window) sticky-buffer-mode)
;;     (setq header-line-format sticky-buffer-previous-header-line-format)))

;;; Dired Plus
(use-package dired+
  :disabled t
  :after dired
  :hook ((dired-mode . diredp--set-up-font-locking))
  ;;(dired-mode . dired-omit-mode))
  :init
  (setq font-lock-maximum-decoration t)
  (setq diredp-omit-files-regexp "\\.?#\\|^\\.$\\|^\\.\\.")
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-toggle-find-file-reuse-dir 1))
;; :custom-face
;; ;; TODO: change colors to work with bespoke theme
;; (diredp-compressed-file-name ((t (:foreground "#00629D"))))
;; (diredp-compressed-file-suffix ((t (:foreground "#839496"))))
;; (diredp-date-time ((t (:foreground "#9EA0E5"))))
;; (diredp-deletion ((t (:background "Red" :foreground "Yellow"))))
;; (diredp-dir-heading ((t (:background "#69B7F0" :foreground "#002b36"))))
;; (diredp-dir-name ((t (:foreground "#69B7F0"))))
;; (diredp-dir-priv ((t (:foreground "#268bd2"))))
;; (diredp-exec-priv ((t (:foreground "#990A1b"))))
;; (diredp-file-name ((t (:foreground "#2aa198"))))
;; (diredp-file-suffix ((t (:foreground "#839496"))))
;; (diredp-flag-mark-line ((t (:foreground "#dc322f"))))
;; (diredp-no-priv ((t (:foreground "#b58900"))))
;; (diredp-number ((t (:foreground "#DEB542"))))
;; (diredp-rare-priv ((t (:background "#cb4b16" :foreground "#B4C342"))))
;; (diredp-read-priv ((t (:foreground "#F2804F"))))
;; (diredp-tagged-autofile-name ((t (:foreground "#328C04113"))))
;; (diredp-write-priv ((t (:foreground "#8b2C02")))))

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
;;; Citations
;;;; Citeproc
(use-package citeproc-el
  :disabled
  :defer 1
  :straight (:host github :repo "andras-simonyi/citeproc-el"))

;;;; Citeproc for Org
;; This is necessary for Hugo to use with org-ref
;; Though maybe it is obsoleted by Org cite?
(use-package citeproc-org
  ;; :straight (:host github :repo "andras-simonyi/citeproc-org")
  :after ox-hugo
  :demand t
  :config
  (citeproc-org-setup))

;;;; Org-Ref
;; I think org-cite/org-ref-cite is going to render org-ref obsolete
;; Note that this requires helm-bibtex, helm, and ivy as dependencies
(use-package org-ref
  :straight nil
  :commands (org-ref org-ref-get-bibtex-entry)
  :after org
  :demand t
  :init
  ;; (setq reftex-default-bibliography (concat (getenv "HOME") "/Dropbox/Work/bibfile.bib"))
  (setq reftex-default-bibliography '("~/Dropbox/Work/bibfile.bib"))
  (setq org-ref-completion-library 'org-ref-reftex)
  (setq org-ref-default-bibliography reftex-default-bibliography
        org-ref-pdf-directory (concat (getenv "HOME") "/Library/Mobile Documents/iCloud~com~sonnysoftware~bot/Documents/be-library/")
        org-ref-notes-directory (concat (getenv "HOME") "/Users/roambot/Dropbox/Work/projects/notebook/content-org")
        bibtex-completion-notes-path "~/Dropbox/Work/projects/notebook/content-org"
        org-ref-notes-function 'org-ref-notes-function-many-files)
  :config
  (setf (cdr (assoc 'org-mode bibtex-completion-format-citation-functions)) 'org-ref-format-citation)
  (setq doi-utils-download-pdf nil)

  ;; workaround for bibtex timer issue described here:
  ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2018-01/msg00472.html
  (cancel-function-timers 'bibtex-parse-buffers-stealthily)
  )

;;;; Bibtex-Actions
;; Use completing read to select bibtex actions
;; Note that this has bibtex-completion (part of helm-bibtex) as a dependency
(use-package bibtex-actions
  :straight (:host github :repo "bdarcus/bibtex-actions" :includes oc-bibtex-actions)
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
  :general
  (cpm/leader-keys
    "ux" 'bibtex-actions-insert-key)
  :custom
  (bibtex-actions-template '((t . "${author:15}   ${title:40}   ${year:4}")))
  (bibtex-actions-template-suffix '((t . "   ${=key=:15}  ${=type=:12}    ${tags:*}")))
  :config
  ;; use icons
  (setq bibtex-actions-symbols
        `((pdf . (,(all-the-icons-icon-for-file "foo.pdf" :face 'all-the-icons-dred) .
                  ,(all-the-icons-icon-for-file "foo.pdf" :face 'bibtex-actions-icon-dim)))
          (note . (,(all-the-icons-icon-for-file "foo.txt") .
                   ,(all-the-icons-icon-for-file "foo.txt" :face 'bibtex-actions-icon-dim)))
          (link .
                (,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'all-the-icons-dpurple) .
                 ,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'bibtex-actions-icon-dim)))))
  ;; Here we define a face to dim non 'active' icons, but preserve alignment
  (defface bibtex-actions-icon-dim
    '((((background dark)) :inherit bespoke-highlight)
      (((background light)) :inherit bespoke-highlight))
    "Face for obscuring/dimming icons"
    :group 'all-the-icons-faces)

  ;; don't autopopulate initial input
  (setq bibtex-actions-initial-inputs
        '((pdf    . nil)
          (note   . nil)
          (link   . nil)
          (source . nil)))

  ;; (setq bibtex-actions-symbols
  ;;       `((pdf  "" . " ")
  ;;         (note "" . " ")
  ;;         (link "" . " ")))


  ;; Library paths
  (setq bibtex-completion-bibliography "~/Dropbox/Work/bibfile.bib"
        bibtex-completion-library-path "~/Library/Mobile Documents/iCloud~com~sonnysoftware~bot/Documents/be-library"
        bibtex-completion-pdf-field nil
        bibtex-completion-notes-path "~/Dropbox/Work/projects/notebook/content-org"
        bibtex-completion-notes-extension ".org")

  ;; Set insert citekey with markdown citekeys for org-mode
  ;; FIXME -- org-mode citation isn't working the way I want it to
  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-ref)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-pandoc-citeproc)))

  ;; Notes templates
  (setq bibtex-completion-notes-template-one-file "* ${author} (${date}): ${title} \n :PROPERTIES:\n :INTERLEAVE_PDF: ${file}\n :Custom_ID: ${=key=}\n :END:\n [[pdfview:${file}][file link]]")
  (setq bibtex-completion-notes-template-multiple-files "#+TITLE: ${author-or-editor} (${year}): ${title}\n#+ROAM_KEY: cite:${=key=}\n#+SETUPFILE: ./hugo_setup.org\n#+HUGO_SECTION: reading-notes\n\n- Tags :: \n- Bookends link :: bookends://sonnysoftware.com/${beref}\n- PDF :: [[${file}][PDF Link]]\n\n#+BEGIN_SRC bibtex\n (insert (org-ref-get-bibtex-entry \"${=key=}\"))\n#+END_SRC")

  ;; using with org-cite
  ;; make sure to set this to ensure open commands work correctly
  (setq bibtex-completion-additional-search-fields '(doi url))

  ;; Use consult-completing-read for enhanced interface.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; use w/embark-act
  (with-eval-after-load 'embark
    ;; Make the 'bibtex-actions' bindings and targets available to `embark'.
    (add-to-list 'embark-target-finders 'bibtex-actions-citation-key-at-point)
    (add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map))
    (add-to-list 'embark-keymap-alist '(citation-key . bibtex-actions-buffer-map))))

;;;; Company-bibtex

(use-package company-bibtex
  :after company
  :demand t
  :general
  (:states 'insert
   "<C-tab>" #'company-bibtex)
  :config
  (add-to-list 'company-backends 'company-bibtex)
  (setq company-bibtex-bibliography "~/Dropbox/Work/bibfile.bib")
  (setq company-bibtex-org-citation-regex "-?cite:"))

;;;; Org-Ref-Cite
;; FIXME: not sure how to configure this yet
(use-package org-ref-cite
  :disabled
  :straight (:host github :repo "jkitchin/org-ref-cite" )
  :after (:all org oc)
  :config
  ;; (require 'org-ref-cite-advice)
  ;; (require 'org-ref-cite-activate)
  ;; (require 'org-ref-cite-follow)
  ;; (require 'org-ref-cite-export)
  ;; (require 'org-ref-cite-compat)
  ;; (org-cite-register-processor 'org-ref-cite
  ;;   :activate #'org-ref-cite-activate
  ;;   :follow #'org-ref-cite-follow
  ;;   :insert #'org-ref-cite-insert-processor
  ;;   :export-bibliography #'org-ref-cite-export-bibliography
  ;;   :export-citation #'org-ref-cite-export-citation
  ;;   :export-finalizer #'org-ref-cite-use-package
  ;;   :cite-styles (mapcar 'car org-ref-cite-styles))

  ;; green links
  (set-face-attribute 'org-cite nil
                      :inherit bespoke-green)

  (set-face-attribute 'org-cite-key nil
                      :inherit bespoke-green)

  (define-key org-mode-map (kbd "C-c \\") 'org-cite-insert)

  (setq  flyspell-duplicate-distance 0
         flyspell-mark-duplications-flag nil
         warning-minimum-level :error))

;;;; Org Ref
;; I think org-cite/org-ref-cite is going to render org-ref obsolete
(use-package org-ref
  :straight nil
  :disabled t
  :commands (org-ref org-ref-get-bibtex-entry)
  :after org
  :init
  (setq reftex-default-bibliography (concat (getenv "HOME") "/Dropbox/Work/bibfile.bib"))
  (setq org-ref-completion-library 'org-ref-reftex)
  (setq org-ref-default-bibliography reftex-default-bibliography
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


;;;; Citation wreckage
;; ;;;; Org-Cite
;; ;; ;; Eventually this should be a full replacement of org-ref
;; (use-package oc
;;   :after org
;;   ;; :config
;;   ;; ;; activate processor for fontification, preview, etc
;;   ;; ;; currently using basic, but would prefer org-cite-csl-activate
;;   ;; (setq org-cite-follow-processor 'oc-bibtex-actions
;;   ;;       org-cite-insert-processor 'oc-bibtex-actions)
;;   ;; (setq org-cite-follow-processor 'basic
;;   ;;       org-cite-insert-processor 'basic)
;;   ;; ;; setup export processor; default csl/citeproc-el, with biblatex for latex
;;   ;; (setq org-cite-export-processors
;;   ;;       '((beamer natbib)
;;   ;;         (latex biblatex)
;;   ;;         (t csl)))
;;   (setq org-cite-global-bibliography cpm-bibliography))

;;   ;;; Org-cite processors
;; (use-package oc-basic
;;   :straight nil
;;   :after oc)

;; (use-package oc-biblatex
;;   :straight nil
;;   :after oc)

;; (use-package oc-csl
;;   :straight nil
;;   :after oc
;;   :config
;;   ;; optional; add to docs instead?
;;   (setq org-cite-csl-styles-dir "~/.local/share/csl/styles")
;;   (setq org-cite-csl-locales-dir "~/.local/share/csl/locales"))

;; (use-package oc-natbib
;;   :straight nil
;;   :after oc)

;;;; Org-Ref-Cite
;; dependency of org-ref-cite
(use-package major-mode-hydra
  :straight (:host github :repo "jerrypnz/major-mode-hydra.el")
  :defer t)

(use-package org-ref-cite
  :straight nil
  :load-path "/Users/roambot/.emacs.d/.local/elisp/org-ref-cite/"
  :after (:all org oc)
  :config
  (setq org-cite-csl-styles-dir "~/.local/share/csl/styles")
  (setq org-cite-csl-locales-dir "~/.local/share/csl/locales")
  (setq org-cite-global-bibliography cpm-bibliography)
  (setq org-cite-activate-processor 'org-ref-cite
        org-cite-export-processor '((html csl "chicago-author-date-16th-edition.csl")
				                    (latex org-ref-cite)
				                    (t csl "chicago-author-date-16th-edition.csl")))
  ;; ;; green links
  ;; (set-face-attribute 'org-cite nil
  ;;                     :inherit bespoke-green)

  ;; (set-face-attribute 'org-cite-key nil
  ;;                     :inherit bespoke-green)
  (define-key org-mode-map (kbd "C-c \\") 'org-cite-insert)

  (setq  flyspell-duplicate-distance 0
         flyspell-mark-duplications-flag nil
         warning-minimum-level :error))




;;; Old org-roam template
;; '(("d" "default" plain (function org-roam-capture--get-point)
;;    :file-name "%<%Y-%m%d-%H%M>-${slug}"
;;    :head "#+SETUPFILE:./hugo_setup.org\n#+HUGO_SECTION: zettel\n#+HUGO_SLUG: ${slug}\n#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>"
;;    :unnarrowed t
;;    :immediate-finish t)
;; ("p" "private" plain (function org-roam-capture--get-point)
;;  "%?"
;;  :file-name "private-${slug}"
;;  :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>"
;;  :unnarrowed t)))
;; (setq org-roam-ref-capture-templates
;;       '(("r" "ref" plain (function org-roam-capture--get-point)
;;          "%?"
;;          :file-name "websites/${slug}"
;;          :head "#+SETUPFILE:./hugo_setup.org\n#+HUGO_SECTION: Weblinks\n#+ROAM_KEY: ${ref}\n #+HUGO_SLUG: ${slug}\n#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>\n\n- source :: ${ref}"
;;          :unnarrowed t))))

;;; Old citation setup
;;;; Bibliography files
(defvar cpm-bibliography '("~/Dropbox/Work/bibfile.bib"))

;;;; Citations
;;;;; Citeproc
(use-package citeproc
  :straight (:host github
             :repo "andras-simonyi/citeproc-el"
             :branch "1-biblatex_support")
  :defer t
  :config
  (setq org-cite-csl-styles-dir "~/.local/share/csl/styles")
  (setq org-cite-csl-locales-dir "~/.local/share/csl/locales"))

;;;;; Bibtex Completion
;; The backend of helm-ivy bibtex
(use-package bibtex-completion
  :straight (bibtex-completion :host github :repo "tmalsburg/helm-bibtex" :files (:defaults (:exclude "helm-bibtex.el" "ivy-bibtex.el")) :includes oc-bibtex-actions)
  :defer t
  :config
  ;; Library paths
  (setq bibtex-completion-bibliography cpm-bibliography
        bibtex-completion-library-path "~/Library/Mobile Documents/iCloud~com~sonnysoftware~bot/Documents/be-library"
        bibtex-completion-pdf-field nil
        bibtex-completion-notes-path "~/Dropbox/Work/projects/notebook/content-org"
        bibtex-completion-notes-extension ".org")
  ;; using with org-cite
  ;; make sure to set this to ensure open commands work correctly
  (setq bibtex-completion-additional-search-fields '(doi url)))

;;;;; Org-Cite
;; Eventually this should be a full replacement of org-ref
(use-package oc
  :after org
  (setq org-cite-global-bibliography cpm-bibliography)
  (setq org-cite-export-processors
        '((beamer natbib)
          (latex biblatex)
          (t csl))))

;; Org-cite processors
(use-package oc-basic
  :straight nil
  :after oc)

(use-package oc-biblatex
  :straight nil
  :after oc)

(use-package oc-csl
  :straight nil
  :after oc
  :config
  ;; optional; add to docs instead?
  (setq org-cite-csl-styles-dir "~/.local/share/csl/styles")
  (setq org-cite-csl-locales-dir "~/.local/share/csl/locales"))

(use-package oc-natbib
  :straight nil
  :after oc)

;;;;; Bibtex-Actions
;; Use completing read to select bibtex actions
(use-package bibtex-actions
  :requires bibtex-completion
  :straight (:host github :repo "bdarcus/bibtex-actions" :includes oc-bibtex-actions)
  :commands (bibtex-actions-open
             bibtex-actions-open-pdf
             bibtex-actions-open-link
             bibtex-actions-insert-citation
             bibtex-actions-insert-reference
             bibtex-actions-insert-bibtex
             bibtex-actions-add-pdf-attachment
             bibtex-actions-open-notes
             bibtex-actions-open-entry
             bibtex-actions-add-pdf-to-library)
  :custom
  (bibtex-actions-template '((t . "${author:15}   ${title:40}   ${year:4}")))
  (bibtex-actions-template-suffix '((t . "   ${=key=:15}  ${=type=:12}    ${tags:*}")))
  :config
  ;; use icons
  (setq bibtex-actions-symbols
        `((pdf . (,(all-the-icons-icon-for-file "foo.pdf" :face 'all-the-icons-dred) .
                  ,(all-the-icons-icon-for-file "foo.pdf" :face 'bibtex-actions-icon-dim)))
          (note . (,(all-the-icons-icon-for-file "foo.txt") .
                   ,(all-the-icons-icon-for-file "foo.txt" :face 'bibtex-actions-icon-dim)))
          (link .
                (,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'all-the-icons-dpurple) .
                 ,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'bibtex-actions-icon-dim)))))
  ;; Here we define a face to dim non 'active' icons, but preserve alignment
  (defface bibtex-actions-icon-dim
    '((((background dark)) :inherit bespoke-highlight)
      (((background light)) :inherit bespoke-highlight))
    "Face for obscuring/dimming icons"
    :group 'all-the-icons-faces)

  ;; don't autopopulate initial input
  (setq bibtex-actions-initial-inputs
        '((pdf    . nil)
          (note   . nil)
          (link   . nil)
          (source . nil)))

  ;; Notes templates
  (setq bibtex-completion-notes-template-one-file "* ${author} (${date}): ${title} \n :PROPERTIES:\n :INTERLEAVE_PDF: ${file}\n :Custom_ID: ${=key=}\n :END:\n [[pdfview:${file}][file link]]")
  (setq bibtex-completion-notes-template-multiple-files "#+TITLE: ${author-or-editor} (${year}): ${title}\n#+ROAM_KEY: cite:${=key=}\n#+SETUPFILE: ./hugo_setup.org\n#+HUGO_SECTION: reading-notes\n\n- Tags :: \n- Bookends link :: bookends://sonnysoftware.com/${beref}\n- PDF :: [[${file}][PDF Link]]\n\n#+BEGIN_SRC bibtex\n (insert (org-ref-get-bibtex-entry \"${=key=}\"))\n#+END_SRC")

  ;; Library paths
  (setq bibtex-completion-bibliography cpm-bibliography
        bibtex-completion-library-path "~/Library/Mobile Documents/iCloud~com~sonnysoftware~bot/Documents/be-library"
        bibtex-completion-pdf-field nil
        bibtex-completion-notes-path "~/Dropbox/Work/projects/notebook/content-org"
        bibtex-completion-notes-extension ".org")

  ;; Set insert citekey with markdown citekeys for org-mode
  ;; FIXME -- org-mode citation isn't working the way I want it to
  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-cite)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-pandoc-citeproc)))

  ;; Make the 'bibtex-actions' bindings and targets available to `embark'.
  (add-to-list 'embark-target-finders 'bibtex-actions-citation-key-at-point)
  (add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map))
  (add-to-list 'embark-keymap-alist '(citation-key . bibtex-actions-buffer-map))
  ;; (with-eval-after-load 'embark
  ;;   (setf (alist-get 'bibtex embark-keymap-alist) 'bibtex-actions-map))

  ;; using with org-cite
  ;; make sure to set this to ensure open commands work correctly
  (setq bibtex-completion-additional-search-fields '(doi url keywords)))

(use-package oc-bibtex-actions
  :after (oc bibtex-actions)
  :general
  (cpm/leader-keys
    "ux" 'org-cite-insert)
  :config
  (setq org-cite-insert-processor 'oc-bibtex-actions
        org-cite-follow-processor 'oc-bibtex-actions)
  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-cite)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-pandoc-citeproc))))

;;;;; Company-bibtex

(use-package company-bibtex
  :after company
  :demand t
  :general
  (:states 'insert
   "<C-tab>" #'company-bibtex)
  :config
  (add-to-list 'company-backends 'company-bibtex)
  (setq company-bibtex-bibliography "~/Dropbox/Work/bibfile.bib")
  (setq company-bibtex-org-citation-regex "-?cite:@"))

;;; Org Rifle
;; Search [[https://github.com/alphapapa/helm-org-rifle][rapidly]] through org files using helm
(use-package helm-org-rifle
  :commands (helm-org-rifle helm-org-rifle-agenda-files helm-org-rifle-org-directory)
  :config
  ;; fix helm taking over various functions after being activated
  (add-hook 'helm-org-rifle-after-command-hook (lambda () (helm-mode -1))))

;;; Helm gitignore
;; generate ignore files with helm
(use-package helm-gitignore
  :commands helm-gitignore)
;;; Eshell helm
;; helm support
(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
            (define-key eshell-mode-map (kbd "M-l") 'helm-eshell-history)
            (cpm/setup-eshell)))

(when (not (functionp 'eshell/rgrep))
  (defun eshell/rgrep (&rest args)
    "Use Emacs grep facility instead of calling external grep."
    (eshell-grep "rgrep" args t)))

;;; Evil Magit

;;  Evil bindings for magit
;; (use-package evil-magit
;;   :after magit
;;   :demand t
;;   :custom
;;   (evil-magit-use-z-for-folds t)
;;   (evil-magit-use-y-for-yank t)
;;   :general
;;   (:states '(motion normal) :keymaps 'magit-mode-map
;;    "C-j" #'magit-section-forward-sibling
;;    "C-k" #'magit-section-backward-sibling))
;;; Doom Themes
(use-package doom-themes
  :disabled
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable custom treemacs theme (all-the-icons must be installed!)
  (setq doom-themes-treemacs-theme "doom-colors")) ; use the colorful treemacs theme

;;; Org-cite processors
(use-package oc-basic
  :straight nil
  :after oc)
;; Org cite processors
(use-package oc-natbib
  :straight nil
  :after oc)

(use-package oc-biblatex
  :straight nil
  :after oc)
;;; Exec path
;; Exec path -- Emacs won't know where to load things without this
(use-package exec-path-from-shell
  :straight t
  :defer 1
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (setq exec-path-from-shell-arguments '("-l"))
  )

;;; Org to beamer PDF
;; Not sure why I have the universal arg stuff here
(defun cpm/org-export-to-beamer-pdf-open ()
  "Export org subtree to beamer pdf and open"
  (interactive)
  (universal-argument)
  (universal-argument-more)
  (org-open-file (org-beamer-export-to-pdf nil t nil nil '(:latex-class "beamer-presentation"))))

;;; Bibtex actions / Citer
;; Use completing read to select bibtex actions
(use-package citar
  :straight (:host github :repo "bdarcus/citar")
  :commands (citar-insert-citation)
  :init
  (setq citar-file-open-note-function 'orb-bibtex-actions-edit-note)
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :config
  ;; Set templates
  (setq citar-templates
        '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
          (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")))

  ;; use icons
  (setq citar-symbols
        `((file . (,(all-the-icons-icon-for-file "foo.pdf" :face 'all-the-icons-dred) .
                   ,(all-the-icons-icon-for-file "foo.pdf" :face 'citar-icon-dim)))
          (note . (,(all-the-icons-icon-for-file "foo.txt") .
                   ,(all-the-icons-icon-for-file "foo.txt" :face 'citar-icon-dim)))
          (link .
                (,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'all-the-icons-dpurple) .
                 ,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'citar-icon-dim)))))
  ;; Here we define a face to dim non 'active' icons, but preserve alignment
  (defface citar-icon-dim
    '((((background dark)) :inherit bespoke-highlight)
      (((background light)) :inherit bespoke-highlight))
    "Face for obscuring/dimming icons"
    :group 'all-the-icons-faces)

  ;; don't autopopulate initial input
  (setq citar-initial-inputs
        '((pdf    . nil)
          (note   . nil)
          (link   . nil)
          (source . nil)))

  ;; embark
  ;; use consult-completing-read for enhanced interface
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Make the 'bibtex-actions' bindings and targets available to `embark'.
  (with-eval-after-load 'embark
    (add-to-list 'embark-target-finders 'citar-citation-key-at-point)
    (add-to-list 'embark-keymap-alist '(bib-reference . citar-map))
    (add-to-list 'embark-keymap-alist '(citation-key . citar-buffer-map)))

  (setq citar-bibliography `(,cpm-bibliography))
  ;; (with-eval-after-load 'embark
  ;;   (setf (alist-get 'bibtex embark-keymap-alist) 'bibtex-actions-map))

  ;; using with org-cite
  ;; make sure to set this to ensure open commands work correctly
  (setq citar-additional-search-fields '(doi url keywords)))

;;  auto-refreshing cache when bib files change
;; (bibtex-actions-filenotify-setup '(LaTeX-mode-hook markdown-mode-hook org-mode-hook)))

;; (use-package oc-bibtex-actions
;;   :after (oc embark)
;;   :commands (oc-bibtex-actions-select-style oc-bibtex-actions-insert)
;;   :config
;;   (setq org-cite-insert-processor 'oc-bibtex-actions
;;         org-cite-follow-processor 'oc-bibtex-actions
;;         org-cite-activate-processor 'oc-bibtex-actions))

;;; Clipboard

(use-package simpleclip
  :disabled t
  :defer 1
  :config
  (simpleclip-mode 1))

;; These seem no longer necessary
;; Copy/Paste functions
;; ;; https://github.com/dakrone/eos/blob/master/eos-core.org#mac-osx
;; (defun copy-from-osx ()
;;   "Handle copy/paste intelligently on osx."
;;   (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
;;     (if (and (eq system-type 'darwin)
;;              (file-exists-p pbpaste))
;;         (let ((tramp-mode nil)
;;               (default-directory "~"))
;;           (shell-command-to-string pbpaste)))))

;; (defun paste-to-osx (text &optional push)
;;   (let ((process-connection-type nil))
;;     (let ((proc (start-process "pbcopy" "*Messages*" "/usr/bin/pbcopy")))
;;       (process-send-string proc text)
;;       (process-send-eof proc))))

;; (setq interprogram-cut-function 'paste-to-osx
;;       interprogram-paste-function 'copy-from-osx)


;;; Org-Reveal
(use-package ox-reveal
  :disabled
  :commands (org-reveal-export-current-subtree org-reveal-export-to-html-and-browse)
  :custom
  ;; no injecting notes into template alist
  (org-reveal-note-key-char nil)
  :after ox
  :demand t
  :load-path (lambda () (concat cpm-elisp-dir "ox-reveal"))
  :config
  (setq org-reveal-root (concat "file://" (getenv "HOME") "/bin/reveal.js")
        org-reveal-theme "moon"
        org-reveal-default-frag-style "roll-in"
        org-reveal-hlevel 2
        ))

(defun cpm/narrowed-subtree-to-html ()
  "export narrowed tree to html"
  (interactive)
  (org-reveal-export-current-subtree)
  (org-narrow-to-subtree))

(fset 'cpm/reveal-to-html-open
      "\C-c\C-e\C-sRB")

(defun cpm/reveal-to-pdf ()
  "print reveal.js slides to pdf"
  (interactive)
  (async-shell-command "phantomjs ~/bin/print-pdf.js 'file:///Users/roambot/Dropbox/Work/projects/phil105/phil105-classplan.html?print-pdf'")
  (delete-windows-on "*Async Shell Command*" t))

;;; Org GTD Stuff
;; TODO: this isn't working
(defun cpm/project-overview ()
  (interactive)
  (cpm/goto-projects.org)
  (org-narrow-to-subtree)
  (org-columns))
;;set defaults to nothing
(setq org-stuck-projects (quote ("" nil nil "")))

;;;; Stuck Projects
;; I'm following [[http://doc.norang.ca/org-mode.html#Projects][Bert Hansen's]] lead on this
(defun cpm/list-stuck-projects-in-buffer ()
  (interactive)
  (bh/skip-non-stuck-projects)
  (org-agenda nil "s" 'subtree))

(defun cpm/list-all-stuck-projects ()
  (interactive)
  (org-agenda nil "s"))

;;;;; Helper Functions
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
    Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
    This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
    This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
    Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
    When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
    When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
    Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
    Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

;;;; GTD Areas
;; TODO: need to rethink this
(defun cpm/go-to-areas ()
  (interactive)
  (find-file (concat org-directory "todo.org"))
  (widen)
  (goto-char (point-min))
  (re-search-forward "* Areas")
  (beginning-of-line))

(defun cpm/areas-overview ()
  (interactive)
  (go-to-areas)
  (org-narrow-to-subtree)
  (org-columns))

;;;; Random Notes
;; FIXME: Need to fix the list of candidates...
(use-package org-randomnote
  :commands (org-randomnote org-randomnote--go-to-random-header org-randomnote--get-random-file org-randomnote--get-random-subtree))

;;; Org Outlook (Disabled)
;; Open outlook message links in org
;; from https://superuser.com/a/100084 and
;; https://emacs.stackexchange.com/a/35916/11934

;; (defun org-outlook-open (id)
;;   "Open the Outlook item identified by ID.  ID should be an Outlook GUID."
;;   (shell-command-to-string (concat "open" id)))

;; (with-eval-after-load 'org
;;   (org-add-link-type "outlook" 'org-outlook-open)

;;   (org-link-set-parameters
;;    "outlook"
;;    :follow (lambda (path) (org-outlook-open path))
;;    :export (lambda (path desc backend)
;;              (cond
;;               ((eq 'html backend)
;;                (format "<a href=\"outlook:%s\">%s</a>" path desc))))))

;;; Meow

(use-package meow
  :straight (:type git :host github :repo "meow-edit/meow")
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . mode-line-other-buffer)))
  (meow-setup)
  (meow-global-mode 1))
;; All packages related to narrowing and completion

;;; Consult Project
(defun consult-projectile ()
  "Create a multi view with projectile integration.   Displays known projects when there are none or the buffers/files accociated with the project."
  (interactive)
  (when-let (buffer (consult--multi consult-projectile-sources
                                    :prompt "Switch to: "
                                    :history 'consult-projectile--project-history
                                    :sort nil))
    ;; When the buffer does not belong to a source,
    ;; create a new buffer with the name.
    (unless (cdr buffer)
      (funcall consult--buffer-display (car buffer)))))
