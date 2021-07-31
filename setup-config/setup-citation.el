;;; Bibliography files
(defvar cpm-bibliography '("~/Dropbox/Work/bibfile.bib"))

;;; Citations
;;;; Org-Cite
;; Eventually this should be a full replacement of org-ref
(use-package oc
  :after org
  :general
  (cpm/leader-keys
    "ux" 'org-cite-insert)
  :config
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
  :init
  ;; optional; add to docs instead?
  (setq org-cite-csl-styles-dir "~/.local/share/csl/styles")
  (setq org-cite-csl-locales-dir "~/.local/share/csl/locales"))

(use-package oc-natbib
  :straight nil
  :after oc)


;;;; Citeproc
(use-package citeproc
  :straight (:host github
             :repo "andras-simonyi/citeproc-el"
             :branch "1-biblatex_support")
  :after (oc oc-csl))

;;;; Bibtex Completion
;; The backend of helm-ivy bibtex
;; Needed for the other packages
(use-package bibtex-completion
  :straight (bibtex-completion :host github :repo "tmalsburg/helm-bibtex" :files (:defaults (:exclude "helm-bibtex.el" "ivy-bibtex.el")) :includes oc-bibtex-actions)
  :after (:any org markdown)
  :init
  ;; Library paths
  (setq bibtex-completion-bibliography cpm-bibliography
        bibtex-completion-library-path "~/Library/Mobile Documents/iCloud~com~sonnysoftware~bot/Documents/be-library"
        bibtex-completion-pdf-field nil
        bibtex-completion-notes-path "~/Dropbox/Work/projects/notebook/content-org"
        bibtex-completion-notes-extension ".org")
  ;; using with org-cite
  ;; make sure to set this to ensure open commands work correctly
  (setq bibtex-completion-additional-search-fields '(doi url keywords))

  ;; Notes templates
  (setq bibtex-completion-notes-template-one-file "* ${author} (${date}): ${title} \n :PROPERTIES:\n :INTERLEAVE_PDF: ${file}\n :Custom_ID: ${=key=}\n :END:\n [[pdfview:${file}][file link]]")
  (setq bibtex-completion-notes-template-multiple-files "#+TITLE: ${author-or-editor} (${year}): ${title}\n#+ROAM_KEY: cite:${=key=}\n#+SETUPFILE: ./hugo_setup.org\n#+HUGO_SECTION: reading-notes\n\n- Tags :: \n- Bookends link :: bookends://sonnysoftware.com/${beref}\n- PDF :: [[${file}][PDF Link]]\n\n#+BEGIN_SRC bibtex\n (insert (org-ref-get-bibtex-entry \"${=key=}\"))\n#+END_SRC")

  ;; Set insert citekey with markdown citekeys for org-mode
  ;; FIXME -- org-mode citation isn't working the way I want it to
  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-cite)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-pandoc-citeproc))))

;;;; Bibtex-Actions
;; Use completing read to select bibtex actions
(use-package bibtex-actions
  :after bibtex-completion
  :demand t
  :straight (:host github :repo "bdarcus/bibtex-actions" :includes oc-bibtex-actions)
  :commands (bibtex-actions-open
             bibtex-actions-open-pdf
             bibtex-actions-open-link
             bibtex-actions-insert-citation
             bibtex-actions-insert-key
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

  ;; Make the 'bibtex-actions' bindings and targets available to `embark'.
  (with-eval-after-load 'embark
    (add-to-list 'embark-target-finders 'bibtex-actions-citation-key-at-point)
    (add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map))
    (add-to-list 'embark-keymap-alist '(citation-key . bibtex-actions-buffer-map)))
  ;; (with-eval-after-load 'embark
  ;;   (setf (alist-get 'bibtex embark-keymap-alist) 'bibtex-actions-map))

  ;; using with org-cite
  ;; make sure to set this to ensure open commands work correctly
  (setq bibtex-completion-additional-search-fields '(doi url keywords)))

(use-package oc-bibtex-actions
  :after (:any oc bibtex-actions)
  :demand t
  :commands (oc-bibtex-actions-select-style)
  :config
  (setq org-cite-insert-processor 'oc-bibtex-actions
        org-cite-follow-processor 'oc-bibtex-actions))

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
  (setq company-bibtex-org-citation-regex "-?cite:@"))


;;; Provide File
(provide 'setup-citation)
