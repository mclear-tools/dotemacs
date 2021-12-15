;;; Bibliography files
(defvar cpm-bibliography (concat (getenv "HOME") "/Dropbox/Work/bibfile.bib"))

;;; Citations

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
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-path "~/Dropbox/Work/projects/notebook/content-org/ref-notes"
        bibtex-completion-notes-extension ".org")
  ;; using with org-cite
  ;; make sure to set this to ensure open commands work correctly
  (setq bibtex-completion-additional-search-fields '(doi url keywords))

  ;; Notes templates
  (setq bibtex-completion-notes-template-one-file "* ${author-or-editor} (${date}): ${title} \n :PROPERTIES:\n :INTERLEAVE_PDF: ${file}\n :Custom_ID: ${=key=}\n :END:\n [[pdfview:${file}][file link]]")
  (setq bibtex-completion-notes-template-multiple-files "#+TITLE: ${author-or-editor} (${year}): ${title}\n#+ROAM_KEY: cite:${=key=}\n#+SETUPFILE: ./hugo_setup.org\n#+HUGO_SECTION: reading-notes\n\n- Tags :: \n- Bookends link :: bookends://sonnysoftware.com/${beref}\n- PDF :: [[${file}][PDF Link]]\n\n#+BEGIN_SRC bibtex\n (insert (org-ref-get-bibtex-entry \"${=key=}\"))\n#+END_SRC")

  ;; Set insert citekey with markdown citekeys for org-mode
  ;; FIXME -- org-mode citation isn't working the way I want it to
  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-cite)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-pandoc-citeproc))))

;;;; Org-Cite
;; Eventually this should be a full replacement for org-ref
(use-package oc
  :straight nil
  :after org
  :config
  (setq org-cite-global-bibliography `(,cpm-bibliography))
  (setq org-cite-export-processors
        '((beamer csl)
          (latex csl)
          (t csl)))
  ;; Color citation links for bespoke-theme
  (set-face-attribute 'org-cite nil :foreground bespoke-blue)
  (set-face-attribute 'org-cite-key nil :foreground bespoke-green))

;; Currently only using csl
(use-package oc-csl
  :straight nil
  :after oc
  :init
  ;; make sure to download csl
  ;; https://citationstyles.org
  ;; https://github.com/citation-style-language
  ;; repos for styles & locales
  (setq org-cite-csl-styles-dir "~/.local/share/csl/styles")
  (setq org-cite-csl-locales-dir "~/.local/share/csl/locales"))

;;;; Citeproc
(use-package citeproc
  :straight (:host github :repo "andras-simonyi/citeproc-el")
  :after (oc oc-csl))

;;;; Org Ref
;; If you installed via MELPA
(use-package org-ref
  :straight (:host github
             :repo "jkitchin/org-ref")
  :after org-roam-bibtex)


;;;; Org Roam Bibtex
(use-package org-roam-bibtex
  :straight (:host github :repo "org-roam/org-roam-bibtex")
  :after org-roam
  :hook (org-roam . org-roam-bibtex-mode)
  :config
  (require 'org-ref)
  ;; fix org-ref lag in typing
  ;; see https://github.com/jkitchin/org-ref/issues/647
  (setq org-ref-colorize-links nil)
  (setq org-ref-show-broken-links nil)

  (setq orb-process-file-keyword nil)
  (setq orb-preformat-keywords '("citekey" "key" "entry-type" "year" "beref" "date" "pdf?" "note?" "file" "author" "editor" "author-abbrev" "editor-abbrev" "author-or-editor-abbrev")))

;; :bind (:map org-mode-map
;;        ("s-b" . orb-note-actions))
;; :config
;; (setq orb-templates
;;       '(("b" "bib" plain (function org-roam-capture--get-point) ""
;;          :file-name "${citekey}"
;;          :head "#+TITLE: ${author-or-editor-abbrev} (${year}): ${title}\n#+ROAM_KEY: cite:${=citekey=}\n#+SETUPFILE: ./hugo_setup.org\n#+HUGO_SECTION: reading-notes\n\n- Tags :: \n- Bookends link :: bookends://sonnysoftware.com/${beref}\n- PDF :: [[${file}][PDF Link]]\n\n#+begin_src bibtex\n (insert (org-ref-get-bibtex-entry \"${=key=}\"))\n#+end_src" ; <--
;;          :unnarrowed t))))


;;;; Citar
(use-package citar
  :straight (:host github :repo "bdarcus/citar")
  :after (org oc)
  :custom
  (citar-bibliography `(,cpm-bibliography))
  (org-cite-global-bibliography `(,cpm-bibliography))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :config
  ;; use consult-completing-read for enhanced interface
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  ;; use embark with at-point
  (setq citar-at-point-function 'embark-act)
  (setq citar-default-action 'citar-open-beref)
  ;; add beref entry for bookends
  (setq citar-additional-fields '("doi" "url" "beref"))
  (setq citar-templates
        '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
          (suffix . "          ${=key= id:15}    ${=type=:12}  ${=beref=:12}  ${tags keywords:*}")))
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
  ;; edit notes
  (setq citar-open-note-function 'orb-citar-edit-note)
  (setq citar-library-paths '("/Users/roambot/Library/Mobile Documents/iCloud~com~sonnysoftware~bot/Documents/be-library")))


;; Citar & Bookends
(defun citar-get-beref (entry)
  (let* ((field (citar-has-a-value '(beref) entry))
         (base-url (pcase field
                     ('beref "bookends://sonnysoftware.com/"))))
    (when field
      (concat base-url (citar-get-value field entry)))))

(defun citar-open-beref (keys-entries)
  "Open bookends link associated with the KEYS-ENTRIES in bookends.

With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (dolist (key-entry keys-entries)
    (let ((link (citar-get-beref (cdr key-entry))))
      (if link
          (browse-url-default-browser link)
        (message "No ref found for %s" key-entry)))))


;; (with-eval-after-load 'citar
;;   (push '(define-key map (kbd "b") (cons "open in bookends" #'citar-open-beref)) citar-citation-map)
;;   (push '(define-key map (kbd "b") (cons "open in bookends" #'citar-open-beref)) citar-map))

;;;; Company-bibtex

(use-package company-bibtex
  :general
  (:states 'insert
   "<C-tab>" #'company-bibtex)
  :config
  (add-to-list 'company-backends 'company-bibtex)
  (setq company-bibtex-bibliography cpm-bibliography)
  (setq company-bibtex-org-citation-regex "-?@"))


;;; Custom Functions
;;;; Insert Bibtex Source Entry

(defun cpm-bibtex (key)
  "insert bibtex entry"
  (require 'org-ref)
  (insert (concat "\n\n#+begin_src bibtex\n" (org-ref-get-bibtex-entry key) "\n#+end_src\n")))
;; (citar--insert-bibtex key)

;;; Provide File
(provide 'setup-citation)
