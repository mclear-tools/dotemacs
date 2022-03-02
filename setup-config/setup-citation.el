;;; Bibliography files
(defvar cpm-bibliography (concat (getenv "HOME") "/Dropbox/Work/bibfile.bib"))
(defvar cpm-bib-notes (concat (getenv "HOME") "/Dropbox/Work/projects/notebook/content-org/ref-notes"))

;;; Citations

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
          (t csl))))

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

;;;; Citar
(use-package citar
  :straight (:host github :repo "bdarcus/citar")
  :commands (citar-open-beref
             citar-open-notes
             citar-insert-citation)
  :bind (:map citar-map
         ("b" .  #'citar-open-beref))
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
        '((main . "${author editor:30} ${date year issued:4} ${title:48}")
          (suffix . "    ${=key= id:15} ${=type=:12}  ${=beref=:12} ${tags keywords:*}")
          (note . "${author-or-editor} (${year}): ${title}\n#+ROAM_KEY: cite:${=key=}\n#+SETUPFILE: ../hugo-notebook-setup.org\n#+HUGO_SECTION: reading-notes\n\n- Tags :: \n- Bookends link :: bookends://sonnysoftware.com/${beref}\n- PDF :: [[${file}][PDF Link]]\n\n\n#+BEGIN_SRC emacs-lisp :exports none\n(insert \"#+BEGIN_SRC bibtex\")\n(newline)\n(citar--insert-bibtex \"${=key=}\")\n(insert \"#+END_SRC\")\n#+END_SRC\n")))
  (setq citar-symbols
        `((file ,(all-the-icons-octicon "file-pdf" :face 'bespoke-red) . " ")
          (note ,(all-the-icons-octicon "file-text" :face 'bespoke-brown) . " ")
          (link ,(all-the-icons-octicon "link-external" :face 'bespoke-green) . " ")))
  ;; edit notes
  (setq citar-notes-paths `(,cpm-bib-notes))
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


;;;; Company-bibtex

(use-package company-bibtex
  :bind (("<C-tab>" . bibtex-capf))
  :after cape
  :config
  ;; use with corfu/cape
  (defalias 'bibtex-capf (cape-interactive-capf (cape-company-to-capf 'company-bibtex)))
  (setq company-bibtex-bibliography cpm-bibliography)
  (setq company-bibtex-org-citation-regex "-?@"))



;;; Provide File
(provide 'setup-citation)
