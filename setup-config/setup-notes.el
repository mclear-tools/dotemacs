;;;; Search Notes
;;;;; Zettelkasten Search

(defvar cpm-zettelkasten "~/Dropbox/Work/projects/notebook/content-org")
(defun cpm/zettelkasten-search ()
  "Serch in Zettelkasten with affe-grep."
  (interactive)
  (affe-grep cpm-zettelkasten))


;;;;; Consult Notes
;; Adapted from https://github.com/minad/consult/wiki/hrm-notes
(use-package consult-notes
  :straight (:local-repo "/Users/roambot/.emacs.d/.local/elisp/consult-notes/")
  :commands (consult-notes consult-notes-all-search)
  :config
  ;; Sources for file search
  (setq consult-notes-sources-data
        '(("Zettel"          ?z "~/Dropbox/Work/projects/notebook/content-org/")
          ("Org"             ?o "~/Dropbox/org-files/")
          ("Lecture Notes"   ?l "~/Dropbox/Work/projects/notebook/content-org/lectures/")
          ("Reference Notes" ?r "~/Dropbox/Work/projects/notebook/content-org/ref-notes/")
          ("Org Refile"      ?R "~/Dropbox/Work/projects/notebook/org-refile/")))
  ;; Dir for affe-grep of all notes
  (setq consult-notes-all-notes "~/Dropbox/Work/projects/notes-all/"))

;;; Org Roam (Wiki & Notes)
;; Good notes package but a lot is still in flux
;; see https://org-roam.readthedocs.io/en/latest/

;;;; Org Roam
(use-package org-roam
  :commands (org-roam org-roam-new-file org-roam-find-file)
  :hook (org-mode . org-roam-setup)
  :init
  ;; No warnings
  (setq org-roam-v2-ack t)
  :config
  ;; Configure dirs
  (setq org-roam-directory "~/Dropbox/Work/projects/notebook/content-org/")
  (setq org-roam-db-location (concat org-roam-directory "org-roam.db"))

  ;; Add completion
  (push 'company-capf company-backends)

  ;; Org Roam Templating
  ;; see https://org-roam.readthedocs.io/en/latest/templating/
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y-%m%d-%H%M>-${slug}.org"
                              "#+SETUPFILE:./hugo_setup.org\n#+HUGO_SECTION: zettel\n#+HUGO_SLUG: ${slug}\n#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>")
           :unnarrowed t)
          ("p" "private" plain "%?"
           :if-new (file+head "private-${slug}.org"
                              "#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>"
                              :unnarrowed t))
          ("r" "reference note" plain "%?"
           :if-new (file+head "${citekey}.org"
                              "#+TITLE: ${author-or-editor-abbrev} (${year}): ${title}\n#+ROAM_KEY: cite:${=citekey=}\n#+SETUPFILE: ./hugo_setup.org\n#+HUGO_SECTION: reading-notes\n\n- Tags :: \n- Bookends link :: bookends://sonnysoftware.com/${beref}\n- PDF :: [[${file}][PDF Link]]\n\n#+begin_src bibtex\n (insert (org-ref-get-bibtex-entry \"${=key=}\"))\n#+end_src"
                              :unnarrowed t))))

  ;; Filtering by subdirectory
  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "(%s)" (string-join (f-split dirs) "/"))
      ""))

  ;; use dashes rather than underscores in your slugs
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    (let ((title (org-roam-node-title node)))
      (cl-flet* ((nonspacing-mark-p (char)
				                    (memq char org-roam-slug-trim-chars))
	             (strip-nonspacing-marks (s)
				                         (ucs-normalize-NFC-string
					                      (apply #'string (seq-remove #'nonspacing-mark-p
								                                      (ucs-normalize-NFD-string s)))))
	             (cl-replace (title pair)
			                 (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-")
		                ("--*" . "-")
		                ("^-" . "")
		                ("-$" . "")))
	           (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
	      (downcase slug)))))


  ;; Showing the number of backlinks for each node in org-roam-node-find
  ;; https://github.com/org-roam/org-roam/wiki/Hitchhiker's-Rough-Guide-to-Org-roam-V2

  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "(%s)" (car (f-split dirs)))
      ""))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                          :from links
                          :where (= dest $s1)
                          :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  ;; Set template disply in find-node
  (setq org-roam-node-display-template "${directories:15} ${tags:10} ${title:*} ${backlinkscount:6}")
  )
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


;;;; Org Roam Bibtex
;; If you installed via MELPA
(use-package org-roam-bibtex
  :straight (:host github :repo "org-roam/org-roam-bibtex")
  :after org-roam
  :hook (org-mode . org-roam-bibtex-mode))

;; :bind (:map org-mode-map
;;        ("s-b" . orb-note-actions))
;; :config
;; (setq orb-templates
;;       '(("b" "bib" plain (function org-roam-capture--get-point) ""
;;          :file-name "${citekey}"
;;          :head "#+TITLE: ${author-or-editor-abbrev} (${year}): ${title}\n#+ROAM_KEY: cite:${=citekey=}\n#+SETUPFILE: ./hugo_setup.org\n#+HUGO_SECTION: reading-notes\n\n- Tags :: \n- Bookends link :: bookends://sonnysoftware.com/${beref}\n- PDF :: [[${file}][PDF Link]]\n\n#+begin_src bibtex\n (insert (org-ref-get-bibtex-entry \"${=key=}\"))\n#+end_src" ; <--
;;          :unnarrowed t))))



;;;; Org Roam Server
(use-package org-roam-server
  :disabled t
  :ensure t
  :disabled t
  :commands org-roam-server-mode
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

;;; Provide Setup-Notes
(provide 'setup-notes)
