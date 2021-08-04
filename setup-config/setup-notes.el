;;; Notebook Setup
;; I use hugo so define a setup file variable
(defvar hugo-notebook-setup-file  "~/Dropbox/Work/projects/notebook/content-org/hugo-notebook-setup.org"
  "Variable for notebook setup using hugo")

(defun cpm/notebook ()
  (interactive)
  (find-file (concat (getenv "HOME") "/Dropbox/Work/projects/notebook/content-org")))

;;; Search Notes
;;;; Zettelkasten Search

(defvar cpm-zettelkasten "~/Dropbox/Work/projects/notebook/content-org")
(defun cpm/zettelkasten-search ()
  "Serch in Zettelkasten with affe-grep."
  (interactive)
  (affe-grep cpm-zettelkasten))


;;;; Consult Notes
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
  :commands (cpm/find-note-relation
             org-roam-node-find
             org-roam-capure
             org-roam-setup)
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
        '(("z" "Zettel" plain "%?"
           :if-new (file+head "%<%Y-%m%d-%H%M>-${slug}.org"
                              ,(concat (concat "#+SETUPFILE:" hugo-notebook-setup-file) "\n#+HUGO_SECTION: zettel\n#+HUGO_SLUG: ${slug}\n#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>"))
           :unnarrowed t)
          ("l" "Lecture" plain "%?"
           :if-new (file+head "lectures/%<%Y-%m%d-%H%M>-${slug}.org"
                              ,(concat (concat "#+SETUPFILE:" hugo-notebook-setup-file) "\n#+HUGO_SECTION: lectures\n#+HUGO_SLUG: ${slug}\n#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>"))
           :unnarrowed t)
          ("p" "private" plain "%?"
           :if-new (file+head "private-${slug}.org"
                              "#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>"
                              :unnarrowed t))
          ("r" "reference note" plain "%?"
           :if-new (file+head "ref-notes/${citekey}.org"
                              ,(concat (concat "#+SETUPFILE:" hugo-notebook-setup-file) "\n#+TITLE: ${author-or-editor-abbrev} (${year}): ${title}\n#+ROAM_KEY: cite:${=citekey=}\n \n#+HUGO_SECTION: reading-notes\n\n- Tags :: \n- Bookends link :: bookends://sonnysoftware.com/${beref}\n- PDF :: [[${file}][PDF Link]]\n\n#+begin_src bibtex\n (insert (org-ref-get-bibtex-entry \"${=key=}\"))\n#+end_src"))
           :unnarrowed t)))

  ;; Filtering by subdirectory
  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "(%s)" (string-join (f-split dirs) "/"))
      ""))

  ;; Use dashes rather than underscores in your slugs
  ;; Need to redefine function (no customization available)
  ;; See https://github.com/org-roam/org-roam/pull/1544
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (let ((title (org-roam-node-title node))
          (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768 ; U+0300 COMBINING GRAVE ACCENT
                             769 ; U+0301 COMBINING ACUTE ACCENT
                             770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771 ; U+0303 COMBINING TILDE
                             772 ; U+0304 COMBINING MACRON
                             774 ; U+0306 COMBINING BREVE
                             775 ; U+0307 COMBINING DOT ABOVE
                             776 ; U+0308 COMBINING DIAERESIS
                             777 ; U+0309 COMBINING HOOK ABOVE
                             778 ; U+030A COMBINING RING ABOVE
                             780 ; U+030C COMBINING CARON
                             795 ; U+031B COMBINING HORN
                             803 ; U+0323 COMBINING DOT BELOW
                             804 ; U+0324 COMBINING DIAERESIS BELOW
                             805 ; U+0325 COMBINING RING BELOW
                             807 ; U+0327 COMBINING CEDILLA
                             813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814 ; U+032E COMBINING BREVE BELOW
                             816 ; U+0330 COMBINING TILDE BELOW
                             817 ; U+0331 COMBINING MACRON BELOW
                             )))
      (cl-flet* ((nonspacing-mark-p (char)
                                    (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s)
                                         (ucs-normalize-NFC-string
                                          (apply #'string (seq-remove #'nonspacing-mark-p
                                                                      (ucs-normalize-NFD-string s)))))
                 (cl-replace (title pair)
                             (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-")  ;; convert anything not alphanumeric
                        ("--*" . "-")  ;; remove sequential underscores
                        ("^-" . "")    ;; remove starting underscore
                        ("-$" . "")))  ;; remove ending underscore
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

  ;; Update when idle
  ;; https://orgmode-exocortex.com/2021/07/22/configure-org-roam-v2-to-update-database-only-when-idle/
  (with-eval-after-load "org-roam"
    ;; queue for files that will be updated in org-roam-db when emacs is idle
    (setq org-roam-db-update-queue (list))
    ;; save the original update function;
    (setq orig-update-file (symbol-function 'org-roam-db-update-file))
    ;; then redefine the db update function to add the filename to a queue
    (defun org-roam-db-update-file (&optional file-path)
      ;; do same logic as original to determine current file-path if not passed as arg
      (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
      (message "org-roam: scheduling update of %s" file-path)
      (if (not (memq file-path org-roam-db-update-queue))
          (push file-path org-roam-db-update-queue)))

    ;; this function will be called when emacs is idle for a few seconds
    (defun org-roam-db-idle-update-files ()
      ;; go through queued filenames one-by-one and update db
      ;; if we're not idle anymore, stop. will get rest of queue next idle.
      (while (and org-roam-db-update-queue (current-idle-time))
        ;; apply takes function var and list
        (apply orig-update-file (list (pop org-roam-db-update-queue)))))

    ;; we'll only start updating db if we've been idle for this many seconds
    (run-with-idle-timer 5 t #'org-roam-db-idle-update-files))
  )


;;;;; Fancy Node Icons
;; Fancy org-roam-node-find with icons and overlays (which allow for better searching whilst keeping the icons
;; From https://github.com/hieutkt/.doom.d/blob/master/config.el#L690-L745 or
;; https://orgroam.slack.com/archives/CV20S23C0/p1626662183035800
(with-eval-after-load 'org-roam
  (require 'all-the-icons)

  ;; Define var for special tags
  (defvar cpm-spec-tags nil)
  ;; Set template disply in find-node
  (setq org-roam-node-display-template (concat "${backlinkscount:16} " "${functiontag:16} " "${othertags:13} " "${hierarchy:183}"))

  (cl-defmethod org-roam-node-filetitle ((node org-roam-node))
    "Return the file TITLE for the node."
    (org-roam-get-keyword "TITLE" (org-roam-node-file node))
    )

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                          :from links
                          :where (= dest $s1)
                          :and (= type "id")]
                         (org-roam-node-id node))))
           )
      (if (> count 0)
          (concat (propertize "=has:backlinks=" 'display (all-the-icons-material "link" :face 'all-the-icons-dblue :height 0.9)) (format "%d" count))
        (concat (propertize "=not-backlinks=" 'display (all-the-icons-material "link" :face 'org-roam-dim :height 0.9))  " ")
        )
      ))

  (cl-defmethod org-roam-node-functiontag ((node org-roam-node))
    "The first tag of notes are used to denote note type"
    (let* ((specialtags cpm-spec-tags)
           (tags (seq-filter (lambda (tag) (not (string= tag "ATTACH"))) (org-roam-node-tags node)))
           (functiontag (seq-intersection specialtags tags 'string=))
           )
      (concat
       ;; (if functiontag
       ;;     (propertize "=has:functions=" 'display (all-the-icons-octicon "gear" :face 'all-the-icons-silver :v-adjust 0.02 :height 0.8))
       ;;   (propertize "=not-functions=" 'display (all-the-icons-octicon "gear" :face 'org-roam-dim :v-adjust 0.02 :height 0.8))
       ;;   )
       (if functiontag
           (propertize "=@=" 'display (all-the-icons-faicon "tags" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.7))
         (propertize "= =" 'display (all-the-icons-faicon "tags" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.7))
         )
       " "
       (string-join functiontag ", "))
      ))

  (cl-defmethod org-roam-node-othertags ((node org-roam-node))
    "Return the file TITLE for the node."
    (let* ((tags (seq-filter (lambda (tag) (not (string= tag "ATTACH"))) (org-roam-node-tags node)))
           (specialtags cpm-spec-tags)
           (othertags (seq-difference tags specialtags 'string=))
           )
      (concat
       ;; " "
       ;; (if othertags
       ;;     (propertize "=has:tags=" 'display (all-the-icons-faicon "tags" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8))
       ;;   (propertize "=not-tags=" 'display (all-the-icons-faicon "tags" :face 'all-the-icons-dgreen :v-adjust 0.02 :height 0.8))
       ;;   )
       ;; " "
       (if othertags
           (propertize "=@=" 'display "")
         (propertize "= =" 'display "")
         )
       (propertize (string-join othertags ", ") 'face 'all-the-icons-dgreen))
      ))

  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    "Return the hierarchy for the node."
    (let* ((title (org-roam-node-title node))
           (olp (mapcar (lambda (s) (if (> (length s) 10) (concat (substring s 0 10)  "...") s)) (org-roam-node-olp node)))
           (level (org-roam-node-level node))
           (filetitle (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
           (shortentitle (if (> (length filetitle) 20) (concat (substring filetitle 0 20)  "...") filetitle))
           (separator (concat " " (all-the-icons-material "chevron_right") " "))
           )
      (cond
       ((>= level 1) (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "list" :face 'all-the-icons-blue))
                             " "
                             (propertize shortentitle 'face 'org-roam-dim)
                             (propertize separator 'face 'org-roam-dim)
                             title))
       (t (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "insert_drive_file" :face 'all-the-icons-yellow))
                  " "
                  title))
       )
      )))

;;;; Find Org-Roam nodes by relation
;; https://ag91.github.io/blog/2021/03/12/find-org-roam-notes-via-their-relations/
(with-eval-after-load 'org-roam
  (defun cpm/find-note-relation (arg &optional node choices)
    "Navigate org-roam notes by link. With universal ARG tries to use only to navigate the tags of the current note. Optionally takes a selected NOTE and filepaths CHOICES."
    (interactive "P")
    (let* ((depth (if (numberp arg) arg 1))
           (choices
            (or choices
                (when arg
                  (-map #'org-roam-backlink-target-node (org-roam-backlinks-get (org-roam-node-from-id (or (ignore-errors (org-roam-node-id node))
                                                                                                           (org-id-get-create))))))))
           (all-notes (org-roam-node--completions))
           (completions
            (or (--filter (-contains-p choices (cdr it)) all-notes) all-notes))
           (next-node
            ;; taken from org-roam-node-read
            (let* ((nodes completions)
                   (node (completing-read
                          "Node: "
                          (lambda (string pred action)
                            (if (eq action 'metadata)
                                '(metadata
                                  (annotation-function . (lambda (title)
                                                           (funcall org-roam-node-annotation-function
                                                                    (get-text-property 0 'node title))))
                                  (category . org-roam-node))
                              (complete-with-action action nodes string pred))))))
              (or (cdr (assoc node nodes))
                  (org-roam-node-create :title node)))))
      (if (equal node next-node)
          (org-roam-node-visit node)
        (cpm/find-note-relation nil next-node (cons next-node (-map #'org-roam-backlink-source-node (org-roam-backlinks-get next-node))))))))

;;;; Org Roam Bibtex
;; If you installed via MELPA
(use-package org-roam-bibtex
  :disabled t
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



;;;; Org Roam UI (Server/Web App)
(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :commands (org-roam-ui-mode)
  )

;;; Provide Setup-Notes
(provide 'setup-notes)
