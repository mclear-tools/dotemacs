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
  "Search in Zettelkasten with affe-grep."
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
  :straight (:host github :repo "mclear-tools/org-roam")
  :commands (cpm/find-note-relation
             org-roam-node-find
             org-roam-node-insert
             org-roam-capture
             org-roam-buffer-toggle)
  :hook (org-mode . org-roam-setup)
  :custom
  ;; Configure dirs
  (org-roam-directory "~/Dropbox/Work/projects/notebook/content-org/")
  (org-roam-db-location (concat org-roam-directory "org-roam.db"))
  :init
  ;; No warnings
  (setq org-roam-v2-ack t)
  :config
  (org-roam-db-autosync-mode 1)

  ;; Add completion
  (push 'company-capf company-backends)

  ;; Org Roam Templating
  ;; see https://org-roam.readthedocs.io/en/latest/templating/
  (setq org-roam-capture-templates
        `(("z" "Zettel" plain "%?"
           :target (file+head "%<%Y-%m%d-%H%M>-${slug}.org"
                              ,(concat (concat "#+SETUPFILE:" hugo-notebook-setup-file) "\n#+HUGO_SECTION: zettel\n#+HUGO_SLUG: ${slug}\n#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>"))
           :unnarrowed t)
          ("l" "Lecture" plain "%?"
           :target (file+head "lectures/%<%Y-%m%d-%H%M>-${slug}.org"
                              ,(concat (concat "#+SETUPFILE:" hugo-notebook-setup-file) "\n#+HUGO_SECTION: lectures\n#+HUGO_SLUG: ${slug}\n#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>"))
           :unnarrowed t)
          ("p" "private" plain "%?"
           :target (file+head "private-${slug}.org"
                              "#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>"
                              :unnarrowed t))
          ("r" "reference note" plain "%?"
           :target (file+head "ref-notes/${citekey}.org"
                              ,(concat (concat "#+SETUPFILE:" hugo-notebook-setup-file) "\n#+TITLE: ${author-or-editor-abbrev} ${year}: ${title}\n#+hugo_section: reading-notes\n\n- tags :: \n- bookends link :: bookends://sonnysoftware.com/${beref}\n- pdf :: [[${file}][pdf link]]\n\n#+begin_src emacs-lisp :results value latex\n (bibtex-actions-insert-bibtex '((\"${citekey}\")))\n#+end_src"))
           :unnarrowed t)))

  ;; Filtering by subdirectory
  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "(%s)" (string-join (f-split dirs) "/"))
      ""))

  ;; Use dashes rather than underscores in your slugs
  ;; Need to redefine function (no customization available)
  ;; See https://github.com/org-roam/org-roam/pull/1544
  ;; Done in personal repo

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
  )
;; Update when idle
;; https://orgmode-exocortex.com/2021/07/22/configure-org-roam-v2-to-update-database-only-when-idle/
;; FIXME -- seems not to be working quite right
;; (with-eval-after-load "org-roam"
;;   ;; queue for files that will be updated in org-roam-db when emacs is idle
;;   (setq org-roam-db-update-queue (list))
;;   ;; save the original update function;
;;   (setq orig-update-file (symbol-function 'org-roam-db-update-file))
;;   ;; then redefine the db update function to add the filename to a queue
;;   (defun org-roam-db-update-file (&optional file-path)
;;     ;; do same logic as original to determine current file-path if not passed as arg
;;     (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
;;     (message "org-roam: scheduling update of %s" file-path)
;;     (if (not (memq file-path org-roam-db-update-queue))
;;         (push file-path org-roam-db-update-queue)))

;;   ;; this function will be called when emacs is idle for a few seconds
;;   (defun org-roam-db-idle-update-files ()
;;     ;; go through queued filenames one-by-one and update db
;;     ;; if we're not idle anymore, stop. will get rest of queue next idle.
;;     (while (and org-roam-db-update-queue (current-idle-time))
;;       ;; apply takes function var and list
;;       (apply orig-update-file (list (pop org-roam-db-update-queue)))))

;;   ;; we'll only start updating db if we've been idle for this many seconds
;;   (run-with-idle-timer 5 t #'org-roam-db-idle-update-files))



;;;; Fancy Node Icons
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
           (all-notes (org-roam-node-read--completions))
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
(use-package org-ref :straight (:host github :repo "jkitchin/org-ref") :after org-roam-bibtex)
  (use-package org-roam-bibtex
    :straight (:host github :repo "org-roam/org-roam-bibtex")
    :after org-roam
    :hook (org-mode . org-roam-bibtex-mode)
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



;;;; Org Roam UI (Server/Web App)
(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :commands (org-roam-ui-mode)
  )

;;; Provide Setup-Notes
(provide 'setup-notes)
