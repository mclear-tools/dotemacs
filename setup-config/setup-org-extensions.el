;; Additions to org core functions

;;; Org Babel
;; org babel source block settings
(setq org-src-fontify-natively t
      org-src-window-setup 'other-window
      org-src-tab-acts-natively nil
      org-confirm-babel-evaluate nil
      org-src-strip-leading-and-trailing-blank-lines t)

;; Avoid `org-babel-do-load-languages' since it does an eager require.
(use-package ob-python
  :straight nil
  :defer t
  :commands (org-babel-execute:python)
  :config
  (progn
    (setq org-babel-python-command "python3"))) ;Default to python 3.x

(use-package ob-ditaa
  :straight nil
  :defer t
  :config
  (progn
    ;; http://pages.sachachua.com/.emacs.d/Sacha.html
    (setq org-ditaa-jar-path (expand-file-name
                              "ditaa.jar"
                              (concat user-emacs-directory "software/")))))

(use-package ob-plantuml
  :straight nil
  :defer t
  :config
  (progn
    (setq org-plantuml-jar-path (expand-file-name
                                 "plantuml.jar"
                                 (concat user-emacs-directory "software/")))

    (defun cpm/advice-org-babel-execute:plantuml (orig-fun &rest args)
      "Force `shell-file-name' to be bash as the \">\" operator is used for redirection.

If this forcing is not done, and if `shell-file-name' is tcsh,
\">\" does not work.  When trying to overwrite files, we get a
\"File exists\" error, and \">!\" would need to be used instead.

Instead it's simpler to use bash."
      (let ((shell-file-name (executable-find "bash")))
        (apply orig-fun args)))
    (advice-add 'org-babel-execute:plantuml :around #'cpm/advice-org-babel-execute:plantuml)))

(use-package ob-shell
  :straight nil
  :defer t
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh
   org-babel-execute:bash
   org-babel-expand-body:bash))

(use-package ob-lisp
  :straight nil
  :defer t
  :commands (org-babel-execute:lisp))

(use-package ob-latex
  :straight nil
  :defer t
  :commands
  (org-babel-execute:latex))

;;; Org Babel Tangle
(use-package ob-tangle
  :straight nil
  :defer t
  :config
  (progn
    ;; Trailing whitespace management
    ;; Delete trailing whitespace in tangled buffer and save it.
    (add-hook 'org-babel-post-tangle-hook #'delete-trailing-whitespace)
    (add-hook 'org-babel-post-tangle-hook #'save-buffer :append)))

;;; Org Bullets
;; Replace org-bullets since it is no longer maintained
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("●" "◉" "⁂" "❖" "🟊" "🞷" "🞻" "✸"))
  (setq org-superstar-prettify-item-bullets t)
  ;; see https://unicode-table.com/en/25C9/ for ideas
  (setq org-superstar-item-bullet-alist
        '((?* . ?•)
          (?+ . ?•)
          (?- . ?•))))

;; (setq org-superstar-item-bullet-alist
;;       '((?* . ?○)
;;         (?+ . ?◉)
;;         (?- . ?●))))

;; Demote sequence for list bullets
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
;; Increase sub-item indentation
(setq org-list-indent-offset 1)

;;; Org Prettify Source Blocks
;; See discussion in https://www.reddit.com/r/emacs/comments/o04it0/share_your_prettifysymbolsalist/
(defun cpm/org-icons ()
  (interactive)
  "Beautify org mode keywords."
  (setq prettify-symbols-alist
        (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                '(("#+header:" . "☰")
                  ("#+begin_src" . "╦")
                  ("#+end_src"   . "╩")
                  ("#+begin_comment" . "✎")
                  ("#+end_comment" . "✎")
                  ("#+begin_notes" . "➤")
                  ("#+end_notes" . "➤")
                  ("#+begin_quote" . "»")
                  ("#+end_quote" . "«")
                  ("[ ]" . "")
                  ("[X]" . "")
                  ("[-]" . "")
                  (":PROPERTIES:" . "")
                  (":END:" . "―")
                  ("#+STARTUP:" . "")
                  ("#+ROAM_TAGS:" . "")
                  ("#+FILETAGS:" . "")
                  ("SCHEDULED:" . "")
                  ("DEADLINE:" . "")
                  (":logbook:" . ""))))
  (prettify-symbols-mode 1))
(add-hook 'org-mode-hook #'cpm/org-icons)


;;; Org Show Markup/Pretty Entities
;; show markup at point -- this should be part of org!
(use-package org-appear
  :straight (:type git :host github :repo "awth13/org-appear")
  :commands (org-appear-mode)
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-appear-autoemphasis  t)
  (setq org-appear-autolinks nil)
  (setq org-appear-autosubmarkers t))

;;; Org-Reveal
(use-package ox-reveal
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



;;; Org GTD
;;;; GTD Project Functions
(defun cpm/org-goto-todo ()
  (interactive)
  (find-file (concat org-directory "todo.org"))
  (widen)
  (goto-char (point-min)))

(defun cpm/org-goto-inbox ()
  (interactive)
  (find-file (concat org-directory "inbox.org"))
  (widen)
  (goto-char (point-min))
  (beginning-of-line))

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

;;; Org Rifle
;; Search [[https://github.com/alphapapa/helm-org-rifle][rapidly]] through org files using helm
(use-package helm-org-rifle
  :commands (helm-org-rifle helm-org-rifle-agenda-files helm-org-rifle-org-directory)
  :config
  ;; fix helm taking over various functions after being activated
  (add-hook 'helm-org-rifle-after-command-hook (lambda () (helm-mode -1))))

;;; Org-Download
;; Drag and drop images to Emacs org-mode. Courtesy of [[https://github.com/abo-abo/org-download][abo-abo]].
(use-package org-download
  :commands (org-download-yank org-download-screenshot org-download-image)
  :config
  (setq org-download-method 'directory
        org-download-image-dir (concat org-directory "org-pictures/")
        org-download-image-latex-width 500
        org-download-timestamp "%Y-%m-%d"))

;;; Org Pomodoro
;; Helps with time tracking
(use-package org-pomodoro
  :commands org-pomodoro
  :init
  (progn
    (setq org-pomodoro-audio-player "/usr/bin/afplay")))

;;; Org Export
;; Some useful settings
;;;; Backends
(setq org-export-backends '(ascii beamer html icalendar latex odt pandoc hugo md))
;; export odt to docx
(setq org-odt-preferred-output-format 'docx)

;;;; Ox-Pandoc
(use-package ox-pandoc
  :straight (:type git :host github :repo "mclear-tools/ox-pandoc")
  :after ox
  :config
  ;; default options for all output formats
  (setq org-pandoc-command (expand-file-name "/usr/local/bin/pandoc"))
  (setq org-pandoc-options '((standalone . t)))
  ;; cancel above settings only for 'docx' format
  (setq org-pandoc-options-for-docx '((standalone . nil)))
  ;; special settings for beamer-pdf and latex-pdf exporters
  (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
  (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))
  (setq org-pandoc-format-extensions '(org+smart)))

;;;;; Export Menu Options
(defcustom org-pandoc-menu-entry
  '(
    ;;(?0 "to jats." org-pandoc-export-to-jats)
    ;;(?0 "to jats and open." org-pandoc-export-to-jats-and-open)
    ;;(?  "as jats." org-pandoc-export-as-jats)
    ;;(?1 "to epub2 and open." org-pandoc-export-to-epub2-and-open)
    ;;(?! "to epub2." org-pandoc-export-to-epub2)
    ;;(?2 "to tei." org-pandoc-export-to-tei)
    ;;(?2 "to tei and open." org-pandoc-export-to-tei-and-open)
    ;;(?" "as tei." org-pandoc-export-as-tei)
    ;;(?3 "to markdown_mmd." org-pandoc-export-to-markdown_mmd)
    ;;(?3 "to markdown_mmd and open." org-pandoc-export-to-markdown_mmd-and-open)
    ;;(?# "as markdown_mmd." org-pandoc-export-as-markdown_mmd)
    ;;(?4 "to html5." org-pandoc-export-to-html5)
    (?4 "to html5 and open." org-pandoc-export-to-html5-and-open)
    (?$ "as html5." org-pandoc-export-as-html5)
    (?5 "to html5-pdf and open." org-pandoc-export-to-html5-pdf-and-open)
    (?% "to html5-pdf." org-pandoc-export-to-html5-pdf)
    ;;(?6 "to markdown_phpextra." org-pandoc-export-to-markdown_phpextra)
    ;;(?6 "to markdown_phpextra and open." org-pandoc-export-to-markdown_phpextra-and-open)
    ;;(?& "as markdown_phpextra." org-pandoc-export-as-markdown_phpextra)
    ;;(?7 "to markdown_strict." org-pandoc-export-to-markdown_strict)
    ;;(?7 "to markdown_strict and open." org-pandoc-export-to-markdown_strict-and-open)
    ;;(?' "as markdown_strict." org-pandoc-export-as-markdown_strict)
    ;; (?8 "to opendocument." org-pandoc-export-to-opendocument)
    ;; (?8 "to opendocument and open." org-pandoc-export-to-opendocument-and-open)
    ;; (?( "as opendocument." org-pandoc-export-as-opendocument)
    (?8 "to opml." org-pandoc-export-to-opml)
    (?9 "to opml and open." org-pandoc-export-to-opml-and-open)
    ;; (?* "as opml." org-pandoc-export-as-opml)
    ;;(?: "to rst." org-pandoc-export-to-rst)
    ;;(?: "to rst and open." org-pandoc-export-to-rst-and-open)
    ;;(?* "as rst." org-pandoc-export-as-rst)
    ;;(?< "to slideous." org-pandoc-export-to-slideous)
    (?< "to slideous and open." org-pandoc-export-to-slideous-and-open)
    (?, "as slideous." org-pandoc-export-as-slideous)
    (?= "to ms-pdf and open." org-pandoc-export-to-ms-pdf-and-open)
    (?- "to ms-pdf." org-pandoc-export-to-ms-pdf)
    ;;(?> "to textile." org-pandoc-export-to-textile)
    ;;(?> "to textile and open." org-pandoc-export-to-textile-and-open)
    ;;(?. "as textile." org-pandoc-export-as-textile)
    ;;(?a "to asciidoc." org-pandoc-export-to-asciidoc)
    ;;(?a "to asciidoc and open." org-pandoc-export-to-asciidoc-and-open)
    ;;(?A "as asciidoc." org-pandoc-export-as-asciidoc)
    (?b "to beamer-pdf and open." org-pandoc-export-to-beamer-pdf-and-open)
    (?B "to beamer-pdf." org-pandoc-export-to-beamer-pdf)
    (?c "to context-pdf and open." org-pandoc-export-to-context-pdf-and-open)
    (?C "to context-pdf." org-pandoc-export-to-context-pdf)
    ;;(?d "to docbook5." org-pandoc-export-to-docbook5)
    (?d "to docbook5 and open." org-pandoc-export-to-docbook5-and-open)
    (?D "as docbook5." org-pandoc-export-as-docbook5)
    (?e "to epub3 and open." org-pandoc-export-to-epub3-and-open)
    (?E "to epub3." org-pandoc-export-to-epub3)
    ;;(?f "to fb2." org-pandoc-export-to-fb2)
    ;;(?f "to fb2 and open." org-pandoc-export-to-fb2-and-open)
    ;;(?F "as fb2." org-pandoc-export-as-fb2)
    ;;(?g "to gfm." org-pandoc-export-to-gfm)
    (?g "to gfm and open." org-pandoc-export-to-gfm-and-open)
    (?G "as gfm." org-pandoc-export-as-gfm)
    ;;(?h "to html4." org-pandoc-export-to-html4)
    (?h "to html4 and open." org-pandoc-export-to-html4-and-open)
    (?H "as html4." org-pandoc-export-as-html4)
    ;;(?i "to icml." org-pandoc-export-to-icml)
    (?i "to icml and open." org-pandoc-export-to-icml-and-open)
    (?I "as icml." org-pandoc-export-as-icml)
    ;;(?j "to json." org-pandoc-export-to-json)
    (?j "to json and open." org-pandoc-export-to-json-and-open)
    (?J "as json." org-pandoc-export-as-json)
    ;; (?k "to markdown." org-pandoc-export-to-markdown)
    (?k "to markdown and open." org-pandoc-export-to-markdown-and-open)
    (?K "as markdown." org-pandoc-export-as-markdown)
    (?l "to latex-pdf and open." org-pandoc-export-to-latex-pdf-and-open)
    (?L "to latex-pdf." org-pandoc-export-to-latex-pdf)
    ;;(?m "to man." org-pandoc-export-to-man)
    (?m "to man and open." org-pandoc-export-to-man-and-open)
    (?M "as man." org-pandoc-export-as-man)
    ;;(?n "to native." org-pandoc-export-to-native)
    (?n "to native and open." org-pandoc-export-to-native-and-open)
    (?N "as native." org-pandoc-export-as-native)
    (?o "to odt and open." org-pandoc-export-to-odt-and-open)
    (?O "to odt." org-pandoc-export-to-odt)
    (?p "to pptx and open." org-pandoc-export-to-pptx-and-open)
    (?P "to pptx." org-pandoc-export-to-pptx)
    ;;(?q "to commonmark." org-pandoc-export-to-commonmark)
    ;;(?q "to commonmark and open." org-pandoc-export-to-commonmark-and-open)
    ;;(?Q "as commonmark." org-pandoc-export-as-commonmark)
    ;;(?r "to rtf." org-pandoc-export-to-rtf)
    (?r "to rtf and open." org-pandoc-export-to-rtf-and-open)
    (?R "as rtf." org-pandoc-export-as-rtf)
    ;;(?s "to s5." org-pandoc-export-to-s5)
    ;;(?s "to s5 and open." org-pandoc-export-to-s5-and-open)
    ;;(?S "as s5." org-pandoc-export-as-s5)
    ;;(?t "to texinfo." org-pandoc-export-to-texinfo)
    ;;(?t "to texinfo and open." org-pandoc-export-to-texinfo-and-open)
    ;;(?T "as texinfo." org-pandoc-export-as-texinfo)
    ;;(?u "to dokuwiki." org-pandoc-export-to-dokuwiki)
    (?u "to dokuwiki and open." org-pandoc-export-to-dokuwiki-and-open)
    (?U "as dokuwiki." org-pandoc-export-as-dokuwiki)
    ;; (?v "to revealjs." org-pandoc-export-to-revealjs)
    (?v "to revealjs and open." org-pandoc-export-to-revealjs-and-open)
    (?V "as revealjs." org-pandoc-export-as-revealjs)
    ;;(?w "to mediawiki." org-pandoc-export-to-mediawiki)
    (?w "to mediawiki and open." org-pandoc-export-to-mediawiki-and-open)
    (?W "as mediawiki." org-pandoc-export-as-mediawiki)
    (?x "to docx and open." org-pandoc-export-to-docx-and-open)
    (?X "to docx." org-pandoc-export-to-docx)
    ;;(?y "to slidy." org-pandoc-export-to-slidy)
    (?y "to slidy and open." org-pandoc-export-to-slidy-and-open)
    (?Y "as slidy." org-pandoc-export-as-slidy)
    ;;(?z "to dzslides." org-pandoc-export-to-dzslides)
    (?z "to dzslides and open." org-pandoc-export-to-dzslides-and-open)
    (?Z "as dzslides." org-pandoc-export-as-dzslides)
    ;;(?{ "to muse." org-pandoc-export-to-muse)
    ;;(?{ "to muse and open." org-pandoc-export-to-muse-and-open)
    ;;(?[ "as muse." org-pandoc-export-as-muse)
    ;;(?} "to zimwiki." org-pandoc-export-to-zimwiki)
    ;;(?} "to zimwiki and open." org-pandoc-export-to-zimwiki-and-open)
    ;;(?] "as zimwiki." org-pandoc-export-as-zimwiki)
    ;;(?~ "to haddock." org-pandoc-export-to-haddock)
    ;;(?~ "to haddock and open." org-pandoc-export-to-haddock-and-open)
    ;;(?^ "as haddock." org-pandoc-export-as-haddock)
    )
  "Pandoc menu-entry."
  :group 'org-pandoc
  :type 'list)

;;;; Ox-Hugo
;; [[https://github.com/kaushalmodi/ox-hugo][Export]] to Hugo with Org
(use-package ox-hugo :after ox)
;; (use-package ox-hugo-auto-export :after ox-hugo)

;;;; Batch Export Files with Org-Hugo
;; mark files and then batch export them with this command
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-+")
    (lambda()
      (interactive)
      (diredp-do-apply/eval 'org-hugo-export-wim-to-md '(4)))))




;;;; Export Top Level Trees to File
;; From a useful [[https://emacs.stackexchange.com/questions/27226/how-to-export-top-level-trees-in-an-org-file-to-corresponding-files][stack exchange]] post
(defun cpm/org-map-entries (org-file in-tags func)
  (let ((tags (if (stringp in-tags)
                  (list in-tags)
                in-tags)))

    (with-temp-buffer
      (org-mode)
      (insert-file-contents org-file-main)

      ;; Execute func at each heading that matches tags.
      (while (< (point) (point-max))

        ;; If find a heading...
        (and (search-forward-regexp "^\* " nil "end")

             ;; ...that matches the given tags...
             (seq-reduce
              (lambda(a b) (and a b))
              (mapcar
               (lambda (tag)
                 (beginning-of-line)
                 (search-forward-regexp
                  (concat ":" tag ":") (line-end-position) "end"))
               tags)
              t)

             ;; ... then execute given function with cursor at beginning of
             ;; heading.
             (progn
               (beginning-of-line)
               (save-excursion
                 (funcall func))
               (end-of-line)))))))

;;;; Slide Notes
;; Allow reveal.js notes to work in beamer
;; See https://joonro.github.io/Org-Coursepack/Lectures/04%20Creating%20Content%20for%20Slides%20and%20Handouts.html
(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t) string) t))

(defun my/process-NOTES-blocks (text backend info)
  "Filter NOTES special blocks in export."
  (cond
   ((eq backend 'latex)
    (if (string/starts-with text "\\begin{NOTES}") ""))
   ((eq backend 'rst)
    (if (string/starts-with text ".. NOTES::") ""))
   ((eq backend 'html)
    (if (string/starts-with text "<div class=\"NOTES\">") ""))
   ((eq backend 'beamer)
    (let ((text (replace-regexp-in-string "\\\\begin{NOTES}" "\\\\note{" text)))
      (replace-regexp-in-string "\\\\end{NOTES}" "}" text)))
   ))

(eval-after-load 'ox '(add-to-list
                       'org-export-filter-special-block-functions
                       'my/process-NOTES-blocks))
;;;; Beamer Options
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("beamer-handout"
                 "\\documentclass[handout]{beamer}
                    [NO-DEFAULT-PACKAGES]
                    [EXTRA]
                    \\setbeameroption{hidenotes}
                    [PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("beamer-presentation"
                 "\\documentclass[presentation]{beamer}
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 \\usepackage{pgfpages}
                 [EXTRA]
                 \\setbeameroption{show notes on second screen=right}
                 \\setbeamertemplate{note page}{\\pagecolor{yellow!5}\\insertnote}
                 "
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))


;;; Org Roam (Wiki & Notes)
;; Good notes package but a lot is still in flux
;; see https://org-roam.readthedocs.io/en/latest/
;; (eval-when-compile
;;   (quelpa
;;    '(org-roam :fetcher github :repo "org-roam/org-roam")))

;;;; Org Roam
(use-package org-roam
  :commands (org-roam org-roam-new-file org-roam-find-file)
  :after org
  ;; :hook (org-mode . org-roam-mode)
  :init
  (setq org-roam-directory "~/Dropbox/Work/projects/notebook/content-org/")
  (setq org-roam-db-location "~/Dropbox/Work/projects/notebook/content-org/org-roam.db")
  ;;;; Org Roam Keybindings
  :general
  (:states '(normal motion insert)
   (cpm/leader-keys
     "z"    #'(:ignore t :which-key "Zettelkasten")
     "z l"  #'org-roam
     "z t"  #'org-roam-today
     "z f"  #'org-roam-find-file
     "z i"  #'org-roam-insert
     "z g"  #'org-roam-show-graph
     "z n"  #'org-roam-new-file
     "z N"  #'org-roam--new-file-named))
  :config
  ;; use org-id links
  (setq org-roam-prefer-id-links t)
  ;;;; Org Roam Formatting
  (setq org-roam-date-filename-format "%Y-%m%d-%H%M")
  (setq org-roam-date-title-format "%Y-%m%d-%H%M")

  ;; fix org roam title conversion
  (defun org-roam--title-to-slug (title)
    "Convert TITLE to a filename-suitable slug."
    (cl-flet* ((nonspacing-mark-p (char)
                                  (eq 'Mn (get-char-code-property char 'general-category)))
               (strip-nonspacing-marks (s)
                                       (apply #'string (seq-remove #'nonspacing-mark-p
                                                                   (ucs-normalize-NFD-string s))))
               (replace (title pair)
                        (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-")  ;; convert anything not alphanumeric
                      ("__*" . "-")  ;; remove sequential underscores
                      ("^_" . "")  ;; remove starting underscore
                      ("_$" . "")))  ;; remove ending underscore
             (slug (-reduce-from #'replace (strip-nonspacing-marks title) pairs)))
        (s-downcase slug))))


  ;;;; Org Roam backlink settings for export
  ;; see https://org-roam.readthedocs.io/en/latest/org_export/
  (defun my/org-roam--backlinks-list (file)
    (if (org-roam--org-roam-file-p file)
        (--reduce-from
         (concat acc (format "- [[file:%s][%s]]\n"
                             (file-relative-name (car it) org-roam-directory)
                             (org-roam--get-title-or-slug (car it))))
         "" (org-roam-sql [:select [from] :from links :where (= to $s1) :and (= from $s2)] file "roam"))
      ""))

  (defun my/org-export-preprocessor (backend)
    (let ((links (my/org-roam--backlinks-list (buffer-file-name))))
      (unless (string= links "")
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n* Backlinks\n") links)))))

  (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)

   ;;;; Org Roam Templating
  ;; see https://org-roam.readthedocs.io/en/latest/templating/
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam-capture--get-point)
           :file-name "%<%Y-%m%d-%H%M>-${slug}"
           :head "#+SETUPFILE:./hugo_setup.org\n#+HUGO_SECTION: zettel\n#+HUGO_SLUG: ${slug}\n#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>"
           :unnarrowed t
           :immediate-finish t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>"
           :unnarrowed t)))
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+SETUPFILE:./hugo_setup.org\n#+HUGO_SECTION: Weblinks\n#+ROAM_KEY: ${ref}\n #+HUGO_SLUG: ${slug}\n#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>\n\n- source :: ${ref}"
           :unnarrowed t))))

;;;; Company Org Roam
(use-package company-org-roam
  :after (org company)
  :demand t
  :config
  (push 'company-org-roam company-backends))

;;;; Org Roam Server
(use-package org-roam-server
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

;;;; Org Roam Bibtex
;; If you installed via MELPA
(use-package org-roam-bibtex
  :straight (:host github :repo "org-roam/org-roam-bibtex")
  :after org-roam
  :requires ivy-bibtex
  :demand t
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
         ("s-b" . orb-note-actions))
  :config
  (setq orb-insert-interface 'ivy-bibtex)
  (setq orb-templates
        '(("b" "bib" plain (function org-roam-capture--get-point) ""
           :file-name "${citekey}"
           :head "#+TITLE: ${author-or-editor-abbrev} (${year}): ${title}\n#+ROAM_KEY: cite:${=citekey=}\n#+SETUPFILE: ./hugo_setup.org\n#+HUGO_SECTION: reading-notes\n\n- Tags :: \n- Bookends link :: bookends://sonnysoftware.com/${beref}\n- PDF :: [[${file}][PDF Link]]\n\n#+begin_src bibtex\n (insert (org-ref-get-bibtex-entry \"${=key=}\"))\n#+end_src" ; <--
           :unnarrowed t))))


;;; Citeproc for Org
(use-package citeproc-org
  :straight (:host github :repo "andras-simonyi/citeproc-org")
  :after org
  :demand t
  :config
  (citeproc-org-setup))

;;; Org Miscellaneous Packages

(use-package htmlize :commands (htmlize-buffer))

(use-package org-inlinetask :straight nil :commands org-inlinetask-insert-task)

;; ignore export of headlines marked with :ignore: tag
(use-package ox-extra
  ;; :straight (org-plus-contrib)
  :straight nil
  :after ox
  :demand t
  :config
  (ox-extras-activate '(ignore-headlines)))

;; Devonthink integration
(use-package org-devonthink
  :straight nil
  :load-path "~/.emacs.d/.local/elisp/org-devonthink"
  :commands (org-insert-dtp-link org-dtp-store-link))


;;; Org Outlook
;; Open outlook message links in org
;; from https://superuser.com/a/100084 and
;; https://emacs.stackexchange.com/a/35916/11934

(defun org-outlook-open (id)
  "Open the Outlook item identified by ID.  ID should be an Outlook GUID."
  (shell-command-to-string (concat "open" id)))

(with-eval-after-load 'org
  (org-add-link-type "outlook" 'org-outlook-open)

  (org-link-set-parameters
   "outlook"
   :follow (lambda (path) (org-outlook-open path))
   :export (lambda (path desc backend)
             (cond
              ((eq 'html backend)
               (format "<a href=\"outlook:%s\">%s</a>" path desc))))))

;;; Org Tree Slides
(use-package org-tree-slide
  :straight t
  :general
  (:states '(normal motion)
   :keymaps 'org-tree-slide-mode-map
   "C-j" 'org-tree-slide-move-next-tree
   "C-k" 'org-tree-slide-move-previous-tree
   "C-s C-c" 'org-tree-slide-content)
  :config
  (setq org-tree-slide-activate-message "Presentation mode ON")
  (setq org-tree-slide-deactivate-message "Presentation mode OFF")
  (setq org-tree-slide-breadcrumbs "    >    ")
  (setq org-tree-slide-content-margin-top 4)
  (setq org-tree-slide-skip-outline-level 4)
  (org-tree-slide-narrowing-control-profile)
  (setq org-tree-slide-skip-done nil)
  (setq org-tree-slide-modeline-display nil))
;;; Org Autolist (Smart Lists)
;; Better list behavior
(use-package org-auto-list
  :straight (:type git :host github :repo "calvinwyoung/org-autolist")
  :hook (org-mode . org-autolist-mode))

;;; Provide Org Extensions
(provide 'setup-org-extensions)
