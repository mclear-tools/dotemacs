;; Org Mode

;;; New Org
;; remove references to older org in path
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))

;; Org package settings -- use org-plus-contrib to get latest org
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :config
;;; Org Settings
;;;; Org Directories
(setq org-directory "~/Dropbox/org-files/")
(setq org-default-notes-file (concat org-directory "inbox.org"))
;;;; Org Config Settings
(setq org-stuck-projects (quote ("" nil nil "")))
(setq org-image-actual-width  500) ;; show all images at 500px using imagemagik
(setq-default org-footnote-section nil ;; place footnotes locally rather than in own section
              org-return-follows-link t ;; make RET follow links
              org-list-allow-alphabetical t ;; allow alphabetical list
              org-hide-emphasis-markers t  ;; hide markers
              org-pretty-entities t ;; make latex look good
              org-pretty-entities-include-sub-superscripts t
              org-hide-leading-stars t
              org-export-with-smart-quotes t ;; export smart quote marks
              org-refile-use-cache t  ;; use cache for org refile
              org-startup-folded t
              org-yank-adjusted-subtrees t  ;; adjust subtrees to depth when yanked
              org-yank-folded-subtrees t  ;; fold subtrees on yank
              org-M-RET-may-split-line '((default . nil))  ;; don't split line when creating a new headline, list item, or table field
              org-fontify-quote-and-verse-blocks t ;; make quotes stand out
              org-table-export-default-format "orgtbl-to-csv" ;; export for org-tables to csv
              ;; org-ellipsis "↷" ;; nicer elipses "↴" "▼"
              org-cycle-separator-lines 0 ;; Give a more compact and consistent view
              org-startup-indented t ;; start in indent mode
              ;; prevent editing invisible area, and show an error message in echo area instead;
              ;; additionally expand text and move focus to the expected point.
              org-catch-invisible-edits 'show-and-error
              org-imenu-depth 8
imenu-auto-rescan t)
   (add-hook 'auto-save-hook 'org-save-all-org-buffers)

;;;; Org Modules
(setq org-modules (quote (org-info org-tempo org-protocol org-habit org-mac-link)))

;;;; Org ID
(setq org-id-locations-file (concat cpm-cache-dir ".org-id-locations"))

;;;; Org State Settings
(setq org-todo-keywords
 '((sequence "TODO(t)" "DOING(g)" "NEXT(n)" "|" "DONE(d)")
   (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)")))
;;;; Org Priority Settings
(setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
                           (?B . (:foreground "orange"))
                           (?C . (:foreground "DarkGoldenrod2"))
                           (?D . (:forefround "green"))))
;;;; Org Logging
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)
;; Don't log the time a task was rescheduled or redeadlined.
(setq org-log-redeadline nil)
(setq org-log-reschedule nil)

;; Prefer rescheduling to future dates and times:
(setq org-read-date-prefer-future 'time)

;;;; Org Tags
(setq org-tag-alist '((:startgrouptag)
                      ("@computer" . ?c)
                      (:grouptags)
                      ("emacs" . ?m)
                      (:endgrouptag)
                      ("@errand" . ?e)
                      ("@phone" . ?p)
                      ("@unl" . ?s)
                      ("email")
                      ("postal-mail")
                      ("@home" . ?h)))
(setq org-fast-tag-selection-single-key nil)

;;;; Org Entities
(setq org-entities-user
      '(("nec" "\Box" nil "◻" "" "" "◻")
        ("pos" "\Diamond" nil "◇" "" "" "◇")))
  (add-hook 'org-mode-hook
      (lambda ()
      (centered-cursor-mode)
        (turn-on-auto-fill)
        ))

;;;; Org Regex (Emphasis)
(with-eval-after-load 'org
; chars for prematch
(setcar org-emphasis-regexp-components            "     ('\"{“”\[\\\_\-")
; chars for postmatch
(setcar (nthcdr 1 org-emphasis-regexp-components) "\] -   .,!?;:''“”\")}/\\“”\_\-")
; forbidden chars
(setcar (nthcdr 2 org-emphasis-regexp-components) "    \t\r\n,\"")
; body
(setcar (nthcdr 3 org-emphasis-regexp-components) ".")
; max newlines
(setcar (nthcdr 4 org-emphasis-regexp-components) 1)
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))

;;;; Org Template Expansions
(setq new-structure-template-alist
      '(("el" . "src emacs-lisp")
        ("t" . "COMMENT TODO:")
        ("b" . "REVEAL: split")
        ("f" . "ATTR_REVEAL: :frag (appear)")))
(dolist (ele new-structure-template-alist)
    (add-to-list 'org-structure-template-alist ele))

;;; Org Archive
;; Tell org where to archive completed tasks
(setq org-archive-location (concat org-directory "/org-archive/archived.org::datetree/"))
;; Also tell org how to archive all the done tasks (DONE or CANCELED) in a file.
;; From [[https://changelog.complete.org/archives/9877-emacs-3-more-on-org-mode][here]] based on a stack overflow [[https://stackoverflow.com/a/27043756][answer]]
(defun cpm/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELED" 'file))

;;; Org Refile
;; Set refile settings.  I got a lot of help on this from [[https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html][Aaron Bieber's discussion]].

;; With this set, you can trigger Refile with C-c C-w in any Org file and
;; get a completing read of all headings up to three levels deep in all
;; files in =org-agenda-files=. You can also refile to the top header in a
;; document and create new parents.
(setq org-refile-targets '((org-agenda-files :maxlevel . 8)
                           ("/Users/roambot/.emacs.d/config.org" :maxlevel . 8)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;;; Open Files in Default Application
;;Open files in their default applications (ms word being the prime example)
(setq org-file-apps
      '(("\\.docx\\'" . default)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . emacs)
        (auto-mode . emacs)))
(setq org-agenda-files '("~/Dropbox/org-files/"))
(general-define-key "C-c a" #'org-agenda)

;;; End Use-Package Config
;; end use-package config settings
)
;;; Org Babel
;; Avoid `org-babel-do-load-languages' since it does an eager require.
(use-package ob-python
  :defer t
  :ensure org-plus-contrib
  :commands (org-babel-execute:python))

(use-package ob-shell
  :defer t
  :ensure org-plus-contrib
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh

   org-babel-execute:bash
   org-babel-expand-body:bash))




;; org babel source block settings
(setq org-src-fontify-natively t
      org-src-window-setup 'other-window
      org-src-tab-acts-natively nil
      org-src-strip-leading-and-trailing-blank-lines t)
;;; Org Babel Tangle
(use-package ob-tangle
  :ensure nil
  :defer t
  :config
  (progn
    ;; Trailing whitespace management
    ;; Delete trailing whitespace in tangled buffer and save it.
    (add-hook 'org-babel-post-tangle-hook #'delete-trailing-whitespace)
    (add-hook 'org-babel-post-tangle-hook #'save-buffer :append)))

;;;; Diagrams
(use-package ob-ditaa
  :ensure nil
  :defer t
  :config
  (progn
    ;; http://pages.sachachua.com/.emacs.d/Sacha.html
    (setq org-ditaa-jar-path (expand-file-name
                              "ditaa.jar"
                              (concat user-emacs-directory "software/")))))

(use-package ob-plantuml
  :ensure nil
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

;;;; Python
(use-package ob-python
  :ensure nil
  :defer t
  :config
  (progn
    (setq org-babel-python-command "python3"))) ;Default to python 3.x


;; ;; Avoid `org-babel-do-load-languages' since it does an eager require.
;; (use-package ob-python
;;   :defer t
;;   :ensure org-plus-contrib
;;   :commands (org-babel-execute:python))

;; (use-package ob-shell
;;   :defer t
;;   :ensure org-plus-contrib
;;   :commands
;;   (org-babel-execute:sh
;;    org-babel-expand-body:sh

;;    org-babel-execute:bash
;;    org-babel-expand-body:bash))

;; ;; Avoid `org-babel-do-load-languages' since it does an eager require.
;; (use-package ob-lisp
;;   :defer t
;;   :ensure org-plus-contrib
;;   :commands (org-babel-execute:lisp))

;; (use-package ob-latex
;;   :defer t
;;   :ensure org-plus-contrib
;;   :commands
;;   (org-babel-execute:latex))



;;; Org Agenda
;; Settings for the [[http://orgmode.org/manual/Agenda-Views.html][agenda]].
;; sorting
'(org-agenda-sorting-strategy
  (quote
   ((agenda scheduled-up deadline-up)
    (todo scheduled-up deadline-up)
    (tags priority-down category-keep)
    (search category-keep))))

;; Display properties
(setq org-cycle-separator-lines 0
      org-tags-column 72
      org-agenda-tags-column org-tags-column
      org-agenda-window-setup 'only-window
      org-agenda-restore-windows-after-quit t
      org-agenda-todo-ignore-scheduled nil
      org-agenda-todo-ignore-deadlines nil
      org-agenda-block-separator ""
      ;; org-agenda-sticky t
      org-agenda-span 'day)

 (with-eval-after-load 'org-agenda
   (general-define-key :keymaps 'org-agenda-mode-map
                       "j" 'org-agenda-next-item
                       "k" 'org-agenda-previous-item))

;; automatically refresh the agenda after adding a task
(defun cpm/org-agenda-refresh ()
(interactive)
(when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
        (org-agenda-redo t)
        (message "[org agenda] refreshed!"))))
(add-hook 'org-capture-after-finalize-hook 'cpm/org-agenda-refresh)

;; show all todos
(defun cpm/jump-to-org-agenda-all-todos ()
  "open agenda with all unscheduled/non-deadline todos"
  (interactive)
  (org-agenda nil "z"))

;; jump to week agenda
(defun cpm/jump-to-week-agenda ()
  "open custom week agenda"
  (interactive)
  (org-agenda nil "W"))

;; from stack overflow https://stackoverflow.com/a/22900459/6277148
;; note that the formatting is nicer that just using '%b'
(setq org-agenda-prefix-format
 '((agenda . " %i %-12:c%?-12t% s")
  (timeline . "  % s")
  (todo .
   " %i %-12:c %(concat \"\"(org-format-outline-path (org-get-outline-path)) \" \->\") ")
  (tags .
   " %i %-12:c %(concat \"\"(org-format-outline-path (org-get-outline-path)) \" \->\") ")
  (search . " %i %-12:c")))

;;;; Agenda Navigation
;; Courtesy of [[https://blog.aaronbieber.com/2016/09/25/agenda-interactions-primer.html][Aaron Bieber]]
(defun air-org-agenda-next-header ()
  "Jump to the next header in an agenda series."
  (interactive)
  (air--org-agenda-goto-header))

(defun air-org-agenda-previous-header ()
  "Jump to the previous header in an agenda series."
  (interactive)
  (air--org-agenda-goto-header t))

(defun air--org-agenda-goto-header (&optional backwards)
  "Find the next agenda series header forwards or BACKWARDS."
  (let ((pos (save-excursion
               (goto-char (if backwards
                              (line-beginning-position)
                            (line-end-position)))
               (let* ((find-func (if backwards
                                     'previous-single-property-change
                                   'next-single-property-change))
                      (end-func (if backwards
                                    'max
                                  'min))
                      (all-pos-raw (list (funcall find-func (point) 'org-agenda-structural-header)
                                         (funcall find-func (point) 'org-agenda-date-header)
                                         (funcall find-func (point) 'org-super-agenda-header)))
                      (all-pos (cl-remove-if-not 'numberp all-pos-raw))
                      (prop-pos (if all-pos (apply end-func all-pos) nil)))
                 prop-pos))))
    (if pos (goto-char pos))
    (if backwards (goto-char (line-beginning-position)))))

(with-eval-after-load 'org-agenda
(general-define-key :keymaps 'org-agenda-mode-map :states '(normal motion)
  "J" 'air-org-agenda-next-header
  "K" 'air-org-agenda-previous-header))

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

;;;; Org Super-Agenda
;; [[https://github.com/alphapapa/org-super-agenda][Supercharge]] org-agenda. Settings courtesy of [[https://github.com/alphapapa/org-super-agenda/blob/master/examples.org#forward-looking][alphapapa]].

(use-package org-super-agenda
 ;; :pin manual ;; throws errors for some reason when I update
 :general
 (:states '(normal motion emacs) :keymaps 'org-agenda-keymap
 ","  'cpm/hydra-org-agenda/body)
 :after (org org-agenda)
 :config
 (org-super-agenda-mode)
 (setq org-super-agenda-groups
   '((:name "Overdue"
      :deadline past)
     (:name "Scheduled"
      :time-grid t)
     (:name "Today"
      :scheduled today
      :deadline nil)
     (:name "Due Today"
      :deadline today)
     (:name "Upcoming"
      :deadline future
      :scheduled future)
     (:name "Scheduled"
      :scheduled t)
      )))
(defun cpm/jump-to-org-super-agenda ()
(interactive)
(org-agenda nil "A"))

;;;; Hydra for Agenda
;; Hydra for org agenda (graciously offered by Spacemacs)
(after! org-agenda
(org-super-agenda-mode)
(defhydra cpm/hydra-org-agenda (:color pink :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo)))

;;;; Agenda Custom Commands
; https://orgmode.org/manual/Storing-searches.html#Storing-searches
(setq org-agenda-custom-commands
      '(("x" agenda)
        ("y" agenda*) ; or agenda entries planned this week/day with an hour specification like [h]h:mm
        ("z" todo "TODO")
        ("i" todo "INPROGRESS")
        ("n" todo "NEXT")
        ("r" todo "REVISE")
        ("s" "Stuck Projects" (
                               (tags-todo "-CANCELED/!"
                                          ((org-agenda-overriding-header "Stuck Projects")
                                           (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                                           (org-agenda-sorting-strategy
                                            '(category-keep))))))
        ("w" todo "WAITING")
        ("A" "Super Agenda" (
                             (agenda "" ((org-agenda-span 'day)))
                             (alltodo "" ((org-agenda-overriding-header "")
                             (org-super-agenda-groups
                               '((:name "Priority"
                                  :priority>= "C")
                                 (:name "Next to do"
                                  :todo "NEXT")
                                 (:name "In Progress"
                                  :todo "DOING")
                                 (:todo ("WAITING" "HOLD"))
                                 (:todo "MAYBE")
                                 (:name "Reading List"
                                  :todo "TOREAD")))))))
        ("W" "Week's agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-span 'week)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks nil)))))
;;; Org Capture
;;;; Capture Settings
(general-define-key "C-c c" 'org-capture)
(add-hook 'org-capture-mode-hook 'evil-insert-state)
(setq org-capture-templates
      '(("p" "Projects" entry (file "~/Dropbox/org-files/projects.org")
         "* %? \n  %i")
        ("c" "Capture" entry (file "~/Dropbox/org-files/inbox.org")
         "* TODO %?\n %i")
        ("j" "Journal" entry (file+olp+datetree "~/Dropbox/org-files/journal.org")
         "**** %<%H:%M>\n%?")
        ("l" "A link, for reading later" entry (file "~/Dropbox/org-files/links.org")
          "* %? :link: \n%(grab-mac-link 'safari 'org)")
        ("m" "Mail-Task" entry (file "~/Dropbox/org-files/inbox.org")
         "* TODO %:description                         :email: \n[[message://%l][Email link]] \n%? ")
        ("r" "Reference" entry (file "~/Dropbox/org-files/reference.org")
        "* %?")
        ("w" "Review: Weekly Review" entry (file+datetree "~/Dropbox/org-files/reviews.org")
        (file "~/Dropbox/org-files/templates/weekly_review_template.org"))))

  ;; Add date to captured items
  (defun add-property-with-date-captured ()
    "Add DATE_CAPTURED property to the current item."
    (interactive)
    (org-set-property "DATE_CAPTURED" (format-time-string "%F %A")))

  (add-hook 'org-capture-before-finalize-hook 'add-property-with-date-captured)

;;;; Org Journal Capture
;; Tell emacs what you're doing a few times a day. Depends on a
;; [[/Users/roambot/bin/scripts/emacs_journal.sh][shell script]] run in the
;; background. I got the idea from
;; [[http://www.diegoberrocal.com/blog/2015/08/19/org-protocol/][Diego Berrocal]].
;; Hat tip to
;; [[http://stackoverflow.com/questions/23517372/hook-or-advice-when-aborting-org-capture-before-template-selection][stack
;; overflow]] for help on hooks for the created frame.

(defun cpm/org-journal ()
  (interactive) (org-capture nil "j"))

(defun cpm/what-are-you-doing-capture ()
  (interactive)
  (make-frame '((name . "What are you doing?") (left . (+ 550)) (top . (+ 400)) (width . 100) (height . 24)))
  (select-frame-by-name "What are you doing?")
  (cpm/org-journal)
  (cpm/insert-weather))

;;;; Alfred Capture Workflow
;; Help alfred and org-capture play nice. Courtesy of [[http://orgmode.org/worg/org-contrib/alfred-org-capture.html][worg]] with some slight modifications.

(defun cpm/org-capture-link-frame ()
 "Capture link from frontmost safari tab"
  (interactive)
  (org-capture nil "l"))
(defun cpm/make-org-capture-link-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "alfred-capture") (width . 90) (height . 20)
                (top . 400) (left . 300)
                ))
  (select-frame-by-name "alfred-capture")
  (cpm/org-capture-link-frame))

(defun cpm/org-capture-frame ()
  (interactive)
  (org-capture nil "c"))
(defun cpm/make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "alfred-capture") (width . 90) (height . 20)
                (top . 400) (left . 300)
                ))
  (select-frame-by-name "alfred-capture")
  (cpm/org-capture-frame))

;;;; Capture Advice
;; Make capture the only window and close after refiling.
(defadvice org-capture
  (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (cond ((equal "What are you doing?" (frame-parameter nil 'name)) (delete-other-windows))
        ((equal "alfred-capture" (frame-parameter nil 'name)) (delete-other-windows))
        ((equal "Email Capture" (frame-parameter nil 'name)) (delete-other-windows))))

 (defadvice org-capture-finalize
   (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (cond ((equal "What are you doing?" (frame-parameter nil 'name)) (delete-frame))
          ((equal "alfred-capture" (frame-parameter nil 'name)) (delete-frame))
          ((equal "Email Capture" (frame-parameter nil 'name)) (delete-frame))
          ))

;;; Org Bullets
(use-package org-bullets
 :hook (org-mode . org-bullets-mode)
 :config
 (setq org-bullets-bullet-list '("⚫")))

;; Asterisks and dashes for bullet lists are fine, but actual circular bullets are better
;; via http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
(font-lock-add-keywords 'org-mode
                      '(("^ +\\([-*]\\) "
                         (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Demote sequence for list bullets
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
;; Increase sub-item indentation
(setq org-list-indent-offset 1)


;; Other bullet patterns
 ;; (setq org-bullets-bullet-list '("◉" "⁑" "⁂" "❖" "✮" "✱" "" "✸")))
 ;; (setq org-bullets-bullet-list '("◉" "⚫")))

 ;; Other bullets
 ;; "●" "◉" "→"
 ;; ("◉" "◎" "⚫" "○" "►" "◇")
 ;;  "∙" "∶" "∵" "∷" "⸭" "∺" )))
 ;; (setq org-bullets-bullet-list '("❂" "⁑" "⁂" "❖" "✮" "✱" "✵")))

;;; Org Prettify Source Blocks
;; Make source blocks look better. Courtesy of
;; [[https://pank.eu/blog/pretty-babel-src-blocks.html][Rasmus Pank Roulund]]. Last
;; updated: 2018-04-06

(with-eval-after-load 'org
  (defvar-local rasmus/org-at-src-begin -1
    "Variable that holds whether last position was a ")

  (defvar rasmus/ob-header-symbol ?☰
    "Symbol used for babel headers")

  (defun rasmus/org-prettify-src--update ()
    (let ((case-fold-search t)
          (re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
          found)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (goto-char (match-end 0))
          (let ((args (org-trim
                       (buffer-substring-no-properties (point)
                                                       (line-end-position)))))
            (when (org-string-nw-p args)
              (let ((new-cell (cons args rasmus/ob-header-symbol)))
                (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
                (cl-pushnew new-cell found :test #'equal)))))
        (setq prettify-symbols-alist
              (cl-set-difference prettify-symbols-alist
                                 (cl-set-difference
                                  (cl-remove-if-not
                                   (lambda (elm)
                                     (eq (cdr elm) rasmus/ob-header-symbol))
                                   prettify-symbols-alist)
                                  found :test #'equal)))
        ;; Clean up old font-lock-keywords.
        (font-lock-remove-keywords nil prettify-symbols--keywords)
        (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
        (font-lock-add-keywords nil prettify-symbols--keywords)
        (while (re-search-forward re nil t)
          (font-lock-flush (line-beginning-position) (line-end-position))))))

  (defun rasmus/org-prettify-src ()
    "Hide src options via `prettify-symbols-mode'.

  `prettify-symbols-mode' is used because it has uncollpasing. It's
  may not be efficient."
    (let* ((case-fold-search t)
           (at-src-block (save-excursion
                           (beginning-of-line)
                           (looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
      ;; Test if we moved out of a block.
      (when (or (and rasmus/org-at-src-begin
                     (not at-src-block))
                ;; File was just opened.
                (eq rasmus/org-at-src-begin -1))
        (rasmus/org-prettify-src--update))
      ;; Remove composition if at line; doesn't work properly.
      ;; (when at-src-block
      ;;   (with-silent-modifications
      ;;     (remove-text-properties (match-end 0)
      ;;                             (1+ (line-end-position))
      ;;                             '(composition))))
      (setq rasmus/org-at-src-begin at-src-block)))

  (defun rasmus/org-prettify-symbols ()
    (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
          (cl-reduce 'append
                     (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                             `(("#+begin_src" . ?╦) ;; ➤ 🖝 ➟ ➤ ✎ ✎
                               ("#+end_src"   . ?╩) ;; □
                               ("#+header:" . ,rasmus/ob-header-symbol)
                               ("#+begin_comment" . ?✎)
                               ("#+end_comment" . ?✎)
                               ("#+begin_notes" . ?➤)
                               ("#+end_notes" . ?➤)
                               ("#+begin_quote" . ?»)
                               ("#+end_quote" . ?«)))))
    (turn-on-prettify-symbols-mode)
    (add-hook 'post-command-hook 'rasmus/org-prettify-src t t))
  (add-hook 'org-mode-hook #'rasmus/org-prettify-symbols))


;;; Org-Goto
;; Make counsel display org headings nicely.
(with-eval-after-load 'org
  (setq counsel-org-goto-display-style 'path)
  (setq counsel-org-goto-separator " ➜ ")
  (setq counsel-org-goto-face-style 'org)
  (define-key org-mode-map (kbd "C-c C-j") 'counsel-org-goto)
  (define-key org-mode-map (kbd "C-u C-c C-j") 'counsel-org-goto-all))

;;; Org-Reveal
(use-package ox-reveal
:commands (org-reveal-export-current-subtree org-reveal-export-to-html-and-browse)
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

;;; Org GTD
;;;; GTD Project Functions
(defun cpm/org-goto-todo ()
  (interactive)
  (find-file "~/Dropbox/org-files/todo.org")
  (widen)
  (beginning-of-buffer))

(defun cpm/org-goto-inbox ()
  (interactive)
  (find-file "~/Dropbox/org-files/inbox.org")
  (widen)
  (beginning-of-buffer)
  (beginning-of-line))

(defun cpm/goto-projects.org ()
  (interactive)
  (find-file "~/Dropbox/org-files/projects.org"))

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
(defun cpm/list-stuck-projects ()
  (interactive)
  (bh/skip-non-stuck-projects)
  (org-agenda nil "#" 'subtree))

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
                (unless (member "WAITING" (org-get-tags-at))
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
                (unless (member "WAITING" (org-get-tags-at))
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
             (member "WAITING" (org-get-tags-at)))
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
(defun cpm/go-to-areas ()
    (interactive)
    (find-file "~/Dropbox/org-files/todo.org")
    (widen)
    (beginning-of-buffer)
    (re-search-forward "* Areas")
    (beginning-of-line))

(defun cpm/areas-overview ()
    (interactive)
    (go-to-areas)
    (org-narrow-to-subtree)
    (org-columns))

;;;; Random Notes
;; FIXME: Need to broaden the list of candidates...
(use-package org-randomnote
  :commands (org-randomnote org-randomnote--go-to-random-header org-randomnote--get-random-file org-randomnote--get-random-subtree)
  :init
  (setq org-randomnote-candidates '("~/Dropbox/org-files/todo.org")))

;;; Org Functions
;;;; Org Fill Functions
;;  Functions to calculate apt offsets and call regular org fill stuff. There's a
;;  useful
;;  [[http://stackoverflow.com/questions/14351154/org-mode-outline-level-specific-fill-column-values][stack
;;  overflow thread]] on this.

(defun calc-offset-on-org-level ()
  "Calculate offset (in chars) on current level in org mode file."
  (* (or (org-current-level) 0) org-indent-indentation-per-level))

(defun my-org-fill-paragraph (&optional JUSTIFY)
  "Calculate apt fill-column value and fill paragraph."
  (let* ((fill-column (- fill-column (calc-offset-on-org-level))))
    (org-fill-paragraph JUSTIFY)))

(defun my-org-auto-fill-function ()
  "Calculate apt fill-column value and do auto-fill"
  (let* ((fill-column (- fill-column (calc-offset-on-org-level))))
    (org-auto-fill-function)))

(defun my-org-mode-hook ()
  (setq fill-paragraph-function   'my-org-fill-paragraph
        normal-auto-fill-function 'my-org-auto-fill-function))

;; (add-hook 'org-load-hook 'my-org-mode-hook)
;; (add-hook 'org-mode-hook 'my-org-mode-hook)

;;;; Narrow & Advance/Retreat
;; Functions to advance forwards or backwards through narrowed tree
(defun cpm/org-advance ()
  (interactive)
  (when (buffer-narrowed-p)
    (beginning-of-buffer)
    (widen)
    (org-forward-heading-same-level 1))
  (org-narrow-to-subtree))

(defun cpm/org-retreat ()
  (interactive)
  (when (buffer-narrowed-p)
    (beginning-of-buffer)
    (widen)
    (org-backward-heading-same-level 1))
  (org-narrow-to-subtree))

;;;; Goto Org Files
(defun cpm/goto-org-files ()
  "goto org-files directory"
  (interactive)
  (helm-find-files-1 org-directory))
(defun cpm/goto-inbox.org ()
  "goto org-inbox"
  (interactive)
  (find-file "~/Dropbox/org-files/inbox.org"))
(defun cpm/goto-todo.org ()
  "goto org-todo"
  (interactive)
  (find-file "~/Dropbox/org-files/todo.org"))
(defun cpm/goto-articles.org ()
  "goto org-articles"
  (interactive)
  (find-file "~/Dropbox/org-files/articles.org"))
(defun cpm/goto-classes.org ()
  "goto org-classes"
  (interactive)
  (find-file "~/Dropbox/org-files/teaching.org"))
(defun cpm/goto-reference.org ()
  "goto org reference notes"
  (interactive)
  (find-file "~/Dropbox/org-files/reference.org"))
(defun cpm/goto-someday.org ()
  "goto org-someday"
  (interactive)
  (find-file "~/Dropbox/org-files/someday.org"))
(defun cpm/goto-links.org ()
  "goto org-links"
  (interactive)
  (find-file "~/Dropbox/org-files/links.org"))
(defun cpm/goto-reading.org ()
  "goto reading list"
  (interactive)
  (find-file "~/Dropbox/org-files/reading.org"))


;;;; Export Headings as Seperate Files
;; export headlines to separate files
;; http://emacs.stackexchange.com/questions/2259/how-to-export-top-level-headings-of-org-mode-buffer-to-separate-files
(defun cpm/org-export-headlines-to-pdf ()
  "Export all subtrees that are *not* tagged with :noexport: to
separate files.

Subtrees that do not have the :EXPORT_FILE_NAME: property set
are exported to a filename derived from the headline text."
  (interactive)
  (save-buffer)
  (let ((modifiedp (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (goto-char (re-search-forward "^*"))
      (set-mark (line-beginning-position))
      (goto-char (point-max))
      (org-map-entries
       (lambda ()
         (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME")))
           (unless export-file
             (org-set-property
              "EXPORT_FILE_NAME"
              (replace-regexp-in-string " " "_" (nth 4 (org-heading-components)))))
           (deactivate-mark)
           (org-pandoc-export-to-latex-pdf nil t)
           (unless export-file (org-delete-property "EXPORT_FILE_NAME"))
           (set-buffer-modified-p modifiedp)))
       "-noexport" 'region-start-level))))

;;;; Org demote/promote region
(defun endless/demote-everything (number beg end)
  "Add a NUMBER of * to all headlines between BEG and END.
Interactively, NUMBER is the prefix argument and BEG and END are
the region boundaries."
  (interactive "p\nr")
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (narrow-to-region beg end)
        (goto-char (point-min))
        (let ((string (make-string number ?*)))
          (while (search-forward-regexp "^\\*" nil t)
            (insert string)))))))

;;;; Org Hide Property Drawers
;; From [[https://www.reddit.com/r/emacs/comments/9htd0r/how_to_completely_hide_the_properties_drawer_in/e6fehiw][Reddit]]

(defun org-toggle-properties ()
  ;; toggle visibility of properties in current header if it exists
  (save-excursion
    (when (not (org-at-heading-p))
      (org-previous-visible-heading 1))
    (when (org-header-property-p)
      (let* ((a (re-search-forward "\n\\:" nil t)))
        (if (outline-invisible-p (point))
            (outline-show-entry)
          (org-cycle-hide-drawers 'all))))))

;;;; Org Return DWIM
(defun unpackaged/org-element-descendant-of (type element)
  "Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'."
  (when-let* ((parent (org-element-property :parent element)))
    (or (eq type (car parent))
        (unpackaged/org-element-descendant-of type parent))))

;;;###autoload
(defun unpackaged/org-return-dwim (&optional default)
  "A helpful replacement for `org-return'.  With prefix, call `org-return'.

On headings, move point to position after entry content.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table."
  ;; Inspired by John Kitchin: http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
  (interactive "P")
  (if default
      (org-return)
    (cond
     ;; Act depending on context around point.

     ;; NOTE: I prefer RET to not follow links, but by uncommenting this block, links will be
     ;; followed.

     ;; ((eq 'link (car (org-element-context)))
     ;;  ;; Link: Open it.
     ;;  (org-open-at-point-global))

     ((org-at-heading-p)
      ;; Heading: Move to position after entry content.
      ;; NOTE: This is probably the most interesting feature of this function.
      (let ((heading-start (org-entry-beginning-position)))
        (goto-char (org-entry-end-position))
        (cond ((and (org-at-heading-p)
                    (= heading-start (org-entry-beginning-position)))
               ;; Entry ends on its heading; add newline after
               (end-of-line)
               (insert "\n\n"))
              (t
               ;; Entry ends after its heading; back up
               (forward-line -1)
               (end-of-line)
               (when (org-at-heading-p)
                 ;; At the same heading
                 (forward-line)
                 (insert "\n")
                 (forward-line -1))
               (while (not (looking-back (rx (repeat 3 (seq (optional blank) "\n")))))
                 (insert "\n"))
               (forward-line -1)))))

     ((org-at-item-checkbox-p)
      ;; Checkbox: Insert new item with checkbox.
      (org-insert-todo-heading nil))

     ((org-in-item-p)
      ;; Plain list.  Yes, this gets a little complicated...
      (let ((context (org-element-context)))
        (if (or (eq 'plain-list (car context))  ; First item in list
                (and (eq 'item (car context))
                     (not (eq (org-element-property :contents-begin context)
                              (org-element-property :contents-end context))))
                (unpackaged/org-element-descendant-of 'item context))  ; Element in list item, e.g. a link
            ;; Non-empty item: Add new item.
            (org-insert-item)
          ;; Empty item: Close the list.
          ;; TODO: Do this with org functions rather than operating on the text. Can't seem to find the right function.
          (delete-region (line-beginning-position) (line-end-position))
          (insert "\n"))))

     ((when (fboundp 'org-inlinetask-in-task-p)
        (org-inlinetask-in-task-p))
      ;; Inline task: Don't insert a new heading.
      (org-return))

     ((org-at-table-p)
      (cond ((save-excursion
               (beginning-of-line)
               ;; See `org-table-next-field'.
               (cl-loop with end = (line-end-position)
                        for cell = (org-element-table-cell-parser)
                        always (equal (org-element-property :contents-begin cell)
                                      (org-element-property :contents-end cell))
                        while (re-search-forward "|" end t)))
             ;; Empty row: end the table.
             (delete-region (line-beginning-position) (line-end-position))
             (org-return))
            (t
             ;; Non-empty row: call `org-return'.
             (org-return))))
     (t
      ;; All other cases: call `org-return'.
      (org-return)))))
;;; Org Rifle
;; Search [[https://github.com/alphapapa/helm-org-rifle][rapidly]] through org files using helm
(use-package helm-org-rifle
  :commands (helm-org-rifle helm-org-rifle-agenda-files helm-org-rifle-org-directory))

;;; Org-Download
;; Drag and drop images to Emacs org-mode. Courtesy of [[https://github.com/abo-abo/org-download][abo-abo]].
(use-package org-download
  :commands (org-download-yank org-download-screenshot org-download-image)
  :config
   (setq org-download-method 'directory)
         org-download-image-dir "~/Dropbox/org-files/org-pictures"
         org-download-image-latex-width 500)

;;; Org Pomodoro
;; Helps with time tracking
(use-package org-pomodoro
  :commands org-pomodoro
  :init
  (progn
    (setq org-pomodoro-audio-player "/usr/bin/afplay")))

;;; Org Indirect Buffer
;; Some advice to automatically switch to a new indirect buffer upon creation
(defadvice org-tree-to-indirect-buffer (after org-tree-to-indirect-buffer-after activate) (other-window 1))

;;; Org Numbers Overlay
;; [[https://github.com/larkery/emacs/blob/master/site-lisp/org-numbers-overlay.el][This]] is a useful minor-mode to number org-mode headings. It came up in
;; the course of [[https://www.reddit.com/r/emacs/comments/6crtzw/it_is_possible_to_display_numbers_at_the/][this reddit]] discussion.
(define-minor-mode org-numbers-overlay-mode
  "Add overlays to org headings which number them"
  nil " *1." nil

  (let ((hooks '(after-save-hook
                 org-insert-heading-hook))
        (funcs '(org-promote
                 org-cycle-level
                 org-promote-subtree
                 org-demote
                 org-demote-subtree
                 org-move-subtree-up
                 org-move-subtree-down
                 org-move-item-down
                 org-move-item-up
                 org-cut-subtree
                 org-insert-todo-heading
                 org-insert-todo-subheading
                 org-meta-return
                 org-set-property
                 org-move)))
    (if org-numbers-overlay-mode
        (progn
          (org-numbers-overlay-update)
          (dolist (fn funcs)
            (advice-add fn :after #'org-numbers-overlay-update))
          (dolist (hook hooks)
            (add-hook hook #'org-numbers-overlay-update)))

      (progn
        (dolist (fn funcs)
          (advice-add fn :after #'org-numbers-overlay-update))
        (dolist (hook hooks)
          (remove-hook hook #'org-numbers-overlay-update))

        (loop for o in (overlays-in (point-min) (point-max))
              if (eq (overlay-get o 'type) 'org-number)
              do (delete-overlay o))))))

(defun org-numbers-overlay-update (&rest args)
  (when org-numbers-overlay-mode
    (let ((levels (make-vector 10 0)))
      (save-excursion
        (widen)
        (goto-char (point-min))
        (while (outline-next-heading)
          (if (assoc "UNNUMBERED" (org-entry-properties))
              ;; if it's unnumbered delete any overlays we have on it
              (loop for o in (overlays-in (point)
                                          (save-excursion (end-of-line) (point)))
                    if (eq (overlay-get o 'type) 'org-number)
                    do (delete-overlay o))
            ;; if it's not unnumbered add a number or update it
            (let* ((detail (org-heading-components))
                   (level (- (car detail) 1))
                   (lcounter (1+ (aref levels level)))
                   (o (or (loop for o in (overlays-in (point)
                                                      (save-excursion (end-of-line) (point)))
                                if (eq (overlay-get o 'type) 'org-number)
                                return o)
                          (make-overlay (point) (+ (point) (car detail))))))
              (aset levels level lcounter)
              (loop for i from (1+ level) to 9
                    do (aset levels i 0))
              (overlay-put o 'type 'org-number)
              (overlay-put o 'evaporate t)
              (overlay-put o 'after-string
                           (let (s)
                             (loop for i across levels
                                   until (zerop i)
                                   do (setf s (if s (format "%s.%d" s i)
                                                (format " %d" i))
                                            ))
                             s)))))))))
(provide 'org-numbers-overlay)

;;; Org Export
;; Some useful settings
;;;; Backends
(setq org-export-backends '(ascii html icalendar latex odt pandoc hugo md))

;;;; Ox-Pandoc
(use-package ox-pandoc
  :after ox
  :defer 5
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
      ;;(?8 "to opendocument." org-pandoc-export-to-opendocument)
      ;;(?8 "to opendocument and open." org-pandoc-export-to-opendocument-and-open)
      ;;(?( "as opendocument." org-pandoc-export-as-opendocument)
      ;;(?9 "to opml." org-pandoc-export-to-opml)
      ;;(?9 "to opml and open." org-pandoc-export-to-opml-and-open)
      ;;(?) "as opml." org-pandoc-export-as-opml)
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
;; (use-package ox-hugo-auto-export :ensure nil :after ox-hugo)

;;;; Export Top Level Trees
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
;;; Org Miscellaneous Packages
(use-package htmlize :commands (htmlize-buffer))
(use-package org-inlinetask :ensure nil :commands org-inlinetask-insert-task)
  ;; ignore export of headlines marked with :ignore: tag
(use-package ox-extra
  :ensure nil
  :after ox
  :demand t
  :config
  (ox-extras-activate '(ignore-headlines)))

;;; Provide
(provide 'setup-org)
