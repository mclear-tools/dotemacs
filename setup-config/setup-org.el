;; Org Mode

;;; New Org
;; remove references to older org in path
(setq load-path (cl-remove-if (lambda (x) (string-match-p "org$" x)) load-path))

;; Org package settings -- use org-plus-contrib to get latest org
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :general (cpm/leader-keys
             "uc" 'org-capture)
  :init
;;; Org Settings
;;;; Org Directories
  (setq-default org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org-files/")
  (setq-default org-default-notes-file (concat org-directory "inbox.org"))
  (setq-default org-agenda-files (list org-directory))

  :config
;;;; Org Config Settings
  (add-hook 'org-mode-hook #'visual-line-mode)
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

  ;; show markup on cursor
  ;; https://www.reddit.com/r/orgmode/comments/43uuck/temporarily_show_emphasis_markers_when_the_cursor
  (defun cpm/org-show-emphasis-markers-at-point ()
    (save-match-data
      (if (and (org-in-regexp org-emph-re 2)
               (>= (point) (match-beginning 3))
               (<= (point) (match-end 4))
               (member (match-string 3) (mapcar 'car org-emphasis-alist)))
          (with-silent-modifications
            (remove-text-properties
             (match-beginning 3) (match-beginning 5)
             '(invisible org-link)))
        (apply 'font-lock-flush (list (match-beginning 3) (match-beginning 5))))))

  (add-hook 'post-command-hook
            'cpm/org-show-emphasis-markers-at-point nil t)


;;;; Org Modules
  (setq org-modules (quote (org-tempo org-protocol org-habit org-mac-link)))

;;;; Org ID
  (setq org-id-locations-file (concat cpm-cache-dir ".org-id-locations"))

;;;; Org State Settings
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(g)" "NEXT(n)" "WAITING(w@/!)" "MAYBE(m)" "SOMEDAY(s)" "|" "DONE(d)")
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
        org-tags-column 0
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
  ;; Supercharge org-agenda: https://github.com/alphapapa/org-super-agenda
  ;; Settings courtesy of alphapapa: https://github.com/alphapapa/org-super-agenda/blob/master/examples.org#forward-looking

  (use-package org-super-agenda
    ;; :pin manual ;; throws errors for some reason when I update
    :commands org-super-agenda-mode
    :general
    (:states '(normal motion emacs) :keymaps 'org-agenda-keymap
     ","  'cpm/hydra-org-agenda/body)
    :after (org org-agenda)
    :config
    (org-super-agenda-mode)
    (setq org-super-agenda-date-format "%A, %e %b")
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
             ;; :deadline future
             ;; :scheduled future
             :auto-ts t)
            ;; (:name "Scheduled"
            ;;  :scheduled t)
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
                               (alltodo "" ((org-agenda-overriding-header nil)
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
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (general-define-key
   :states '(insert normal motion emacs)
   :keymaps 'override
   "C-c c" #'org-capture)
  (setq org-capture-templates
        ;; Note the ` and , to get concat to evaluate properly
        `(("c" "Capture" entry (file ,(concat org-directory "inbox.org"))
           "* TODO %?\n %i")
          ("j" "Journal" entry (file+olp+datetree ,(concat org-directory "journal.org"))
           "**** %<%H:%M>\n%?")
          ("l" "A link, for reading later" entry (file ,(concat org-directory "inbox.org"))
           "* %? :link: \n%(grab-mac-link 'safari 'org)")
          ("m" "Mail-Task" entry (file ,(concat org-directory "inbox.org"))
           "* TODO %? :email: \n%(grab-mac-link 'mail 'org)")
          ;; ("m" "Mail-Task" entry (file ,(concat org-directory "inbox.org"))
          ;;  "* TODO %:description                         :email: \n[[message://%:link][Email link]] \n%? ")
          ("r" "Reference" entry (file ,(concat org-directory "reference.org"))
           "* %?")
          ("M" "UNL Merit Review" entry (file ,(concat org-directory "merit-reviews.org"))
           (file ,(concat org-directory "templates/merit-review-template.org")))
          ("w" "Review: Weekly Review" entry (file+datetree ,(concat org-directory "reviews.org"))
           (file ,(concat org-directory "templates/weekly_review_template.org")))
          ("R" "Referee report" entry (file+datetree ,(concat org-directory "referee-reports.org"))
           (file ,(concat org-directory "templates/referee-report-template.org")))))

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
    (cpm/insert-weather)
    (goto-char (point-max)))

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

  (defun cpm/org-capture-mail-frame ()
    (interactive)
    (org-capture nil "m"))
  (defun cpm/make-org-capture-mail-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame '((name . "Email Capture") (width . 90) (height . 20)
                  (top . 400) (left . 300)
                  ))
    (select-frame-by-name "Email Capture")
    (cpm/org-capture-mail-frame))




;;;; Capture Hooks
  ;; ;; Make capture the only window and close after refiling.
  (defun cpm/capture-single-window-frame ()
    "make org capture the only window in the new frame"
    (cond ((equal "What are you doing?" (frame-parameter nil 'name)) (delete-other-windows))
          ((equal "alfred-capture" (frame-parameter nil 'name)) (delete-other-windows))
          ((equal "Email Capture" (frame-parameter nil 'name)) (delete-other-windows))))
  (defun cpm/delete-capture-frame ()
    "kill frame after capture"
    (cond ((equal "What are you doing?" (frame-parameter nil 'name)) (delete-frame))
          ((equal "alfred-capture" (frame-parameter nil 'name)) (delete-frame))
          ((equal "Email Capture" (frame-parameter nil 'name)) (delete-frame))))
  (add-hook! 'org-capture-mode-hook #'cpm/capture-single-window-frame)
  (add-hook! 'org-capture-after-finalize-hook #'cpm/delete-capture-frame)

  ;; (defadvice org-capture
  ;;     (after make-full-window-frame activate)
  ;;   "Advise capture to be the only window when used as a popup"
  ;;   (cond ((equal "What are you doing?" (frame-parameter nil 'name)) (delete-other-windows))
  ;;         ((equal "alfred-capture" (frame-parameter nil 'name)) (delete-other-windows))
  ;;         ((equal "Email Capture" (frame-parameter nil 'name)) (delete-other-windows))))

  ;; (defadvice org-capture-finalize
  ;;     (after org-capture-finalize activate)
  ;;   "Advise capture-finalize to close the frame"
  ;;   (cond ((equal "What are you doing?" (frame-parameter nil 'name)) (delete-frame))
  ;;         ((equal "alfred-capture" (frame-parameter nil 'name)) (delete-frame))
  ;;         ((equal "Email Capture" (frame-parameter nil 'name)) (delete-frame))
  ;;         ))


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
          ("\\.pdf\\'" . default)
          (auto-mode . emacs)))
  (general-define-key "C-c a" #'org-agenda)

  ;; Open bookends file links in bookends
  (org-add-link-type
   "bookends" 'cpm/follow-bookends-link)
  (defun cpm/follow-bookends-link (path)
    "run bookends link in org files"
    (shell-command-to-string (concat "open bookends:" path)))


;;; End Use-Package Config
  ;; end use-package config settings
  )
;;; Org Babel
;; org babel source block settings
(setq org-src-fontify-natively t
      org-src-window-setup 'other-window
      org-src-tab-acts-natively nil
      org-src-strip-leading-and-trailing-blank-lines t)

;; Avoid `org-babel-do-load-languages' since it does an eager require.
(use-package ob-python
  :defer t
  :ensure org-plus-contrib
  :commands (org-babel-execute:python)
  :config
  (progn
    (setq org-babel-python-command "python3"))) ;Default to python 3.x

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

(use-package ob-shell
  :defer t
  :ensure org-plus-contrib
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh
   org-babel-execute:bash
   org-babel-expand-body:bash))

(use-package ob-lisp
  :defer t
  :ensure org-plus-contrib
  :commands (org-babel-execute:lisp))

(use-package ob-latex
  :defer t
  :ensure org-plus-contrib
  :commands
  (org-babel-execute:latex))

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

;;; Org Bullets
;; Replace org-bullets since it is no longer maintained
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("⚫" "◉" "⁂" "❖" "✮" "✱" "⚫" "✸"))
  (setq org-superstar-prettify-item-bullets t)
  ;; see https://unicode-table.com/en/25C9/ for ideas
  (setq org-superstar-item-bullet-alist
        '((?* . ?○)
          (?+ . ?◉)
          (?- . ?●))))

;; Demote sequence for list bullets
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
;; Increase sub-item indentation
(setq org-list-indent-offset 1)

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
  (setq counsel-outline-display-style 'path)
  (setq counsel-outline-path-separator " ➜ ")
  (setq counsel-outline-face-style 'org)
  (general-define-key :keymaps 'org-mode-map "C-c C-j" #'counsel-org-goto)
  (general-define-key :keymaps 'org-mode-map "C-u C-c C-j" #'counsel-org-goto-all))

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
    (goto-char (point-min))
    (widen)
    (org-forward-heading-same-level 1))
  (org-narrow-to-subtree))

(defun cpm/org-retreat ()
  (interactive)
  (when (buffer-narrowed-p)
    (goto-char (point-min))
    (widen)
    (org-backward-heading-same-level 1))
  (org-narrow-to-subtree))

;;;; Goto Org Files
(defun cpm/goto-org-files ()
  "goto org-files directory"
  (interactive)
  (counsel-find-file org-directory))
(defun cpm/goto-inbox.org ()
  "goto org-inbox"
  (interactive)
  (find-file (concat org-directory "inbox.org")))
(defun cpm/goto-todo.org ()
  "goto org-todo"
  (interactive)
  (find-file (concat org-directory "todo.org")))
(defun cpm/goto-conferences.org ()
  "goto org-conferences"
  (interactive)
  (find-file (concat org-directory "conferences.org")))
(defun cpm/goto-referee-reports.org ()
  "goto org referee reports"
  (interactive)
  (find-file (concat org-directory "referee-reports.org")))
(defun cpm/goto-reference.org ()
  "goto org reference notes"
  (interactive)
  (find-file (concat org-directory "reference.org")))
(defun cpm/goto-someday.org ()
  "goto org-someday"
  (interactive)
  (find-file (concat org-directory "someday.org")))
(defun cpm/goto-reading.org ()
  "goto reading list"
  (interactive)
  (find-file (concat org-directory "reading.org")))
(defun cpm/goto-writing.org ()
  "goto writing list"
  (interactive)
  (find-file (concat org-directory "writing.org")))
(defun cpm/goto-teaching.org ()
  "goto teaching file"
  (interactive)
  (find-file (concat org-directory "teaching.org")))


;;;; Export Headings as Seperate Files
;; export headlines to separate files
;; http://pragmaticemacs.com/emacs/export-org-mode-headlines-to-separate-files/ ; see also:
;; http://emacs.stackexchange.com/questions/2259/how-to-export-top-level-headings-of-org-mode-buffer-to-separate-files

(defun cpm/org-export-headlines-to-docx ()
  "Export all subtrees that are *not* tagged with :noexport: to
    separate files.

    Subtrees ;TODO: hat do not have the :EXPORT_FILE_NAME: property set
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
           (org-pandoc-export-to-docx nil t)
           (unless export-file (org-delete-property "EXPORT_FILE_NAME"))
           (set-buffer-modified-p modifiedp)))
       "-noexport" 'region-start-level)))
  (shell-command-to-string "open ~/Dropbox/Work/Comments/Referee-Reports/ref-report.docx"))

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
;; Note that i've disabled this for now as it was causing issues
;; https://gist.github.com/alphapapa/61c1015f7d1f0d446bc7fd652b7ec4fe
(defun cpm/org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
    A double return on an empty element deletes it. Use a prefix arg
    to get regular RET. "
  ;; See https://gist.github.com/alphapapa/61c1015f7d1f0d446bc7fd652b7ec4fe and
  ;; http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
  (interactive "P")
  (if ignore
      (org-return)
    (cond ((eq 'link (car (org-element-context)))
           ;; Open links like usual
           (org-open-at-point-global))
          ((and (fboundp 'org-inlinetask-in-task-p) (org-inlinetask-in-task-p))
           ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
           ;; Johansson!
           (org-return))
          ((org-at-item-checkbox-p)
           ;; Add checkboxes
           (org-insert-todo-heading nil))
          ((and (org-in-item-p) (not (bolp)))
           ;; Lists end with two blank lines, so we need to make sure we are also not
           ;; at the beginning of a line to avoid a loop where a new entry gets
           ;; created with only one blank line.
           (if (org-element-property :contents-begin (org-element-context))
               (org-insert-heading)
             (beginning-of-line)
             (delete-region (line-beginning-position) (line-end-position))
             (org-return)))
          ((org-at-heading-p)
           (if (s-present? (org-element-property :title (org-element-context)))
               (progn
                 (org-end-of-meta-data)
                 (org-insert-heading))
             (beginning-of-line)
             (delete-region (line-beginning-position) (line-end-position))))
          ((org-at-table-p)
           (if (--any? (string-empty-p it)
                       (nth (- (org-table-current-dline) 1) (org-table-to-lisp)))
               (org-return)
             ;; Empty row
             (beginning-of-line)
             (delete-region (line-beginning-position) (line-end-position))
             (org-return)))
          (t
           (org-return)))))

;; (general-define-key :keymaps 'org-mode-map "RET" #'cpm/org-return)

;;;; Org Create Check Box From List Item
;; A useful macro for converting list items to checkboxes
(fset 'cpm/org-checkbox-from-list
      [?a ?  ?\[ ?  ?\] escape ?\M-x return])


;;;; Org Expand Region
;; see https://www.reddit.com/r/emacs/comments/f9e6kw/expandregion_for_org_mode_with_org_element_api/
;; Expansion functions for Org mode based on Org element API.
(defun gb/er/mark-org-element (&optional parent)
  "Mark the smallest Org element or object around point.
Uses the Org Element API to identify those elements or objects.
With argument PARENT, mark the parent element instead."
  (interactive)
  (let* ((el-at-point (org-element-context))
         (up-el-at-point (org-element-property :parent el-at-point))
         (el (if parent
                 (cond
                  ((not up-el-at-point)
                   (save-excursion
                     (ignore-errors (org-up-element))
                     ;; Given el-at-point has no parent at this point,
                     ;; `org-up-element' will bring point to a heading
                     ;; (back-to-heading, if not on a heading, and
                     ;; up-heading, if on one), unless it is before the
                     ;; first one.
                     ;; Note: 'org-element-at-point' and
                     ;; 'org-element-context' won't normally get a
                     ;; headline's parent (which will return nil), we'd need
                     ;; 'org-element-parse-buffer' for that.  But we don't
                     ;; want to parse the whole buffer for an expansion
                     ;; either.
                     (when (org-with-limited-levels (org-at-heading-p))
                       (org-element-at-point))))
                  ((and
                    (memq (org-element-type el-at-point)
                          org-element-all-objects)
                    (eq (org-element-type up-el-at-point) 'paragraph)
                    (memq (org-element-type
                           (org-element-property :parent up-el-at-point))
                          '(item quote-block center-block drawer)))
                   ;; Corner case, when an 'object' is also the first thing
                   ;; on a plain list item.  In this case, if we simply get
                   ;; the parent, it will be paragraph, and further
                   ;; expansion will lose the list structure from there.
                   ;; Same thing happens on quote-blocks.  So, if element at
                   ;; point is an object, its parent is a paragraph, and its
                   ;; grandparent is one of those types, we pass the
                   ;; grandparent, to follow the structure properly.
                   ;; Probably, other cases will emerge with use, which can
                   ;; just be added here.  Unfortunately, we cannot simply
                   ;; pass the granparent for all cases: e.g. if the parent
                   ;; is a headline, there is no grandparent.
                   (org-element-property :parent up-el-at-point))
                  (t
                   up-el-at-point))
               el-at-point))
         (type (org-element-type el))
         beg end)
    (when el
      (cond
       ((memq type org-element-all-objects)
        (setq beg (org-element-property :begin el))
        (setq end (- (org-element-property :end el)
                     (org-element-property :post-blank el))))
       ((memq type '(src-block center-block comment-block
                               example-block export-block quote-block
                               special-block verse-block
                               latex-environment
                               drawer property-drawer))
        (setq beg (org-element-property :begin el))
        (setq end (save-excursion
                    (goto-char (org-element-property :end el))
                    (forward-line
                     (- (org-element-property :post-blank el)))
                    (point))))
       (t
        (setq beg (org-element-property :begin el))
        (setq end (org-element-property :end el)))))
    (when (and beg end)
      (goto-char end)
      (set-mark (point))
      (goto-char beg))))

(defun gb/er/mark-org-element-parent ()
  "Mark the parent of the Org element or object around point."
  (interactive)
  (gb/er/mark-org-element t))

(defun gb/er/mark-org-element-inside ()
  "Mark contents of the smallest Org element or object around point."
  (interactive)
  (let* ((el (org-element-context))
         (type (org-element-type el))
         beg end)
    ;; Here we handle just special cases, remaining ones will fall back to
    ;; 'gb/er/mark-org-element'. So, there is no need for a residual
    ;; condition.
    (cond
     ((memq type '(bold italic strike-through underline
                        quote-block special-block verse-block
                        drawer property-drawer
                        footnote-definition footnote-reference))
      (setq beg (org-element-property :contents-begin el))
      (setq end (org-element-property :contents-end el)))
     ((memq type '(code verbatim))
      (setq beg (save-excursion
                  (goto-char (org-element-property :begin el))
                  (unless (bolp) (backward-char 1))
                  (when (looking-at org-verbatim-re)
                    (goto-char (match-beginning 4))
                    (point))))
      (setq end (save-excursion
                  (goto-char (org-element-property :begin el))
                  (unless (bolp) (backward-char 1))
                  (when (looking-at org-verbatim-re)
                    (goto-char (match-end 4))
                    (point)))))
     ((memq type '(src-block center-block comment-block
                             example-block export-block
                             latex-environment))
      (setq beg (save-excursion
                  (goto-char (org-element-property :post-affiliated el))
                  (forward-line)
                  (point)))
      (setq end (save-excursion
                  (goto-char (org-element-property :end el))
                  (forward-line
                   (1- (- (org-element-property :post-blank el))))
                  (point))))
     ;; Unsure whether this is a good handling for headlines.
     ;; ((eq type 'headline)
     ;;  (save-excursion
     ;;    ;; Following the steps of 'org-element-headline-parser' to get the
     ;;    ;; start and end position of the title.
     ;;    (goto-char (org-element-property :begin el))
     ;;    (skip-chars-forward "*")
     ;;    (skip-chars-forward " \t")
     ;;    (and org-todo-regexp
     ;;         (let (case-fold-search) (looking-at (concat org-todo-regexp " ")))
     ;;         (goto-char (match-end 0))
     ;;         (skip-chars-forward " \t"))
     ;;    (when (looking-at "\\[#.\\][ \t]*")
     ;;      (goto-char (match-end 0)))
     ;;    (when (let (case-fold-search) (looking-at org-comment-string))
     ;;      (goto-char (match-end 0)))
     ;;    (setq beg (point))
     ;;    (when (re-search-forward
     ;;           "[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$"
     ;;           (line-end-position)
     ;;           'move)
     ;;      (goto-char (match-beginning 0)))
     ;;    (setq end (point))))
     )
    (when (and beg end)
      (goto-char end)
      (set-mark (point))
      (goto-char beg))))

;; The default sentence expansion is quite frequently fooled by a regular
;; Org document (plain lists, code blocks, in particular), so we use Org's
;; sentence commands and restrict the mark to a single paragraph.
(defun gb/er/mark-org-sentence ()
  "Marks one sentence."
  (interactive)
  (let ((par-el (org-element-lineage
                 (org-element-context) '(paragraph) t))
        (beg-quote (when (use-region-p)
                     (save-excursion
                       (goto-char (region-beginning))
                       (looking-back "[\"“]" (1- (point))))))
        (end-quote (when (use-region-p)
                     (save-excursion
                       (goto-char (region-end))
                       (looking-at "[\"”]")))))
    (when (and
           ;; Do not mark sentences when not in a paragraph.
           par-el
           ;; Also do not mark a sentence when current region is
           ;; equivalent to an 'inside-quotes' expansion, let
           ;; 'outside-quotes' expand first.
           (not (and beg-quote end-quote)))
      (save-restriction
        ;; Never mark beyond the limits of the current paragraph.
        (narrow-to-region (org-element-property :contents-begin par-el)
                          (org-element-property :contents-end par-el))
        (forward-char 1)
        (org-backward-sentence 1)
        ;; Sentences which start or end with quotes will not be expanded
        ;; into by the heuristics of 'expand-region', as the 'inside-quotes'
        ;; expansion will prevail.  Thus, we expand the sentence up to the
        ;; quote only, to be able to expand one sentence when multiple
        ;; sentences are between quotes.  This rule of thumb will not always
        ;; be ideal: e.g. when the sentence is a sequence of multiple quoted
        ;; strings.
        (when (looking-at "[\"“]")
          (goto-char (match-end 0)))
        (set-mark (point))
        (org-forward-sentence 1)
        ;; Ditto.
        (when (looking-back "[\"”]" (1- (point)))
          (goto-char (match-beginning 0)))
        (exchange-point-and-mark)))))

;; Alternate version: simpler, but no control for quotes.
;; (defun gb/er/mark-org-sentence ()
;;   "Marks one sentence."
;;   (interactive)
;;   (let ((par-el (org-element-lineage
;;                  (org-element-context) '(paragraph) t)))
;;     ;; Do not mark sentences when not in a paragraph.
;;     (when par-el
;;       (save-restriction
;;         ;; Never mark beyond the limits of the current paragraph.
;;         (narrow-to-region (org-element-property :contents-begin par-el)
;;                           (org-element-property :contents-end par-el))
;;         (forward-char 1)
;;         (org-backward-sentence 1)
;;         (set-mark (point))
;;         (org-forward-sentence 1)
;;         (exchange-point-and-mark)))))

;; Mark curved quotes in Org mode.
(defun gb/er/mark-org-inside-curved-quotes (&optional outside)
  "Mark the inside of the current curved quotes string, not
including the quotation marks."
  (interactive)
  (let ((par-el (org-element-lineage
                 (org-element-context) '(paragraph) t)))
    (when par-el
      (save-restriction
        (narrow-to-region (org-element-property :contents-begin par-el)
                          (org-element-property :contents-end par-el))
        (let* ((beg-quote (save-excursion
                            (when (search-backward "“" nil t)
                              (if outside
                                  (match-beginning 0)
                                (match-end 0)))))
               (end-quote (save-excursion
                            (when (search-forward "”" nil t)
                              (if outside
                                  (match-end 0)
                                (match-beginning 0))))))
          (when (and beg-quote end-quote)
            (goto-char end-quote)
            (set-mark (point))
            (goto-char beg-quote)))))))

(defun gb/er/mark-org-outside-curved-quotes ()
  "Mark the current curved-quotes string, including the quotation marks."
  (interactive)
  (gb/er/mark-org-inside-curved-quotes t))

;; Control pair marking in Org: let some between-pairs objects be marked as
;; Org elements.
(defun gb/er/mark-org-inside-pairs ()
  "Mark inside pairs (as defined by the mode), not including the pairs.
Don't mark when at certain Org objects."
  (interactive)
  (unless (memq (org-element-type (org-element-context))
                '(link footnote-definition macro
                       target radio-target timestamp))
    (er/mark-inside-pairs)))

(defun gb/er/mark-org-outside-pairs ()
  "Mark pairs (as defined by the mode), including the pair chars.
Don't mark when at certain Org objects."
  (interactive)
  (unless (memq (org-element-type (org-element-context))
                '(link footnote-definition macro
                       target radio-target timestamp))
    (er/mark-outside-pairs)))

;; The default expansion for symbol includes Org's emphasis markers which
;; are contiguous to symbols (they do indeed belong to the syntax class).
;; Thus, the default expansion to symbol "leaks" beyond
;; 'inside-emphasis-markers'.  To avoid this, we restrict symbol expansion
;; to the contents of Org emphasis objects.
(defun gb/er/mark-org-symbol ()
  "Mark the entire symbol around or in front of point."
  (interactive)
  (let* ((symbol-regexp "\\s_\\|\\sw")
         (el (org-element-context))
         (type (org-element-type el))
         beg-emph end-emph)
    (cond ((memq type '(bold italic underline strike-through))
           (setq beg-emph (org-element-property :contents-begin el))
           (setq end-emph (org-element-property :contents-end el)))
          ((memq type '(code verbatim))
           (setq beg-emph (save-excursion
                            (goto-char (org-element-property :begin el))
                            (unless (bolp) (backward-char 1))
                            (when (looking-at org-verbatim-re)
                              (goto-char (match-beginning 4))
                              (point))))
           (setq end-emph (save-excursion
                            (goto-char (org-element-property :begin el))
                            (unless (bolp) (backward-char 1))
                            (when (looking-at org-verbatim-re)
                              (goto-char (match-end 4))
                              (point))))))
    (save-restriction
      (when (and beg-emph end-emph)
        (narrow-to-region beg-emph end-emph))
      (when (or (looking-at symbol-regexp)
                (er/looking-back-on-line symbol-regexp))
        (skip-syntax-forward "_w")
        (set-mark (point))
        (skip-syntax-backward "_w")))))

;; expand-region configuration for Org mode
(defun gb/er/config-org-mode-expansions ()
  (when (< emacs-major-version 27)
    (require 'seq))
  (setq-local er/try-expand-list
              (append
               ;; Removing some expansions from the list
               (seq-remove
                (lambda (x)
                  (memq x '(;; The expansions based on the Org element API
                            ;; cover most of the default expansions, others
                            ;; don't seem that useful (to me) and may
                            ;; introduce some noise in the expansion
                            ;; sequence.
                            org-mark-subtree
                            er/mark-org-element
                            er/mark-org-element-parent
                            er/mark-org-code-block
                            er/mark-org-parent
                            er/mark-comment
                            er/mark-url
                            er/mark-email
                            mark-page

                            ;; The basic symbol and method-call expansion
                            ;; consider Org emphasis markers as part of the
                            ;; unit,  so I created a dedicated function for
                            ;; symbol, disabled the others.
                            er/mark-symbol
                            er/mark-symbol-with-prefix
                            er/mark-next-accessor
                            er/mark-method-call

                            ;; er/mark-paragraph actually confuses
                            ;; expand-region on plain lists, and paragraphs
                            ;; actually do work with the other expansions on
                            ;; the list (as an org-element).  For the same
                            ;; reason, remove er/mark-text-paragraph.
                            er/mark-paragraph
                            er/mark-text-paragraph

                            ;; Remove er/mark-sentence, better to work with
                            ;; Org sentence commands, which are in
                            ;; gb/er/mark-org-sentence.  For the same reason
                            ;; remove er/mark-text-sentence.
                            er/mark-sentence
                            er/mark-text-sentence

                            ;; Tweak pair expansion for Org.
                            er/mark-inside-pairs
                            er/mark-outside-pairs)))
                er/try-expand-list)
               '(gb/er/mark-org-symbol
                 gb/er/mark-org-element
                 gb/er/mark-org-element-parent
                 gb/er/mark-org-element-inside
                 gb/er/mark-org-sentence
                 gb/er/mark-org-inside-curved-quotes
                 gb/er/mark-org-outside-curved-quotes
                 gb/er/mark-org-inside-pairs
                 gb/er/mark-org-outside-pairs))))

;;;; Org link Syntax
(defun org-update-link-syntax (&optional no-query)
  "Update syntax for links in current buffer.
Query before replacing a link, unless optional argument NO-QUERY
is non-nil."
  (interactive "P")
  (org-with-point-at 1
    (let ((case-fold-search t))
      (while (re-search-forward "\\[\\[[^]]*?%\\(?:2[05]\\|5[BD]\\)" nil t)
        (let ((object (save-match-data (org-element-context))))
          (when (and (eq 'link (org-element-type object))
                     (= (match-beginning 0)
                        (org-element-property :begin object)))
            (goto-char (org-element-property :end object))
            (let* ((uri-start (+ 2 (match-beginning 0)))
                   (uri-end (save-excursion
                              (goto-char uri-start)
                              (re-search-forward "\\][][]" nil t)
                              (match-beginning 0)))
                   (uri (buffer-substring-no-properties uri-start uri-end)))
              (when (or no-query
                        (y-or-n-p
                         (format "Possibly obsolete URI syntax: %S.  Fix? "
                                 uri)))
                (setf (buffer-substring uri-start uri-end)
                      (org-link-escape (org-link-decode uri)))))))))))


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

;;; Org Indirect Buffer
(setq org-indirect-buffer-display 'current-window)
;; Some advice to automatically switch to a new indirect buffer upon creation
;; (defadvice org-tree-to-indirect-buffer (after org-tree-to-indirect-buffer-after activate) (other-window 1))

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

;;; Org Roam (Wiki & Notes)
;; Good notes package but a lot is still in flux
;; see https://org-roam.readthedocs.io/en/latest/
(eval-when-compile
  (quelpa
   '(org-roam :fetcher github :repo "jethrokuan/org-roam")))

(use-package org-roam
  :ensure nil
  :commands (org-roam org-roam-new-file org-roam-find-file)
  :after org
  :hook
  (;; (org-mode . org-roam-mode)
   (after-init . org-roam--build-cache-async) ;; optional!
   )
  :custom
  (org-roam-directory "~/Dropbox/Work/projects/notebook/org/")
  ;;;; Org Roam Keybindings
  :general
  (:states '(normal motion)
   (cpm/leader-keys
     "R l"  #'org-roam
     "R t"  #'org-roam-today
     "R f"  #'org-roam-find-file
     "R i"  #'org-roam-insert
     "R g"  #'org-roam-show-graph
     "R n"  #'org-roam-new-file
     "R N"  #'org-roam--new-file-named))
  :config
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
         "" (org-roam-sql [:select [file-from] :from file-links :where (= file-to $s1)] file))
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
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y-%m%d-%H%M>-${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettel
#+HUGO_SLUG: ${slug}
#+TITLE: %<%Y-%m%d-%H%M>-${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: %<%Y-%m%d-%H%M>-${title}\n"
           :unnarrowed t)))
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: Weblinks
#+ROAM_KEY: ${ref}
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}
- source :: ${ref}"
           :unnarrowed t))))


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

;; Devonthink integration
(use-package org-devonthink
  :commands (org-insert-dtp-link org-dtp-store-link)
  :ensure nil
  :load-path cpm-elisp-dir)


;;; Provide
(provide 'setup-org)
