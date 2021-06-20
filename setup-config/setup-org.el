;; Org Mode

;;; New Org
;; remove references to older org in path
;; (setq load-path (cl-remove-if (lambda (x) (string-match-p "org$" x)) load-path))

;; Org package settings -- use org-plus-contrib to get latest org
(use-package org
  :straight t
  ;; :straight (:host github :repo "yantar92/org" :branch "feature/org-fold"
  ;;            :files (:defaults "contrib/lisp/*.el")) ;; fixes org-folding
  :mode (("\\.org$" . org-mode))
  :general (cpm/leader-keys
             "uc" 'org-capture)
  :init
;;; Org Settings
;;;; Org Directories
  ;; (setq-default org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org-files/")
  (setq-default org-directory "~/Dropbox/org-files/")
  (setq-default org-default-notes-file (concat org-directory "inbox.org"))
  (setq-default org-agenda-files (list org-directory))

;;;; Org Config Settings
  :config
  ;; (require 'org-fold)   ;; hack to make org and evil-surround work right now FIXME
  ;; use timestamp for id
  (setq org-latex-listings 'engraved) ;; relies on engrave-faces package for highlighting
  (add-hook 'org-mode-hook #'visual-line-mode)
  (setq org-stuck-projects (quote ("" nil nil "")))
  (setq org-image-actual-width  500) ;; show all images at 500px using imagemagik
  (setf org-export-with-smart-quotes t)
  (setq-default org-footnote-section nil ;; place footnotes locally rather than in own section
                org-return-follows-link t ;; make RET follow links
                org-list-allow-alphabetical t ;; allow alphabetical list
                org-hide-emphasis-markers t  ;; hide markers
                org-pretty-entities t ;; make latex look good
                org-pretty-entities-include-sub-superscripts t
                org-hide-leading-stars t
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
                org-use-fast-todo-selection 'expert ;; don't use popup window
                org-imenu-depth 8
                imenu-auto-rescan t)
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)


;;;; Org Modules
  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-habit t)
    (add-to-list 'org-modules 'org-tempo t)
    (add-to-list 'org-modules 'org-protocol t)
    (add-to-list 'org-modules 'org-mac-link t)
    )


;;;; Org ID
  (setq org-id-locations-file (concat cpm-cache-dir ".org-id-locations"))
  (setq org-id-method 'ts) ;; use timestamp for id
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id) ;; create ids

;;;; Org State Settings
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(g)" "NEXT(n)" "WAITING(w@/!)" "MAYBE(m)" "SOMEDAY(s)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)")))
;;;; Org Priority Settings
  (setq org-priority-faces '((?A . (:foreground "red"))
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
          ("pos" "\Diamond" nil "◇" "" "" "◇")
          ("space" "~" nil "&nbsp;" " " " " " ")))
  (add-hook 'org-mode-hook
            (lambda ()
              (centered-cursor-mode)
              (turn-on-auto-fill)
              ))

;;;; Org Regex (Emphasis)
  (with-eval-after-load 'org
                                        ; chars for prematch
    ;; (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
    (setcar org-emphasis-regexp-components            "     ('\"{“”\[\\\_\-")
                                        ; chars for postmatch
    ;; (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
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
          ("t" . "COMMENT \TODO:")
          ("b" . "REVEAL: split")
          ("f" . "ATTR_REVEAL: :frag (appear)")
          ("n" . "notes")))
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

  (general-define-key "C-c a" #'org-agenda)
  (with-eval-after-load 'org-agenda
    (general-define-key :keymaps 'org-agenda-mode-map
      "j" 'org-agenda-next-item
      "k" 'org-agenda-previous-item))

  ;; automatically refresh the agenda after adding a task
  (defun cpm/org-agenda-refresh ()
    (interactive)
    (when (get-buffer "*Org Agenda*")
      (with-current-buffer "*Org Agenda*"
        (org-agenda-redo)
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

;;;; Org Super-Agenda
  ;; Supercharge org-agenda: https://github.com/alphapapa/org-super-agenda
  ;; Settings courtesy of alphapapa: https://github.com/alphapapa/org-super-agenda/blob/master/examples.org#forward-looking

  (use-package org-super-agenda
    :straight t
    :commands org-super-agenda-mode
    :after org
    :general
    (:states '(normal motion emacs) :keymaps 'org-agenda-keymap
     ","  'cpm/hydra-org-agenda/body)
    :config
    (org-super-agenda-mode)
    (setq org-super-agenda-date-format "%A, %e %b")
    (let ((two-weeks-from-today (format-time-string "%Y-%m-%d" (org-read-date nil t "+2w"))))
      (setq org-super-agenda-groups
            '(
              (:name "Today"
               :time-grid t
               :date today
               :order 1)
              (:name "Scheduled earlier"
               :scheduled past
               :order 4)
              (:name "Overdue"
               :deadline past
               :order 6)
              (:name "Due Today"
               :deadline today
               :order 8)
              (:name "Due Soon"
               :deadline future
               :order 10)
              )))

    (defun cpm/jump-to-org-super-agenda ()
      (interactive)
      (require 'org)
      (require 'org-super-agenda)
      (org-agenda nil "A")))

;;;; Agenda Toggle
  (defun cpm/toggle-org-agenda-file-set ()
    (interactive)
    (if (equal org-agenda-files (list org-directory))
        (setq org-agenda-files (list "~/Dropbox/Work/projects/notebook/content-org/"))
      (setq org-agenda-files (list org-directory)))
    (message "Using %s" org-agenda-files))

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

  ;; (with-eval-after-load 'org-agenda
  ;;   (general-define-key :keymaps 'org-agenda-mode-map :states '(normal motion)
  ;;     "J" 'air-org-agenda-next-header
  ;;     "K" 'air-org-agenda-previous-header))

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

;;;; Hydra for Agenda
  ;; Hydra for org agenda (graciously offered by Spacemacs)
  (with-eval-after-load 'org-agenda
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
          ("A" "Super Agenda" ((agenda "" ((org-agenda-span 'day)))))
          ;; (alltodo "" ((org-agenda-overriding-header "")
          ;; (org-super-agenda-groups
          ;;  '(
          ;;    (:name "Priority"
          ;;     :priority>= "C")
          ;;    (:name "Next to do"
          ;;     :todo "NEXT")
          ;;    (:name "In Progress"
          ;;     :todo "DOING")
          ;;    (:todo ("WAITING" "HOLD"))
          ;;    (:todo "MAYBE")
          ;;    (:name "Reading List"
          ;;     :todo "TOREAD")
          ;;    ))
          ;; )
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
  ;; (general-define-key
  ;;  :states '(insert normal motion emacs)
  ;;  :keymaps 'override
  ;;  "C-c c" #'org-capture)
  (setq org-capture-templates
        ;; Note the ` and , to get concat to evaluate properly
        `(("c" "Capture" entry (file ,(concat org-directory "inbox.org"))
           "* TODO %?\n %i")
          ("j" "Journal" entry (file+olp+datetree ,(concat org-directory "journal.org"))
           "**** %<%H:%M>\n%?")
          ("l" "A link, for reading later" entry (file ,(concat org-directory "inbox.org"))
           "* %? :link: \n%(grab-mac-link 'safari 'org)")
          ;; ("m" "Mail-Task" entry (file ,(concat org-directory "inbox.org"))
          ;;  "* TODO %? :email: \n%(org-mac-outlook-message-get-links)")
          ;; ("m" "Mail-Task" entry (file ,(concat org-directory "inbox.org"))
          ;;  "* TODO %? :email: \n%(grab-mac-link 'mail 'org)")
          ("m" "Mail-Task" entry (file ,(concat org-directory "inbox.org"))
           "* TODO %:description                         :email: \n[[message://%:link][Email link]] \n%? ")
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

  ;; Add newline to captured items
  (defun cpm/org-capture-newlines-at-end ()
    (goto-char (point-max))
    (insert "\n\n"))
  (add-hook 'org-capture-prepare-finalize 'cpm/org-capture-newlines-at-end)

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

  ;; (defun cpm/what-are-you-doing-capture ()
  ;;   (interactive)
  ;;   (make-frame '((name . "What are you doing?") (left . (+ 550)) (top . (+ 400)) (width . 100) (height . 24)))
  ;;   (select-frame-by-name "What are you doing?")
  ;;   (cpm/org-journal)
  ;;   (delete-other-windows)
  ;;   (cpm/insert-weather)
  ;;   (goto-char (point-max)))

;;;; Alfred Capture Workflow
  ;; Help alfred and org-capture play nice.
  ;; Courtesy of http://orgmode.org/worg/org-contrib/alfred-org-capture.html with some slight modifications.
  ;; Current functions also from https://github.com/Isimoro/org-global-capture.el/blob/master/org-global-capture.el

  (defadvice org-switch-to-buffer-other-window
      (after supress-window-splitting activate)
    "Delete the extra window if we're in a capture frame"
    (if (equal "capture" (frame-parameter nil 'name))
        (delete-other-windows)))

  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (when (and (equal "capture" (frame-parameter nil 'name))
               (not (eq this-command 'org-capture-refile)))
      (delete-frame)))

  (defadvice org-capture-refile
      (after delete-capture-frame activate)
    "Advise org-refile to close the frame"
    (when (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))


;;;; Fix Org Capture Recentering Window problem
  (eval-after-load "org-agenda"
    '(defun org-agenda-redo (&optional all)
       "Rebuild possibly ALL agenda view(s) in the current buffer."
       (interactive "P")
       (let* ((p (or (and (looking-at "\\'") (1- (point))) (point)))
              (cpa (unless (eq all t) current-prefix-arg))
              (org-agenda-doing-sticky-redo org-agenda-sticky)
              (org-agenda-sticky nil)
              (org-agenda-buffer-name (or org-agenda-this-buffer-name
                                          org-agenda-buffer-name))
              (org-agenda-keep-modes t)
              (tag-filter org-agenda-tag-filter)
              (tag-preset (get 'org-agenda-tag-filter :preset-filter))
              (top-hl-filter org-agenda-top-headline-filter)
              (cat-filter org-agenda-category-filter)
              (cat-preset (get 'org-agenda-category-filter :preset-filter))
              (re-filter org-agenda-regexp-filter)
              (re-preset (get 'org-agenda-regexp-filter :preset-filter))
              (effort-filter org-agenda-effort-filter)
              (effort-preset (get 'org-agenda-effort-filter :preset-filter))
              (org-agenda-tag-filter-while-redo (or tag-filter tag-preset))
              (cols org-agenda-columns-active)
              (line (org-current-line))
              (window-line (- line (org-current-line (window-start))))
              (lprops (get 'org-agenda-redo-command 'org-lprops))
              (redo-cmd (get-text-property p 'org-redo-cmd))
              (last-args (get-text-property p 'org-last-args))
              (org-agenda-overriding-cmd (get-text-property p 'org-series-cmd))
              (org-agenda-overriding-cmd-arguments
               (unless (eq all t)
                 (cond ((listp last-args)
                        (cons (or cpa (car last-args)) (cdr last-args)))
                       ((stringp last-args)
                        last-args))))
              (series-redo-cmd (get-text-property p 'org-series-redo-cmd)))
         (put 'org-agenda-tag-filter :preset-filter nil)
         (put 'org-agenda-category-filter :preset-filter nil)
         (put 'org-agenda-regexp-filter :preset-filter nil)
         (put 'org-agenda-effort-filter :preset-filter nil)
         (and cols (org-columns-quit))
         (message "Rebuilding agenda buffer...")
         (if series-redo-cmd
             (eval series-redo-cmd)
           (org-let lprops redo-cmd))
         (setq org-agenda-undo-list nil
               org-agenda-pending-undo-list nil
               org-agenda-tag-filter tag-filter
               org-agenda-category-filter cat-filter
               org-agenda-regexp-filter re-filter
               org-agenda-effort-filter effort-filter
               org-agenda-top-headline-filter top-hl-filter)
         (message "Rebuilding agenda buffer...done")
         (put 'org-agenda-tag-filter :preset-filter tag-preset)
         (put 'org-agenda-category-filter :preset-filter cat-preset)
         (put 'org-agenda-regexp-filter :preset-filter re-preset)
         (put 'org-agenda-effort-filter :preset-filter effort-preset)
         (let ((tag (or tag-filter tag-preset))
               (cat (or cat-filter cat-preset))
               (effort (or effort-filter effort-preset))
               (re (or re-filter re-preset)))
           (when tag (org-agenda-filter-apply tag 'tag t))
           (when cat (org-agenda-filter-apply cat 'category))
           (when effort (org-agenda-filter-apply effort 'effort))
           (when re  (org-agenda-filter-apply re 'regexp)))
         (and top-hl-filter (org-agenda-filter-top-headline-apply top-hl-filter))
         (and cols (called-interactively-p 'any) (org-agenda-columns))
         (org-goto-line line))))

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
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 8)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; fix refile
  (defun cpm/fix-org-refile ()
    (interactive)
    (shell-command-to-string "cd ~/.emacs.d/.local/straight/build && find org*/*.elc -print0 | xargs -0 rm")
    (org-reload))

;;; Open Files in Default Application
  ;;Open files in their default applications (ms word being the prime example)
  (setq org-file-apps
        '(("\\.docx\\'" . default)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . emacs)
          (auto-mode . emacs)))

  ;; Open bookends file links in bookends
  (org-add-link-type
   "bookends" 'cpm/follow-bookends-link)
  (defun cpm/follow-bookends-link (path)
    "run bookends link in org files"
    (shell-command-to-string (concat "open bookends:" path)))

;;; End Org Use-Package Config
  ;; end org use-package config settings
  )
;;; Org-Goto
;; Make counsel display org headings nicely.
;; (with-eval-after-load 'org
;;   (setq counsel-outline-display-style 'path)
;;   (setq counsel-outline-path-separator " ➜ ")
;;   (setq counsel-outline-face-style 'org))
;; (general-define-key :keymaps 'org-mode-map "C-c C-j" #'counsel-org-goto)
;; (general-define-key :keymaps 'org-mode-map "C-u C-c C-j" #'counsel-org-goto-all))

;;; Org Indirect Buffer
(setq org-indirect-buffer-display 'current-window)
;; Some advice to automatically switch to a new indirect buffer upon creation
;; (defadvice org-tree-to-indirect-buffer (after org-tree-to-indirect-buffer-after activate) (other-window 1))

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

;;;; Clone and Narrow
(defun cpm/clone-buffer-and-narrow ()
  "Clone buffer and narrow outline tree"
  (interactive)
  (let ((buf (clone-indirect-buffer-other-window nil nil)))
    (with-current-buffer buf
      (cond ((derived-mode-p 'org-mode)
             (org-narrow-to-element))
            ((derived-mode-p 'markdown-mode)
             (markdown-narrow-to-subtree))))
    (switch-to-buffer-other-window buf)))

;;;; Goto Org Files
(defun cpm/goto-org-files ()
  "goto org-files directory"
  (interactive)
  (require 'projectile)
  (projectile-find-file-in-directory org-directory))
;; (ido-find-file-in-dir org-directory))
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

;; FIXME: neither of these functions work right now for some reason.
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
  (require 'ox-pandoc)
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


;;;; Org Table Wrap
;; see https://emacs.stackexchange.com/a/30871/11934
(defun org-table-wrap-to-width (width)
  "Wrap current column to WIDTH."
  (interactive (list (read-number "Enter column width: ")))
  (org-table-check-inside-data-field)
  (org-table-align)

  (let (cline (ccol (org-table-current-column)) new-row-count (more t))
    (org-table-goto-line 1)
    (org-table-goto-column ccol)

    (while more
      (setq cline (org-table-current-line))

      ;; Cut current field
      (org-table-copy-region (point) (point) 'cut)

      ;; Justify for width
      (setq org-table-clip
            (mapcar 'list (org-wrap (caar org-table-clip) width nil)))

      ;; Add new lines and fill
      (setq new-row-count (1- (length org-table-clip)))
      (if (> new-row-count 0)
          (org-table-insert-n-row-below new-row-count))
      (org-table-goto-line cline)
      (org-table-goto-column ccol)
      (org-table-paste-rectangle)
      (org-table-goto-line (+ cline new-row-count))

      ;; Move to next line
      (setq more (org-table-goto-line (+ cline new-row-count 1)))
      (org-table-goto-column ccol))

    (org-table-goto-line 1)
    (org-table-goto-column ccol)))

(defun org-table-insert-n-row-below (n)
  "Insert N new lines below the current."
  (let* ((line (buffer-substring (point-at-bol) (point-at-eol)))
         (new (org-table-clean-line line)))
    ;; Fix the first field if necessary
    (if (string-match "^[ \t]*| *[#$] *|" line)
        (setq new (replace-match (match-string 0 line) t t new)))
    (beginning-of-line 2)
    (setq new
          (apply 'concat (make-list n (concat new "\n"))))
    (let (org-table-may-need-update) (insert-before-markers new))  ;;; remove?
    (beginning-of-line 0)
    (re-search-forward "| ?" (point-at-eol) t)
    (and (or org-table-may-need-update org-table-overlay-coordinates) ;;; remove?
         (org-table-align))
    (org-table-fix-formulas "@" nil (1- (org-table-current-dline)) n)))
;;;; Org to Beamer PDF
(defun cpm/org-export-to-beamer-pdf-open ()
  "Export org subtree to beamer pdf and open"
  (interactive)
  (universal-argument)
  (universal-argument-more)
  (org-open-file (org-beamer-export-to-pdf nil t)))

;;;; Org to beamer slides or handout
(defun cpm/org-export-beamer-presentation ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (org-beamer-export-to-pdf nil t nil nil '(:latex-class "beamer-presentation"))))

;; (org-open-file (org-beamer-export-to-pdf nil t nil nil '(:latex-class "beamer-presentation")))))

(defun cpm/org-export-beamer-handout ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (org-open-file (org-beamer-export-to-pdf nil t nil nil '(:latex-class "beamer-handout")))))


;;;; Org Export Last Subtree
;; bind f5 to keyboard macro of export-last-subtree
(fset 'export-last-subtree
      "\C-u\C-c\C-e")

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "<f5>") 'export-last-subtree)))




;;;; Org Tag Selection

(defun cpm/org-select-tags-completing-read ()
  "Select tags to add to headline."
  (interactive)
  (let* ((current (org-get-tags (point)))
         (selected (completing-read-multiple "Select org tag(s): " (org-get-buffer-tags))))
    (alet (-uniq (append (-difference current selected)
                         (-difference selected current)))
      (org-set-tags it))))

;;;; Org Copy Link
;; see https://emacs.stackexchange.com/a/63038/11934
(defun cpm/org-link-copy-at-point ()
  (interactive)
  (save-excursion
    (let* ((ol-regex "\\[\\[.*?:.*?\\]\\(\\[.*?\\]\\)?\\]")
           (beg (re-search-backward "\\[\\["))
           (end (re-search-forward ol-regex))
           (link-string (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
      (kill-new link-string)
      (message "Org link %s is copied." link-string))))

;;;; Remove Org Links
;; https://emacs.stackexchange.com/a/10714/11934
(defun cpm/org-replace-link-by-link-description ()
  "Replace an org link by its description or, if empty, its address"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2)
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))

;;;; Org Contrib
(use-package org-contrib
  :straight t
  :after org)

;;; End Org Setup
(provide 'setup-org)
