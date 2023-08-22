;;; cpm-setup-org.el --- personal setup  -*- lexical-binding: t -*-

;; Author: Colin McLear
;; Maintainer: Colin McLear
;; Version: 0.1

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Personal setup for org-mode, including capture templates.
;; More basic configuration goes in the lem setup files.

;;; Code:
;;;; Org Capture
;;;;; Capture Functions

;; Add date to captured items
(defun lem-add-property-with-date-captured ()
  "Add CREATED property to the current item."
  (interactive)
  (org-set-property "CREATED" (format-time-string "%F")))
(add-hook 'org-capture-before-finalize-hook 'lem-add-property-with-date-captured)

;; Add newline to captured items
(defun lem-org-capture-newline-at-end ()
  (goto-char (point-max))
  (insert "\n"))
(add-hook 'org-capture-prepare-finalize 'lem-org-capture-newline-at-end)

;;;;; Capture From Browser
;; This is an unwieldy check but it works!
;; https://apple.stackexchange.com/questions/313454/applescript-find-the-users-set-default-browser
(defun cpm-capture-browser ()
  "Check which browser---firefox or safari---is default and set grab link accordingly."
  (if (string= (shell-command-to-string "defaults read ~/Library/Preferences/com.apple.LaunchServices/com.apple.launchservices.secure | awk -F'\"' '/http;/{print window[(NR)-1]}{window[NR]=$2}'") "org.mozilla.firefox\n")
      (grab-mac-link 'firefox 'org)
    (grab-mac-link 'safari 'org)))

;;;;; Capture templates
(setq org-capture-templates
      ;; Note the ` and , to get concat to evaluate properly
      `(("c" "Capture" entry (file ,(concat org-directory "/inbox.org"))
         "* TODO %?\n %i" :empty-lines 1)

        ("j" "Journal"
         entry
         (file+olp+datetree ,(concat org-directory "/journal.org"))
         "**** %<%H:%M>\n%?" :empty-lines 1)

        ("l" "A link, for reading later" entry (file ,(concat org-directory "/inbox.org"))
         "* %? :link: \n%(cpm-capture-browser)"  :empty-lines 1)

        ("m" "eMail Workflow")
        ("ms" "Schedule" entry (file+olp ,(concat org-directory "/Mail.org") "Respond")
         "* TODO Respond to %:from | %:subject :email: \nSCHEDULED:%t\n\nMessage: %a\n  %i" :immediate-finish t  :empty-lines 1)
        ("mr" "Respond" entry (file+olp ,(concat org-directory "/Mail.org") "Respond")
         "* TODO Respond to %:from | %:subject :email: \nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))  SCHEDULED:%t\n\nMessage: %a\n  %i" :immediate-finish t  :empty-lines 1)
        ("ml" "Link" entry (file+olp ,(concat org-directory "/Mail.org") "Mail Link")
         "* %:from | %:subject :email: \n%(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\nMessage: %a\n  %i" :immediate-finish t  :empty-lines 1)

        ("r" "Reference" entry (file ,(concat org-directory "/reference.org"))
         "* %?"  :empty-lines 1)

        ("s" "Music Review" entry  (file ,(concat org-directory "/music.org"))
         ,(concat "\n** Artist - Album :Artist:Genre: %?\n"
	              "  - Date: %T\n"
	              "  - Listened While: \n"
	              "  - Suggested By: \n"
	              "  - Standout Tracks: \n"
	              "  - Rating: /10\n"
	              "  - Thoughts: \n"))

        ("M" "UNL Merit Review" entry (file ,(concat org-directory "/merit-reviews.org"))
         (file ,(concat org-directory "/templates/merit-review-template.org")))

        ("w" "Review: Weekly Review" entry (file+datetree ,(concat org-directory "/reviews.org"))
         (file ,(concat org-directory "/templates/weekly_review_template.org")))

        ("R" "Referee report" entry (file+datetree ,(concat org-directory "/referee-reports.org"))
         (file ,(concat org-directory "/templates/referee-report-template.org")))))

;;;;; Org Journal Capture
;; Tell emacs what you're doing a few times a day. Depends on a shell script run
;; in the background. See ~/bin/scripts/emacs_journal.sh I got the idea from
;; Diego Berroca; see
;; http://www.diegoberrocal.com/blog/2015/08/19/org-protocol/. Hat tip to stack
;; overflow for help on hooks for the created frame.
;; https://stackoverflow.com/q/23517372

(defun lem-org-journal ()
  (interactive) (org-capture nil "j"))

;;;;; Alfred Capture Workflow
;; Help alfred and org-capture play nice. Courtesy of
;; http://orgmode.org/worg/org-contrib/alfred-org-capture.html with some slight
;; modifications. Current functions also from
;; https://github.com/Isimoro/org-global-capture.el/blob/master/org-global-capture.el

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

;;;; Personal Agenda Config

;;;;; Agenda Icons
;; https://florianwinkelbauer.com/posts/2020-07-13-org-agenda-icons/
(defun cpm-agenda-icon-material (name)
  "Returns an all-the-icons-material icon"
  (list (all-the-icons-material name)))
(defun cpm-agenda-icon-faicon (name)
  "Returns an all-the-icons-faicon icon"
  (list (all-the-icons-faicon name)))

(setq org-agenda-category-icon-alist
      `(("Birthday"    ,(cpm-agenda-icon-material "cake")            nil nil :ascent center)
        ("Anniversary" ,(cpm-agenda-icon-material "favorite")        nil nil :ascent center)
        ("Diary"       ,(cpm-agenda-icon-faicon   "book")            nil nil :ascent center)
        ("Inbox"       ,(cpm-agenda-icon-material "inbox")           nil nil :ascent center)
        ("Todo"        ,(cpm-agenda-icon-material "check_circle")    nil nil :ascent center)
        ("Mail"        ,(cpm-agenda-icon-material "email")           nil nil :ascent center)
        ("Reading"     ,(cpm-agenda-icon-material "library_books")   nil nil :ascent center)
        ("Service"     ,(cpm-agenda-icon-faicon   "th-list")         nil nil :ascent center)
        ("Teaching"    ,(cpm-agenda-icon-material "school")          nil nil :ascent center)
        ("Writing"     ,(cpm-agenda-icon-faicon   "pencil-square-o") nil nil :ascent center)
        ("Advising"    ,(cpm-agenda-icon-faicon   "users")           nil nil :ascent center)
        ("Admin"       ,(cpm-agenda-icon-material "folder")          nil nil :ascent center)))

;;;;; Agenda Faces
(setopt org-agenda-fontify-priorities 'cookies)
(with-eval-after-load 'org-modern
  (when (lem-font-available-p "SF Pro Text")
    (setq org-modern-priority
          `((?A . ,(propertize "􀂔" 'face 'lambda-red))
            (?B . ,(propertize "􀂖" 'face 'lambda-orange))
            (?C . ,(propertize "􀂘" 'face 'lambda-purple))))))

;;;;; Agenda Date Display
(setq org-agenda-format-date 'my-org-agenda-format-date-aligned)

(defun my-org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date 1 nil))
         (day (cadr date))
         (day-of-week (calendar-day-of-week date))
         (month (car date))
         (monthname (calendar-month-name month 1))
         (year (nth 2 date))
         (iso-week (org-days-to-iso-week
                    (calendar-absolute-from-gregorian date)))
         (weekyear (cond ((and (= month 1) (>= iso-week 52))
                          (1- year))
                         ((and (= month 12) (<= iso-week 1))
                          (1+ year))
                         (t year)))
         (weekstring (if (= day-of-week 1)
                         (format " W%02d" iso-week)
                       "")))
    (format " %-2s. %2d %s"
            dayname day monthname)))

;;;;; Agenda Search
;; https://orgmode.org/worg/org-tutorials/advanced-searching.html
;; https://github.com/psamim/dotfiles/blob/master/doom/config.el
;; https://emacs.stackexchange.com/questions/38742/implement-scheduling-as-suggested-in-deep-work-using-emacs-org-mode
(setq org-agenda-sorting-strategy '((agenda habit-down time-up ts-up
                                            priority-down category-keep)
                                    (todo priority-down category-keep)
                                    (tags priority-down category-keep)
                                    (search category-keep)))

;; https://github.com/d12frosted/d12frosted.io/issues/15#issuecomment-908260553
(require 'cl-lib)
(defvar org-agenda--todo-keyword-regex
  (cl-reduce (lambda (cur acc)
               (concat acc "\\|" cur))
             (mapcar (lambda (entry) (concat "\\* " entry))
                     '("TODO" "NEXT" "WAIT" "REVIEW" "BLOCKED" "DONE" "CANCELED" "REFILE" "SOMEDAY" "MAYBE")))
  "Regex which filters all TODO keywords")

(defun org-agenda--calculate-files-for-regex (regex)
  "Yields a fresh array with all files containing todos which match REGEX.
Uses grep to discover all files containing anything stored in
org-agenda--todo-keyword-regex."
  (let ((files
         (cl-remove-if #'file-directory-p
                       (split-string
                        (shell-command-to-string
                         (concat "grep --include=\"*.org\" --exclude-dir=\"archive\" -rl -e '" regex "' ~/Documents/notes/denotes/"))
                        "\n"))))
    (cl-concatenate
     'list
     files
     `(,(concat org-directory "/admin.org")
       ,(concat org-directory "/advising.org")
       ,(concat org-directory "/conferences.org")
       ,(concat org-directory "/grants.org")
       ,(concat org-directory "/inbox.org")
       ,(concat org-directory "/mail.org")
       ,(concat org-directory "/music.org")
       ,(concat org-directory "/reading.org")
       ,(concat org-directory "/service.org")
       ,(concat org-directory "/someday.org")
       ,(concat org-directory "/teaching.org")
       ,(concat org-directory "/todo.org")
       ,(concat org-directory "/writing.org")))))


;;;; Delete Empty Agenda Blocks
;; Not using currently as it is incompatible with a compact agenda
(defun org-agenda-delete-empty-blocks ()
  "Remove empty agenda blocks.
  A block is identified as empty if there are fewer than 2
  non-empty lines in the block (excluding the line with
  `org-agenda-block-separator' characters)."
  (when org-agenda-compact-blocks
    (user-error "Cannot delete empty compact blocks"))
  (setq buffer-read-only nil)
  (save-excursion
    (goto-char (point-min))
    (let* ((blank-line-re "^\\s-*$")
           (content-line-count (if (looking-at-p blank-line-re) 0 1))
           (start-pos (point))
           (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
      (while (and (not (eobp)) (forward-line))
        (cond
         ((looking-at-p block-re)
          (when (< content-line-count 2)
            (delete-region start-pos (1+ (point-at-bol))))
          (setq start-pos (point))
          (forward-line)
          (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
         ((not (looking-at-p blank-line-re))
          (setq content-line-count (1+ content-line-count)))))
      (when (< content-line-count 2)
        (delete-region start-pos (point-max)))
      (goto-char (point-min))
      ;; The above strategy can leave a separator line at the beginning
      ;; of the buffer.
      (when (looking-at-p block-re)
        (delete-region (point) (1+ (point-at-eol))))))
  (setq buffer-read-only t))
;; (add-hook 'org-agenda-finalize-hook #'org-agenda-delete-empty-blocks)


;;;;; Agenda Custom Commands
;; https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
;; On priorities https://stackoverflow.com/a/66582772
;; Prot's custom commands https://protesilaos.com/codelog/2021-12-09-emacs-org-block-agenda/
;; Bierber's walk-through https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
;; See also:
;; https://www.reddit.com/r/emacs/comments/hnf3cw/my_orgmode_agenda_much_better_now_with_category/
;; https://github.com/psamim/dotfiles/blob/master/doom/config.el

;; NOTE: default priority must be set to 68 to see *only* explicit priorities A-C
(setopt org-priority-default 68)
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         (;; Timed daily agenda with due items
          (agenda "" ((org-agenda-span 'day)
                      (org-agenda-include-diary t)
                      (org-agenda-prefix-format " %-2i  %?-2 t%s")
                      (org-agenda-time-grid
                       (quote
                        ((today require-timed remove-match) ()
                         " ┄┄┄┄┄ " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))
                      (org-agenda-sorting-strategy '((agenda time-up priority-down tag-up category-keep)
                                                     (todo priority-down category-keep)
                                                     (tags priority-down category-keep)
                                                     (search category-keep)))
                      (org-agenda-entry-types '(:deadline* :scheduled*))))
          ;; Due!
          (tags-todo "+DEADLINE<=\"<+2d>\""
                     ((org-agenda-sorting-strategy '(priority-down time-up tag-up))
                      (org-agenda-overriding-header
                       (concat "\n" "    " "⸺ " "Due!" " ⸺" ))))
          ;; Priorities
          (tags "+PRIORITY=\"A\"|+PRIORITY=\"B\"|+PRIORITY=\"C\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-tags-match-list-sublevels nil)
                 (org-agenda-overriding-header
                  (concat "\n" "    " "⸺ " "Priority Tasks" " ⸺" ))))
          ;; Scheduled
          (tags-todo "+SCHEDULED<=\"<+2w>\"-PRIORITY=\"A\"-PRIORITY=\"B\"-PRIORITY=\"C\"-DEADLINE<=\"<+2d>\""
                     ((org-agenda-overriding-header
                       (concat "\n" "    " "⸺ " "Scheduled/Upcoming" " ⸺"))))

          ;; Areas (Grading, Teaching, Research, Editing)
          ;; Skip priorities
          (tags "+grading"
                ((org-agenda-overriding-header
                  (concat "\n" "    "  "⸺ " "Grading" " ⸺"))
                 (org-agenda-skip-function
                  `(org-agenda-skip-entry-if
                    'regexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))))
          (tags-todo "+teaching-grading"
                     ((org-agenda-overriding-header
                       (concat "\n" "    " "⸺ " "Teaching" " ⸺"))
                      (org-agenda-skip-function
                       `(org-agenda-skip-entry-if
                         'regexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))))
          (tags-todo "writing|book|paper"
                     ((org-agenda-overriding-header
                       (concat "\n" "    "  "⸺ " "Research & Writing" " ⸺"))
                      (org-agenda-skip-function
                       `(org-agenda-skip-entry-if
                         'regexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))))
          (tags-todo "editing"
                     ((org-agenda-overriding-header
                       (concat "\n" "    "  "⸺ " "Editing" " ⸺"))
                      (org-agenda-skip-function
                       `(org-agenda-skip-entry-if
                         'regexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest)))))))
         ;; Options
         (;; files
          (org-agenda-files
           (org-agenda--calculate-files-for-regex org-agenda--todo-keyword-regex))
          ;; skip
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
          (org-agenda-repeating-timestamp-show-all nil)
          (org-agenda-skip-scheduled-if-deadline-is-shown t)
          (org-agenda-skip-scheduled-if-done t)
          (org-agenda-skip-deadline-if-done t)
          (org-agenda-skip-timestamp-if-done t)
          (org-agenda-skip-timestamp-if-deadline-is-shown t)
          (org-agenda-prefer-last-repeat nil)
          (org-agenda-show-future-repeats nil)
          ;; time
          (org-agenda-time-leading-zero t)
          (org-agenda-timegrid-use-ampm nil)
          (org-agenda-current-time-string "–––––––––––––– Now")
          ;; display
          (org-agenda-remove-tags t)
          (org-agenda-compact-blocks t)

          (org-agenda-scheduled-leaders '("" ""))))))
;;;; Org Custom Keybinds
;; Vim movement
(with-eval-after-load 'org-agenda
  (bind-key "j"  #'org-agenda-next-item org-agenda-mode-map)
  (bind-key "k" #'org-agenda-previous-item org-agenda-mode-map))

(defvar cpm-setup--org-movement-bindings
  '((up    . "k")
    (down  . "j")
    (left  . "h")
    (right . "l"))
  "AList of normal keys to use for arrows.")

(defun cpm-setup-org--populate-calendar-bindings ()
"Bindings for easy date selection."

;; Easy month shift
(define-key org-read-date-minibuffer-local-map
  (kbd "C-f") (lambda () (interactive)
                (org-eval-in-calendar
                 '(calendar-scroll-left-three-months 1))))
(define-key org-read-date-minibuffer-local-map
  (kbd "C-b") (lambda () (interactive)
                (org-eval-in-calendar
                 '(calendar-scroll-right-three-months 1))))

(let-alist cpm-setup--org-movement-bindings
  ;; Week movement
  (define-key org-read-date-minibuffer-local-map
    (kbd (concat "M-" .left)) (lambda () (interactive)
                                (org-eval-in-calendar '(calendar-backward-day 1))))
  (define-key org-read-date-minibuffer-local-map
    (kbd (concat "M-" .right)) (lambda () (interactive)
                                 (org-eval-in-calendar '(calendar-forward-day 1))))
  (define-key org-read-date-minibuffer-local-map
    (kbd (concat "M-" .up)) (lambda () (interactive)
                              (org-eval-in-calendar '(calendar-backward-week 1))))
  (define-key org-read-date-minibuffer-local-map
    (kbd (concat "M-" .down)) (lambda () (interactive)
                                (org-eval-in-calendar '(calendar-forward-week 1))))
  ;; Month movement
  (define-key org-read-date-minibuffer-local-map
    (kbd (concat "M-" (capitalize .left))) (lambda () (interactive)
                                             (org-eval-in-calendar '(calendar-backward-month 1))))
  (define-key org-read-date-minibuffer-local-map
    (kbd (concat "M-" (capitalize .right))) (lambda () (interactive)
                                              (org-eval-in-calendar '(calendar-forward-month 1))))
  ;; Year movement
  (define-key org-read-date-minibuffer-local-map
    (kbd (concat "M-" (capitalize .up))) (lambda () (interactive)
                                           (org-eval-in-calendar '(calendar-backward-year 1))))
  (define-key org-read-date-minibuffer-local-map
    (kbd (concat "M-" (capitalize .down))) (lambda () (interactive)
                                             (org-eval-in-calendar '(calendar-forward-year 1))))))

;;;; Org Indirect Buffer
(setq org-indirect-buffer-display 'current-window)
;; Some advice to automatically switch to a new indirect buffer upon creation
(defadvice org-tree-to-indirect-buffer (after org-tree-to-indirect-buffer-after activate) (other-window 1))

;;;; Org Tags
(setq org-tag-alist '((:grouptags)
                      ("emacs" . ?e)
                      (:endgrouptag)
                      ("@phone" . ?p)
                      ("@work" . ?s)
                      ("@home" . ?h)
                      ("email" . ?m)
                      ("book" . ?b)
                      ("service" . ?s)))
(setq org-fast-tag-selection-single-key t)

;;;; Org Template Expansions
(setq new-structure-template-alist
      '(("dc" . "commentbox")
        ("dp" . "paperbox")
        ("dq" . "quotebox")
        ("ds" . "subtitle")
        ("el" . "src emacs-lisp")
        ("n" . "notes")
        ("t" . "COMMENT \TODO:")))
(dolist (ele new-structure-template-alist)
  (add-to-list 'org-structure-template-alist ele))

;;;; Readability/UI
;; Make agenda more readable
(defun cpm-setup-org-agenda--set-line-spacing ()
  (setq-local default-text-properties '(line-spacing 0.10 line-height 1)))

(defun cpm-org--set-extra-faces ()
  "Make prop, etc., faces smaller."
  (mapc ;; This sets the fonts to a smaller size
   (lambda (face)
     (set-face-attribute face nil :height 0.8))
   (list
    'org-drawer
    'org-special-keyword
    'org-property-value)))

;;;; Org Hooks
(defun cpm-org-mode-setup ()
  "Functions to add to org-mode-hook."
  (visual-line-mode 1)
  (cpm-org--set-extra-faces)
  (cpm-setup-org--populate-calendar-bindings))

(defun cpm-org-agenda-mode-setup ()
  "Functions to add to org-agenda-mode-hook."
  (cpm-setup-org-agenda--set-line-spacing)
  (hl-line-mode))

(add-hook 'org-mode-hook #'cpm-org-mode-setup)
(add-hook 'org-agenda-mode-hook #'cpm-org-agenda-mode-setup)

;;;; Org-BB Export
;; Export for BB format; useful for WorldAnvil
(use-package ox-bb
  :disabled
  :after org)



;;; Provide
(provide 'cpm-setup-org)
;;; cpm-setup-org.el ends here
