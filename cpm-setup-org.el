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
      `(("c" "Capture" entry (file ,(concat org-directory "inbox.org"))
         "* TODO %?\n %i" :empty-lines 1)

        ("j" "Journal"
         entry
         (file+olp+datetree ,(concat org-directory "journal.org"))
         "**** %<%H:%M>\n%?" :empty-lines 1)

        ("l" "A link, for reading later" entry (file ,(concat org-directory "inbox.org"))
         "* %? :link: \n%(cpm-capture-browser)"  :empty-lines 1)

        ("m" "eMail Workflow")
        ("mc" "Comment" entry (file+olp ,(concat org-directory "Mail.org") "Mail Comment")
         "* TODO Comment re: %:fromname about %:subject :email: \nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%?  %i"  :empty-lines 1)
        ("mm" "Remember" entry (file+olp ,(concat org-directory "Mail.org") "Remember")
         "* TODO %:subject :email: \nSCHEDULED:%t\nFrom: %:from \nMessage: %a \n\n  %i" :immediate-finish t  :empty-lines 1)
        ("mr" "Respond" entry (file+olp ,(concat org-directory "Mail.org") "Respond")
         "* TODO Respond to %:from | %:subject :email: \nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\nMessage: %a\n  %i" :immediate-finish t  :empty-lines 1)

        ("ml" "Link" entry (file+olp ,(concat org-directory "Mail.org") "Link")
         "* %:from | %:subject :email: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\nMessage: %a\n  %i" :immediate-finish t  :empty-lines 1)

        ("r" "Reference" entry (file ,(concat org-directory "reference.org"))
         "* %?"  :empty-lines 1)

        ("s" "Music Review" entry  (file ,(concat org-directory "music.org"))
         ,(concat "\n** Artist - Album :Artist:Genre: %?\n"
	              "  - Date: %T\n"
	              "  - Listened While: \n"
	              "  - Suggested By: \n"
	              "  - Standout Tracks: \n"
	              "  - Rating: /10\n"
	              "  - Thoughts: \n"))

        ("M" "UNL Merit Review" entry (file ,(concat org-directory "merit-reviews.org"))
         (file ,(concat org-directory "templates/merit-review-template.org")))

        ("w" "Review: Weekly Review" entry (file+datetree ,(concat org-directory "reviews.org"))
         (file ,(concat org-directory "templates/weekly_review_template.org")))

        ("R" "Referee report" entry (file+datetree ,(concat org-directory "referee-reports.org"))
         (file ,(concat org-directory "templates/referee-report-template.org")))))

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

;;;;; Custom Agenda

;; Vim movement
(with-eval-after-load 'org-agenda
  (bind-key "j"  #'org-agenda-next-item org-agenda-mode-map)
  (bind-key "k" #'org-agenda-previous-item org-agenda-mode-map))

;; Faces
(setq org-faces-easy-properties '((todo . :foreground)
                                  (tag  . :foreground)
                                  (priority . :foreground)))
(setq org-agenda-fontify-priorities 'cookies)
(setq org-priority-faces '((?A . (:foreground 'red :weight regular))
                           (?B . (:foreground 'orange :weight regular))
                           (?C . (:foreground 'yellow :weight regular))))
(with-eval-after-load 'org-modern
  (setq org-modern-priority (quote ((?A . "􀂔")
                                    (?B . "􀂖")
                                    (?C . "􀂘")))))
;;;;; Agenda Search
;; https://orgmode.org/worg/org-tutorials/advanced-searching.html
;; https://github.com/psamim/dotfiles/blob/master/doom/config.el
;; https://emacs.stackexchange.com/questions/38742/implement-scheduling-as-suggested-in-deep-work-using-emacs-org-mode
(setq org-agenda-sorting-strategy '((agenda habit-down time-up ts-up
                                            priority-down category-keep)
                                    (todo priority-down category-keep)
                                    (tags priority-down category-keep)
                                    (search category-keep)))
(setq org-agenda-files (list org-directory))

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
     '("~/Dropbox/org-files/admin.org"
       "~/Dropbox/org-files/advising.org"
       "~/Dropbox/org-files/conferences.org"
       "~/Dropbox/org-files/grants.org"
       "~/Dropbox/org-files/inbox.org"
       "~/Dropbox/org-files/mail.org"
       "~/Dropbox/org-files/music.org"
       "~/Dropbox/org-files/reading.org"
       "~/Dropbox/org-files/service.org"
       "~/Dropbox/org-files/someday.org"
       "~/Dropbox/org-files/teaching.org"
       "~/Dropbox/org-files/todo.org"
       "~/Dropbox/org-files/writing.org"))))
(setq org-agenda-files
      (org-agenda--calculate-files-for-regex org-agenda--todo-keyword-regex))

;;;;; Custom Commands
;; https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
;; On priorities https://stackoverflow.com/a/66582772
;; Prot's custom commands https://protesilaos.com/codelog/2021-12-09-emacs-org-block-agenda/
;; Bierber's walk-through https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
;; See also:
;; https://www.reddit.com/r/emacs/comments/hnf3cw/my_orgmode_agenda_much_better_now_with_category/
;; https://github.com/psamim/dotfiles/blob/master/doom/config.el

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         (;; Timed daily agenda with due items
          (agenda "" ((org-agenda-span 1)
                      (org-agenda-entry-types '(:timestamp :deadline* :scheduled*))))
          ;; Due!
          (tags-todo "+DEADLINE<=\"<+6d>\""
                     ((org-agenda-sorting-strategy '(priority-down time-up tag-up))
                      (org-agenda-overriding-header
                       (concat "\n" "    " "⸺ " "Due!" " ⸺" ))))
          ;; Priorities
          (tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-tags-match-list-sublevels t)
                 (org-agenda-overriding-header
                  (concat "\n" "    " "⸺ " "Priority Tasks" " ⸺" ))))
          ;; Scheduled
          (tags-todo "SCHEDULED>=\"<today>\""
                     ((org-agenda-overriding-header
                       (concat "\n" "    " "⸺ " "Scheduled/Upcoming" " ⸺"))))

          ;; Areas (Grading, Teaching, Research, Editing)
          (tags "+grading"
                ((org-agenda-overriding-header
                  (concat "\n" "    "  "⸺ " "Grading" " ⸺"))))
          (tags-todo "+teaching-grading"
                     ((org-agenda-overriding-header
                       (concat "\n" "    " "⸺ " "Teaching" " ⸺"))))
          (tags-todo "writing|book|paper"
                     ((org-agenda-overriding-header
                       (concat "\n" "    "  "⸺ " "Research & Writing" " ⸺"))))
          (tags-todo "editing"
                     ((org-agenda-overriding-header
                       (concat "\n" "    "  "⸺ " "Editing" " ⸺")))))
         ;; Options
         (;; files
          (org-agenda-files
           (org-agenda--calculate-files-for-regex org-agenda--todo-keyword-regex))
          ;; skip
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
          (org-agenda-skip-scheduled-if-deadline-is-shown t)
          (org-agenda-skip-scheduled-if-done t)
          (org-agenda-skip-deadline-if-done t)
          (org-agenda-skip-timestamp-if-done t)
          ;; time
          (org-agenda-time-leading-zero t)
          (org-agenda-timegrid-use-ampm nil)
          (org-agenda-current-time-string "–––––––––––––– Now")
          ;; display
          (org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (org-agenda-scheduled-leaders '("" ""))))))

;;; Org Scheduling Keybinds
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
      '(("el" . "src emacs-lisp")
        ("t" . "COMMENT \TODO:")
        ("n" . "notes")))
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

;;; Provide
(provide 'cpm-setup-org)
;;; cpm-setup-org.el ends here
