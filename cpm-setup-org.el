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
;; Defer loading
(with-eval-after-load 'org

;;; Org Capture
;;;; Capture Settings

  ;; This is an unwieldy check but it works!
  ;; https://apple.stackexchange.com/questions/313454/applescript-find-the-users-set-default-browser
  (defun cpm-capture-browser ()
    "Check which browser---firefox or safari---is default and set grab link accordingly."
    (if (string= (shell-command-to-string "defaults read ~/Library/Preferences/com.apple.LaunchServices/com.apple.launchservices.secure | awk -F'\"' '/http;/{print window[(NR)-1]}{window[NR]=$2}'") "org.mozilla.firefox\n")
        (grab-mac-link 'firefox 'org)
      (grab-mac-link 'safari 'org)))

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
           "* TODO Comment re: %:fromname about %:subject 📧 \nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%?  %i"  :empty-lines 1)
          ("mm" "Remember" entry (file+olp ,(concat org-directory "Mail.org") "Remember")
           "* TODO %:subject 📧 \nSCHEDULED:%t\nFrom: %:from \nMessage: %a \n\n  %i" :immediate-finish t  :empty-lines 1)
          ("mr" "Respond" entry (file+olp ,(concat org-directory "Mail.org") "Respond")
           "* TODO Respond to %:from | %:subject 📧 \nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\nMessage: %a\n  %i" :immediate-finish t  :empty-lines 1)

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

  ;; Add date to captured items
  (defun add-property-with-date-captured ()
    "Add DATE_CAPTURED property to the current item."
    (interactive)
    (org-set-property "DATE_CAPTURED" (format-time-string "%F %A")))

  (add-hook 'org-capture-before-finalize-hook 'add-property-with-date-captured)

  ;; Add newline to captured items
  (defun lem-org-capture-newlines-at-end ()
    (goto-char (point-max))
    (insert "\n\n"))
  (add-hook 'org-capture-prepare-finalize 'lem-org-capture-newlines-at-end)

;;;; Org Journal Capture
  ;; Tell emacs what you're doing a few times a day. Depends on a
  ;; [[/Users/roambot/bin/scripts/emacs_journal.sh][shell script]] run in the
  ;; background. I got the idea from
  ;; [[http://www.diegoberrocal.com/blog/2015/08/19/org-protocol/][Diego Berrocal]].
  ;; Hat tip to
  ;; [[http://stackoverflow.com/questions/23517372/hook-or-advice-when-aborting-org-capture-before-template-selection][stack
  ;; overflow]] for help on hooks for the created frame.

  (defun lem-org-journal ()
    (interactive) (org-capture nil "j"))

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

;;; Org Indirect Buffer
  (setq org-indirect-buffer-display 'current-window)
  ;; Some advice to automatically switch to a new indirect buffer upon creation
  (defadvice org-tree-to-indirect-buffer (after org-tree-to-indirect-buffer-after activate) (other-window 1))



;;; Org Entities
  (setq org-entities-user
        '(("nec" "\Box" nil "◻" "" "" "◻")
          ("pos" "\Diamond" nil "◇" "" "" "◇")
          ("space" "~" nil "&nbsp;" " " " " " ")))

;;; Org Tags
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
  (setq org-fast-tag-selection-single-key t)


;;; Org Template Expansions
  (setq new-structure-template-alist
        '(("el" . "src emacs-lisp")
          ("t" . "COMMENT \TODO:")
          ("b" . "REVEAL: split")
          ("f" . "ATTR_REVEAL: :frag (appear)")
          ("n" . "notes")))
  (dolist (ele new-structure-template-alist)
    (add-to-list 'org-structure-template-alist ele))

;;; Org Scheduling Keybinds
  (defvar cpm-setup--org-movement-bindings
    '((up . "k")
      (down . "j")
      (left . "h")
      (right . "l"))
    "AList of normal keys to use for arrows.
This can be used by non-qwerty users who don't use hjkl.")

  (defun cpm-setup-org--populate-calendar-bindings ()
    "Bindings for easy date selection."
    (define-key org-read-date-minibuffer-local-map
      (kbd "C-f") (lambda () (interactive)
                    (org-eval-in-calendar
                     '(calendar-scroll-left-three-months 1))))
    (define-key org-read-date-minibuffer-local-map
      (kbd "C-b") (lambda () (interactive)
                    (org-eval-in-calendar
                     '(calendar-scroll-right-three-months 1))))
    (let-alist cpm-setup--org-movement-bindings
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
      (define-key org-read-date-minibuffer-local-map
        (kbd (concat "M-" (capitalize .left))) (lambda () (interactive)
                                                 (org-eval-in-calendar '(calendar-backward-month 1))))
      (define-key org-read-date-minibuffer-local-map
        (kbd (concat "M-" (capitalize .right))) (lambda () (interactive)
                                                  (org-eval-in-calendar '(calendar-forward-month 1))))
      (define-key org-read-date-minibuffer-local-map
        (kbd (concat "M-" (capitalize .up))) (lambda () (interactive)
                                               (org-eval-in-calendar '(calendar-backward-year 1))))
      (define-key org-read-date-minibuffer-local-map
        (kbd (concat "M-" (capitalize .down))) (lambda () (interactive)
                                                 (org-eval-in-calendar '(calendar-forward-year 1))))))

  (add-hook 'org-mode-hook #'cpm-setup-org--populate-calendar-bindings)

;;; Org Hooks
  (defun cpm-org-mode-hooks ()
    "Functions to add to org-mode-hook."
    (visual-line-mode 1))
  ;; Make agenda more readable
  (defun cpm-setup-org-agenda--set-line-spacing ()
    (setq-local default-text-properties '(line-spacing 0.20 line-height 1.20)))
  (defun cpm-org--set-extra-faces ()
    "Make prop, etc., faces smaller."
    (mapc ;; This sets the fonts to a smaller size
     (lambda (face)
       (set-face-attribute face nil :height 0.8))
     (list
      'org-drawer
      'org-special-keyword
      'org-property-value)))

  (add-hook 'org-agenda-mode-hook #'cpm-setup-org-agenda--set-line-spacing)
  (add-hook 'org-agenda-mode-hook #'cpm-org--set-extra-faces)
  (add-hook 'org-mode-hook #'cpm-org-mode-hooks)

;;; Delete Empty Blocks
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
  (add-hook 'org-agenda-finalize-hook #'org-agenda-delete-empty-blocks)

;;; End load of personal org
  )

;;; Provide
(provide 'cpm-setup-org)
;;; cpm-setup-org.el ends here
