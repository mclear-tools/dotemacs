;;; excorporate-diary.el --- Diary integration        -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Keywords: calendar

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Wrap interactive `diary-lib' functions so that they query the
;; Exchange server asynchronously, then display retrieved results
;; interleaved with local diary entries.

;;; Code:

(require 'diary-lib)
(require 'calendar)
(require 'icalendar)
(require 'appt)
(require 'excorporate)
(require 'nadvice)

;; FIXME: Add something like this to diary-lib.el.
(defun exco-diary-diary-make-entry (string &optional nonmarking file)
  "Insert a diary entry STRING which may be NONMARKING in FILE.
If omitted, NONMARKING defaults to nil and FILE defaults to
`diary-file'."
  (with-current-buffer (find-file-noselect (or file diary-file))
    (when (eq major-mode (default-value 'major-mode)) (diary-mode))
    (widen)
    (diary-unhide-everything)
    (goto-char (point-max))
    (when (let ((case-fold-search t))
	    (search-backward "Local Variables:"
			     (max (- (point-max) 3000) (point-min))
			     t))
      (beginning-of-line)
      (insert "\n")
      (forward-line -1))
    (insert
     (if (bolp) "" "\n")
     (if nonmarking diary-nonmarking-symbol "")
     string)))

;; FIXME: Have icalendar--add-diary-entry use the new diary-lib
;; function instead of diary-make-entry.
(defun exco-diary-icalendar--add-diary-entry-around (original &rest arguments)
  "Prevent whitespace workaround from selecting diary buffer.
Also prevent `diary-make-entry' from putting the diary file
where (other-buffer (current-buffer)) will return it."
  (cl-letf (((symbol-function #'find-file)
	     (symbol-function #'find-file-noselect))
	    ;; This override suppresses diary-make-entry's window
	    ;; and buffer manipulations.
	    ((symbol-function #'diary-make-entry)
	     (symbol-function #'exco-diary-diary-make-entry)))
    (apply original arguments)))
(advice-add #'icalendar--add-diary-entry :around
	    #'exco-diary-icalendar--add-diary-entry-around)

(defvar excorporate-diary-today-file
  "~/.emacs.d/excorporate/diary-excorporate-today"
  "The diary file where Excorporate should save today's meetings.
This file will be #include'd in `diary-file' by
`excorporate-diary-enable'.")

(defvar excorporate-diary-transient-file
  "~/.emacs.d/excorporate/diary-excorporate-transient"
  "The diary file where Excorporate should save retrieved meetings.
This file will be #include'd in `diary-file' by
`excorporate-diary-enable'.")

(defun exco-diary-initialize (today)
  "Initialize diary files used by Excorporate.
Run before retrieving diary entries from servers.  TODAY is t to
initialize for today's date, nil otherwise."
  ;; Keep today's entries if running on a day other than today.  If
  ;; retrieving results for today, delete results from days other than
  ;; today, in case the transient file (having been filled in on a
  ;; prior day) contains duplicate or stale results for today.
  (let ((files (if today
		   (list excorporate-diary-today-file
			 excorporate-diary-transient-file)
		 (list excorporate-diary-transient-file))))
    (dolist (file files)
      (let ((directory (file-name-directory file)))
	(unless (file-exists-p directory)
	  (make-directory directory))
	(with-current-buffer (find-file-noselect file)
	  (delete-region (point-min) (point-max))
	  ;; Do not call `save-buffer' to avoid any hooks from being
	  ;; run.  Otherwise `appt-update-list' in
	  ;; `write-file-functions' can cause an infinite
	  ;; connnection-callback loop.
	  (basic-save-buffer-1))))))

(defun exco-diary-insert-meeting (finalize
				  subject start _end _location
				  _main-invitees _optional-invitees
				  icalendar-text)
  "Insert a retrieved meeting into the diary.
See also the documentation for `exco-calendar-item-iterate'.  The
arguments are SUBJECT, a string, the subject of the meeting,
START, the start date and time in Emacs internal representation,
and ICALENDAR-TEXT, iCalendar text representing the meeting.
_END, _LOCATION, _MAIN-INVITEES, and _OPTIONAL-INVITEES are
unused.

Call FINALIZE after the meeting has been inserted."
  (when (not (string-match "^Cancel[l]?ed: " subject))
    ;; FIXME: Sometimes meetings are duplicated if they have
    ;; overlapping (and (diary-cyclic ...) (diary-block ...)) ranges,
    ;; e.g., on in the today file and one in the transient file.
    ;; Maybe we should de-duplicate them in the final display.  If the
    ;; meeting start time is sometime today then put it in today's
    ;; diary file, otherwise put it in the transient one.
    (let* ((time (decode-time (current-time)))
	   (now (list (elt time 3) (elt time 4) (elt time 5)))
	   (dawn (apply #'encode-time 0 0 0 now))
	   (dusk (time-add dawn (seconds-to-time 86400)))
	   (file (if (and (time-less-p dawn start) (time-less-p start dusk))
		     excorporate-diary-today-file
		   excorporate-diary-transient-file)))
      (with-temp-buffer
	(insert icalendar-text)
	(icalendar-import-buffer file t))))
  (funcall finalize))

;; Bound in appt-check.
(defvar appt-display-diary)

(defun exco-diary-diary-advice (today date advisee &rest arguments)
  "Advise `diary' and `diary-view-entries' to add Excorporate support.
TODAY is today's date in `calendar-current-date' format.  DATE is
the desired date to retrieve meetings for, in the same format.
ADVISEE is the original function being advised.  ARGUMENTS are
the arguments to the advisee."
  ;; FIXME: Currently numeric arguments to `diary' and
  ;; `diary-view-entries' are ignored.
  (exco-connection-iterate
   (lambda ()
     (message "Retrieving diary entries via Excorporate...")
     (exco-diary-initialize (calendar-date-equal today date)))
   (lambda (identifier callback)
     (cl-destructuring-bind (month day year) date
       (exco-get-meetings-for-day identifier month day year callback)))
   (lambda (identifier response finalizer)
     (exco-calendar-item-with-details-iterate identifier response
					      #'exco-diary-insert-meeting
					      finalizer))
   (lambda ()
     (apply advisee arguments)
     ;; Warning: It is crucial to set appt-display-diary to nil here,
     ;; so that diary advice isn't entered repeatedly (ultimately via
     ;; the `appt-update-list' hook in `write-file-functions'), which
     ;; would create a connection-callback loop.
     (let ((appt-display-diary nil))
       (appt-check t))
     (message "Done retrieving diary entries via Excorporate."))
   t)
  ;; Just return nil from this advice.  We eventually run the advisee
  ;; asynchronously so there is no way of providing the same return
  ;; value as the unadvised `diary' and `diary-view-entries'
  ;; functions.  Luckily they seem to only be used interactively, at
  ;; least within Emacs itself.
  nil)

(defun exco-diary-diary-around (original-diary &rest arguments)
  "Call `diary' asynchronously.
Retrieve diary entries via Excorporate before showing results.
ORIGINAL-DIARY is the original `diary' function, and ARGUMENTS
are the arguments to it."
  (let ((today (calendar-current-date))
	(date (calendar-current-date)))
    (apply #'exco-diary-diary-advice today date original-diary arguments)))

(defun exco-diary-diary-view-entries-override (&rest arguments)
  "Override `diary-view-entries' to make it asynchronous.
Retrieve diary entries via Excorporate before showing results.
ARGUMENTS are the arguments to `diary-view-entries'."
  (interactive "p")
  (diary-check-diary-file)
  (let ((today (calendar-current-date))
	(date (calendar-cursor-to-date t)))
    (apply #'exco-diary-diary-advice today date
	   #'diary-list-entries date arguments)))

;;;###autoload
(defun excorporate-diary-enable ()
  "Enable Excorporate diary support."
  (interactive)
  ;; Create the directory for Excorporate diary files if it doesn't
  ;; already exist.
  (exco-diary-initialize t)
  ;; Remove advice first so that `diary' will not be run by any save
  ;; hooks.
  (advice-remove #'diary #'exco-diary-diary-around)
  (advice-remove #'diary-view-entries #'exco-diary-diary-view-entries-override)
  (with-current-buffer (find-file-noselect diary-file)
    (dolist (file (list excorporate-diary-transient-file
			excorporate-diary-today-file))
      (save-excursion
	(goto-char (point-min))
	(when (not (re-search-forward
		    (concat "^ *" diary-include-string " *\"" file "\"") nil t))
	  (exco-diary-diary-make-entry
	   (concat diary-include-string " \"" file "\""))
	  (save-buffer)))))
  (advice-add #'diary :around #'exco-diary-diary-around)
  (advice-add #'diary-view-entries :override
	      #'exco-diary-diary-view-entries-override)
  (add-hook 'diary-list-entries-hook #'diary-sort-entries)
  (add-hook 'diary-list-entries-hook #'diary-include-other-diary-files)
  (appt-activate 1)
  (message "Excorporate diary support enabled."))

(defun excorporate-diary-disable ()
  "Disable Excorporate diary support."
  (interactive)
  (advice-remove #'diary #'exco-diary-diary-around)
  (advice-remove #'diary-view-entries #'exco-diary-diary-view-entries-override)
  (with-current-buffer (find-file-noselect diary-file)
    (dolist (file (list excorporate-diary-transient-file
			excorporate-diary-today-file))
      (save-excursion
	(goto-char (point-min))
	(when (search-forward
	       (concat diary-include-string " \"" file "\"") nil t)
	  (delete-region (progn (beginning-of-line) (point))
			 (progn (forward-line 1) (point)))
	  (save-buffer)))))
  (message "Excorporate diary support disabled."))

(provide 'excorporate-diary)

;;; excorporate-diary.el ends here
