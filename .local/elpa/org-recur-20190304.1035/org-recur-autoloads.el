;;; org-recur-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-recur" "org-recur.el" (0 0 0 0))
;;; Generated autoloads from org-recur.el

(autoload 'org-recur-finish "org-recur" "\
Reschedule an `org-mode' task according to its org-recur date string.
The org-recur syntax is '|DATE|', where DATE can be either an
absolute date or more commonly a delta, e.g. a task heading
containing '|+2|' indicates to `org-recur-finish' to reschedule
the task to two days from now.

All date strings supported by `org-read-date' are available. Also
available is 'wkdy' (customizable with `org-recur-weekday') which
schedules the task to the next weekday (customizable with
`org-recur-weekday-recurrence'). Also possible is the 'N1,N2,...'
syntax, wherein the earliest date string among the set of N is
selected. For example, '|Mon,Fri|' indicates that the task should
recur every Monday and Friday, and the soonest among them is
chosen when calling `org-recur-finish'.

If the task does not contain org-recur syntax, then depending on
the values of `org-recur-finish-done' and
`org-recur-finish-archive' change the task status to DONE and/or
archive it, respectively

\(fn)" t nil)

(autoload 'org-recur-mode "org-recur" "\
Highlight org-recur dates in org-mode.

With a prefix argument ARG, enable org-recur mode if ARG is
positive, and disable it otherwise. If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'.

\(fn &optional ARG)" t nil)

(autoload 'org-recur-agenda-mode "org-recur" "\
Highlight org-recur dates in `org-agenda'.

With a prefix argument ARG, enable org-recur-agenda mode if ARG
is positive, and disable it otherwise. If called from Lisp,
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-recur" '("org-recur-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-recur-autoloads.el ends here
