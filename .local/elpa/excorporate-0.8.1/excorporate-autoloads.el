;;; excorporate-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "excorporate" "excorporate.el" (0 0 0 0))
;;; Generated autoloads from excorporate.el

(autoload 'excorporate "excorporate" "\
Start Excorporate.
Prompt for a mail address to use for autodiscovery, with an
initial suggestion of `user-mail-address'.  However, if
`excorporate-configuration' is non-nil, `excorporate' will use
that without prompting.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "excorporate" '("exco")))

;;;***

;;;### (autoloads nil "excorporate-calendar" "excorporate-calendar.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from excorporate-calendar.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "excorporate-calendar" '("exco")))

;;;***

;;;### (autoloads nil "excorporate-calfw" "excorporate-calfw.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from excorporate-calfw.el

(autoload 'exco-calfw-show-day "excorporate-calfw" "\
Show meetings for the date specified by MONTH DAY YEAR.

\(fn MONTH DAY YEAR)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "excorporate-calfw" '("exco" "cfw:cp-set-contents-sources")))

;;;***

;;;### (autoloads nil "excorporate-diary" "excorporate-diary.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from excorporate-diary.el

(autoload 'excorporate-diary-enable "excorporate-diary" "\
Enable Excorporate diary support.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "excorporate-diary" '("exco")))

;;;***

;;;### (autoloads nil "excorporate-org" "excorporate-org.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from excorporate-org.el

(autoload 'exco-org-show-day "excorporate-org" "\
Show meetings for the date specified by MONTH DAY YEAR.

\(fn MONTH DAY YEAR)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "excorporate-org" '("exco")))

;;;***

;;;### (autoloads nil nil ("excorporate-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; excorporate-autoloads.el ends here
