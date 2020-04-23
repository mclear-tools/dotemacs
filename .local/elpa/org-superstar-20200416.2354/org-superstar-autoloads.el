;;; org-superstar-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-superstar" "org-superstar.el" (0 0 0 0))
;;; Generated autoloads from org-superstar.el

(put 'org-superstar-leading-bullet 'safe-local-variable #'char-or-string-p)

(autoload 'org-superstar-toggle-lightweight-lists "org-superstar" "\
Toggle syntax checking for plain list items.

Disabling syntax checking will cause Org Superstar to display
lines looking like plain lists (for example in code) like plain
lists.  However, this may cause significant speedup for org files
containing several hundred list items.

\(fn)" t nil)

(autoload 'org-superstar-mode "org-superstar" "\
Use UTF8 bullets for headlines and plain lists.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-superstar" '("org-superstar-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-superstar-autoloads.el ends here
