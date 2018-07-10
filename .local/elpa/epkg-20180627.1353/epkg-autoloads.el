;;; epkg-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "epkg" "epkg.el" (0 0 0 0))
;;; Generated autoloads from epkg.el

(autoload 'epkg-update "epkg" "\
Update the Epkg database.

In the `epkg-repository', pull the master branch and reload
the Epkg database.  Return a connection to the updated Epkg
database.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "epkg" '("epkg")))

;;;***

;;;### (autoloads nil "epkg-desc" "epkg-desc.el" (0 0 0 0))
;;; Generated autoloads from epkg-desc.el

(autoload 'epkg-describe-package "epkg-desc" "\
Display the full documentation of PACKAGE.

\(fn PACKAGE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "epkg-desc" '("epkg-")))

;;;***

;;;### (autoloads nil "epkg-gelpa" "epkg-gelpa.el" (0 0 0 0))
;;; Generated autoloads from epkg-gelpa.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "epkg-gelpa" '("gelpa-")))

;;;***

;;;### (autoloads nil "epkg-list" "epkg-list.el" (0 0 0 0))
;;; Generated autoloads from epkg-list.el

(autoload 'epkg-list-packages "epkg-list" "\
Display a list of packages.

Respect option `epkg-list-exclude-types' unless a prefix argument
is used.

\(fn &optional ALL)" t nil)

(autoload 'epkg-list-matching-packages "epkg-list" "\
Display a list of packages whose summaries match REGEXP.

Respect option `epkg-list-exclude-types' unless a prefix argument
is used.

\(fn PATTERN &optional ALL)" t nil)

(autoload 'epkg-list-keyworded-packages "epkg-list" "\
Display a list of packages that have KEYWORD set.

Only keywords that are members of `finder-known-keywords' are
offered as completion candidates, but you can also enter other
keywords.

Respect option `epkg-list-exclude-types' unless a prefix argument
is used.

\(fn KEYWORD &optional ALL)" t nil)

(autoload 'epkg-list-packages-by-author "epkg-list" "\
Display a list of packages authored or maintained by AUTHOR.

AUTHOR may be a name or an email address.  Packages whose
Author(s) or Maintainer(s) header keywords contain that author
are listed.

Respect option `epkg-list-exclude-types' unless a prefix argument
is used.

\(fn AUTHOR &optional ALL)" t nil)

(autoload 'epkg-list-packages-of-type "epkg-list" "\
Display a list of all packages of a certain type.

To list all packages of a certain type as well as its subtypes
use `TYPE*' instead of just `TYPE'.

\(fn TYPE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "epkg-list" '("epkg-")))

;;;***

;;;### (autoloads nil "epkg-melpa" "epkg-melpa.el" (0 0 0 0))
;;; Generated autoloads from epkg-melpa.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "epkg-melpa" '("melpa-")))

;;;***

;;;### (autoloads nil "epkg-org" "epkg-org.el" (0 0 0 0))
;;; Generated autoloads from epkg-org.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "epkg-org" '("melpa-org-link" "epkg-")))

;;;***

;;;### (autoloads nil "epkg-schemata" "epkg-schemata.el" (0 0 0 0))
;;; Generated autoloads from epkg-schemata.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "epkg-schemata" '("epkg--db-table-schemata")))

;;;***

;;;### (autoloads nil "epkg-utils" "epkg-utils.el" (0 0 0 0))
;;; Generated autoloads from epkg-utils.el

(autoload 'epkg-find-file "epkg-utils" "\
Visit a file in a submodule of `epkg-repository'.

\(fn FILENAME &optional WILDCARDS)" t nil)

(function-put 'epkg-find-file 'interactive-only 'find-file)

(autoload 'epkg-find-file-other-window "epkg-utils" "\
Visit a file in a submodule of `epkg-repository' in another window.

\(fn FILENAME &optional WILDCARDS)" t nil)

(function-put 'epkg-find-file-other-window 'interactive-only 'find-file-other-window)

(autoload 'epkg-find-file-other-frame "epkg-utils" "\
Visit a file in a submodule of `epkg-repository' in another frame.

\(fn FILENAME &optional WILDCARDS)" t nil)

(function-put 'epkg-find-file-other-frame 'interactive-only 'find-file-other-frame)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "epkg-utils" '("epkg-find-file-" "with-epkg-repository")))

;;;***

;;;### (autoloads nil nil ("epkg-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; epkg-autoloads.el ends here
