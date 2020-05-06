;;; gited-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gited" "gited.el" (0 0 0 0))
;;; Generated autoloads from gited.el

(autoload 'gited-list-branches "gited" "\
List all branches or tags for the current repository.
Optional arg PATTERN if non-nil, then must be \"local\", \"remote\",
 or \"tags\".  That lists local branches, remote branches and tags,
 respectively.  When PATTERN is nil, then list the local branches.
Optional arg OTHER-WINDOW means to display the Gited buffer in another window.
Optional arg UPDATE if non-nil, then force to update the gited buffer.
 Otherwise, just switch to the Gited buffer if already exists.
When called interactively prompt for PATTERN.
When called interactively with a prefix set OTHER-WINDOW non-nil.

\(fn &optional PATTERN OTHER-WINDOW UPDATE)" t nil)

(defalias 'gited-list 'gited-list-branches)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gited" '("gited-")))

;;;***

;;;### (autoloads nil "gited-ci" "gited-ci.el" (0 0 0 0))
;;; Generated autoloads from gited-ci.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gited-ci" '("gited-")))

;;;***

;;;### (autoloads nil "gited-tests" "gited-tests.el" (0 0 0 0))
;;; Generated autoloads from gited-tests.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gited-tests" '("with-" "gited-")))

;;;***

;;;### (autoloads nil nil ("gited-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gited-autoloads.el ends here
