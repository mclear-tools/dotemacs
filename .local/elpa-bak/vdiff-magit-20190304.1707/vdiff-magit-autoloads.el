;;; vdiff-magit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vdiff-magit" "vdiff-magit.el" (0 0 0 0))
;;; Generated autoloads from vdiff-magit.el
 (autoload 'vdiff-magit "magit-vdiff" nil)

(autoload 'vdiff-magit-resolve "vdiff-magit" "\
Resolve outstanding conflicts in FILE using vdiff.
FILE has to be relative to the top directory of the repository.

In the rare event that you want to manually resolve all
conflicts, including those already resolved by Git, use
`ediff-merge-revisions-with-ancestor'.

\(fn FILE)" t nil)

(autoload 'vdiff-magit-stage "vdiff-magit" "\
Stage and unstage changes to FILE using vdiff.
FILE has to be relative to the top directory of the repository.

\(fn FILE)" t nil)

(autoload 'vdiff-magit-dwim "vdiff-magit" "\
Compare, stage, or resolve using vdiff.

This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using vdiff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `vdiff-magit-*'
command has to be used explicitly.  If it cannot read the user's
mind at all, then it asks the user for a command to run.

\(fn)" t nil)

(autoload 'vdiff-magit-show-unstaged "vdiff-magit" "\
Show unstaged changes using vdiff.

This only allows looking at the changes; to stage, unstage,
and discard changes using vdiff, use `vdiff-magit-stage'.

FILE must be relative to the top directory of the repository.

\(fn FILE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vdiff-magit" '("vdiff-magit")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vdiff-magit-autoloads.el ends here
