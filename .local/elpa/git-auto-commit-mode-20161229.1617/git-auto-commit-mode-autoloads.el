;;; git-auto-commit-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "git-auto-commit-mode" "git-auto-commit-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from git-auto-commit-mode.el

(autoload 'git-auto-commit-mode "git-auto-commit-mode" "\
Automatically commit any changes made when saving with this
mode turned on and optionally push them too.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-auto-commit-mode" '("gac-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; git-auto-commit-mode-autoloads.el ends here
