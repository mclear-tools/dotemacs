;;; use-package-ensure-system-package-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "use-package-ensure-system-package" "use-package-ensure-system-package.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from use-package-ensure-system-package.el

(autoload 'use-package-normalize/:ensure-system-package "use-package-ensure-system-package" "\
Turn `arg' into a list of cons-es of (`package-name' . `install-command').

\(fn NAME-SYMBOL KEYWORD ARGS)" nil nil)

(autoload 'use-package-handler/:ensure-system-package "use-package-ensure-system-package" "\
Execute the handler for `:ensure-system-package' keyword in `use-package'.

\(fn NAME KEYWORD ARG REST STATE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-ensure-system-package" '("use-package-ensure-system-package-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; use-package-ensure-system-package-autoloads.el ends here
