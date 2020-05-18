;;; multi-compile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "multi-compile" "multi-compile.el" (0 0 0 0))
;;; Generated autoloads from multi-compile.el

(autoload 'multi-compile-locate-file-dir "multi-compile" "\
Look up the directory hierarchy from current file for a directory containing file NAME.

\(fn NAME)" nil nil)

(autoload 'multi-compile-run "multi-compile" "\
Choice target and start compile.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multi-compile" '("multi-compile-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; multi-compile-autoloads.el ends here
