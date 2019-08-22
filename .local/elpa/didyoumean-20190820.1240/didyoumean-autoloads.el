;;; didyoumean-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "didyoumean" "didyoumean.el" (0 0 0 0))
;;; Generated autoloads from didyoumean.el

(autoload 'didyoumean "didyoumean" "\
Prompt for files similar to the current file if they exist.

\(fn)" t nil)

(add-hook 'find-file-hook #'didyoumean)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "didyoumean" '("didyoumean-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; didyoumean-autoloads.el ends here
