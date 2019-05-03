;;; tldr-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tldr" "tldr.el" (0 0 0 0))
;;; Generated autoloads from tldr.el

(autoload 'tldr-update-docs "tldr" "\
Get or update tldr docs from source.

\(fn)" t nil)

(autoload 'tldr "tldr" "\
Lookup tldr docs.

\(fn &optional CMD)" t nil)

(autoload 'helm-tldr "tldr" "\
Helm interface for `tldr'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tldr" '("tldr-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tldr-autoloads.el ends here
