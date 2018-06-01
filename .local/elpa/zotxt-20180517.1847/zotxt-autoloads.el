;;; zotxt-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-zotxt" "org-zotxt.el" (0 0 0 0))
;;; Generated autoloads from org-zotxt.el

(autoload 'org-zotxt-mode "org-zotxt" "\
Toggle org-zotxt-mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

This is a minor mode for managing your citations with Zotero in a
org-mode document.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-zotxt" '("org-zotxt-")))

;;;***

;;;### (autoloads nil "zotxt" "zotxt.el" (0 0 0 0))
;;; Generated autoloads from zotxt.el

(autoload 'zotxt-easykey-mode "zotxt" "\
Toggle zotxt-easykey-mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

This is a minor mode for managing your easykey citations,
including completion.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "zotxt" '("zotxt-")))

;;;***

;;;### (autoloads nil nil ("zotxt-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; zotxt-autoloads.el ends here
