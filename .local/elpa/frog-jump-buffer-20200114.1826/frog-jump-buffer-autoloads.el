;;; frog-jump-buffer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "frog-jump-buffer" "frog-jump-buffer.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from frog-jump-buffer.el

(autoload 'frog-jump-buffer "frog-jump-buffer" "\
Present a `frog-menu' for jumping to an open buffer.
If FILTER-FUNCTION is present, filter the `buffer-list' with it.

\(fn)" t nil)

(autoload 'frog-jump-buffer-other-window "frog-jump-buffer" "\
Launch `frog-jump-buffer' with `other-window' being the default target window.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "frog-jump-buffer" '("frog-jump-buffer-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; frog-jump-buffer-autoloads.el ends here
