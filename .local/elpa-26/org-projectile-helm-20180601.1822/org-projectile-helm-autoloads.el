;;; org-projectile-helm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-projectile-helm" "org-projectile-helm.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-projectile-helm.el

(autoload 'org-projectile-helm-template-or-project "org-projectile-helm" "\
Select a project or `org-capture' template and record a TODO.

If provided, CAPTURE-TEMPLATE-FOR-PROJECT will be the capture
template used for project TODO capture.

\(fn &optional CAPTURE-TEMPLATE-FOR-PROJECT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-projectile-helm" '("org-projectile-helm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-projectile-helm-autoloads.el ends here
