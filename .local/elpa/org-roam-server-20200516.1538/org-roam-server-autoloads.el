;;; org-roam-server-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-roam-server" "org-roam-server.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-roam-server.el

(defvar org-roam-server-mode nil "\
Non-nil if Org-Roam-Server mode is enabled.
See the `org-roam-server-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-roam-server-mode'.")

(custom-autoload 'org-roam-server-mode "org-roam-server" nil)

(autoload 'org-roam-server-mode "org-roam-server" "\
Start the http server and serve org-roam files.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-server" '("org-roam-" "roam-data" "current-buffer-data")))

;;;***

;;;### (autoloads nil nil ("org-roam-server-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-roam-server-autoloads.el ends here
