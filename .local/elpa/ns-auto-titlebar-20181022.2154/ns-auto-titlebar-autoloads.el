;;; ns-auto-titlebar-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ns-auto-titlebar" "ns-auto-titlebar.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ns-auto-titlebar.el

(defvar ns-auto-titlebar-mode nil "\
Non-nil if Ns-Auto-Titlebar mode is enabled.
See the `ns-auto-titlebar-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ns-auto-titlebar-mode'.")

(custom-autoload 'ns-auto-titlebar-mode "ns-auto-titlebar" nil)

(autoload 'ns-auto-titlebar-mode "ns-auto-titlebar" "\
Set the MacOS transparent titlebar background automatically.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ns-auto-titlebar" '("ns-auto-titlebar-set-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ns-auto-titlebar-autoloads.el ends here
