;;; company-prescient-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-prescient" "company-prescient.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-prescient.el

(defvar company-prescient-mode nil "\
Non-nil if Company-Prescient mode is enabled.
See the `company-prescient-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `company-prescient-mode'.")

(custom-autoload 'company-prescient-mode "company-prescient" nil)

(autoload 'company-prescient-mode "company-prescient" "\
Minor mode to use prescient.el in Company completions.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-prescient" '("company-prescient-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-prescient-autoloads.el ends here
