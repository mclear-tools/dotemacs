;;; centered-cursor-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "centered-cursor-mode" "centered-cursor-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from centered-cursor-mode.el

(autoload 'ccm-visible-text-lines "centered-cursor-mode" "\
Visible text lines

\(fn)" nil nil)

(autoload 'centered-cursor-mode "centered-cursor-mode" "\
Makes the cursor stay vertically in a defined
position (usually centered).

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "centered-cursor-mode" '("global-centered-cursor-mode" "ccm-" "recenter-sequence" "animate-first-start-p")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; centered-cursor-mode-autoloads.el ends here
