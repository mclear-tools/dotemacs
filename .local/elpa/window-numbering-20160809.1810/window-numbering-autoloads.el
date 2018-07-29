;;; window-numbering-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "window-numbering" "window-numbering.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from window-numbering.el

(defvar window-numbering-mode nil "\
Non-nil if Window-Numbering mode is enabled.
See the `window-numbering-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `window-numbering-mode'.")

(custom-autoload 'window-numbering-mode "window-numbering" nil)

(autoload 'window-numbering-mode "window-numbering" "\
A minor mode that assigns a number to each window.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "window-numbering" '("window-numbering-" "select-window-by-number")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; window-numbering-autoloads.el ends here
