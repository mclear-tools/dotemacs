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

(defvar didyoumean-mode nil "\
Non-nil if Didyoumean mode is enabled.
See the `didyoumean-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `didyoumean-mode'.")

(custom-autoload 'didyoumean-mode "didyoumean" nil)

(autoload 'didyoumean-mode "didyoumean" "\
DidYouMean minor mode.

Prompt for files similar to the current file if they exist.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "didyoumean" '("didyoumean-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; didyoumean-autoloads.el ends here
