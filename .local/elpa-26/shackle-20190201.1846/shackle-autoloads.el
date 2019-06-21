;;; shackle-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "shackle" "shackle.el" (0 0 0 0))
;;; Generated autoloads from shackle.el

(defvar shackle-mode nil "\
Non-nil if Shackle mode is enabled.
See the `shackle-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `shackle-mode'.")

(custom-autoload 'shackle-mode "shackle" nil)

(autoload 'shackle-mode "shackle" "\
Toggle `shackle-mode'.
This global minor mode allows you to easily set up rules for
popups in Emacs.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "shackle" '("shackle-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; shackle-autoloads.el ends here
