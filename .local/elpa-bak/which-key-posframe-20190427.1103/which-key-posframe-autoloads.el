;;; which-key-posframe-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "which-key-posframe" "which-key-posframe.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from which-key-posframe.el

(autoload 'which-key-posframe-enable "which-key-posframe" "\
Enable which-key-posframe.

\(fn)" t nil)

(defvar which-key-posframe-mode nil "\
Non-nil if Which-Key-Posframe mode is enabled.
See the `which-key-posframe-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `which-key-posframe-mode'.")

(custom-autoload 'which-key-posframe-mode "which-key-posframe" nil)

(autoload 'which-key-posframe-mode "which-key-posframe" "\
Toggle which key posframe mode on of off.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "which-key-posframe" '("which-key-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; which-key-posframe-autoloads.el ends here
