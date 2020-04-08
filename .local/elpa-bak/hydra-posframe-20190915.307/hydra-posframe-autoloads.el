;;; hydra-posframe-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hydra-posframe" "hydra-posframe.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from hydra-posframe.el

(defvar hydra-posframe-mode nil "\
Non-nil if Hydra-Posframe mode is enabled.
See the `hydra-posframe-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `hydra-posframe-mode'.")

(custom-autoload 'hydra-posframe-mode "hydra-posframe" nil)

(autoload 'hydra-posframe-mode "hydra-posframe" "\
Display hydra via posframe.

\(fn &optional ARG)" t nil)

(autoload 'hydra-posframe-enable "hydra-posframe" "\
Enable hydra-posframe.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra-posframe" '("hydra-posframe-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hydra-posframe-autoloads.el ends here
