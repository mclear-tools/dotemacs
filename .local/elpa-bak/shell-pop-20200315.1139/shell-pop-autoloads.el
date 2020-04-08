;;; shell-pop-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "shell-pop" "shell-pop.el" (0 0 0 0))
;;; Generated autoloads from shell-pop.el

(defvar shell-pop-universal-key nil "\
Key binding used to pop in and out of the shell.

The input format is the same as that of `kbd'.")

(custom-autoload 'shell-pop-universal-key "shell-pop" nil)

(autoload 'shell-pop "shell-pop" "\


\(fn ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "shell-pop" '("shell-pop-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; shell-pop-autoloads.el ends here
