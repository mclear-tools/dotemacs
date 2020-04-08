;;; vterm-toggle-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vterm-toggle" "vterm-toggle.el" (0 0 0 0))
;;; Generated autoloads from vterm-toggle.el

(autoload 'vterm-toggle "vterm-toggle" "\
Vterm toggle.
Optional argument ARGS .

\(fn &optional ARGS)" t nil)

(autoload 'vterm-toggle-cd "vterm-toggle" "\
Vterm toggle and insert a cd command.
Optional argument ARGS .

\(fn &optional ARGS)" t nil)

(autoload 'vterm-toggle-insert-cd "vterm-toggle" "\
Cd to the directory where your previous buffer file exists.
after you have toggle to the vterm buffer with `vterm-toggle'.

\(fn)" t nil)

(autoload 'vterm-toggle-forward "vterm-toggle" "\
Go to the next term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET.

\(fn &optional OFFSET)" t nil)

(autoload 'vterm-toggle-backward "vterm-toggle" "\
Go to the previous term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET.

\(fn &optional OFFSET)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vterm-toggle" '("vterm-toggle-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vterm-toggle-autoloads.el ends here
