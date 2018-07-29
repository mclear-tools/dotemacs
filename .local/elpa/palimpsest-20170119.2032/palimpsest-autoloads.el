;;; palimpsest-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "palimpsest" "palimpsest.el" (0 0 0 0))
;;; Generated autoloads from palimpsest.el

(autoload 'palimpsest-mode "palimpsest" "\
Toggle palimpsest mode.
Interactively with no argument, this command toggles the mode.
to show buffer size and position in mode-line.  You can customize
this minor mode, see option `palimpsest-mode'.

Note: If you turn this mode on then you probably want to turn off
option `scroll-bar-mode'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "palimpsest" '("palimpsest-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; palimpsest-autoloads.el ends here
