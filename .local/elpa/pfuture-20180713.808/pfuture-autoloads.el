;;; pfuture-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pfuture" "pfuture.el" (0 0 0 0))
;;; Generated autoloads from pfuture.el

(autoload 'pfuture-new "pfuture" "\
Create a new future process for command CMD and arguments CMD-ARGS.
This will return a process object with one additional 'result property which
can be read via (process-get process 'result) or alternatively with
\(pfuture-result process).

Note that CMD-ARGS must be a *sequence* of strings, meaning
this is wrong: (pfuture-new \"git status\")
this is right: (pfuture-new \"git\" \"status\")

\(fn CMD &rest CMD-ARGS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pfuture" '("pfuture-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pfuture-autoloads.el ends here
