;;; undo-fu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "undo-fu" "undo-fu.el" (0 0 0 0))
;;; Generated autoloads from undo-fu.el

(autoload 'undo-fu-disable-checkpoint "undo-fu" "\
Remove the undo-fu checkpoint, making all future actions unconstrained.

This command is needed when `undo-fu-ignore-keyboard-quit' is t,
since in this case `keyboard-quit' cannot be used
to perform unconstrained undo/redo actions.

\(fn)" t nil)

(autoload 'undo-fu-only-redo-all "undo-fu" "\
Redo all actions until the initial undo step.

wraps the `undo' function.

\(fn)" t nil)

(autoload 'undo-fu-only-redo "undo-fu" "\
Redo an action until the initial undo action.

wraps the `undo' function.

Optional argument ARG The number of steps to redo.

\(fn &optional ARG)" t nil)

(autoload 'undo-fu-only-undo "undo-fu" "\
Undo the last action.

wraps the `undo-only' function.

Optional argument ARG the number of steps to undo.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "undo-fu" '("undo-fu-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; undo-fu-autoloads.el ends here
