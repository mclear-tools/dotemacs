;;; undo-fu-session-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "undo-fu-session" "undo-fu-session.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from undo-fu-session.el

(autoload 'undo-fu-session-mode "undo-fu-session" "\
Toggle saving the undo data in the current buffer (Undo-Fu Session Mode).

\(fn &optional ARG)" t nil)

(defvar global-undo-fu-session-mode nil "\
Non-nil if Global Undo-Fu-Session mode is enabled.
See the `global-undo-fu-session-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-undo-fu-session-mode'.")

(custom-autoload 'global-undo-fu-session-mode "undo-fu-session" nil)

(autoload 'global-undo-fu-session-mode "undo-fu-session" "\
Toggle Undo-Fu-Session mode in all buffers.
With prefix ARG, enable Global Undo-Fu-Session mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Undo-Fu-Session mode is enabled in all buffers where
`undo-fu-session-mode-turn-on' would do it.
See `undo-fu-session-mode' for more information on Undo-Fu-Session mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "undo-fu-session" '("undo-fu-session-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; undo-fu-session-autoloads.el ends here
