;;; evil-mc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-mc" "evil-mc.el" (0 0 0 0))
;;; Generated autoloads from evil-mc.el

(autoload 'evil-mc-mode "evil-mc" "\
Toggle evil multiple cursors in a single buffer.

\(fn &optional ARG)" t nil)

(defvar global-evil-mc-mode nil "\
Non-nil if Global Evil-Mc mode is enabled.
See the `global-evil-mc-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-mc-mode'.")

(custom-autoload 'global-evil-mc-mode "evil-mc" nil)

(autoload 'global-evil-mc-mode "evil-mc" "\
Toggle Evil-Mc mode in all buffers.
With prefix ARG, enable Global Evil-Mc mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Mc mode is enabled in all buffers where
`evil-mc-initialize' would do it.
See `evil-mc-mode' for more information on Evil-Mc mode.

\(fn &optional ARG)" t nil)

(autoload 'evil-mc-initialize "evil-mc" "\
Enable `evil-mc-mode' in the current buffer.

\(fn)" nil nil)

(autoload 'turn-on-evil-mc-mode "evil-mc" "\
Turn on evil-mc mode in the current buffer.

\(fn)" t nil)

(autoload 'turn-off-evil-mc-mode "evil-mc" "\
Turn off evil-mc mode in the current buffer.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-mc" '("evil-mc-")))

;;;***

;;;### (autoloads nil "evil-mc-command-execute" "evil-mc-command-execute.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-mc-command-execute.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-mc-command-execute" '("evil-mc-")))

;;;***

;;;### (autoloads nil "evil-mc-command-record" "evil-mc-command-record.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-mc-command-record.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-mc-command-record" '("evil-mc-")))

;;;***

;;;### (autoloads nil "evil-mc-common" "evil-mc-common.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from evil-mc-common.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-mc-common" '("evil-mc-")))

;;;***

;;;### (autoloads nil "evil-mc-cursor-make" "evil-mc-cursor-make.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-mc-cursor-make.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-mc-cursor-make" '("evil-mc-")))

;;;***

;;;### (autoloads nil "evil-mc-cursor-state" "evil-mc-cursor-state.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-mc-cursor-state.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-mc-cursor-state" '("evil-mc-")))

;;;***

;;;### (autoloads nil "evil-mc-known-commands" "evil-mc-known-commands.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-mc-known-commands.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-mc-known-commands" '("evil-mc-known-commands")))

;;;***

;;;### (autoloads nil "evil-mc-region" "evil-mc-region.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from evil-mc-region.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-mc-region" '("evil-mc-")))

;;;***

;;;### (autoloads nil "evil-mc-scratch" "evil-mc-scratch.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from evil-mc-scratch.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-mc-scratch" '("evil-mc-")))

;;;***

;;;### (autoloads nil "evil-mc-setup" "evil-mc-setup.el" (0 0 0 0))
;;; Generated autoloads from evil-mc-setup.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-mc-setup" '("evil-mc-")))

;;;***

;;;### (autoloads nil "evil-mc-undo" "evil-mc-undo.el" (0 0 0 0))
;;; Generated autoloads from evil-mc-undo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-mc-undo" '("evil-mc-")))

;;;***

;;;### (autoloads nil "evil-mc-vars" "evil-mc-vars.el" (0 0 0 0))
;;; Generated autoloads from evil-mc-vars.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-mc-vars" '("evil-mc-")))

;;;***

;;;### (autoloads nil nil ("evil-mc-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-mc-autoloads.el ends here
