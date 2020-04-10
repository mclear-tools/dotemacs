;;; centaur-tabs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "centaur-tabs" "centaur-tabs.el" (0 0 0 0))
;;; Generated autoloads from centaur-tabs.el

(autoload 'centaur-tabs-local-mode "centaur-tabs" "\
Toggle local display of the tab bar.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.
When turned on, if a local header line is shown, it is hidden to show
the tab bar.  The tab bar is locally hidden otherwise.  When turned
off, if a local header line is hidden or the tab bar is locally
hidden, it is shown again.  Signal an error if Centaur-Tabs mode is off.

\(fn &optional ARG)" t nil)

(defvar centaur-tabs-mode nil "\
Non-nil if Centaur-Tabs mode is enabled.
See the `centaur-tabs-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `centaur-tabs-mode'.")

(custom-autoload 'centaur-tabs-mode "centaur-tabs" nil)

(autoload 'centaur-tabs-mode "centaur-tabs" "\
Toggle display of a tab bar in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\{centaur-tabs-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "centaur-tabs" '("centaur-tabs-")))

;;;***

;;;### (autoloads nil "centaur-tabs-elements" "centaur-tabs-elements.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from centaur-tabs-elements.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "centaur-tabs-elements" '("centaur-tabs-")))

;;;***

;;;### (autoloads nil "centaur-tabs-functions" "centaur-tabs-functions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from centaur-tabs-functions.el

(autoload 'centaur-tabs-backward "centaur-tabs-functions" "\
Select the previous available tab.
Depend on the setting of the option `centaur-tabs-cycle-scope'.

\(fn)" t nil)

(autoload 'centaur-tabs-forward "centaur-tabs-functions" "\
Select the next available tab.
Depend on the setting of the option `centaur-tabs-cycle-scope'.

\(fn)" t nil)

(autoload 'centaur-tabs-backward-group "centaur-tabs-functions" "\
Go to selected tab in the previous available group.

\(fn)" t nil)

(autoload 'centaur-tabs-forward-group "centaur-tabs-functions" "\
Go to selected tab in the next available group.

\(fn)" t nil)

(autoload 'centaur-tabs-backward-tab "centaur-tabs-functions" "\
Select the previous visible tab.

\(fn)" t nil)

(autoload 'centaur-tabs-forward-tab "centaur-tabs-functions" "\
Select the next visible tab.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "centaur-tabs-functions" '("centaur-tabs-")))

;;;***

;;;### (autoloads nil "centaur-tabs-interactive" "centaur-tabs-interactive.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from centaur-tabs-interactive.el

(autoload 'centaur-tabs-counsel-switch-group "centaur-tabs-interactive" "\
Display a list of current buffer groups using Counsel.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "centaur-tabs-interactive" '("centaur-tabs-")))

;;;***

;;;### (autoloads nil nil ("centaur-tabs-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; centaur-tabs-autoloads.el ends here
