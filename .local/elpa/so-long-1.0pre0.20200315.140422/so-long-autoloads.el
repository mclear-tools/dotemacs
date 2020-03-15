;;; so-long-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "so-long" "so-long.el" (0 0 0 0))
;;; Generated autoloads from so-long.el

(autoload 'so-long-commentary "so-long" "\
View the so-long documentation in `outline-mode'.

\(fn)" t nil)

(autoload 'so-long-customize "so-long" "\
Open the so-long `customize' group.

\(fn)" t nil)

(autoload 'so-long-minor-mode "so-long" "\
This is the minor mode equivalent of `so-long-mode'.

Any active minor modes listed in `so-long-minor-modes' are disabled for the
current buffer, and buffer-local values are assigned to variables in accordance
with `so-long-variable-overrides'.

This minor mode is a standard `so-long-action' option.

\(fn &optional ARG)" t nil)

(autoload 'so-long-mode "so-long" "\
This major mode is the default `so-long-action' option.

The normal reason for this mode being active is that `global-so-long-mode' is
enabled, and `so-long-predicate' has detected that the file contains long lines.

Many Emacs modes struggle with buffers which contain excessively long lines,
and may consequently cause unacceptable performance issues.

This is commonly on account of \"minified\" code (i.e. code that has been
compacted into the smallest file size possible, which often entails removing
newlines should they not be strictly necessary).  These kinds of files are
typically not intended to be edited, so not providing the usual editing mode
in these cases will rarely be an issue.

This major mode disables any active minor modes listed in `so-long-minor-modes'
for the current buffer, and buffer-local values are assigned to variables in
accordance with `so-long-variable-overrides'.

To restore the original major mode (along with the minor modes and variable
values), despite potential performance issues, type \\[so-long-revert].

Use \\[so-long-commentary] for more information.

Use \\[so-long-customize] to configure the behavior.

\(fn)" t nil)

(autoload 'so-long "so-long" "\
Invoke `so-long-action' and run `so-long-hook'.

This command is called automatically when long lines are detected, when
`global-so-long-mode' is enabled.

The effects of the action can be undone by calling `so-long-revert'.

If ACTION is provided, it is used instead of `so-long-action'.

With a prefix argument, select the action to use interactively.

If an action was already active in the buffer, it will be reverted before
invoking the new action.

\(fn &optional ACTION)" t nil)

(autoload 'so-long-enable "so-long" "\
Enable the so-long library's functionality.

Equivalent to calling (global-so-long-mode 1)

\(fn)" t nil)

(defvar global-so-long-mode nil "\
Non-nil if Global So-Long mode is enabled.
See the `global-so-long-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-so-long-mode'.")

(custom-autoload 'global-so-long-mode "so-long" nil)

(autoload 'global-so-long-mode "so-long" "\
Toggle automated performance mitigation for files with long lines.

Many Emacs modes struggle with buffers which contain excessively long lines,
and may consequently cause unacceptable performance issues.

This is commonly on account of \"minified\" code (i.e. code that has been
compacted into the smallest file size possible, which often entails removing
newlines should they not be strictly necessary).

When such files are detected by `so-long-predicate', we invoke the selected
`so-long-action' to mitigate potential performance problems in the buffer.

Use \\[so-long-commentary] for more information.

Use \\[so-long-customize] to configure the behavior.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "so-long" '("so-long-" "turn-o")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; so-long-autoloads.el ends here
