;;; counsel-projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "counsel-projectile" "counsel-projectile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from counsel-projectile.el

(autoload 'counsel-projectile-find-file "counsel-projectile" "\
Jump to a file in the current project.

With a prefix ARG, invalidate the cache first.

\(fn &optional ARG)" t nil)

(autoload 'counsel-projectile-find-dir "counsel-projectile" "\
Jump to a directory in the current project.

With a prefix ARG, invalidate the cache first.

\(fn &optional ARG)" t nil)

(autoload 'counsel-projectile-switch-to-buffer "counsel-projectile" "\
Jump to a buffer in the current project.

\(fn)" t nil)

(autoload 'counsel-projectile-ag "counsel-projectile" "\
Search the current project with ag.

OPTIONS, if non-nil, is a string containing additional options to
be passed to ag. It is read from the minibuffer if the function
is called with a prefix argument.

\(fn &optional OPTIONS)" t nil)

(autoload 'counsel-projectile-rg "counsel-projectile" "\
Search the current project with rg.

OPTIONS, if non-nil, is a string containing additional options to
be passed to rg. It is read from the minibuffer if the function
is called with a prefix argument.

\(fn &optional OPTIONS)" t nil)

(autoload 'counsel-projectile-org-capture "counsel-projectile" "\
Capture into the current project.

This command is a replacement for `org-capture' (or
`counsel-org-capture') offering project-specific capture
templates, in addition to the regular templates available from
`org-capture'. These project templates, which are \"expanded\"
relatively to the current project, are determined by the
variables `counsel-projectile-org-capture-templates' and
`counsel-projectile-org-capture-templates-contexts'. See the
former variable in particular for details.

Optional argument FROM-BUFFER specifies the buffer from which to
capture.

\(fn &optional FROM-BUFFER)" t nil)

(autoload 'counsel-projectile-org-agenda "counsel-projectile" "\
Open project agenda.

This command simply calls `org-agenda' after filtering out all
agenda files that do not belong to the current project.

Optional arguments ARG, ORG-KEYS, and RESTRICTION are as in
`org-agenda'.

\(fn &optional ARG ORG-KEYS RESTRICTION)" t nil)

(autoload 'counsel-projectile-switch-project "counsel-projectile" "\
Switch project.

\(fn)" t nil)

(autoload 'counsel-projectile "counsel-projectile" "\
Jump to a buffer or file in the current project.

With a prefix ARG, invalidate the cache first.

If not inside a project, call `counsel-projectile-switch-project'.

\(fn &optional ARG)" t nil)

(defvar counsel-projectile-mode nil "\
Non-nil if Counsel-Projectile mode is enabled.
See the `counsel-projectile-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `counsel-projectile-mode'.")

(custom-autoload 'counsel-projectile-mode "counsel-projectile" nil)

(autoload 'counsel-projectile-mode "counsel-projectile" "\
Toggle Counsel-Projectile mode on or off.

With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil, and toggle it if ARG is `toggle'.

Counsel-Projectile mode triggers Projectile mode, remaps
Projectile commands that have counsel replacements, and adds key
bindings for Counsel-Projectile commands that have no Projectile
counterpart.

\\{counsel-projectile-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "counsel-projectile" '("counsel-projectile-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; counsel-projectile-autoloads.el ends here
