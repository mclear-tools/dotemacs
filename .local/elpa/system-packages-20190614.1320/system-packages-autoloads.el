;;; system-packages-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "system-packages" "system-packages.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from system-packages.el

(autoload 'system-packages-install "system-packages" "\
Install system packages.

Use the package manager from `system-packages-package-manager' to
install PACK.  You may use ARGS to pass options to the package
manger.

\(fn PACK &optional ARGS)" t nil)

(autoload 'system-packages-ensure "system-packages" "\
Ensure PACK is installed on system.
Search for PACK with `system-packages-package-installed-p', and
install the package if not found.  Use ARGS to pass options to
the package manager.

\(fn PACK &optional ARGS)" t nil)

(defalias 'system-packages-package-installed-p #'executable-find "\
Return t if PACK is installed.
Currently an alias for `executable-find', so it will give wrong
results if the package and executable names are different.")

(autoload 'system-packages-search "system-packages" "\
Search for system packages.

Use the package manager named in `system-packages-package-manager'
to search for PACK.  You may use ARGS to pass options to the
package manager.

\(fn PACK &optional ARGS)" t nil)

(autoload 'system-packages-uninstall "system-packages" "\
Uninstall system packages.

Uses the package manager named in
`system-packages-package-manager' to uninstall PACK.  You may use
ARGS to pass options to the package manager.

\(fn PACK &optional ARGS)" t nil)

(autoload 'system-packages-list-dependencies-of "system-packages" "\
List the dependencies of PACK.

You may use ARGS to pass options to the package manager.

\(fn PACK &optional ARGS)" t nil)

(autoload 'system-packages-get-info "system-packages" "\
Display local package information of PACK.

With a prefix argument, display remote package information.  You
may use ARGS to pass options to the package manager.

\(fn PACK &optional ARGS)" t nil)

(autoload 'system-packages-list-files-provided-by "system-packages" "\
List the files provided by PACK.

You may use ARGS to pass options to the package manager.

\(fn PACK &optional ARGS)" t nil)

(autoload 'system-packages-update "system-packages" "\
Update system packages.

Use the package manager `system-packages-package-manager'.  You
may use ARGS to pass options to the package manger.

\(fn &optional ARGS)" t nil)

(autoload 'system-packages-remove-orphaned "system-packages" "\
Remove orphaned packages.

Uses the package manager named in
`system-packages-package-manager'.  You may use ARGS to pass
options to the package manger.

\(fn &optional ARGS)" t nil)

(autoload 'system-packages-list-installed-packages "system-packages" "\
List explicitly installed packages.

Uses the package manager named in
`system-packages-package-manager'.  With
\\[universal-argument] (for ALL), list all installed packages.
You may use ARGS to pass options to the package manger.

\(fn ALL &optional ARGS)" t nil)

(autoload 'system-packages-clean-cache "system-packages" "\
Clean the cache of the package manager.

You may use ARGS to pass options to the package manger.

\(fn &optional ARGS)" t nil)

(autoload 'system-packages-log "system-packages" "\
Show a log from `system-packages-package-manager'.

You may use ARGS to pass options to the package manger.

\(fn &optional ARGS)" t nil)

(autoload 'system-packages-verify-all-packages "system-packages" "\
Check that files owned by packages are present on the system.

You may use ARGS to pass options to the package manger.

\(fn &optional ARGS)" t nil)

(autoload 'system-packages-verify-all-dependencies "system-packages" "\
Verify that all required dependencies are installed on the system.

You may use ARGS to pass options to the package manger.

\(fn &optional ARGS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "system-packages" '("system-packages-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; system-packages-autoloads.el ends here
