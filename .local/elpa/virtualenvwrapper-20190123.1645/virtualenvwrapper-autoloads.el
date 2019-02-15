;;; virtualenvwrapper-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "virtualenvwrapper" "virtualenvwrapper.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from virtualenvwrapper.el

(autoload 'venv-projectile-auto-workon "virtualenvwrapper" "\
If a venv in the projetile root exists, activates it.
Set your common venvs names in `venv-dirlookup-names'

\(fn)" nil nil)

(autoload 'venv-deactivate "virtualenvwrapper" "\
Deactivate the current venv.

\(fn)" t nil)

(autoload 'venv-set-location "virtualenvwrapper" "\
Set where to look for virtual environments to LOCATION.
This is useful e.g. when using tox.

\(fn &optional LOCATION)" t nil)

(autoload 'venv-workon "virtualenvwrapper" "\
Interactively switch to virtualenv NAME. Prompts for name if called
interactively.

\(fn &optional NAME)" t nil)

(autoload 'venv-mkvirtualenv-using "virtualenvwrapper" "\
Create new virtualenvs NAMES using INTERPRETER. If venv-location
is a single directory, the new virtualenvs are made there; if it
is a list of directories, the new virtualenvs are made in the
current `default-directory'.

\(fn INTERPRETER &rest NAMES)" t nil)

(autoload 'venv-mkvirtualenv "virtualenvwrapper" "\
Create new virtualenvs NAMES. If venv-location is a single
directory, the new virtualenvs are made there; if it is a list of
directories, the new virtualenvs are made in the current
`default-directory'.

\(fn &rest NAMES)" t nil)

(autoload 'venv-rmvirtualenv "virtualenvwrapper" "\
Delete virtualenvs NAMES.

\(fn &rest NAMES)" t nil)

(autoload 'venv-lsvirtualenv "virtualenvwrapper" "\
List all available virtualenvs in a temp buffer.

\(fn)" t nil)

(autoload 'venv-cdvirtualenv "virtualenvwrapper" "\
Change to the directory of current virtualenv. If
SUBDIR is passed, append that to the path such that
we are immediately in that directory.

\(fn &optional SUBDIR)" t nil)

(autoload 'venv-cpvirtualenv "virtualenvwrapper" "\
Copy virtualenv NAME to NEWNAME. Any arguments not passed will be
prompted for This comes with the same caveat as cpvirtualenv in the
original virtualenvwrapper, which is that is far from guarenteed to
work well. Many packages hardcode absolute paths in various places an
will break if moved to a new location. Use with caution. If used with
a single virtualenv directory, behaves just like cpvirtualenv in
virtualenvwrapper.sh.  If used with virtualenvs spread around the
filesystem, creates the new virtualenv in the current default
directory.

\(fn &optional NAME NEWNAME)" t nil)

(autoload 'venv-shell-init "virtualenvwrapper" "\
Activate the current virtualenv in a newly opened shell.

\(fn PROCESS)" nil nil)

(autoload 'venv-initialize-interactive-shells "virtualenvwrapper" "\
Configure interactive shells for use with
virtualenvwrapper.el.

\(fn)" nil nil)

(autoload 'venv-initialize-eshell "virtualenvwrapper" "\
Configure eshell for use with virtualenvwrapper.el.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "virtualenvwrapper" '("venv-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; virtualenvwrapper-autoloads.el ends here
