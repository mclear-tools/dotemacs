;;; rg-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rg" "rg.el" (0 0 0 0))
;;; Generated autoloads from rg.el

(autoload 'rg-define-toggle "rg" "\
Define a command line flag that can be toggled from the rg result buffer.

This will create a function with prefix 'rg-custom-toggle-flag-'
concatenated with the FLAG name, stripped of any leading dashes.  Flag
must be a form that will be evaluated to a string at macro expansion
time.  For instance, if FLAG is '--invert-match' the function name
will be 'rg-custom-toggle-flag-invert-match.  If the flag contains a
value that will be excluded from the function name.

Optional KEY is a key binding that is added to `rg-mode-map'.  If the
optional DEFAULT parameter is non nil the flag will be enabled by default.

\(fn FLAG &optional KEY DEFAULT)" nil t)

(autoload 'rg-enable-default-bindings "rg" "\
Enable the global `rg' default key bindings under PREFIX key.
If prefix is not supplied `rg-keymap-prefix' is used.

\(fn &optional PREFIX)" t nil)

(autoload 'rg-use-old-defaults "rg" "\
Restore default settings pre version 2.0.0.

\(fn)" nil nil)

(autoload 'rg-define-search "rg" "\
Define an rg search functions named NAME.
ARGS is a search specification that defines parameters of a search.
It optionally starts with a string that is used as the docstring for
the defined function.  The rest of ARGS contains key value pairs
according to the specification below.  All keys are optional with
specified default if left out.

:query      Method for retrieving the search string.  Allowed values
            are `point' which means extract thing at point and `ask'
            which means prompt the user for a string.  Any form that
            evaluates to a string is allowed.
            Default is `ask'.
:format     Specifies if :query is interpreted literally (`literal')
            or as a regexp (`regexp').  If it is a form, eg.
            (not `current-prefix-arg'), and is non-nil the :query is
            interpreted literally, otherwise as a regexp.
            Default is `regexp'.
:files      Form that evaluates to a file alias or custom file glob.
            `current' means extract alias from current buffer file name,
            `ask' will prompt the user.
            Default is `ask'.
:dir        Root search directory.  Allowed values are `ask' for user
            prompt, `current' for current dir and `project' for project
            root.  Any form that evaluates to a directory string is
            also allowed.
            Default is `ask'.
:confirm    `never', `always', or `prefix' are allowed values.  Specifies
            if the the final search command line string can be modified
            and confirmed the user.
            Default is `never'.
:flags      `ask' or a list of command line flags that will be used when
            invoking the search.
:menu       Bind the command into `rg-menu'.  Must be a list with three
            items in it.  The first item is the description of the
            group in witch the new command will appear.  If the group
            does not exist a new will be created.  The second item is
            the key binding for this new command (ether a key vector
            or a key description string) and the third item is the
            description of the command that will appear in the menu.

Example:
\(rg-define-search search-home-dir-in-elisp
  \"Doc string.\"
  :query ask
  :format literal
  :files \"elisp\"
  :dir (getenv \"HOME\"))
  :menu (\"Custom\" \"H\" \"Home dir\")

\(fn NAME &rest ARGS)" nil t)

(function-put 'rg-define-search 'lisp-indent-function 'defun)
 (autoload 'rg-project "rg.el" "" t)
 (autoload 'rg-dwim-project-dir "rg.el" "" t)
 (autoload 'rg-dwim-current-dir "rg.el" "" t)
 (autoload 'rg-dwim-current-file "rg.el" "" t)

(autoload 'rg-dwim "rg" "\
Run ripgrep without user interaction figuring out the intention by magic(!).
The default magic searches for thing at point in files matching
current file under project root directory.

With \\[universal-argument] prefix (CURDIR), search is done in
current dir instead of project root.

With repeated \\[universal-argument] prefix, search is done in
the current dir and using the current variable `buffer-file-name'
as a pattern.  Subdirectories are still searched, so different
files with the same name pattern still will be searched.

\(fn &optional CURDIR)" t nil)
 (autoload 'rg-literal "rg.el" "" t)
 (autoload 'rg "rg.el" "" t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rg" '("rg-" "kill-rg")))

;;;***

;;;### (autoloads nil "rg-header" "rg-header.el" (0 0 0 0))
;;; Generated autoloads from rg-header.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rg-header" '("rg-")))

;;;***

;;;### (autoloads nil "rg-history" "rg-history.el" (0 0 0 0))
;;; Generated autoloads from rg-history.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rg-history" '("rg-history-")))

;;;***

;;;### (autoloads nil "rg-ibuffer" "rg-ibuffer.el" (0 0 0 0))
;;; Generated autoloads from rg-ibuffer.el

(autoload 'rg-list-searches "rg-ibuffer" "\
List all `rg-mode' buffers in `ibuffer'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rg-ibuffer" '("rg-")))

;;;***

;;;### (autoloads nil "rg-menu" "rg-menu.el" (0 0 0 0))
;;; Generated autoloads from rg-menu.el

(autoload 'rg-enable-menu "rg-menu" "\
Bind `rg-menu' to PREFIX key.
If prefix is not supplied `rg-keymap-prefix' is used.

\(fn &optional PREFIX)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rg-menu" '("rg-menu")))

;;;***

;;;### (autoloads nil "rg-result" "rg-result.el" (0 0 0 0))
;;; Generated autoloads from rg-result.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rg-result" '("rg-")))

;;;***

;;;### (autoloads nil "wgrep-rg" "wgrep-rg.el" (0 0 0 0))
;;; Generated autoloads from wgrep-rg.el

(autoload 'wgrep-rg-setup "wgrep-rg" "\
Setup wgrep rg support.

\(fn)" nil nil)

(add-hook 'rg-mode-hook 'wgrep-rg-setup)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "wgrep-rg" '("wgrep-rg-")))

;;;***

;;;### (autoloads nil nil ("rg-info-hack.el" "rg-pkg.el") (0 0 0
;;;;;;  0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rg-autoloads.el ends here
