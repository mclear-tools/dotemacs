;;; frog-menu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "frog-menu" "frog-menu.el" (0 0 0 0))
;;; Generated autoloads from frog-menu.el

(autoload 'frog-menu-call "frog-menu" "\
Read a command from CMDS and execute it.

CMDS is of format as specified by `completing-read'
collections. If PROMPT is given it should be a string with prompt
information for the user.

\(fn CMDS &optional PROMPT)" nil nil)

(autoload 'frog-menu-read "frog-menu" "\
Read from a menu of variable `frog-menu-type'.

PROMPT is a string with prompt information for the user.

COLLECTION is a list from which the user can choose an item. It
can be a list of strings or an alist mapping strings to return
values. Users can switch to `completing-read' from COLLECTION
using the TAB key. For sorting the displayed strings see
`frog-menu-sort-function'.

ACTIONS is an additional list of actions that can be given to let
the user choose an action instead an item from COLLECTION.

Each ACTION is a list of the form:

    (KEY DESCRIPTION RETURN)

Where KEY is a string to be interpreted as spelled-out
keystrokes, using the same format as for `kbd'.

DESCRIPTION is a string to be displayed along with KEY to
describe the action.

RETURN will be the returned value if KEY is pressed.

\(fn PROMPT COLLECTION &optional ACTIONS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "frog-menu" '("frog-menu-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; frog-menu-autoloads.el ends here
