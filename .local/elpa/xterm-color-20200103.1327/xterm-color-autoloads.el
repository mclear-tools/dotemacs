;;; xterm-color-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "xterm-color" "xterm-color.el" (0 0 0 0))
;;; Generated autoloads from xterm-color.el

(autoload 'xterm-color-filter-strip "xterm-color" "\
Translate ANSI color sequences in STRING into text properties.
Return new STRING with text properties applied.

This function strips text properties that may be present in STRING.

\(fn STRING)" nil nil)

(autoload 'xterm-color-filter "xterm-color" "\
Translate ANSI color sequences in STRING into text properties.
Return new STRING with text properties applied.

This function will check if `xterm-color-preserve-properties' is
set to T and only call `xterm-color-filter-strip' on substrings
that do not have text properties applied (passing through the rest
unmodified).  Preserving properties in this fashion is really a hack
and not very robust as there may be situations where text properties
are applied on ANSI data, which will mess up the state machine.
It works fine with and is really meant for eshell though.

This can be inserted into `comint-preoutput-filter-functions'.

\(fn STRING)" nil nil)

(autoload 'xterm-color-256 "xterm-color" "\


\(fn COLOR)" nil nil)

(autoload 'xterm-color-colorize-buffer "xterm-color" "\
Apply `xterm-color-filter' to current buffer, and replace its contents.

\(fn)" t nil)

(autoload 'xterm-color-clear-cache "xterm-color" "\
Clear xterm color face attribute cache.
You may want to call this if you change `xterm-color-names' or
`xterm-color-names-bright' at runtime and you want to see the changes
take place in a pre-existing buffer that has had xterm-color initialized.

Since the cache is buffer-local and created on-demand when needed, this has no
effect when called from a buffer that does not have a cache.

\(fn)" t nil)

(autoload 'xterm-color-test "xterm-color" "\
Create/display and render a new buffer that contains ANSI control sequences.

\(fn)" t nil)

(autoload 'xterm-color-test-raw "xterm-color" "\
Create and display a new buffer that contains ANSI SGR control sequences.
The ANSI sequences will not be processed.  One can use a different Emacs
package (e.g. ansi-color.el) to do so.  This is really meant to be used for
easy comparisons/benchmarks with libraries that offer similar functionality.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xterm-color" '("xterm-color-" "+xterm-color--table-256+")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; xterm-color-autoloads.el ends here
