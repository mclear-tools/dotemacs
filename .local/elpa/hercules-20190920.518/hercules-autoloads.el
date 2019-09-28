;;; hercules-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hercules" "hercules.el" (0 0 0 0))
;;; Generated autoloads from hercules.el

(autoload 'hercules-def "hercules" "\
Summon hercules.el to banish your hydras.

TOGGLE-FUNS, SHOW-FUNS, and HIDE-FUNS define entry and exit
points for hercules.el to show KEYMAP. Both single functions and
lists work. As all other arguments to `hercules-def', these must
be quoted.

KEYMAP specifies the keymap for hercules.el to make a pop-up out
of.  If KEYMAP is nil, it is assumed that one of SHOW-FUNS or
TOGGLE-FUNS results in a `which-key--show-popup' call. This may
be useful for functions such as `which-key-show-top-level'. I use
it to remind myself of some obscure Evil commands from time to
time.

FLATTEN displays all maps and sub-maps without redrawing on
prefix-key presses. This allows for multi-key combinations in a
single hercules.el buffer.

BLACKLIST-KEYS and WHITELIST-KEYS specify
which (`kbd'-interpretable) keys should removed from/allowed to
remain on KEYMAP. Handy if you want to unbind things in bulk and
don't want to get your hands dirty with keymaps. Both single
characters and lists work. Blacklists take precedence over
whitelists.

BLACKLIST-FUNS and WHITELIST-FUNS are analogous to BLACKLIST-KEYS
and WHITELIST-KEYS except that they operate on function
symbols. These might be useful if a keymap specifies multiple
bindings for a commands and pruning it is more efficient this
way. Blacklists again take precedence over whitelists.

PACKAGE must be passed along with BLACKLIST-KEYS, WHITELIST-KEYS,
BLACKLIST-FUNS, or WHITELIST-FUNS if KEYMAP belongs to a lazy
loaded package. Its contents should be the package name as a
quoted symbol.

Setting TRANSIENT to t allows you to get away with not setting
HIDE-FUNS or TOGGLE-FUNS by dismissing hercules.el whenever you
press a key not on KEYMAP.

CONFIG (to be deprecated in v0.3) is a quoted s-expression for
the pedantic among us who would like to keep related
configurations together. This might be useful if you wish to
manually tweak KEYMAP, or even create a new one from scratch.

\(fn &key TOGGLE-FUNS SHOW-FUNS HIDE-FUNS KEYMAP FLATTEN TRANSIENT BLACKLIST-KEYS WHITELIST-KEYS BLACKLIST-FUNS WHITELIST-FUNS PACKAGE CONFIG)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hercules" '("hercules-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hercules-autoloads.el ends here
