;;; shell-switcher-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rswitcher" "rswitcher.el" (0 0 0 0))
;;; Generated autoloads from rswitcher.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rswitcher" '("rswitcher-")))

;;;***

;;;### (autoloads nil "shell-switcher" "shell-switcher.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from shell-switcher.el

(defvar shell-switcher-new-shell-function 'shell-switcher-make-eshell "\
This variable references a function used to create new shells.
The function must take 0 arguments and return a newly created
shell buffer.
`shell-switcher-make-shell',`shell-switcher-make-eshell' and
`shell-switcher-make-ansi-term' are possible functions.")

(custom-autoload 'shell-switcher-new-shell-function "shell-switcher" t)

(defvar shell-switcher-ask-before-creating-new nil "\
If non-nil ask the user before creating a new shell buffer.
A new shell buffer is automatically created if there are no
buffers to switch to and this variable is set to nil.")

(custom-autoload 'shell-switcher-ask-before-creating-new "shell-switcher" t)

(defvar shell-switcher-ansi-term-shell "" "\
If non-empty use this shell with `ansi-term'.
Otherwise the shell will be chosen based on the environment with
a fallback to /bin/sh")

(custom-autoload 'shell-switcher-ansi-term-shell "shell-switcher" t)

(defvar shell-switcher-mode-map (let ((map (make-sparse-keymap))) (define-key map (kbd "C-'") 'shell-switcher-switch-buffer) (define-key map (kbd "C-x 4 '") 'shell-switcher-switch-buffer-other-window) (define-key map (kbd "C-M-'") 'shell-switcher-new-shell) map) "\
Keymap to use in shell-switcher mode.")

(defvar shell-switcher-mode nil "\
Non-nil if Shell-Switcher mode is enabled.
See the `shell-switcher-mode' command
for a description of this minor mode.")

(custom-autoload 'shell-switcher-mode "shell-switcher" nil)

(autoload 'shell-switcher-mode "shell-switcher" "\
Toggle shell-switcher mode.
Interactively with no argument, this command toggles the mode. A
positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When shell-switcher mode is enabled, switching and creating shell
buffers is just a matter of calling
\\[shell-switcher-switch-buffer]. Then, you can type the last key
of this key binding again to continue switching. Additionally,
see commands \\[shell-switcher-switch-buffer-other-window] and
\\[shell-switcher-new-shell].

\(fn &optional ARG)" t nil)

(autoload 'shell-switcher-switch-buffer "shell-switcher" "\
Switch to the most recently accessed buffer.
Switch to the most recently accessed shell buffer that is not the
current one.  Pressing the last key of the key sequence that call
this command will result in switching to the next shell buffer :
for example, if `C-'' is bound to this command, repeatedly
pressing `'' (quote) will let the user visit all shell
buffers (this is actually done by `sswitcher--switch-partially'.

If there is no shell buffer or if the only shell buffer is the
current buffer, propose the creation of a new shell buffer.

\(fn)" t nil)

(autoload 'shell-switcher-switch-buffer-other-window "shell-switcher" "\
Switch to the most recently accessed buffer in another window.
Same as `shell-switcher-switch-buffer' but change another
window.

\(fn)" t nil)

(autoload 'shell-switcher-new-shell "shell-switcher" "\
Unconditionally create and display a new shell buffer.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "shell-switcher" '("shell-switcher-" "sswitcher--")))

;;;***

;;;### (autoloads nil nil ("shell-switcher-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; shell-switcher-autoloads.el ends here
