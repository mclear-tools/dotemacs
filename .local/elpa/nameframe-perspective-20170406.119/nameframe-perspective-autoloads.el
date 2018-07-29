;;; nameframe-perspective-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "nameframe-perspective" "nameframe-perspective.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from nameframe-perspective.el

(defvar nameframe-perspective-mode nil "\
Non-nil if Nameframe-Perspective mode is enabled.
See the `nameframe-perspective-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `nameframe-perspective-mode'.")

(custom-autoload 'nameframe-perspective-mode "nameframe-perspective" nil)

(autoload 'nameframe-perspective-mode "nameframe-perspective" "\
Global minor mode that switches perspective when creating frames.
With `nameframe-perspective-mode' enabled, creating frames with
`nameframe-make-frame' will automatically switch to a perspective
with that frame's name.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nameframe-perspective" '("nameframe-perspective--make-frame-persp-switch-hook")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nameframe-perspective-autoloads.el ends here
