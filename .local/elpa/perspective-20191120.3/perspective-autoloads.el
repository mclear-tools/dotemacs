;;; perspective-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "perspective" "perspective.el" (0 0 0 0))
;;; Generated autoloads from perspective.el

(defvar persp-mode nil "\
Non-nil if Persp mode is enabled.
See the `persp-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `persp-mode'.")

(custom-autoload 'persp-mode "perspective" nil)

(autoload 'persp-mode "perspective" "\
Toggle perspective mode.
When active, keeps track of multiple 'perspectives',
named collections of buffers and window configurations.

\(fn &optional ARG)" t nil)

(autoload 'persp-state-save "perspective" "\
Save the current perspective state to FILE.

FILE defaults to the value of persp-state-default-file if it is
set.

Each perspective's buffer list and window layout will be saved.
Frames and their associated perspectives will also be saved
\(but not the original frame sizes).

Buffers with * characters in their names, as well as buffers without
associated files will be ignored. If such buffers are currently
visible in a perspective as windows, they will be saved as
'*scratch* (persp)' buffers.

\(fn &optional FILE INTERACTIVE\\=\\?)" t nil)

(autoload 'persp-state-load "perspective" "\
Restore the perspective state saved in FILE.

FILE defaults to the value of persp-state-default-file if it is
set.

Frames are restored, along with each frame's perspective list.
Each perspective's buffer list and window layout are also
restored.

\(fn FILE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "perspective" '("persp" "quick-perspective-keys" "with-perspective" "make-persp" "check-persp")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; perspective-autoloads.el ends here
