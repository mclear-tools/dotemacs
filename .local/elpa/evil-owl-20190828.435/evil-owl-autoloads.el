;;; evil-owl-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-owl" "evil-owl.el" (0 0 0 0))
;;; Generated autoloads from evil-owl.el

(defvar evil-owl-mode nil "\
Non-nil if Evil-Owl mode is enabled.
See the `evil-owl-mode' command
for a description of this minor mode.")

(custom-autoload 'evil-owl-mode "evil-owl" nil)

(autoload 'evil-owl-mode "evil-owl" "\
A minor mode to preview marks and registers before using them.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-owl" '("evil-owl-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-owl-autoloads.el ends here
