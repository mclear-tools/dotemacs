;;; helm-evil-markers-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-evil-markers" "helm-evil-markers.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from helm-evil-markers.el

(autoload 'helm-evil-markers "helm-evil-markers" "\
List evil markers with helm.

\(fn)" t nil)

(autoload 'helm-evil-markers-set "helm-evil-markers" "\
Wrapper to set marker denoted by CHAR to position POS and update markers.
If ADVANCE is t, the marker advances when inserting text at it; otherwise,
it stays behind.

\(fn CHAR &optional POS ADVANCE)" t nil)

(autoload 'helm-evil-markers-toggle "helm-evil-markers" "\
Enable or disable helm-evil-markers keybindings.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-evil-markers" '("helm-evil-markers-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-evil-markers-autoloads.el ends here
