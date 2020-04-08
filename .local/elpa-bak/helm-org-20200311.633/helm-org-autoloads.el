;;; helm-org-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-org" "helm-org.el" (0 0 0 0))
;;; Generated autoloads from helm-org.el

(require 'helm-easymenu)

(easy-menu-add-item nil '("Tools" "Helm") '("Org" ["Org headlines in org agenda files" helm-org-agenda-files-headings t] ["Org headlines in buffer" helm-org-in-buffer-headings t]) "Elpa")

(autoload 'helm-org-agenda-files-headings "helm-org" "\
Preconfigured helm for org files headings.

\(fn &optional ARG)" t nil)

(autoload 'helm-org-in-buffer-headings "helm-org" "\
Preconfigured helm for org buffer headings.

\(fn &optional ARG)" t nil)

(autoload 'helm-org-parent-headings "helm-org" "\
Preconfigured helm for org headings that are parents of the current heading.

\(fn &optional ARG)" t nil)

(autoload 'helm-org-capture-templates "helm-org" "\
Preconfigured helm for org templates.

\(fn)" t nil)

(autoload 'helm-org-completing-read-tags "helm-org" "\
Completing read function for Org tags.

This function is used as a `completing-read' function in
`helm-completing-read-handlers-alist' by `org-set-tags' and
`org-capture'.

NOTE: Org tag completion will work only if you disable org fast tag
selection, see (info \"(org) setting tags\").

\(fn PROMPT COLLECTION PRED REQ INITIAL HIST DEF INHERIT-INPUT-METHOD NAME BUFFER)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-org" '("helm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-org-autoloads.el ends here
