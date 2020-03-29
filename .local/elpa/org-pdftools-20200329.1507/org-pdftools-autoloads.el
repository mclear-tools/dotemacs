;;; org-pdftools-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-pdftools" "org-pdftools.el" (0 0 0 0))
;;; Generated autoloads from org-pdftools.el

(autoload 'org-pdftools-open "org-pdftools" "\
Function to open org-pdftools LINK.

\(fn LINK)" nil nil)

(autoload 'org-pdftools-store-link "org-pdftools" "\
Store a link to a pdfview/pdfoccur buffer.

\(fn)" nil nil)

(autoload 'org-pdftools-complete-link "org-pdftools" "\
Use the existing file name completion for file.
Links to get the file name, then ask the user for the page number
and append it. ARG is passed to `org-link-complete-file'.

\(fn &optional ARG)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-pdftools" '("org-pdftools-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-pdftools-autoloads.el ends here
