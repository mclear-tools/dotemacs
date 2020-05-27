;;; bicycle-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bicycle" "bicycle.el" (0 0 0 0))
;;; Generated autoloads from bicycle.el

(autoload 'bicycle-cycle "bicycle" "\
Cycle local or global visibility.

With a prefix argument call `bicycle-cycle-global'.
Without a prefix argument call `bicycle-cycle-local'.

\(fn &optional GLOBAL)" t nil)

(autoload 'bicycle-cycle-global "bicycle" "\
Cycle visibility of all sections.

1. OVERVIEW: Show only top-level heading.
2. TOC:      Show all headings, without treating top-level
             code blocks as sections.
3. TREES:    Show all headings, treaing top-level code blocks
             as sections (i.e. their first line is treated as
             a heading).
4. ALL:      Show everything, except code blocks that have been
             collapsed individually (using a `hideshow' command
             or function).

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bicycle" '("bicycle-" "outline-code-level")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bicycle-autoloads.el ends here
