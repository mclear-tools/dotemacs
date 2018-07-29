;;; pandoc-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pandoc-mode" "pandoc-mode.el" (0 0 0 0))
;;; Generated autoloads from pandoc-mode.el

(autoload 'pandoc-mode "pandoc-mode" "\
Minor mode for interacting with Pandoc.

\(fn &optional ARG)" t nil)

(autoload 'conditionally-turn-on-pandoc "pandoc-mode" "\
Turn on pandoc-mode if a pandoc settings file exists.
This is for use in major mode hooks.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pandoc-mode" '("pandoc-")))

;;;***

;;;### (autoloads nil "pandoc-mode-utils" "pandoc-mode-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pandoc-mode-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pandoc-mode-utils" '("latex" "jsmath" "mimetex" "mathjax" "wrap" "webtex" "katex" "base-header-level" "bibliography" "columns" "citation-abbreviations" "csl" "eol" "email-obfuscation" "epub-chapter-level" "number-offset" "indented-code-classes" "id-prefix" "track-changes" "tab-stop" "title-prefix" "resource-path" "reference-location" "slide-level" "pandoc-" "pdf-engine" "def" "dpi" "highlight-style")))

;;;***

;;;### (autoloads nil nil ("pandoc-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pandoc-mode-autoloads.el ends here
