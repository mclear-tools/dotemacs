;;; alect-themes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "alect-themes" "alect-themes.el" (0 0 0 0))
;;; Generated autoloads from alect-themes.el

(autoload 'alect-generate-colors "alect-themes" "\
Return alist of themes suitable for the variable `alect-colors'.

THEME-NAMES is a list of symbols.

COLORS is a list of lists (COLOR-NAME COLOR-VAL...) where
COLOR-VAL is a color for specified theme (theme names and color
values should be in matching order).

\(fn THEME-NAMES COLORS)" nil nil)

(and load-file-name (boundp 'custom-theme-load-path) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alect-themes" '("alect-")))

;;;***

;;;### (autoloads nil nil ("alect-black-alt-theme.el" "alect-black-theme.el"
;;;;;;  "alect-dark-alt-theme.el" "alect-dark-theme.el" "alect-light-alt-theme.el"
;;;;;;  "alect-light-theme.el" "alect-themes-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; alect-themes-autoloads.el ends here
