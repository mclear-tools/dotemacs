;;; outorg-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "outorg" "outorg.el" (0 0 0 0))
;;; Generated autoloads from outorg.el

(autoload 'outorg-edit-as-org "outorg" "\
Convert and copy to temporary Org buffer

With ARG, act conditional on the raw value of ARG:

| prefix | raw | action 1          | action 2                       |
|--------+-----+-------------------+--------------------------------|
| C-u    | (4) | edit-whole-buffer | ---                            |
| C-1    |   1 | edit-whole-buffer | insert default export-template |
| C-2    |   2 | edit-whole-buffer | prompt user for template-file  |
| C-3    |   3 | edit-whole-buffer | insert & keep default template |
| C-4    |   4 | edit-whole-buffer | insert & keep template-file    |
| C-5    |   5 | propagate changes | ---                            |

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "outorg" '("out" "backward-line-comments" "forward-line-comments" "skip-line-comment-or-ws")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; outorg-autoloads.el ends here
