;;; Useful Libraries
;; https://github.com/jwiegley/emacs-async, https://github.com/magnars/s.el,
;; https://github.com/magnars/dash.el http://elpa.gnu.org/packages/cl-lib.html are
;; libraries for asynchronous processing string manipulation, list manipulation and
;; backward compatibility respectively. The git package is also a library.

(use-package async   :defer 2)
(use-package dash    :defer 2)
(use-package s       :defer 2)
(use-package f       :defer 2)
(use-package subr-x  :defer 2 :ensure nil)
; lots of packages depend on these libraries
(use-package cl-lib  :demand t :ensure nil)
;; get rid of compile warnings
;;https://stackoverflow.com/a/5020049/6277148
(eval-when-compile
  (use-package cl      :demand t :ensure nil))

(provide 'setup-libraries)
