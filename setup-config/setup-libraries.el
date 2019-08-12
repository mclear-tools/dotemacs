;;; Useful Libraries
;; [[https://github.com/jwiegley/emacs-async][async]], [[https://github.com/magnars/s.el][s]], [[https://github.com/magnars/dash.el][dash]], and [[http://elpa.gnu.org/packages/cl-lib.html][cl-lib]] are libraries for asynchronous processing,
;; string manipulation, list manipulation and backward compatibility
;; respectively. The git package is also a library.

(use-package async   :defer 2)
(use-package dash    :defer 2)
(use-package s       :defer 2)
(use-package f       :defer 2)
(use-package subr-x  :defer 2 :ensure nil)
; lots of packages depend on these libraries
(use-package cl-lib  :demand t :ensure nil)
(use-package cl      :demand t :ensure nil)

(provide 'setup-libraries)
