;;; Useful Libraries
;; https://github.com/jwiegley/emacs-async, https://github.com/magnars/s.el,
;; https://github.com/magnars/dash.el http://elpa.gnu.org/packages/cl-lib.html are
;; libraries for asynchronous processing string manipulation, list manipulation and
;; backward compatibility respectively. The git package is also a library.

(use-package async
  :straight t
  :defer
  :config
  (dired-async-mode 1)
  (setq dired-async--modeline-mode nil))

(use-package anaphora :straight t :defer 1)
(use-package dash     :straight t :defer 2)
(use-package s        :straight t :defer 2)
(use-package f        :straight t :defer 2)
(use-package subr-x   :straight (:type built-in) :defer 1)
;; lots of packages depend on these libraries
(use-package cl       :straight (:type built-in) :defer t)
(use-package cl-lib   :straight (:type built-in) :demand t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-libraries)
