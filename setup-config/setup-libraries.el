;;; Useful Libraries
;; https://github.com/jwiegley/emacs-async, https://github.com/magnars/s.el,
;; https://github.com/magnars/dash.el http://elpa.gnu.org/packages/cl-lib.html are
;; libraries for asynchronous processing string manipulation, list manipulation and
;; backward compatibility respectively. The git package is also a library.

(use-package async
  :straight nil
  :defer
  :config
  (dired-async-mode 1)
  (setq dired-async--modeline-mode nil))

(use-package dash    :straight nil :defer 2)
(use-package s       :straight nil :defer 2)
(use-package f       :straight nil :defer 2)
(use-package subr-x  :straight nil :defer 1)
;; lots of packages depend on these libraries
(use-package cl      :straight nil :defer t)
(use-package cl-lib  :straight nil :demand t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-libraries)
