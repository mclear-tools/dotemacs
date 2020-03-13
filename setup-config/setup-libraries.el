;;; Useful Libraries
;; https://github.com/jwiegley/emacs-async, https://github.com/magnars/s.el,
;; https://github.com/magnars/dash.el http://elpa.gnu.org/packages/cl-lib.html are
;; libraries for asynchronous processing string manipulation, list manipulation and
;; backward compatibility respectively. The git package is also a library.

(use-package async
  :ensure t
  :defer 2
  :init
  (setq async-bytecomp-allowed-packages '(all))
  :custom
  ;; async compiling package
  (async-bytecomp-package-mode 1)
  :config
  (dired-async-mode 1)
  ;; limit number of async processes
  (eval-when-compile
    (require 'cl-lib))
  (defvar async-maximum-parallel-procs 4)
  (defvar async--parallel-procs 0)
  (defvar async--queue nil)
  (defvar-local async--cb nil)
  (advice-add #'async-start :around
              (lambda (orig-func func &optional callback)
                (if (>= async--parallel-procs async-maximum-parallel-procs)
                    (push `(,func ,callback) async--queue)
                  (cl-incf async--parallel-procs)
                  (let ((future (funcall orig-func func
                                         (lambda (re)
                                           (cl-decf async--parallel-procs)
                                           (when async--cb (funcall async--cb re))
                                           (when-let (args (pop async--queue))
                                             (apply #'async-start args))))))
                    (with-current-buffer (process-buffer future)
                      (setq async--cb callback)))))
              '((name . --queue-dispatch)))
  )
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
