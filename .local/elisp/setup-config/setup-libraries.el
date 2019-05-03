;;; Useful Libraries
(use-package async   :defer 4)
(use-package dash    :defer 4)
(use-package s       :defer 4)
(use-package f       :defer 4)
(use-package subr-x  :defer 4 :ensure nil)
; lots of packages depend on these libraries
(use-package cl-lib  :demand t :ensure nil)
(use-package cl      :demand t :ensure nil)

(provide 'setup-libraries)
