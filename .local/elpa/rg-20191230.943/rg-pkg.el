(define-package "rg" "20191230.943" "A search tool based on ripgrep."
  '((cl-lib "0.5")
    (emacs "25.1")
    (s "1.10.0")
    (transient "0.1.0")
    (wgrep "2.1.10"))
  :keywords
  '("matching" "tools")
  :authors
  '(("David Landell" . "david.landell@sunnyhill.email")
    ("Roland McGrath" . "roland@gnu.org"))
  :maintainer
  '("David Landell" . "david.landell@sunnyhill.email")
  :url "https://github.com/dajva/rg.el")
;; Local Variables:
;; no-byte-compile: t
;; End:
