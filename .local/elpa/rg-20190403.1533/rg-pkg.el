(define-package "rg" "20190403.1533" "A search tool based on ripgrep."
  '((cl-lib "0.5")
    (emacs "24.4")
    (s "1.10.0")
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
