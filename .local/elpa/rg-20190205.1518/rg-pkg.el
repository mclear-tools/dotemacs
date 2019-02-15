(define-package "rg" "20190205.1518" "A search tool based on ripgrep."
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
