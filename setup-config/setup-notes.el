;;;; Search Notes
;;;;; Zettelkasten Search

(defvar cpm-zettelkasten "~/Dropbox/Work/projects/notebook/content-org")
(defun cpm/zettelkasten-search ()
  "Serch in Zettelkasten with affe-grep."
  (interactive)
  (affe-grep cpm-zettelkasten))


;;;;; Consult Notes
;; Adapted from https://github.com/minad/consult/wiki/hrm-notes
(use-package consult-notes
  :straight (:local-repo "/Users/roambot/.emacs.d/.local/elisp/consult-notes/")
  :commands (consult-notes consult-notes-all-search))


;;; Provide Setup-Notes
(provide 'setup-notes)
