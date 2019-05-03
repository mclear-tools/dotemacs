(use-package ob
  :defer t
  :ensure nil
  :init
  (progn
    (defvar cpm/ob-enabled-languages `("emacs-lisp"
                                        "org"
                                        "latex"
                                        "dot" ;graphviz
                                        "ditaa"
                                        ,(if (version< (org-version) "8.3")
                                             "sh" ;ob-shell.el was called ob-sh.el in older Org versions
                                           "shell"))
      "List of languages for which the ob-* packages need to be loaded.")

    (defvar cpm/ob-eval-unsafe-languages '("emacs-lisp"
                                            "shell")
      "List of languages which are unsafe for babel evaluation without
confirmation. Languages present in `cpm/ob-enabled-languages' will be marked
as safe for babel evaluation except for the languages in this variable.")

    (let (ob-lang-alist)
      (dolist (lang cpm/ob-enabled-languages)
        (add-to-list 'ob-lang-alist `(,(intern lang) . t)))
      (org-babel-do-load-languages 'org-babel-load-languages ob-lang-alist))

    (defun cpm/org-confirm-babel-evaluate-fn (lang body)
      "Returns a non-nil value if the user should be prompted for execution,
or nil if no prompt is required.

Babel evaluation will happen without confirmation for the Org src blocks for
the languages in `cpm/ob-enabled-languages'."
      (let ((re-all-lang (regexp-opt cpm/ob-enabled-languages 'words))
            (re-unsafe-lang (regexp-opt cpm/ob-eval-unsafe-languages 'words))
            (unsafe t)) ;Set the return value `unsafe' to t by default
        (when (and (not (string-match-p re-unsafe-lang lang))
                   (string-match-p re-all-lang lang))
          (setq unsafe nil))
        ;; (message "re-all:%s\nre-unsafe:%s\nlang:%s\nbody:%S\nret-val:%S"
        ;; re-all-lang re-unsafe-lang lang body unsafe)
        unsafe))
    (setq org-confirm-babel-evaluate #'cpm/org-confirm-babel-evaluate-fn)))

