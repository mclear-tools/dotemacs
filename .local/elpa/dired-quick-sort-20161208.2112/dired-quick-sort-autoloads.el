;;; dired-quick-sort-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dired-quick-sort" "dired-quick-sort.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from dired-quick-sort.el

(autoload 'dired-quick-sort "dired-quick-sort" "\
Sort dired by the given criteria.

The possible values of SORT-BY, REVERSE, GROUP-DIRECTORIES and TIME are
explained in the variable `dired-quick-sort-reverse-last',
`dired-quick-sort-reverse-last', `dired-quick-sort-group-directories-last' and
`dired-quick-sort-time-last' respectively.  Besides, passing nil to any of these
arguments to use the value used last time (that is, the values of the four
variables mentioned before), even after restarting Emacs if `savehist-mode' is
enabled.  When invoked interactively, nil's are passed to all arguments.

\(fn &optional SORT-BY REVERSE GROUP-DIRECTORIES TIME)" t nil)

(autoload 'dired-quick-sort-setup "dired-quick-sort" "\
Run the default setup.

This will bind the key S in `dired-mode' to run
`hydra-dired-quick-sort/body', and automatically run the sorting
criteria after entering `dired-mode'.  You can choose to not call
this setup function and run a modified version of this function
to use your own preferred setup:

  ;; Replace \"S\" with other keys to invoke the dired-quick-sort hydra.
  (define-key dired-mode-map \"S\" 'hydra-dired-quick-sort/body)
  ;; Automatically use the sorting defined here to sort.
  (add-hook 'dired-mode-hook 'dired-quick-sort)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-quick-sort" '("dired-quick-sort-" "hydra-dired-quick-sort")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-quick-sort-autoloads.el ends here
