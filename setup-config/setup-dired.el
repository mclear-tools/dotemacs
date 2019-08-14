;;;; Dired
;; I used to use ranger but it was buggy and I can get almost everything I want from
;; dired. See https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer for
;; discussion of how to avoid creating lots of dired buffers.
(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-jump-other-window)
  :general
  (:keymaps 'dired-mode-map
   :states '(normal motion)
   "l" #'dired-find-alternate-file
   "h" #'cpm/dired-updirectory
   "q" #'quit-window)
  :config
  ;; Function to move up a directory like in ranger
  (defun cpm/dired-updirectory ()
    (interactive)
    (find-alternate-file ".."))
  (when sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when (executable-find "gls")
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (when (or (and sys/macp (executable-find "gls"))
            (and (not sys/macp) (executable-find "ls")))
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)

    ;; list directories first
    (setq dired-listing-switches "-laFGh1v --group-directories-first"))

  ;; Like with ls, append "@" to file names if they're symlinks
  (setq dired-ls-F-marks-symlinks t)
  ;; don't ask about killing buffer visiting file
  (setq dired-clean-confirm-killing-deleted-buffers t)
  ;; always delete and copy recursively
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  ;; allow editing file permissions
  (setq wdired-allow-to-change-permissions t))

;; Async dired
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;;narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :general (:keymaps 'dired-mode-map
            "/"  'dired-narrow))

;; Colourful dired
(use-package diredfl
  :commands (diredfl-global-mode)
  :init (diredfl-global-mode 1)
  :custom-face
  (diredfl-compressed-file-name ((t (:foreground "#00629D"))))
  (diredfl-compressed-file-suffix ((t (:foreground "#839496"))))
  (diredfl-date-time ((t (:foreground "#9EA0E5"))))
  (diredfl-deletion ((t (:background "Red" :foreground "Yellow"))))
  (diredfl-dir-heading ((t (:background "#69B7F0" :foreground "#002b36"))))
  (diredfl-dir-name ((t (:foreground "#69B7F0"))))
  (diredfl-dir-priv ((t (:foreground "#268bd2"))))
  (diredfl-exec-priv ((t (:foreground "#990A1b"))))
  (diredfl-file-name ((t (:foreground "#2aa198"))))
  (diredfl-file-suffix ((t (:foreground "#839496"))))
  (diredfl-flag-mark-line ((t (:background "#dc322f"))))
  (diredfl-no-priv ((t (:foreground "#b58900"))))
  (diredfl-number ((t (:foreground "#DEB542"))))
  (diredfl-rare-priv ((t (:background "#cb4b16" :foreground "#B4C342"))))
  (diredfl-read-priv ((t (:foreground "#F2804F"))))
  (diredfl-tagged-autofile-name ((t (:foreground "#328C0411328"))))
  (diredfl-write-priv ((t (:foreground "#8b2C02")))))

;; dired extras
(use-package dired-x
  :ensure nil
  :after dired
  :demand t
  :init (setq-default dired-omit-mode nil)
  :config
  (setq dired-omit-verbose nil)
  (add-to-list 'dired-omit-extensions ".DS_Store"))

(use-package dired-aux
  :ensure nil
  :after dired
  :demand t)

;;;; Dired Sort
(use-package dired-quick-sort
  :ensure t
  :general
  (:keymaps 'dired-mode-map
            :states '(normal motion)
            "s" #'hydra-dired-quick-sort/body))

;;;;  Dired Plus
;; I used this mainly for getting rid of unnecesary dired buffers, but I think I have that solved independently now
(use-package dired+
  :disabled t
  :ensure t
  :defer 2
  :init
  (setq font-lock-maximum-decoration nil)
  (setq dired-omit-files-regexp "^\\.?#\\|^\\.$\\|^\\.\\.$")
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-toggle-find-file-reuse-dir 1))

;;;;  Peep Dired
(use-package peep-dired
  :ensure t
  :commands (peep-dired)
  :functions (peep-dired-kill-buffers-without-window)
  :general
  (:keymaps 'dired-mode-map
   :states '(normal motion)
   "p" #'peep-dired)
  (:keymaps 'peep-dired-mode-map
   :states '(normal)
   "j" #'peep-dired-next-file
   "k" #'peep-dired-prev-file
   "RET" #'cpm/peep-dired-open
   "TAB" #'other-window)
  :config
  ;; helper function for opening files in full window
  (defun cpm/peep-dired-open ()
    "open files from peep-dired & clean-up"
    (interactive)
    (peep-dired-kill-buffers-without-window)
    (dired-find-file)
    (delete-other-windows))
  (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4" "pdf" "gif")
        peep-dired-max-size 5242880))



;;;;   Dired Ranger
;; https://github.com/Fuco1/dired-hacks#dired-ranger
;; Very helpful way of copying/moving files
;; Note that to move first you need to copy the file and then go to the target directory and move
(use-package dired-ranger
  :after dired
  :general (:keymaps 'dired-mode-map
            :states '(normal motion)
            "s-c"  'dired-ranger-copy
            "s-m"  'dired-ranger-move
            "s-v"  'dired-ranger-paste))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-dired)
