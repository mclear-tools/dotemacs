;;; Dired
;; I used to use ranger but it was buggy and I can get almost everything I want from
;; dired. See https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer for
;; discussion of how to avoid creating lots of dired buffers.

;;;; Dired Settings
(use-package dired
  :straight nil
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
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  ;; always delete and copy recursively
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-deletion-confirmer 'y-or-n-p)
  (setq dired-dwim-target t)
  ;; allow editing file permissions
  (setq wdired-allow-to-change-permissions t)
  ;; open PDF files in external viewer
  (setq dired-guess-shell-alist-user '(("\.pdf$" . default))))

;;;; Narrow Dired to Match Filter
(use-package dired-narrow
  :general (:keymaps 'dired-mode-map
            "/"  'dired-narrow))

;;;; Dired Sort
(use-package dired-quick-sort
  :general
  (:keymaps 'dired-mode-map
   :states '(normal motion)
   "s" #'hydra-dired-quick-sort/body))

;;;; Dired Colors
(use-package diredfl
  :straight t
  :hook (dired-mode . diredfl-global-mode))

;;;; Dired Plus
(use-package dired+
  :disabled t
  :after dired
  :hook ((dired-mode . diredp--set-up-font-locking))
  ;;(dired-mode . dired-omit-mode))
  :init
  (setq font-lock-maximum-decoration t)
  (setq diredp-omit-files-regexp "\\.?#\\|^\\.$\\|^\\.\\.")
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-toggle-find-file-reuse-dir 1))
  ;; :custom-face
  ;; ;; TODO: change colors to work with bespoke theme
  ;; (diredp-compressed-file-name ((t (:foreground "#00629D"))))
  ;; (diredp-compressed-file-suffix ((t (:foreground "#839496"))))
  ;; (diredp-date-time ((t (:foreground "#9EA0E5"))))
  ;; (diredp-deletion ((t (:background "Red" :foreground "Yellow"))))
  ;; (diredp-dir-heading ((t (:background "#69B7F0" :foreground "#002b36"))))
  ;; (diredp-dir-name ((t (:foreground "#69B7F0"))))
  ;; (diredp-dir-priv ((t (:foreground "#268bd2"))))
  ;; (diredp-exec-priv ((t (:foreground "#990A1b"))))
  ;; (diredp-file-name ((t (:foreground "#2aa198"))))
  ;; (diredp-file-suffix ((t (:foreground "#839496"))))
  ;; (diredp-flag-mark-line ((t (:foreground "#dc322f"))))
  ;; (diredp-no-priv ((t (:foreground "#b58900"))))
  ;; (diredp-number ((t (:foreground "#DEB542"))))
  ;; (diredp-rare-priv ((t (:background "#cb4b16" :foreground "#B4C342"))))
  ;; (diredp-read-priv ((t (:foreground "#F2804F"))))
  ;; (diredp-tagged-autofile-name ((t (:foreground "#328C04113"))))
  ;; (diredp-write-priv ((t (:foreground "#8b2C02")))))


;;;; Peep Dired
(use-package peep-dired
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
        peep-dired-max-size 5242880)
  (setq peep-dired-cleanup-eagerly t))

;;;; Dired Ranger
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

;;; End Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-dired)
