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
   "l" #'dired-find-alternate-file
   "h" #'cpm/dired-updirectory)
  ;; "q" #'quit-window)
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

;;;; Peep Dired
(use-package peep-dired
  :commands (peep-dired)
  :general
  (:keymaps 'dired-mode-map
   :states '(normal)
   "p" #'peep-dired)
  (:keymaps 'peep-dired-mode-map
   :states '(normal)
   "j" #'peep-dired-next-file
   "k" #'peep-dired-prev-file
   "RET" #'cpm/peep-dired-open
   "TAB" #'evil-window-next)
  :config
  ;; helper function for opening files in full window
  (defun cpm/peep-dired-open ()
    "open files from peep-dired & clean-up"
    (interactive)
    (peep-dired-kill-buffers-without-window)
    (dired-find-file)
    (delete-other-windows))
  (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4" "pdf" "gif"))
  (setq peep-dired-cleanup-eagerly nil)
  (setq peep-dired-enable-on-directories t)
  (setq peep-dired-cleanup-on-disable t))


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


;;;; Cycle Dired Buffer
;;https://www.reddit.com/r/emacs/comments/qnthhw/comment/hjiv2uc/?utm_source=share&utm_medium=web2x&context=3
(add-hook 'dired-mode-hook
          (defun cpm-dired-wrap ()
            "Cycle from bottom to top of buffer"
            (make-local-variable 'post-command-hook)
            (add-hook 'post-command-hook
                      (defun cpm-dired-wrap-1 ()
                        ""
                        (if (= 1 (save-excursion
                                   (forward-line)))
                            (goto-line 3))
                        (if (= -1 (save-excursion
                                    (forward-line -1)))
                            (goto-line (count-lines
                                        (point-min)
                                        (point-max))))))))
;;; End Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-dired)
